-module(honey_pool).

-export([
         get/1, get/2, get/3, get/4,
         post/2, post/3, post/4, post/5,
         request/6,
         return_to/3,
         dump_state/0, summarize_state/0
        ]).

-include_lib("kernel/include/logger.hrl").
-include("include/honey_pool.hrl").

-define(USER_AGENT, "honey-pool/0.1").
-define(WORKER, honey_pool_worker).

-define(METHOD_GET, <<"GET">>).
-define(METHOD_POST, <<"POST">>).

-type method() :: binary().
-type req_headers() :: gun:req_headers().
-type resp() :: {ok, {status(), resp_headers(), binary()|no_data}} | {timeout, term()} | {error, Reason::any()}.
-type resp_headers() :: gun:resp_headers().
-type status() :: integer().
-type url() :: string().

-spec get(Url::url()) -> resp().
get(Url) ->
    get(Url, []).

-spec get(Url::url(), Headers::req_headers()|timeout()) -> resp().
get(Url, Headers) when is_list(Headers) ->
    get(Url, Headers, #{});
get(Url, Timeout) ->
    get(Url, [], Timeout).

-spec get(Url::url(), Headers::req_headers(), Opts::gun_req_opts()|timeout()) -> resp().
get(Url, Headers, Opts) when is_map(Opts) ->
    get(Url, Headers, Opts, infinity);
get(Url, Headers, Timeout) ->
    get(Url, Headers, #{}, Timeout).

-spec get(Url::url(), Headers::req_headers(), Opts::gun_req_opts(), Timeout::timeout()) -> resp().
get(Url, Headers, Opts, Timeout) ->
    request(?METHOD_GET, Url, Headers, <<>>, Opts, Timeout).

-spec post(Url::url(), Headers::req_headers()) -> resp().
post(Url, Headers) ->
    post(Url, Headers, <<>>).

-spec post(Url::url(), Headers::req_headers(), Body::binary()) -> resp().
post(Url, Headers, Body) ->
    post(Url, Headers, Body, #{}).

-spec post(Url::url(), Headers::req_headers(), Body::binary(), Opts::gun_req_opts()|timeout()) -> resp().
post(Url, Headers, Body, Opts) when is_map(Opts) ->
    post(Url, Headers, Body, Opts, infinity);
post(Url, Headers, Body, Timeout) ->
    post(Url, Headers, Body, #{}, Timeout).

-spec post(Url::url(), Headers::req_headers(), Body::binary(), Opts::gun_req_opts(), Timeout::timeout()) -> resp().
post(Url, Headers, Body, Opt, Timeout) ->
    request(?METHOD_POST, Url, Headers, Body, Opt, Timeout).

-spec request(
        Method::method(),
        Url::url(),
        Headers::req_headers(),
        Body::binary() | no_data,
        Opts::gun_req_opts(),
        Timeout0::timeout()
       ) -> resp().
request(Method, Url, Headers, Body, Opts, Timeout0) ->
    case honey_pool_uri:parse(Url) of
        {ok, U} ->
            HostInfo = {U#uri.host, U#uri.port, U#uri.transport},
            {Elapsed, Checkout} = timer:tc(
                                    fun checkout/2,
                                    [HostInfo,
                                     Timeout0]
                                   ),
            case Checkout of
                {ok, {ReturnTo, {Pid, _} = Conn}} ->
                    Result = do_request(
                               Conn,
                               Method,
                               U#uri.pathquery,
                               Headers,
                               Body,
                               Opts,
                               next_timeout(Timeout0, Elapsed)
                              ),
                    case Result of
                        {ok, {Status, _, _}} ->
                            ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~.10b", [self(), Pid, Method, Url, Status]),
                            checkin(ReturnTo, HostInfo, Conn);
                        {error, {timeout, _}} = TimeoutErr ->
                            ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~p", [self(), Pid, Method, Url, TimeoutErr]),
                            checkin(ReturnTo, HostInfo, Conn);
                        ReqErr ->
                            ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~p", [self(), Pid, Method, Url, ReqErr]),
                            cleanup(Conn)
                    end,
                    Result;
                {error, Reason} ->
                    {error, {checkout, Reason}}
            end;
        {error, Reason} ->
            {error, {uri, Reason}}
    end.

-spec do_request(
        Conn::conn(),
        Method::method(),
        Path::url(),
        Headers::req_headers(),
        Body::iodata(),
        Opts::gun_req_opts(),
        Timeout0::timeout()
       ) -> resp().
do_request({Pid, MRef}, Method, Path, Headers, Body, Opts, Timeout0) ->
    ReqHeaders = headers(Headers),
    StreamRef = gun:request(Pid, Method, Path, ReqHeaders, Body, Opts),
    {Elapsed, Result}
    = timer:tc(
        fun gun:await/4,
        [Pid, StreamRef, Timeout0, MRef]
       ),
    Resp = case Result of
               {response, fin, Status, RespHeaders} ->
                   {ok, {Status, RespHeaders, no_data}};
               {response, nofin, 200, RespHeaders} ->
                   %% read body only when status code is 200
                   Timeout1 = next_timeout(Timeout0, Elapsed),
                   case gun:await_body(Pid, StreamRef, Timeout1, MRef) of
                       {ok, RespBody} ->
                           {ok, {200, RespHeaders, RespBody}};
                       {error, timeout} ->
                           {error, {timeout, await_body}};
                       {error, Reason} ->
                           {error, {await_body, Reason}}
                   end;
               {response, nofin, Status, RespHeaders} ->
                   %% stream is nofin but skip reading body
                   gun:cancel(Pid, StreamRef),
                   {ok, {Status, RespHeaders, no_data}};
               {error,{stream_error,
                       {stream_error,protocol_error,
                        'Content-length header received in a 204 response. (RFC7230 3.3.2)'}
                      }} ->
                   %% there exist servers that return content-length header and handle such responses as ordinary 204 response
                   gun:cancel(Pid, StreamRef),
                   {ok, {204, [], no_data}};
               {error,{stream_error, _} = Reason} ->
                   gun:cancel(Pid, StreamRef),
                   {error, {await, Reason}};
               {error, timeout} ->
                   {error, {timeout, await}};
               {error, Reason} ->
                   {error, {await, Reason}}
           end,
    ?LOG_DEBUG("(~p) conn: ~p, request: ~p, response: ~p",
               [self(), Pid, {Method, Path, ReqHeaders, Body, Opts}, Resp]),
    Resp.


-spec headers(req_headers()) -> req_headers().
headers(Headers) ->
    [{<<"User-Agent">>, ?USER_AGENT}
     | Headers].

-spec next_timeout(Timeout0::timeout(), MicroSec::integer()) -> timeout().
next_timeout(infinity, _) ->
    infinity;
next_timeout(Timeout0, MicroSec) ->
    Interval = trunc(MicroSec/1000),
    case Timeout0 > Interval of
        true ->
            Timeout0 - Interval;
        _ ->
            0
    end.

-spec checkout(HostInfo::hostinfo(), Timeout0::timeout()) -> {ok, {ReturnTo::pid(), Conn::conn()}} | {error, Reason::term()}.
checkout(HostInfo, Timeout0) ->
    {Elapsed, Result}
    = timer:tc(
        fun wpool:call/4,
        [?WORKER, {checkout, HostInfo}, best_worker, Timeout0]
       ),
    try Result of
        {ok, {await_up, {ReturnTo, Pid}}} ->
            MRef = monitor(process, Pid),
            Timeout1 = next_timeout(Timeout0, Elapsed),
            case gun:await_up(Pid, Timeout1, MRef) of
                {ok, _} ->
                    {ok, {ReturnTo, {Pid, MRef}}};
                {error, timeout} ->
                    %% even on timeout, let gun continue for the future use
                    cancel_await_up(ReturnTo, {Pid, MRef}),
                    {error, {timeout, await_up}};
                {error, Reason} ->
                    cleanup({Pid, MRef}),
                    {error, {await_up, Reason}}
            end;
        {ok, {up, {ReturnTo, Pid}}} ->
            {ok, {ReturnTo, {Pid, monitor(process, Pid)}}};
        {error, Reason} ->
            {error, {pool_checkout, Reason}}
    catch
        _:Err ->
            {error, {checkout, Err}}
    end.

-spec cancel_await_up(ReturnTo::pid(), Conn::conn()) -> ok.
cancel_await_up(ReturnTo, {Pid, MRef}) ->
    demonitor(MRef),
    gun:flush(Pid),
    return_to(ReturnTo, Pid, {cancel_await_up, Pid}).

-spec checkin(ReturnTo::pid(), HostInfo::hostinfo(), Conn::conn()) -> ok.
checkin(ReturnTo, HostInfo, {Pid, MRef}) ->
    demonitor(MRef, [flush]),
    gun:flush(Pid),
    return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}).

-spec cleanup(Conn::conn()) -> ok.
cleanup({Pid, MRef}) ->
    demonitor(MRef, [flush]),
    gun:close(Pid),
    ok.

-spec return_to(ReturnTo::pid(), Pid::pid(), Msg::term()) -> ok.
return_to(ReturnTo, Pid, Msg) ->
    gun:set_owner(Pid, ReturnTo),
    gen_server:cast(ReturnTo, Msg).

-spec dump_state() -> [map()].
dump_state() ->
    [gen_server:call(Proc, dump_state)
     || Proc <- wpool:get_workers(honey_pool_worker)].

-spec summarize_state() -> [map()].
summarize_state() ->
    [summarize_state(S) || S <- dump_state()].

summarize_state(
  #{up_conns := UC,
    await_up_conns := AC,
    host_conns := HC}
 ) ->
    HostConns = lists:foldl(
                  fun({Host, Conns}, Acc) ->
                          [{Host, length(Conns)} | Acc]
                  end, [], maps:to_list(HC)),
    #{total_conns => #{up => maps:size(UC),
                       await_up => maps:size(AC)},
      host_conns => lists:sort(
                      fun({_, A}, {_, B}) -> A > B end,
                      HostConns
                     )}.
