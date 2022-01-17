-module(honey_pool).

-export([
         get/1, get/2, get/3, get/4,
         post/2, post/3, post/4, post/5,
         request/6,
         dump_state/0, summarize_state/0
        ]).

-export([
         checkout/2,
         checkin/2
        ]).

-include_lib("kernel/include/logger.hrl").
-include("include/honey_pool.hrl").

-define(USER_AGENT, "honey-pool/0.1").
-define(WORKER, honey_pool_worker).
-define(DEFAULT_REQUEST_TIMEOUT, 1000). %% msec

-define(METHOD_GET, <<"GET">>).
-define(METHOD_POST, <<"POST">>).

-type gun_opts() :: gun:opts().
-type method() :: binary().
-type req_headers() :: gun:req_headers().
-type resp() :: {ok, {status(), resp_headers(), binary() | no_data}} | {error, Reason::any()}.
-type resp_headers() :: gun:resp_headers().
-type status() :: integer().
-type url() :: string().

-spec get(Url::url()) -> resp().
get(Url) ->
    get(Url, []).

-spec get(Url::url(), Headers::req_headers()) -> resp().
get(Url, Headers) ->
    get(Url, Headers, #{}).

-spec get(Url::url(), Headers::req_headers(), Opts::gun_opts()|timeout()) -> resp().
get(Url, Headers, Opts) when is_map(Opts) ->
    get(Url, Headers, Opts, ?DEFAULT_REQUEST_TIMEOUT);
get(Url, Headers, Timeout) ->
    get(Url, Headers, #{}, Timeout).

-spec get(Url::url(), Headers::req_headers(), Opts::gun_opts(), Timeout::timeout()) -> resp().
get(Url, Headers, Opts, Timeout) ->
    request(?METHOD_GET, Url, Headers, <<>>, Opts, Timeout).

-spec post(Url::url(), Headers::req_headers()) -> resp().
post(Url, Headers) ->
    post(Url, Headers, <<>>).

-spec post(Url::url(), Headers::req_headers(), Body::binary()) -> resp().
post(Url, Headers, Body) ->
    post(Url, Headers, Body, #{}).

-spec post(Url::url(), Headers::req_headers(), Body::binary(), Opts::gun_opts()|timeout()) -> resp().
post(Url, Headers, Body, Opts) when is_map(Opts) ->
    post(Url, Headers, Body, Opts, ?DEFAULT_REQUEST_TIMEOUT);
post(Url, Headers, Body, Timeout) ->
    post(Url, Headers, Body, #{}, Timeout).

-spec post(Url::url(), Headers::req_headers(), Body::binary(), Opts::gun_opts(), Timeout::timeout()) -> resp().
post(Url, Headers, Body, Opt, Timeout) ->
    request(?METHOD_POST, Url, Headers, Body, Opt, Timeout).

-spec request(
        Method::method(),
        Url::url(),
        Headers::req_headers(),
        Body::binary() | no_data,
        Opts::gun_opts(),
        Timeout0::timeout()
       ) -> resp().
request(Method, Url, Headers, Body, Opts, Timeout0) ->
    case honey_pool_uri:parse(Url) of
        {ok, U} ->
            {Elapsed, Checkout} = timer:tc(
                                    fun checkout/2,
                                    [{U#uri.host, U#uri.port, U#uri.transport},
                                     Timeout0]),
            case Checkout of
                {ok, {ReturnTo, Pid}} ->
                    Result = do_request(
                               Pid,
                               Method,
                               U#uri.pathquery,
                               Headers,
                               Body,
                               Opts,
                               next_timeout(Timeout0, Elapsed)),
                    case Result of
                        {ok, {Status, _, _}} ->
                            ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~.10b", [self(), Pid, Method, Url, Status]);
                        ReqErr ->
                            ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~p", [self(), Pid, Method, Url, ReqErr])
                    end,
                    checkin(ReturnTo, Pid),
                    Result;
                {error, Reason} ->
                    {error, {checkout_error, Reason}}
            end;
        {error, Reason} ->
            {error, {url_error, Reason}}
    end.

-spec do_request(
        Pid::pid(),
        Method::method(),
        Path::url(),
        Headers::req_headers(),
        Body::iodata(),
        Opts::gun:req_opts(),
        Timeout0::timeout()
       ) -> resp().
do_request(Pid, Method, Path, Headers, Body, Opts, Timeout0) ->
    ReqHeaders = headers(Headers),
    StreamRef = gun:request(Pid, Method, Path, ReqHeaders, Body, Opts),
    Resp = case timer:tc(
                  fun gun:await/3,
                  [Pid, StreamRef, Timeout0]) of
               {_Elapsed,
                {response, fin, Status, RespHeaders}} ->
                   {ok, {Status, RespHeaders, no_data}};
               {Elapsed,
                {response, nofin, 200, RespHeaders}} ->
                   case gun:await_body(
                          Pid,
                          StreamRef,
                          next_timeout(Timeout0, Elapsed)) of
                       {ok, RespBody} ->
                           {ok, {200, RespHeaders, RespBody}};
                       Err ->
                           Err
                   end;
               {_Elapsed,
                {response, nofin, Status, RespHeaders}} ->
                   {ok, {Status, RespHeaders, no_data}};
               {_Elapsed,
                {error,{stream_error,
                        {stream_error,protocol_error,
                         'Content-length header received in a 204 response. (RFC7230 3.3.2)'}
                       }}} ->
                   %% there exist servers that return content-length header
                   {ok, {204, [], no_data}};
               {_Elapsed,
                {error, Reason}} ->
                   {error, Reason};
               {_Elapsed, V} ->
                   gun:cancel(Pid, StreamRef),
                   {error, {unsupported, V}}
           end,
    gun:flush(StreamRef),
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

-spec checkout(
        HostInfo::hostinfo(),
        Timeout0::timeout()
       ) -> {ok, {ReturnTo::pid(), Pid::pid()}} | {error, Reason::term()}.
checkout(HostInfo, Timeout0) ->
    try timer:tc(
          fun wpool:call/4,
          [?WORKER,
           {checkout, HostInfo},
           available_worker,
           Timeout0]) of
        {_Elapsed,
         {ReturnTo, {ok, Pid}}} ->
            {ok, {ReturnTo, Pid}};
        {Elapsed,
         {ReturnTo, {awaiting, Pid}}} ->
            Timeout1 = next_timeout(Timeout0, Elapsed),
            receive
                {ok, _} ->
                    {ok, {ReturnTo, Pid}};
                Err ->
                    %% received unexpected -> maybe next time
                    checkin(ReturnTo, Pid),
                    {error, Err}
            after
                Timeout1 ->
                    %% timeout exceeded -> maybe next time
                    checkin(ReturnTo, Pid),
                    {error, await_timeout}
            end
    catch
        _:Err ->
            {error, Err}
    end.

-spec checkin(ReturnTo::pid(), Pid::pid()) -> ok.
checkin(ReturnTo, Pid) ->
    gun:flush(Pid), %% flush before check-in
    ReturnTo ! {checkin, Pid},
    ok.

-spec dump_state() -> [map()].
dump_state() ->
    [gen_server:call(Proc, dump_state)
     || Proc <- wpool:get_workers(honey_pool_worker)].

-spec summarize_state() -> [map()].
summarize_state() ->
    [summarize_state(S) || S <- dump_state()].

summarize_state(#{host_conns := HC, conn_host := CH}) ->
    #{total_conns => maps:size(CH),
      host_conns => lists:foldl(
                      fun({Host, Bag}, Acc) ->
                              Acc#{
                                Host => #{
                                          available_conns =>
                                          case maps:find(available_conns, Bag) of
                                              {ok, V} ->
                                                  length(V);
                                              _ ->
                                                  0
                                          end,
                                          in_use_conns =>
                                          case maps:find(in_use_conns, Bag) of
                                              {ok, V} ->
                                                  length(V);
                                              _ ->
                                                  0
                                          end,
                                          awaiting_conns =>
                                          case maps:find(awaiting_conns, Bag) of
                                              {ok, V} ->
                                                  length(V);
                                              _ ->
                                                  0
                                          end
                                         }}
                      end,
                      #{},
                      maps:to_list(HC))
     }.
