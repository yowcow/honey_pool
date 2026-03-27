-module(honey_pool).

-export([get/1, get/2, get/3, get/4,
         post/2, post/3, post/4, post/5,
         request/6,
         async_post/5,
         async_request/6,
         start_async_checkout/3,
         fire_request/7,
         parse_url/1,
         checkin/3,
         cleanup/1,
         return_to/3,
         dump_state/0,
         summarize_state/0]).

-include_lib("kernel/include/logger.hrl").

-include("honey_pool.hrl").

-define(USER_AGENT,  "honey-pool/0.1").
-define(WORKER,      honey_pool_worker).
-define(METHOD_GET,  <<"GET">>).
-define(METHOD_POST, <<"POST">>).

-type method() :: binary().
-type req_headers() :: gun:req_headers().
-type resp() ::
        {ok, {status(), resp_headers(), binary() | no_data}} |
        {error,
         {await, term()} |
         {await_body, term()} |
         {checkout,
          {limit, max_conns | max_pending_conns} |
          {timeout, term()} |
          {await_up, term()} |
          {pool_checkout, term()} |
          {checkout, term()}} |
         {uri, term()} |
         {timeout, term()}}.
-type resp_headers() :: [{binary(), binary()}].
-type status() :: integer().
-type url() :: string().


%% @doc Performs a GET request.
-spec get(Url :: url()) -> resp().
get(Url) ->
    get(Url, []).


%% @doc Performs a GET request with the given headers and timeout.
-spec get(Url :: url(), Headers :: req_headers() | timeout()) -> resp().
get(Url, Headers) when is_list(Headers) ->
    get(Url, Headers, #{});
get(Url, Timeout) ->
    get(Url, [], Timeout).


%% @doc Performs a GET request with the given headers, options and timeout.
-spec get(Url :: url(), Headers :: req_headers(), Opts :: gun_req_opts() | timeout()) ->
          resp().
get(Url, Headers, Opts) when is_map(Opts) ->
    get(Url, Headers, Opts, infinity);
get(Url, Headers, Timeout) ->
    get(Url, Headers, #{}, Timeout).


%% @doc Performs a GET request with the given headers, options, and timeout.
-spec get(Url :: url(),
          Headers :: req_headers(),
          Opts :: gun_req_opts(),
          Timeout :: timeout()) ->
          resp().
get(Url, Headers, Opts, Timeout) ->
    request(?METHOD_GET, Url, Headers, <<>>, Opts, Timeout).


%% @doc Performs a POST request with the given headers.
-spec post(Url :: url(), Headers :: req_headers()) -> resp().
post(Url, Headers) ->
    post(Url, Headers, <<>>).


%% @doc Performs a POST request with the given headers and body.
-spec post(Url :: url(), Headers :: req_headers(), Body :: binary()) -> resp().
post(Url, Headers, Body) ->
    post(Url, Headers, Body, #{}).


%% @doc Performs a POST request with the given headers, body, and either options (map) or timeout.
-spec post(Url :: url(),
           Headers :: req_headers(),
           Body :: binary(),
           Opts :: gun_req_opts() | timeout()) ->
          resp().
post(Url, Headers, Body, Opts) when is_map(Opts) ->
    post(Url, Headers, Body, Opts, infinity);
post(Url, Headers, Body, Timeout) ->
    post(Url, Headers, Body, #{}, Timeout).


%% @doc Performs a POST request with the given headers, body, options, and timeout.
-spec post(Url :: url(),
           Headers :: req_headers(),
           Body :: binary(),
           Opts :: gun_req_opts(),
           Timeout :: timeout()) ->
          resp().
post(Url, Headers, Body, Opts, Timeout) ->
    request(?METHOD_POST, Url, Headers, Body, Opts, Timeout).


%% @doc Performs an HTTP request.
%% This is the main entry point for making requests. It parses the URL,
%% checks out a connection from the pool, sends the request, and handles the response.
-spec request(Method :: method(),
              Url :: url(),
              Headers :: req_headers(),
              Body :: binary() | no_data,
              Opts :: gun_req_opts(),
              Timeout :: timeout()) ->
          resp().
request(Method, Url, Headers, Body, Opts, Timeout) ->
    case honey_pool_uri:parse(Url) of
        {ok, U} ->
            HostInfo = {U#uri.host, U#uri.port, U#uri.transport},
            {Elapsed, Checkout} = timer:tc(fun checkout/2, [HostInfo, Timeout]),
            case Checkout of
                {ok, {ReturnTo, Conn}} ->
                    Result =
                        do_request(Conn,
                                   Method,
                                   U#uri.pathquery,
                                   Headers,
                                   Body,
                                   Opts,
                                   next_timeout(Timeout, Elapsed)),
                    handle_request_result(Result, ReturnTo, HostInfo, Conn, Method, Url);
                {error, Reason} ->
                    {error, {checkout, Reason}}
            end;
        {error, Reason} ->
            {error, {uri, Reason}}
    end.


%% @doc Performs an async POST request.
%% Fires the HTTP request and returns immediately. The calling process will receive
%% gun_response / gun_data messages when the response arrives.
-spec async_post(Url :: url(),
                 Headers :: req_headers(),
                 Body :: binary(),
                 Opts :: gun_req_opts(),
                 Timeout :: timeout()) ->
          {ok,
           #{stream_ref := reference(),
             return_to := pid(),
             host_info := hostinfo(),
             conn := conn()}} |
          {error, term()}.
async_post(Url, Headers, Body, Opts, Timeout) ->
    async_request(?METHOD_POST, Url, Headers, Body, Opts, Timeout).


%% @doc Performs an async HTTP request.
%% Fires the HTTP request and returns immediately. The calling process will receive
%% gun_response / gun_data messages when the response arrives.
-spec async_request(Method :: method(),
                    Url :: url(),
                    Headers :: req_headers(),
                    Body :: binary() | no_data,
                    Opts :: gun_req_opts(),
                    Timeout :: timeout()) ->
          {ok,
           #{stream_ref := reference(),
             return_to := pid(),
             host_info := hostinfo(),
             conn := conn()}} |
          {error, term()}.
async_request(Method, Url, Headers, Body, Opts, Timeout) ->
    case honey_pool_uri:parse(Url) of
        {ok, U} ->
            HostInfo = {U#uri.host, U#uri.port, U#uri.transport},
            case checkout(HostInfo, Timeout) of
                {ok, {ReturnTo, {Pid, _MRef} = Conn}} ->
                    ReqHeaders = headers(Headers),
                    StreamRef = gun:request(Pid, Method, U#uri.pathquery, ReqHeaders, Body, Opts),
                    {ok,
                     #{stream_ref => StreamRef,
                       return_to => ReturnTo,
                       host_info => HostInfo,
                       conn => Conn}};
                {error, Reason} ->
                    {error, {checkout, Reason}}
            end;
        {error, Reason} ->
            {error, {uri, Reason}}
    end.


%% @private
%% @doc Handles the result of a request, checking the connection back in or cleaning it up.
handle_request_result({ok, {Status, _, _}} = Result,
                      ReturnTo,
                      HostInfo,
                      {Pid, _} = Conn,
                      Method,
                      Url) ->
    ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~.10b", [self(), Pid, Method, Url, Status]),
    checkin(ReturnTo, HostInfo, Conn),
    Result;
handle_request_result(Err, _ReturnTo, _HostInfo, {Pid, _} = Conn, Method, Url) ->
    ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p -> ~p", [self(), Pid, Method, Url, Err]),
    cleanup(Conn),
    Err.


%% @private
%% @doc Executes the actual HTTP request using gun.
-spec do_request(Conn :: conn(),
                 Method :: method(),
                 Path :: url(),
                 Headers :: req_headers(),
                 Body :: iodata(),
                 Opts :: gun_req_opts(),
                 Timeout :: timeout()) ->
          resp().
do_request({Pid, MRef}, Method, Path, Headers, Body, Opts, Timeout) ->
    ReqHeaders = headers(Headers),
    StreamRef = gun:request(Pid, Method, Path, ReqHeaders, Body, Opts),
    {Elapsed, Result} = timer:tc(fun gun:await/4, [Pid, StreamRef, Timeout, MRef]),
    Resp =
        case Result of
            {response, fin, Status, RespHeaders} ->
                {ok, {Status, RespHeaders, no_data}};
            {response, nofin, 200, RespHeaders} ->
                %% read body only when status code is 200
                TimeoutRemaining = next_timeout(Timeout, Elapsed),
                case gun:await_body(Pid, StreamRef, TimeoutRemaining, MRef) of
                    {ok, RespBody} ->
                        {ok, {200, RespHeaders, RespBody}};
                    {error, timeout} ->
                        {error, {timeout, await_body}};
                    {error, Reason} ->
                        {error, {await_body, Reason}}
                end;
            {response, nofin, Status, RespHeaders} ->
                %% For non-200 nofin responses, we cancel the stream and do not read the body.
                %% This is to prevent blocking on potentially large or infinite streams
                %% when the status indicates an error or redirection.
                ?LOG_DEBUG("(~p) (conn: ~p) cancelling stream ~p for non-200 nofin response "
                           "(Status: ~p)",
                           [self(), Pid, StreamRef, Status]),
                gun:cancel(Pid, StreamRef),
                {ok, {Status, RespHeaders, no_data}};
            {error,
             {stream_error,
              {stream_error,
               protocol_error,
               'Content-length header received in a 204 response. (RFC7230 3.3.2)'}}} ->
                %% there exist servers that return content-length header and handle such responses as ordinary 204 response
                {ok, {204, [], no_data}};
            {error, {stream_error, _} = Reason} ->
                {error, {await, Reason}};
            {error, timeout} ->
                {error, {timeout, await}};
            {error, Reason} ->
                {error, {await, Reason}}
        end,
    ?LOG_DEBUG("(~p) conn: ~p, request: ~p, response: ~p",
               [self(), Pid, {Method, Path, ReqHeaders, Body, Opts}, Resp]),
    Resp.


%% @private
%% @doc Prepends a User-Agent header to the request headers.
-spec headers(req_headers()) -> req_headers().
headers(Headers) ->
    [{<<"User-Agent">>, ?USER_AGENT} | Headers].


%% @private
%% @doc Calculates the remaining timeout value.
-spec next_timeout(Timeout :: timeout(), MicroSec :: integer()) -> timeout().
next_timeout(infinity, _) ->
    infinity;
next_timeout(Timeout, MicroSec) ->
    Interval = trunc(MicroSec / 1000),
    case Timeout > Interval of
        true ->
            Timeout - Interval;
        _ ->
            0
    end.


%% @doc Starts an async checkout for an already-parsed HostInfo.
%% CallerPid will receive:
%%   {checkout_result, Ref, {ok, {ReturnTo, HostInfo, GunPid}}} on success
%%   {checkout_result, Ref, {error, Reason}}                    on failure
-spec start_async_checkout(HostInfo :: hostinfo(), CallerPid :: pid(), Ref :: reference()) -> ok.
start_async_checkout(HostInfo, CallerPid, Ref) ->
    wpool:cast(?WORKER, {async_checkout, HostInfo, CallerPid, Ref}, worker_strategy(HostInfo)).


%% @doc Fires an HTTP request on an already checked-out gun connection.
%% Url is used only for debug logging; PathQuery is what is sent over the wire.
%% Returns the gun stream reference.
-spec fire_request(Method :: method(),
                   Url :: url(),
                   PathQuery :: string(),
                   Headers :: req_headers(),
                   Body :: binary() | no_data,
                   Opts :: gun_req_opts(),
                   GunPid :: pid()) -> reference().
fire_request(Method, Url, PathQuery, Headers, Body, Opts, GunPid) ->
    ReqHeaders = headers(Headers),
    ?LOG_DEBUG("(~p) (conn: ~p) ~p ~p", [self(), GunPid, Method, Url]),
    gun:request(GunPid, Method, PathQuery, ReqHeaders, Body, Opts).


%% @doc Parses a URL and returns HostInfo and PathQuery.
-spec parse_url(Url :: url()) ->
          {ok, #{host_info := hostinfo(), path_query := string()}} | {error, term()}.
parse_url(Url) ->
    case honey_pool_uri:parse(Url) of
        {ok, U} ->
            {ok, #{
               host_info => {U#uri.host, U#uri.port, U#uri.transport},
               path_query => U#uri.pathquery
              }};
        {error, Reason} ->
            {error, Reason}
    end.


%% @private
%% @doc Returns the wpool worker routing strategy.
%% Defaults to `best_worker` for backward compatibility.
%% Set `{honey_pool, worker_strategy, hash_worker}` in sys.config to use
%% `hash_worker` instead, which improves connection pool affinity by routing
%% all operations for the same host to the same worker.
-spec worker_strategy(HostInfo :: hostinfo()) -> wpool:strategy().
worker_strategy(HostInfo) ->
    case application:get_env(honey_pool, worker_strategy, best_worker) of
        hash_worker -> {hash_worker, HostInfo};
        best_worker -> best_worker
    end.


%% @private
%% @doc Checks out a connection from the worker pool.
-spec checkout(HostInfo :: hostinfo(), Timeout :: timeout()) ->
          {ok, {ReturnTo :: pid(), Conn :: conn()}} | {error, Reason :: term()}.
checkout(HostInfo, Timeout) ->
    T0 = erlang:monotonic_time(microsecond),
    Result =
        try
            wpool:call(?WORKER, {checkout, HostInfo}, worker_strategy(HostInfo), Timeout)
        catch
            exit:{timeout, _} -> timeout
        end,
    Elapsed = erlang:monotonic_time(microsecond) - T0,
    handle_checkout_result(Result, Timeout, Elapsed).


%% @private
-spec handle_checkout_result(Result :: term(), Timeout :: timeout(), ElapsedMicro :: integer()) ->
          {ok, {ReturnTo :: pid(), Conn :: conn()}} | {error, Reason :: term()}.
handle_checkout_result({ok, {await_up, {ReturnTo, Pid}}}, Timeout, Elapsed) ->
    MRef = monitor(process, Pid),
    TimeoutRemaining = next_timeout(Timeout, Elapsed),
    case gun:await_up(Pid, TimeoutRemaining, MRef) of
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
handle_checkout_result({ok, {up, {ReturnTo, Pid}}}, _Timeout, _Elapsed) ->
    {ok, {ReturnTo, {Pid, monitor(process, Pid)}}};
handle_checkout_result({error, {limit, _} = Reason}, _Timeout, _Elapsed) ->
    %% Unwrap limit errors for direct pattern matching
    {error, Reason};
handle_checkout_result({error, Reason}, _Timeout, _Elapsed) ->
    {error, {pool_checkout, Reason}};
handle_checkout_result(timeout, _Timeout, _Elapsed) ->
    {error, {timeout, checkout}}.


%% @private
%% @doc Cancels a pending connection attempt.
-spec cancel_await_up(ReturnTo :: pid(), Conn :: conn()) -> ok.
cancel_await_up(ReturnTo, {Pid, MRef}) ->
    demonitor(MRef, [flush]),
    return_to(ReturnTo, Pid, {cancel_await_up, Pid}).


%% @doc Returns a connection to the pool.
-spec checkin(ReturnTo :: pid(), HostInfo :: hostinfo(), Conn :: conn()) -> ok.
checkin(ReturnTo, HostInfo, {Pid, MRef}) ->
    demonitor(MRef, [flush]),
    return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}).


%% @doc Cleans up a connection by closing it.
-spec cleanup(Conn :: conn()) -> ok.
cleanup({Pid, MRef}) ->
    demonitor(MRef, [flush]),
    gun:close(Pid),
    ok.


%% @private
%% @doc Returns a message to the worker process.
-spec return_to(ReturnTo :: pid(), Pid :: pid(), Msg :: term()) -> ok.
return_to(ReturnTo, Pid, Msg) ->
    gun:flush(Pid),
    gen_server:cast(ReturnTo, Msg).


%% @doc Dumps the state of all honey_pool_worker processes.
%% This is useful for debugging and monitoring.
-spec dump_state() -> [map()].
dump_state() ->
    [ gen_server:call(Proc, dump_state) || Proc <- wpool:get_workers(honey_pool_worker) ].


%% @doc Summarizes the state of all honey_pool_worker processes.
%% Provides a more concise overview of the connection pool status.
-spec summarize_state() -> [map()].
summarize_state() ->
    [ summarize_state(S) || S <- dump_state() ].


%% @private
%% @doc Summarizes the state of a single worker process.
summarize_state(#{
                  await_up_conns := AC,
                  checked_in_conns := IC,
                  checked_out_conns := OC,
                  pool_conns := PC,
                  cur_conns := CC,
                  cur_pending_conns := CPC,
                  max_conns := MC,
                  max_pending_conns := MPC
                 }) ->
    PoolConns =
        lists:foldl(fun({Host, Pids}, Acc) -> [{Host, length(Pids)} | Acc] end,
                    [],
                    maps:to_list(PC)),
    #{
      total_conns =>
          #{
            await_up => maps:size(AC),
            checked_in => maps:size(IC),
            checked_out => maps:size(OC),
            current => CC,
            current_pending => CPC,
            max => MC,
            max_pending => MPC
           },
      pool_conns => lists:sort(fun({_, A}, {_, B}) -> A > B end, PoolConns)
     }.
