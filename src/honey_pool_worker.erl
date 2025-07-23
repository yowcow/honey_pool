-module(honey_pool_worker).

-behavior(gen_server).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-include("honey_pool.hrl").

-define(DEFAULT_OPTS,
        #{
          retry => 0,
          connect_timeout => 1000,
          http_opts =>
              #{  %% 30 sec
                keepalive => 30 * 1000
               },
          http2_opts =>
              #{  %% 30 sec
                keepalive => 30 * 1000
               }
         }).
-define(ETS_TABLE, honey_pool).

-record(conn, {
          hostinfo :: hostinfo(),
          state :: await_up | checked_out | checked_in,
          requester :: requester(),
          monitor_ref :: monitor_ref(),
          timer_ref :: timer_ref()
         }).

-type requester() :: pid() | undefined.
-type timer_ref() :: reference() | undefined.

%% @doc Callback functions for the honey_pool_worker gen_server.


%% @doc Initializes the worker.
%% Creates an ETS table to store connection states and merges
%% default options with the provided arguments.
-spec init(Args :: list()) -> {ok, state()}.
init(Args) ->
    Opts = maps:from_list(Args),
    {ok,
     #state{
       tabid = ets:new(?ETS_TABLE, [set]),
       gun_opts = maps:merge(?DEFAULT_OPTS, maps:get(gun_opts, Opts, #{})),
       idle_timeout = maps:get(idle_timeout, Opts, infinity),
       await_up_timeout = maps:get(await_up_timeout, Opts, 5000)
      }}.


%% @doc Terminates the worker.
%% Closes all active gun connections before termination.
-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("(~p) terminating worker: ~p", [self(), Reason]),
    Pids =
        ets:foldl(fun(V, Acc) ->
                          case V of
                              {{pid, Pid}, _} -> [Pid | Acc];
                              _ -> Acc
                          end
                  end,
                  [],
                  State#state.tabid),
    lists:map(fun(Pid) -> gun:close(Pid) end, Pids),
    ok.


%% @doc Handles call messages.
%% - `{checkout, HostInfo}`: Checks out a connection for the given host.
%% - `dump_state`: Dumps the current state of the worker.
-spec handle_call(Req :: term(), From :: {pid(), term()}, State :: state()) ->
          {reply, term(), state()}.
handle_call({checkout, HostInfo} = Req, {Requester, _}, State) ->
    Result = conn_checkout(HostInfo, Requester, State),
    ?LOG_DEBUG("(~p) handle_call (~p) -> ~p", [self(), Req, Result]),
    {reply, Result, State};
handle_call(dump_state, _From, State) ->
    {reply, dump_state(State), State};
handle_call(Req, From, State) ->
    ?LOG_WARNING("(~p) unhandled call (~p, ~p, ~p)", [self(), Req, From, State]),
    {reply, {error, no_handler}, State}.


%% @doc Handles cast messages.
%% - `{checkin, HostInfo, Pid}`: Checks a connection back into the pool.
%% - `{cancel_await_up, Pid}`: Cancels a pending connection attempt.
-spec handle_cast(Req :: term(), State :: state()) -> {noreply, state()}.
handle_cast({checkin, HostInfo, Pid} = Req, State) ->
    Result = conn_checkin(HostInfo, Pid, State),
    ?LOG_DEBUG("(~p) handle_cast (~p) -> ~p", [self(), Req, Result]),
    {noreply, State};
handle_cast({cancel_await_up, Pid} = Req, State) ->
    Result = conn_cancel_await_up(Pid, State),
    ?LOG_DEBUG("(~p) handle_cast (~p) -> ~p", [self(), Req, Result]),
    {noreply, State};
handle_cast(Req, State) ->
    ?LOG_WARNING("(~p) unhandled cast (~p, ~p)", [self(), Req, State]),
    {noreply, State}.


%% @doc Handles info messages.
%% - `{idle_timeout, Pid}`: Closes an idle connection.
%% - `{gun_up, Pid, Protocol}`: Handles a successful connection upgrade.
%% - `{gun_down, Pid, ...}`: Handles a connection down event.
%% - `{'DOWN', MRef, ...}`: Handles a process down event.
-spec handle_info(Req :: term(), State :: state()) -> {noreply, state()}.
handle_info({idle_timeout, Pid} = Req, State) ->
    Result = conn_down(Pid, State),
    gun:close(Pid),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, State};
handle_info({gun_up, Pid, Protocol} = Req, State) ->
    Result = conn_up(Pid, Protocol, State),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, State};
handle_info({gun_down, Pid, _Protocol, _Reason, _} = Req, State) ->
    Result = conn_down(Pid, State),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, State};
handle_info({'DOWN', MRef, _, Pid, _Reason} = Req, State) ->
    demonitor(MRef, [flush]),
    Result = conn_down(Pid, State),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, State};
handle_info(Req, State) ->
    ?LOG_WARNING("(~p) unhandled info (~p, ~p)", [self(), Req, State]),
    {noreply, State}.


%%
%% private funs
%%


%% @private
%% @doc Starts a timer to detect idle connections.
-spec idle_timer(Pid :: pid(), Timeout :: timeout()) -> timer_ref().
idle_timer(_Pid, infinity) ->
    undefined;
idle_timer(Pid, Timeout) ->
    erlang:send_after(Timeout, self(), {idle_timeout, Pid}).


%% @private
%% @doc Cancels an idle timer.
-spec cancel_idle_timer(TRef :: timer_ref()) -> ok.
cancel_idle_timer(undefined) ->
    ok;
cancel_idle_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.


%% @private
%% @doc Checks out a connection from the pool or opens a new one.
-spec conn_checkout(HostInfo :: hostinfo(), Requester :: pid(), State :: state()) ->
          {ok, {up | await_up, {ReturnTo :: pid(), Pid :: pid()}}} |
          {error, Reason :: term()}.
conn_checkout(HostInfo, Requester, #state{tabid = TabId} = State) ->
    Result =
        case ets:lookup(TabId, {pool, HostInfo}) of
            [] ->
                conn_open(HostInfo, Requester, State);
            [{_, []}] ->
                conn_open(HostInfo, Requester, State);
            [{_, Pids}] ->
                case checkout_from_pool(HostInfo, Requester, Pids, State) of
                    no_available_worker ->
                        conn_open(HostInfo, Requester, State);
                    Other ->
                        Other
                end
        end,
    case Result of
        {ok, {Status, ConnPid}} ->
            %%gun:set_owner(ConnPid, Requester),
            {ok, {Status, {self(), ConnPid}}};
        _ ->
            Result
    end.


%% @private
%% @doc Tries to check out an existing connection from the pool.
-spec checkout_from_pool(hostinfo(), pid(), [pid()], state()) ->
          {ok, {up, pid()}} | no_available_worker.
checkout_from_pool(_HostInfo, _Requester, [], _State) ->
    no_available_worker;
checkout_from_pool(HostInfo, Requester, [Pid | Pids], #state{tabid = TabId} = State) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [] ->
            %% Pid not in the table, try next one
            checkout_from_pool(HostInfo, Requester, Pids, State);
        [{_, Conn}] ->
            ets:insert(TabId, {{pool, HostInfo}, Pids}),
            cancel_idle_timer(Conn#conn.timer_ref),
            ets:insert(TabId, {{pid, Pid}, Conn#conn{state = checked_out, timer_ref = undefined}}),
            {ok, {up, Pid}}
    end.


%% @private
%% @doc Opens a new gun connection.
-spec conn_open(HostInfo :: hostinfo(), Requester :: pid(), State :: state()) ->
          {ok, {await_up, Pid :: pid()}} | {error, Reason :: term()}.
conn_open({Host, Port, Transport},
          Requester,
          #state{gun_opts = GunOpts, tabid = TabId}) ->
    GunOptsWithTransport = GunOpts#{transport => Transport},
    HostInfo = {Host, Port, Transport},
    case gun:open(Host, Port, GunOptsWithTransport) of
        {ok, Pid} ->
            ets:insert(TabId,
                       {{pid, Pid},
                        #conn{
                          hostinfo = HostInfo,
                          state = await_up,
                          requester = Requester,
                          monitor_ref = monitor(process, Pid)
                         }}),
            {ok, {await_up, Pid}};
        {error, Reason} ->
            {error, {gun_open, Reason}}
    end.


%% @private
%% @doc Cancels a pending connection that is waiting for `gun_up`.
-spec conn_cancel_await_up(Pid :: pid(), State :: state()) -> ok.
conn_cancel_await_up(Pid, #state{tabid = TabId, await_up_timeout = AwaitUpTimeout}) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn = #conn{timer_ref = TRef}}] ->
            cancel_idle_timer(TRef),
            ets:insert(TabId,
                       {{pid, Pid},
                        Conn#conn{
                          requester = undefined,
                          timer_ref = idle_timer(Pid, AwaitUpTimeout)
                         }}),
            ok;
        _ ->
            ok
    end.


%% @private
%% @doc Checks a connection back into the pool.
-spec conn_checkin(HostInfo :: hostinfo(), Pid :: pid(), State :: state()) ->
          {ok, term()}.
conn_checkin(HostInfo, Pid, #state{tabid = TabId, idle_timeout = IdleTimeout}) ->
    Conn =
        case ets:lookup(TabId, {pid, Pid}) of
            [] ->
                #conn{
                  hostinfo = HostInfo,
                  state = checked_in,
                  monitor_ref = monitor(process, Pid),
                  timer_ref = idle_timer(Pid, IdleTimeout)
                 };
            [{_, OldConn}] ->
                cancel_idle_timer(OldConn#conn.timer_ref),
                OldConn#conn{state = checked_in, timer_ref = idle_timer(Pid, IdleTimeout)}
        end,
    %% insert to the connection pid table
    ets:insert(TabId, {{pid, Pid}, Conn}),
    PidsToPool =
        case ets:lookup(TabId, {pool, HostInfo}) of
            [] ->
                [Pid];
            [{_, Pids}] ->
                [Pid | Pids]
        end,
    %% insert to the host connection pool
    ets:insert(TabId, {{pool, HostInfo}, PidsToPool}),
    {ok, {HostInfo, Pid}}.


%% @private
%% @doc Handles the `gun_up` message, indicating a connection is ready.
-spec conn_up(pid(), tcp | tls, state()) -> {ok, term()}.
conn_up(Pid, Protocol, #state{tabid = TabId} = State) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn}] ->
            case Conn#conn.requester of
                undefined ->
                    %% requester has canceled -> just keep pid in the pool
                    cancel_idle_timer(Conn#conn.timer_ref),
                    conn_checkin(Conn#conn.hostinfo, Pid, State);
                Requester ->
                    %% notify requester and tag that conn is checked out
                    cancel_idle_timer(Conn#conn.timer_ref),
                    Requester ! {gun_up, Pid, Protocol},
                    ets:insert(TabId,
                               {{pid, Pid}, Conn#conn{state = checked_out, requester = undefined}}),
                    {ok, {Conn#conn.hostinfo, Pid}}
            end;
        _ ->
            %% arrived out of the blue -> just keep pid in the pool
            #{
              origin_host := Host,
              origin_port := Port,
              transport := Transport
             } =
                gun:info(Pid),
            HostInfo = {Host, Port, Transport},
            conn_checkin(HostInfo, Pid, State)
    end.


%% @private
%% @doc Handles the `gun_down` message, indicating a connection has been lost.
-spec conn_down(pid(), state()) -> {ok, term()}.
conn_down(Pid, #state{tabid = TabId}) ->
    case ets:take(TabId, {pid, Pid}) of
        [{_, Conn}] ->
            HostInfo = Conn#conn.hostinfo,
            demonitor(Conn#conn.monitor_ref, [flush]),
            cancel_idle_timer(Conn#conn.timer_ref),
            case ets:lookup(TabId, {pool, HostInfo}) of
                [{_, Pids}] ->
                    ets:insert(TabId, {{pool, HostInfo}, [ P || P <- Pids, P =/= Pid ]});
                _ ->
                    ok
            end,
            {ok, {HostInfo, Pid}};
        _ ->
            %% Some servers close connections on the fly, and gun_down is fired before the conn checks-in.
            %% In that case, we just forget until checkin, and the monitor will detect noproc.
            {ok, nonexisting}
    end.


%% @private
%% @doc Dumps the current state of the ETS table for debugging.
-spec dump_state(state()) -> map().
dump_state(#state{tabid = TabId}) ->
    ets:foldl(fun dump_state/2,
              #{
                await_up_conns => #{},
                checked_in_conns => #{},
                checked_out_conns => #{},
                pool_conns => #{}
               },
              TabId).


%% @private
%% @doc Helper function for `dump_state` to fold over the ETS table.
dump_state({{pid, Pid}, #conn{hostinfo = HostInfo, state = checked_out}},
           #{checked_out_conns := M} = Acc) ->
    Acc#{checked_out_conns => M#{Pid => HostInfo}};
dump_state({{pid, Pid}, #conn{hostinfo = HostInfo, state = checked_in}},
           #{checked_in_conns := M} = Acc) ->
    Acc#{checked_in_conns => M#{Pid => HostInfo}};
dump_state({{pid, Pid}, #conn{hostinfo = HostInfo, state = await_up}},
           #{await_up_conns := M} = Acc) ->
    Acc#{await_up_conns => M#{Pid => HostInfo}};
dump_state({{pool, HostInfo}, Pids}, #{pool_conns := M} = Acc) ->
    case Pids of
        [] ->
            Acc;
        _ ->
            Acc#{pool_conns => M#{HostInfo => Pids}}
    end;
dump_state(_, Acc) ->
    Acc.
