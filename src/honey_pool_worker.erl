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

-type requester() :: pid() | {pid(), reference()} | undefined.
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
       await_up_timeout = maps:get(await_up_timeout, Opts, 5000),
       max_conns = maps:get(max_conns, Opts, infinity),
       max_pending_conns = maps:get(max_pending_conns, Opts, infinity),
       min_conns = maps:get(min_conns, Opts, 0)
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
    {Result, NewState} = conn_checkout(HostInfo, Requester, State),
    ?LOG_DEBUG("(~p) handle_call (~p) -> ~p", [self(), Req, Result]),
    {reply, Result, NewState};
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
    {Result, NewState} = conn_checkin(HostInfo, Pid, State),
    ?LOG_DEBUG("(~p) handle_cast (~p) -> ~p", [self(), Req, Result]),
    {noreply, NewState};
handle_cast({cancel_await_up, Pid} = Req, State) ->
    {Result, NewState} = conn_cancel_await_up(Pid, State),
    ?LOG_DEBUG("(~p) handle_cast (~p) -> ~p", [self(), Req, Result]),
    {noreply, NewState};
%% Async checkout: caller receives {checkout_result, Ref, {ok, {ReturnTo, HostInfo, GunPid}}}
%% or {checkout_result, Ref, {error, Reason}} when checkout completes.
%% For the await_up case, conn_up/3 will deliver the notification when gun_up arrives.
handle_cast({async_checkout, HostInfo, CallerPid, Ref} = Req, State) ->
    {Result, NewState} = conn_checkout(HostInfo, {CallerPid, Ref}, State),
    ?LOG_DEBUG("(~p) handle_cast (~p) -> ~p", [self(), Req, Result]),
    case Result of
        {ok, {up, {ReturnTo, Pid}}} ->
            CallerPid ! {checkout_result, Ref, {ok, {ReturnTo, HostInfo, Pid}}};
        {ok, {await_up, _}} ->
            ok;  %% conn_up/3 will notify when gun_up arrives
        {error, Reason} ->
            CallerPid ! {checkout_result, Ref, {error, Reason}}
    end,
    {noreply, NewState};
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
    {Result, NewState} = conn_down(Pid, State),
    gun:close(Pid),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, NewState};
handle_info({gun_up, Pid, Protocol} = Req, State) ->
    {Result, NewState} = conn_up(Pid, Protocol, State),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, NewState};
handle_info({gun_down, Pid, _Protocol, _Reason, _} = Req, State) ->
    {Result, NewState} = conn_down(Pid, State),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, NewState};
handle_info({'DOWN', MRef, _, Pid, _Reason} = Req, State) ->
    demonitor(MRef, [flush]),
    {Result, NewState} = conn_down(Pid, State),
    ?LOG_DEBUG("(~p) handle_info (~p) -> ~p", [self(), Req, Result]),
    {noreply, NewState};
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
          {{ok, {up | await_up, {ReturnTo :: pid(), Pid :: pid()}}} |
           {error, Reason :: term()},
           state()}.
conn_checkout(HostInfo, Requester, #state{tabid = TabId} = State) ->
    case ets:lookup(TabId, {pool, HostInfo}) of
        [] ->
            conn_open_with_returnto(HostInfo, Requester, State);
        [{_, []}] ->
            conn_open_with_returnto(HostInfo, Requester, State);
        [{_, Pids}] ->
            case checkout_from_pool(HostInfo, Requester, Pids, State) of
                no_available_worker ->
                    conn_open_with_returnto(HostInfo, Requester, State);
                {ok, {Status, Pid}} ->
                    NewState = maintain_min_conns(HostInfo, State),
                    {{ok, {Status, {self(), Pid}}}, NewState}
            end
    end.


%% @private
%% @doc Opens a new connection and wraps the result with ReturnTo.
-spec conn_open_with_returnto(hostinfo(), pid(), state()) ->
          {{ok, {await_up, {pid(), pid()}}} | {error, term()}, state()}.
conn_open_with_returnto(HostInfo, Requester, State) ->
    case conn_open(HostInfo, Requester, State) of
        {{ok, {Status, Pid}}, NewState} ->
            {{ok, {Status, {self(), Pid}}}, NewState};
        {Error, NewState} ->
            {Error, NewState}
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
          {{ok, {await_up, Pid :: pid()}} | {error, Reason :: term()}, state()}.
conn_open(_HostInfo, _Requester, #state{max_conns = Max, cur_conns = Cur} = State)
  when Cur >= Max ->
    {{error, {limit, max_conns}}, State};
conn_open(_HostInfo, _Requester, #state{max_pending_conns = Max, cur_pending_conns = Cur} = State)
  when Cur >= Max ->
    {{error, {limit, max_pending_conns}}, State};
conn_open({Host, Port, Transport},
          Requester,
          #state{
            gun_opts = GunOpts,
            tabid = TabId,
            cur_conns = CurConns,
            cur_pending_conns = CurPending,
            pending_per_host = PendingPerHost
           } = State) ->
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
            {{ok, {await_up, Pid}},
             State#state{
               cur_conns = CurConns + 1,
               cur_pending_conns = CurPending + 1,
               pending_per_host = PendingPerHost#{
                   HostInfo => maps:get(HostInfo, PendingPerHost, 0) + 1
               }
              }};
        {error, Reason} ->
            {{error, {gun_open, Reason}}, State}
    end.


%% @private
%% @doc Cancels a pending connection that is waiting for `gun_up`.
-spec conn_cancel_await_up(Pid :: pid(), State :: state()) -> {ok, state()}.
conn_cancel_await_up(Pid,
                     #state{
                       tabid = TabId,
                       await_up_timeout = AwaitUpTimeout
                      } = State) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn = #conn{timer_ref = TRef, state = await_up}}] ->
            cancel_idle_timer(TRef),
            ets:insert(TabId,
                       {{pid, Pid},
                        Conn#conn{
                          requester = undefined,
                          timer_ref = idle_timer(Pid, AwaitUpTimeout)
                         }}),
            %% We don't decrement cur_pending_conns here because the connection is still
            %% in 'await_up' state in the ETS table. It will be decremented when gun_up
            %% or gun_down arrives.
            {ok, State};
        _ ->
            {ok, State}
    end.


%% @private
%% @doc Adds a PID to the pool for the given HostInfo.
%% Callers must ensure the PID is not already in checked_in state to avoid duplicates.
-spec add_to_pool(TabId :: ets:tid(), HostInfo :: hostinfo(), Pid :: pid()) -> ok.
add_to_pool(TabId, HostInfo, Pid) ->
    PidsToPool =
        case ets:lookup(TabId, {pool, HostInfo}) of
            [] ->
                [Pid];
            [{_, Pids}] ->
                [Pid | Pids]
        end,
    ets:insert(TabId, {{pool, HostInfo}, PidsToPool}),
    ok.


%% @private
%% @doc Checks a connection back into the pool.
-spec conn_checkin(HostInfo :: hostinfo(), Pid :: pid(), State :: state()) ->
          {{ok, term()}, state()}.
conn_checkin(HostInfo, Pid, #state{tabid = TabId, idle_timeout = IdleTimeout, cur_conns = CurConns} = State) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [] ->
            %% Unknown PID: verify it's alive before inserting.
            %% This prevents double-decrementing cur_conns if the process died and was
            %% cleaned up by conn_down/2 before this late checkin arrived.
            case erlang:is_process_alive(Pid) of
                true ->
                    Conn =
                        #conn{
                          hostinfo = HostInfo,
                          state = checked_in,
                          monitor_ref = monitor(process, Pid),
                          timer_ref = idle_timer(Pid, IdleTimeout)
                         },
                    ets:insert(TabId, {{pid, Pid}, Conn}),
                    add_to_pool(TabId, HostInfo, Pid),
                    {{ok, {HostInfo, Pid}}, State#state{cur_conns = CurConns + 1}};
                false ->
                    %% Process is already dead, ignore this checkin
                    {{ok, {dead_process, Pid}}, State}
            end;
        [{_, OldConn}] ->
            cancel_idle_timer(OldConn#conn.timer_ref),
            Conn = OldConn#conn{state = checked_in, timer_ref = idle_timer(Pid, IdleTimeout)},
            ets:insert(TabId, {{pid, Pid}, Conn}),
            case OldConn#conn.state of
                checked_in ->
                    %% Already in pool (e.g. double-checkin), skip to avoid duplicate
                    ok;
                _ ->
                    add_to_pool(TabId, HostInfo, Pid)
            end,
            {{ok, {HostInfo, Pid}}, State}
    end.


%% @private
%% @doc Handles the `gun_up` message, indicating a connection is ready.
-spec conn_up(pid(), tcp | tls, state()) -> {{ok, term()}, state()}.
conn_up(Pid, Protocol, #state{tabid = TabId, cur_pending_conns = CurPending, pending_per_host = PendingPerHost} = State) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn}] ->
            %% Only decrement pending counts if connection is in await_up state
            State1 = case Conn#conn.state of
                         await_up ->
                             HostInfo = Conn#conn.hostinfo,
                             NewPPH = PendingPerHost#{
                                 HostInfo => max(0, maps:get(HostInfo, PendingPerHost, 0) - 1)
                             },
                             State#state{cur_pending_conns = CurPending - 1, pending_per_host = NewPPH};
                         _ -> State
                     end,
            case Conn#conn.requester of
                undefined ->
                    %% requester has canceled (or min_conns proactive open) -> keep in pool
                    cancel_idle_timer(Conn#conn.timer_ref),
                    conn_checkin(Conn#conn.hostinfo, Pid, State1);
                {CallerPid, Ref} ->
                    %% async checkout: notify caller with checkout_result
                    cancel_idle_timer(Conn#conn.timer_ref),
                    CallerPid ! {checkout_result, Ref, {ok, {self(), Conn#conn.hostinfo, Pid}}},
                    ets:insert(TabId,
                               {{pid, Pid}, Conn#conn{state = checked_out, requester = undefined}}),
                    {{ok, {Conn#conn.hostinfo, Pid}}, State1};
                Requester when is_pid(Requester) ->
                    %% sync checkout: gun:await_up/3 in caller is waiting for {gun_up, ...}
                    cancel_idle_timer(Conn#conn.timer_ref),
                    Requester ! {gun_up, Pid, Protocol},
                    ets:insert(TabId,
                               {{pid, Pid}, Conn#conn{state = checked_out, requester = undefined}}),
                    {{ok, {Conn#conn.hostinfo, Pid}}, State1}
            end;
        _ ->
            %% arrived out of the blue -> close it instead of accepting into pool
            %% This prevents tracking connections we didn't initiate
            gun:close(Pid),
            {{ok, closed_unexpected}, State}
    end.


%% @private
%% @doc Handles the `gun_down` message, indicating a connection has been lost.
-spec conn_down(pid(), state()) -> {{ok, term()}, state()}.
conn_down(Pid, #state{tabid = TabId, cur_conns = CurConns, cur_pending_conns = CurPending, pending_per_host = PendingPerHost} = State) ->
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
            NewState =
                case Conn#conn.state of
                    await_up ->
                        NewPPH = PendingPerHost#{
                            HostInfo => max(0, maps:get(HostInfo, PendingPerHost, 0) - 1)
                        },
                        State#state{cur_conns = CurConns - 1, cur_pending_conns = CurPending - 1, pending_per_host = NewPPH};
                    _ ->
                        State#state{cur_conns = CurConns - 1}
                end,
            FinalState = maintain_min_conns(HostInfo, NewState),
            {{ok, {HostInfo, Pid}}, FinalState};
        _ ->
            %% Some servers close connections on the fly, and gun_down is fired before the conn checks-in.
            %% In that case, we just forget until checkin, and the monitor will detect noproc.
            {{ok, nonexisting}, State}
    end.


%% @private
%% @doc Dumps the current state of the ETS table for debugging.
-spec dump_state(state()) -> map().
dump_state(#state{
             tabid = TabId,
             cur_conns = CurConns,
             cur_pending_conns = CurPending,
             max_conns = MaxConns,
             max_pending_conns = MaxPending
            }) ->
    ets:foldl(fun dump_state/2,
              #{
                await_up_conns => #{},
                checked_in_conns => #{},
                checked_out_conns => #{},
                pool_conns => #{},
                cur_conns => CurConns,
                cur_pending_conns => CurPending,
                max_conns => MaxConns,
                max_pending_conns => MaxPending
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


%% @private
%% @doc Ensures the pool for HostInfo has at least min_conns idle connections.
%% Opens new connections with requester=undefined so they go directly to the
%% pool via conn_up/3 when gun_up fires.
-spec maintain_min_conns(hostinfo(), state()) -> state().
maintain_min_conns(_HostInfo, #state{min_conns = 0} = State) ->
    State;
maintain_min_conns(HostInfo, #state{tabid = TabId, min_conns = MinConns, pending_per_host = PendingPerHost} = State) ->
    PoolSize =
        case ets:lookup(TabId, {pool, HostInfo}) of
            [{_, Pids}] -> length(Pids);
            _ -> 0
        end,
    %% Use per-host pending count to avoid blocking replenishment of other hosts
    %% on the same worker when they happen to hash to the same worker.
    PendingForHost = maps:get(HostInfo, PendingPerHost, 0),
    replenish_pool(HostInfo, max(0, MinConns - PoolSize - PendingForHost), State).

%% @private
-spec replenish_pool(hostinfo(), non_neg_integer(), state()) -> state().
replenish_pool(_HostInfo, 0, State) ->
    State;
replenish_pool(HostInfo, N, State) ->
    case conn_open(HostInfo, undefined, State) of
        {{ok, {await_up, _}}, NewState} ->
            replenish_pool(HostInfo, N - 1, NewState);
        {_Error, State} ->
            State
    end.
