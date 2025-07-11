-module(honey_pool_worker).
-behavior(gen_server).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-include("include/honey_pool.hrl").

-define(DEFAULT_OPTS, #{
                        retry => 0,
                        connect_timeout => 1000,
                        http_opts => #{
                                       %% 30 sec
                                       keepalive => 30 * 1000
                                      },
                        http2_opts => #{
                                        %% 30 sec
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


%%
%% gen_server funs
%%
init(Args) ->
    {ok, #state{
           tabid = ets:new(?ETS_TABLE, [set]),
           gun_opts = maps:merge(
                        ?DEFAULT_OPTS,
                        proplists:get_value(gun_opts, Args, #{})),
           idle_timeout = proplists:get_value(idle_timeout, Args, infinity)
          }}.


terminate(Reason, State) ->
    ?LOG_INFO("(~p) terminating worker: ~p", [self(), Reason]),
    Pids = ets:foldl(
             fun(V, Acc) ->
                     case V of
                         {{pid, Pid}, _} ->
                             [Pid | Acc];
                         _ ->
                             Acc
                     end
             end,
             [],
             State#state.tabid),
    lists:map(
      fun(Pid) -> gun:close(Pid) end,
      Pids),
    ok.


handle_call({checkout, HostInfo} = Req, {Requester, _}, State) ->
    Result = conn_checkout(HostInfo, Requester, State),
    ?LOG_DEBUG("(~p) handle_call (~p) -> ~p", [self(), Req, Result]),
    {reply, Result, State};
handle_call(dump_state, _From, State) ->
    {reply, dump_state(State), State};
handle_call(Req, From, State) ->
    ?LOG_WARNING("(~p) unhandled call (~p, ~p, ~p)", [self(), Req, From, State]),
    {reply, {error, no_handler}, State}.


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
-spec idle_timer(Pid :: pid(), Timeout :: timeout()) -> timer_ref().
idle_timer(_Pid, infinity) ->
    undefined;
idle_timer(Pid, Timeout) ->
    erlang:send_after(Timeout, self(), {idle_timeout, Pid}).


-spec cancel_idle_timer(TRef :: timer_ref()) -> ok.
cancel_idle_timer(undefined) ->
    ok;
cancel_idle_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.


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
            ets:insert(
              TabId,
              {{pid, Pid},
               Conn#conn{
                 state = checked_out,
                 timer_ref = undefined
                }}),
            {ok, {up, Pid}}
    end.


-spec conn_open(HostInfo :: hostinfo(), Requester :: pid(), State :: state()) ->
          {ok, {await_up, Pid :: pid()}} |
          {error, Reason :: term()}.
conn_open({Host, Port, Transport},
          Requester,
          #state{gun_opts = GunOpts, tabid = TabId}) ->
    GunOptsWithTransport = GunOpts#{transport => Transport},
    HostInfo = {Host, Port, Transport},
    case gun:open(Host, Port, GunOptsWithTransport) of
        {ok, Pid} ->
            ets:insert(
              TabId,
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


-spec conn_cancel_await_up(Pid :: pid(), State :: state()) -> ok.
conn_cancel_await_up(Pid, #state{tabid = TabId}) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn}] ->
            ets:insert(
              TabId,
              {{pid, Pid},
               Conn#conn{
                 requester = undefined
                }}),
            ok;
        _ ->
            ok
    end.


-spec conn_checkin(HostInfo :: hostinfo(), Pid :: pid(), State :: state()) -> {ok, term()}.
conn_checkin(HostInfo, Pid, #state{tabid = TabId, idle_timeout = IdleTimeout}) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [] ->
            ets:insert(
              TabId,
              {{pid, Pid},
               #conn{
                 hostinfo = HostInfo,
                 state = checked_in,
                 monitor_ref = monitor(process, Pid),
                 timer_ref = idle_timer(Pid, IdleTimeout)
                }});
        [{_, Conn}] ->
            ets:insert(
              TabId,
              {{pid, Pid},
               Conn#conn{
                 state = checked_in,
                 timer_ref = idle_timer(Pid, IdleTimeout)
                }})
    end,
    case ets:lookup(TabId, {pool, HostInfo}) of
        [] ->
            ets:insert(TabId, {{pool, HostInfo}, [Pid]});
        [{_, Pids}] ->
            ets:insert(TabId, {{pool, HostInfo}, [Pid | Pids]})
    end,
    {ok, {HostInfo, Pid}}.


-spec conn_up(pid(), tcp | tls, state()) -> {ok, term()}.
conn_up(Pid, Protocol, #state{tabid = TabId} = State) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn}] ->
            case Conn#conn.requester of
                undefined ->
                    %% requester has canceled -> just keep pid in the pool
                    conn_checkin(Conn#conn.hostinfo, Pid, State);
                Requester ->
                    %% notify requester and tag that conn is checked out
                    Requester ! {gun_up, Pid, Protocol},
                    ets:insert(
                      TabId,
                      {{pid, Pid},
                       Conn#conn{
                         state = checked_out,
                         requester = undefined
                        }}),
                    {ok, {Conn#conn.hostinfo, Pid}}
            end;
        _ ->
            %% arrived out of the blue -> just keep pid in the pool
            #{
              origin_host := Host,
              origin_port := Port,
              transport := Transport
             } = gun:info(Pid),
            HostInfo = {Host, Port, Transport},
            conn_checkin(HostInfo, Pid, State)
    end.


-spec conn_down(pid(), state()) -> {ok, term()}.
conn_down(Pid, #state{tabid = TabId}) ->
    case ets:lookup(TabId, {pid, Pid}) of
        [{_, Conn}] ->
            HostInfo = Conn#conn.hostinfo,
            demonitor(Conn#conn.monitor_ref, [flush]),
            cancel_idle_timer(Conn#conn.timer_ref),
            ets:delete(TabId, {pid, Pid}),
            case ets:lookup(TabId, {pool, HostInfo}) of
                [{_, Pids}] ->
                    ets:insert(
                      TabId,
                      {{pool, HostInfo}, [ P || P <- Pids, P =/= Pid ]});
                _ ->
                    ok
            end,
            {ok, {HostInfo, Pid}};
        _ ->
            %% Some servers close connections on the fly, and gun_down is fired before the conn checks-in.
            %% In that case, we just forget until checkin, and the monitor will detect noproc.
            {ok, nonexisting}
    end.


-spec dump_state(state()) -> map().
dump_state(#state{tabid = TabId}) ->
    ets:foldl(
      fun dump_state/2,
      #{
        await_up_conns => #{},
        checked_in_conns => #{},
        checked_out_conns => #{},
        pool_conns => #{}
       },
      TabId).


dump_state({{pid, Pid}, #conn{hostinfo = HostInfo, state = checked_out}},
           #{checked_out_conns := M} = Acc) ->
    Acc#{checked_out_conns => M#{Pid => HostInfo}};
dump_state({{pid, Pid}, #conn{hostinfo = HostInfo, state = checked_in}},
           #{checked_in_conns := M} = Acc) ->
    Acc#{checked_in_conns => M#{Pid => HostInfo}};
dump_state({{pid, Pid}, #conn{hostinfo = HostInfo, state = await_up}},
           #{await_up_conns := M} = Acc) ->
    Acc#{await_up_conns => M#{Pid => HostInfo}};
dump_state({{pool, HostInfo}, Pids},
           #{pool_conns := M} = Acc) ->
    case Pids of
        [] ->
            Acc;
        _ ->
            Acc#{pool_conns => M#{HostInfo => Pids}}
    end;
dump_state(_, Acc) ->
    Acc.
