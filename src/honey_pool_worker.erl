-module(honey_pool_worker).
-behavior(gen_server).

-export([
         init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-include_lib("kernel/include/logger.hrl").
-include("include/honey_pool.hrl").

-define(DEFAULT_OPTS, #{
                        retry => 0,
                        connect_timeout => 1000,
                        http_opts => #{
                                       keepalive => 30 * 1000 %% 30 sec
                                      },
                        http2_opts => #{
                                        keepalive => 30 * 1000 %% 30 sec
                                       }
                       }).

-define(ETS_TABLE, honey_pool).

%%
%% gen_server funs
%%
init(Args) ->
    {ok, #state{
            tabid = ets:new(?ETS_TABLE, [set]),
            gun_opts = maps:merge(
                         ?DEFAULT_OPTS,
                         proplists:get_value(gun_opts, Args, #{})
                        ),
            idle_timeout = proplists:get_value(idle_timeout, Args, infinity)
           }}.

terminate(Reason, State) ->
    ?LOG_INFO("(~p) terminating worker: ~p", [self(), Reason]),
    Pids = ets:foldl(
             fun(V, Acc) ->
                     case V of
                         {{pid, Pid}, _} ->
                             [Pid|Acc];
                         _ ->
                             Acc
                     end
             end, [], State#state.tabid),
    lists:map(
      fun(Pid) -> gun:close(Pid) end,
      Pids),
    ok.

handle_call({checkout, HostInfo}, {Requester, _}, State) ->
    Ret = conn_checkout(HostInfo, Requester, State),
    ?LOG_DEBUG("(~p) checkout a conn: ~p -> ~p", [self(), HostInfo, Ret]),
    {reply, Ret, State};
handle_call(dump_state, _From, State) ->
    {reply, dump_state(State), State};
handle_call(Req, From, State) ->
    ?LOG_WARNING("(~p) unhandled call (~p, ~p, ~p)", [self(), Req, From ,State]),
    {reply, {error, no_handler}, State}.

handle_cast({checkin, HostInfo, Pid}, State) ->
    ?LOG_DEBUG("(~p) checkin a conn: ~p -> ~p", [self(), HostInfo, Pid]),
    ok = conn_checkin(HostInfo, Pid, State),
    {noreply, State};
handle_cast(Req, State) ->
    ?LOG_WARNING("(~p) unhandled cast (~p, ~p)", [self(), Req, State]),
    {noreply, State}.

handle_info({cancel_await_up, Pid} = Req, State) ->
    ?LOG_DEBUG("(~p) cancel await_up: ~p", [self(), Req]),
    ok = conn_cancel_await_up(Pid, State),
    {noreply, State};
handle_info({idle_timeout, Pid} = Req, State) ->
    ?LOG_DEBUG("(~p) idle timeout: ~p", [self(), Req]),
    ok = gun:close(Pid),
    {noreply, State};
handle_info({gun_up, Pid, Protocol} = Req, State) ->
    ?LOG_DEBUG("(~p) gun_up on a conn: ~p", [self(), Req]),
    ok = conn_up(Pid, Protocol, State),
    {noreply, State};
handle_info({gun_down, Pid, _Protocol, Reason, _} = Req, State) ->
    ?LOG_DEBUG("(~p) gun_down on a conn: ~p", [self(), Req]),
    ok = conn_down(Pid, Reason, State),
    {noreply, State};
handle_info({'DOWN', MRef, _, Pid, Reason} = Req, State) ->
    ?LOG_DEBUG("(~p) conn has gone away: ~p", [self(), Req]),
    demonitor(MRef, [flush]),
    ok = conn_down(Pid, {error, Reason}, State),
    {noreply, State};
handle_info(Req, State) ->
    ?LOG_WARNING("(~p) unhandled info (~p, ~p)", [self(), Req, State]),
    {noreply, State}.


%%
%% private funs
%%
-spec idle_timer(Pid::pid(), Timeout::timeout()) -> timer_ref().
idle_timer(_Pid, infinity) ->
    no_ref;
idle_timer(Pid, Timeout) ->
    erlang:send_after(Timeout, self(), {idle_timeout, Pid}).

-spec cancel_idle_timer(TRef::timer_ref()) -> ok.
cancel_idle_timer(no_ref) ->
    ok;
cancel_idle_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.

-spec conn_checkout(HostInfo::hostinfo(), Requester::pid(), State::state()) ->
    {ok, {ReturnTo::pid(), Pid::pid()}} |
    {await_up, {ReturnTo::pid(), Pid::pid()}} |
    {error, Reason::term()}.
conn_checkout(HostInfo, Requester, #state{tabid = TabId} = State) ->
    case ets:lookup(TabId, {hostinfo, HostInfo}) of
        [] ->
            conn_open(HostInfo, Requester, State);
        [{_, []}] ->
            conn_open(HostInfo, Requester, State);
        [{_, [{Pid, MRef, TRef}|T]}] ->
            ets:insert(TabId, {{hostinfo, HostInfo}, T}),
            ets:delete(TabId, {up, Pid}),
            cancel_idle_timer(TRef),
            demonitor(MRef),
            {ok, {self(), Pid}}
    end.

-spec conn_open(HostInfo::hostinfo(), Requester::pid(), State::state()) ->
    {await_up, {ReplyTo::pid(), Pid::pid()}} |
    {error, Reason::term()}.
conn_open(
  {Host, Port, Transport},
  Requester,
  #state{gun_opts = GunOpts0, tabid = TabId}
 ) ->
    GunOpts = GunOpts0#{transport => Transport},
    case gun:open(Host, Port, GunOpts) of
        {ok, Pid} ->
            ets:insert(TabId, {{await_up, Pid}, {Requester, monitor(process, Pid)}}),
            {await_up, {self(), Pid}};
        {error, Reason} ->
            {error, {gun_open, Reason}}
    end.

-spec conn_cancel_await_up(Pid::pid(), State::state()) -> ok.
conn_cancel_await_up(Pid, #state{tabid = TabId}) ->
    case ets:lookup(TabId, {await_up, Pid}) of
        [{_, {_Requester, MRef}}] ->
            ets:delete(TabId, {await_up, Pid}),
            demonitor(MRef),
            ok;
        _ ->
            ok
    end.

-spec conn_checkin(HostInfo::hostinfo(), Pid::pid(), State::state()) -> ok.
conn_checkin(HostInfo, Pid, #state{tabid = TabId, idle_timeout = IdleTimeout}) ->
    PoolConn = {Pid,
                monitor(process, Pid),
                idle_timer(Pid, IdleTimeout)},
    Conns = case ets:lookup(TabId, {hostinfo, HostInfo}) of
                [] ->
                    [PoolConn];
                [{_, T}] ->
                    [PoolConn|T]
            end,
    ets:insert(TabId, {{hostinfo, HostInfo}, Conns}),
    ets:insert(TabId, {{up, Pid}, HostInfo}),
    ok.

-spec conn_up(pid(), tcp|tls, state()) -> ok.
conn_up(Pid, Protocol, #state{tabid = TabId} = State) ->
    case ets:lookup(TabId, {await_up, Pid}) of
        [{_, {Requester, MRef}}] ->
            ets:delete(TabId, {await_up, Pid}),
            demonitor(MRef),
            Requester ! {gun_up, Pid, Protocol},
            ok;
        _ ->
            %% keep pid in the pool if no one is awaiting
            #{origin_host := Host,
              origin_port := Port,
              transport := Transport
             } = gun:info(Pid),
            HostInfo = {Host, Port, Transport},
            conn_checkin(HostInfo, Pid, State)
    end.

-spec conn_down(pid(), term(), state()) -> ok.
conn_down(Pid, Msg, #state{tabid = TabId}) ->
    case ets:lookup(TabId, {up, Pid}) of
        [{_, HostInfo}] ->
            case ets:lookup(TabId, {hostinfo, HostInfo}) of
                [{_, Conns}] ->
                    {ToDeleteConns, NextConns}
                    = lists:partition(
                        fun({P, _MRef, _TRef}) -> P =:= Pid end,
                        Conns),
                    lists:map(
                      fun({_Pid, MRef, TRef}) ->
                              cancel_idle_timer(TRef),
                              demonitor(MRef, [flush])
                      end, ToDeleteConns),
                    ets:insert(TabId, {{hostinfo, HostInfo}, NextConns});
                _ ->
                    ?LOG_INFO("(~p) unknown conn ~p for host ~p has gone down: ~p", [self(), Pid, HostInfo, Msg])
            end,
            ets:delete(TabId, {up, Pid}),
            ok;
        _ ->
            case ets:lookup(TabId, {await_up, Pid}) of
                [{_, {_, MRef}}] ->
                    demonitor(MRef, [flush]),
                    ets:delete(TabId, {await_up, Pid}),
                    ok;
                _ ->
                    %% Some servers close connections on the fly, and gun_down is fired before the conn checks-in.
                    %% In that case, we just forget until checkin, and the monitor will detect noproc.
                    ok
            end
    end.

-spec dump_state(state()) -> map().
dump_state(#state{tabid = TabId}) ->
    ets:foldl(
      fun dump_state/2,
      #{
        up_conns => #{},
        await_up_conns => #{},
        host_conns => #{}
       },
      TabId).

dump_state({{await_up, Pid}, Awaiting}, #{await_up_conns := M} = Acc) ->
    Acc#{await_up_conns => M#{Pid => Awaiting}};
dump_state({{up, Pid}, HostInfo}, #{up_conns := M} = Acc) ->
    Acc#{up_conns => M#{Pid => HostInfo}};
dump_state({{hostinfo, HostInfo}, Conns}, #{host_conns := M} = Acc) ->
    case Conns of
        [] ->
            Acc;
        _ ->
            Acc#{host_conns => M#{HostInfo => Conns}}
    end;
dump_state(_, Acc) ->
    Acc.
