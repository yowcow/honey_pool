-module(honey_pool_worker).
-behavior(gen_server).

-export([
         init/1,
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

%%
%% gen_server funs
%%
init(Args) ->
    ReqOpts = maps:merge(?DEFAULT_OPTS, proplists:get_value(gun_opts, Args, #{})),
    InactivityTimeout = proplists:get_value(inactivity_timeout, Args, infinity),
    {ok, #state{
            new_conn = fun(Host, Port, Opt) ->
                               case gun:open(Host, Port, maps:merge(ReqOpts, Opt)) of
                                   {ok, Pid} ->
                                       MRef = monitor(process, Pid),
                                       {ok, {Pid, MRef}};
                                   Err ->
                                       Err
                               end
                       end,
            inactivity_timeout = InactivityTimeout
           }}.

handle_call({checkout, HostInfo}, {Requester, _}, State) ->
    {Ret, NextState} = conn_checkout(HostInfo, Requester, State),
    ?LOG_DEBUG("(~p) checkout a conn: ~p -> ~p", [self(), HostInfo, Ret]),
    {reply, {self(), Ret}, NextState};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(Req, From, State) ->
    ?LOG_WARNING("(~p) unhandled call (~p, ~p, ~p)", [self(), Req, From ,State]),
    {reply, {error, no_handler}, State}.

handle_cast(state, State) ->
    io:format("state: ~p~n", [State]),
    {noreply, State};
handle_cast(Req, State) ->
    ?LOG_WARNING("(~p) unhandled cast (~p, ~p)", [self(), Req, State]),
    {noreply, State}.

handle_info({inactivity_timeout, Pid}, State) ->
    ?LOG_DEBUG("(~p) inactivity timeout: ~p", [self(), Pid]),
    ok = gun:close(Pid),
    {noreply, State};
handle_info({checkin, Pid}, State) ->
    ?LOG_DEBUG("(~p) checkin a conn: ~p", [self(), Pid]),
    NextState = conn_checkin(Pid, State),
    {noreply, NextState};
handle_info({gun_up, Pid, Proto}, State) ->
    ?LOG_DEBUG("(~p) gun_up a conn: ~p (~p)", [self(), Pid, Proto]),
    NextState = conn_up(Pid, {ok, Proto}, State),
    {noreply, NextState};
handle_info({gun_down, Pid, _, _, _}, State) ->
    ?LOG_DEBUG("(~p) gun_down a conn: ~p", [self(), Pid]),
    NextState = conn_down(Pid, {error, gun_down}, State),
    {noreply, NextState};
handle_info({'DOWN', MRef, _, Pid, Reason}, State) ->
    ?LOG_DEBUG("(~p) conn has gone away: ~p (~p)", [self(), Pid, Reason]),
    demonitor(MRef),
    NextState = conn_down(Pid, {error, Reason}, State),
    {noreply, NextState};
handle_info(Req, State) ->
    ?LOG_WARNING("(~p) unhandled info (~p, ~p)", [self(), Req, State]),
    {noreply, State}.



%%
%% private funs
%%
-spec conn_checkout(
        HostInfo::hostinfo(),
        Pid::pid(),
        State::state()
       ) -> {{ok, pid()}
             | {awaiting, pid()}
             | {error, Reason::any()},
             state()}.
conn_checkout(HostInfo, Requester, State) ->
    {Ret, NextConns} = conn_checkout_host_conns(
                         HostInfo,
                         Requester,
                         maps:get(HostInfo, State#state.host_conns, #connections{}),
                         State#state.new_conn,
                         State#state.inactivity_timeout
                        ),
    case Ret of
        {ok, {_, Pid} = Result} ->
            {Result,
             State#state{
               host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
               conn_host = maps:put(Pid, HostInfo, State#state.conn_host)
              }};
        _ ->
            {Ret, State}
    end.

conn_checkout_host_conns(
  {Host, Port, Opt},
  Requester,
  Conns,
  NewConnFun,
  Timeout
 ) ->
    case Conns#connections.available of
        [{{Pid, _} = Conn, TRef} = Available|_] ->
            case TRef of
                no_ref ->
                    ok;
                _ ->
                    erlang:cancel_timer(TRef)
            end,
            InUse = case Timeout of
                        infinity ->
                            {Conn, no_ref};
                        _ ->
                            {Conn, erlang:send_after(
                                     Timeout,
                                     self(),
                                     {inactivity_timeout, Pid}
                                    )}
                    end,
            {{ok, {ok, Pid}},
             Conns#connections{
               available = lists:delete(Available, Conns#connections.available),
               in_use = [InUse|Conns#connections.in_use]
              }};
        _ ->
            %% make a new conn since there's no conn available
            case NewConnFun(Host, Port, Opt) of
                {ok, {Pid, _} = Conn} ->
                    InUse = case Timeout of
                                infinity ->
                                    {Conn, no_ref};
                                _ ->
                                    {Conn, erlang:send_after(
                                             Timeout,
                                             self(),
                                             {inactivity_timeout, Pid}
                                            )}
                            end,
                    {{ok, {awaiting, Pid}},
                     Conns#connections{
                       awaiting = [{Conn, Requester}|Conns#connections.awaiting],
                       in_use = [InUse|Conns#connections.in_use]
                      }};
                Err ->
                    {Err, Conns}
            end
    end.

-spec conn_checkin(pid(), state()) -> state().
conn_checkin(Pid, State) ->
    case maps:find(Pid, State#state.conn_host) of
        {ok, HostInfo} ->
            NextConns = conn_checkin_host_conns(
                          Pid,
                          maps:find(HostInfo, State#state.host_conns),
                          State#state.inactivity_timeout
                         ),
            State#state{
              host_conns = maps:put(HostInfo, NextConns, State#state.host_conns)
             };
        _ ->
            ?LOG_DEBUG("(~p) unknown conn has checked-in: ~p", [self(), Pid]),
            State
    end.

conn_checkin_host_conns(Pid, {ok, Conns}, Timeout) ->
    case find_active_conn(Pid, Conns#connections.in_use) of
        {{ok, {Conn, TRef}}, InUse} ->
            case TRef of
                no_ref ->
                    ok;
                _ ->
                    erlang:cancel_timer(TRef)
            end,
            Available = case Timeout of
                            infinity ->
                                {Conn, no_ref};
                            _ ->
                                {Conn, erlang:send_after(
                                         Timeout,
                                         self(),
                                         {inactivity_timeout, Pid}
                                        )}
                        end,
            Conns#connections{
              available = [Available|Conns#connections.available],
              in_use = InUse
             };
        _ ->
            ?LOG_WARNING("(~p) conn is not in use: ~p", [self(), Pid]),
            Conns
      end;
conn_checkin_host_conns(Pid, _, _) ->
    %% this pid has nowhere to go
    gun:close(Pid),
    #connections{}.

-spec conn_up(pid(), term(), state()) -> state().
conn_up(Pid, Msg, State) ->
    case maps:find(Pid, State#state.conn_host) of
        {ok, HostInfo} ->
            NextConns = conn_up_host_conns(Pid, Msg, maps:find(HostInfo, State#state.host_conns)),
            State#state{
              host_conns = maps:put(HostInfo, NextConns, State#state.host_conns)
             };
        _ ->
            ?LOG_DEBUG("(~p) unknown conn has gone up: ~p (~p)", [self(), Pid, Msg]),
            State
    end.

conn_up_host_conns(Pid, Msg, {ok, Conns}) ->
    case find_awaiting_conn(Pid, Conns#connections.awaiting) of
        {{ok, {_, Requester}}, Awaiting} ->
            Requester ! Msg,
            Conns#connections{
              awaiting = Awaiting
             };
        _ ->
            ?LOG_WARNING("(~p) conn is not awaiting: ~p (~p)", [self(), Pid, Msg]),
            Conns
    end;
conn_up_host_conns(_, _, _) ->
    #connections{}.

-spec conn_down(pid(), term(), state()) -> state().
conn_down(Pid, Msg, State) ->
    case maps:find(Pid, State#state.conn_host) of
        {ok, HostInfo} ->
            NextConns = conn_down_host_conns(
                          Pid,
                          Msg,
                          maps:find(HostInfo, State#state.host_conns)),
            State#state{
              host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
              conn_host = maps:remove(Pid, State#state.conn_host)
             };
        _ ->
            ?LOG_DEBUG("(~p) unknown conn has gone down: ~p (~p)", [self(), Pid, Msg]),
            State
    end.

conn_down_host_conns(Pid, Msg, {ok, Conns}) ->
    Available = case find_active_conn(Pid, Conns#connections.available) of
                    {{ok, {{_, MRef1}, TRef1}}, Rem1} ->
                        demonitor(MRef1, [flush]),
                        case TRef1 of
                            no_ref ->
                                ok;
                            _ ->
                                erlang:cancel_timer(TRef1)
                        end,
                        Rem1;
                    _ ->
                        Conns#connections.available
                end,
    InUse = case find_active_conn(Pid, Conns#connections.in_use) of
                {{ok, {{_, MRef2}, TRef2}}, Rem2} ->
                    demonitor(MRef2, [flush]),
                    case TRef2 of
                        no_ref ->
                            ok;
                        _ ->
                            erlang:cancel_timer(TRef2)
                    end,
                    Rem2;
                _ ->
                    Conns#connections.in_use
            end,
    Awaiting = case find_awaiting_conn(Pid, Conns#connections.awaiting) of
                   {{ok, {{_, MRef3}, Requester}}, Rem3} ->
                       Requester ! Msg,
                       demonitor(MRef3, [flush]),
                       Rem3;
                   _ ->
                       Conns#connections.awaiting
               end,
    Conns#connections{
      available = Available,
      in_use = InUse,
      awaiting = Awaiting
     };
conn_down_host_conns(_, _, _) ->
    #connections{}.

-spec find_active_conn(pid(), [active_conn()]) ->
    {{ok, active_conn()}, [active_conn()]} |
    {{error, term()}, [active_conn()]}.
find_active_conn(Pid, ActiveConns) ->
    case lists:partition(fun({{P, _}, _}) -> P =:= Pid end, ActiveConns) of
        {[], _} ->
            {{error, no_active_conn}, ActiveConns};
        {[ActiveConn|_], Rem} ->
            {{ok, ActiveConn}, Rem}
    end.

-spec find_awaiting_conn(pid(), [awaiting_conn()]) ->
    {{ok, awaiting_conn()}, [awaiting_conn()]} |
    {{error, term()}, [awaiting_conn()]}.
find_awaiting_conn(Pid, AwaitingConns) ->
    case lists:partition(fun({{P, _}, _}) -> P =:= Pid end, AwaitingConns) of
        {[], _} ->
            {{error, no_awaiting_pid}, AwaitingConns};
        {[AwaitingConn|_], Rem} ->
            {{ok, AwaitingConn}, Rem}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find_active_conn_test_() ->
    Conns = [{{pid1, ref1}, tref1}, {{pid2, ref2}, tref2}],
    Cases = [
             {
              "no conn",
              pid3,
              {{error, no_active_conn}, Conns}
             },
             {
              "with conn",
              pid2,
              {{ok, {{pid2, ref2}, tref2}}, [{{pid1, ref1}, tref1}]}
             }
            ],
    F = fun({Title, Pid, Expected}) ->
                Actual = find_active_conn(Pid, Conns),
                [{Title, ?_assertEqual(Expected, Actual)}]
        end,
    lists:map(F, Cases).

find_awaiting_conn_test_() ->
    AwaitingConns = [
                     {{pid1, ref1}, req1},
                     {{pid2, ref2}, req2}],
    Cases = [
            {
             "no conn",
             pid3,
             {{error, no_awaiting_pid}, AwaitingConns}
            },
            {
             "with conn",
             pid2,
             {{ok, {{pid2, ref2}, req2}},
              [{{pid1, ref1}, req1}]}
            }
           ],
    F = fun({Title, Pid, Expected}) ->
                Actual = find_awaiting_conn(Pid, AwaitingConns),
                [{Title, ?_assertEqual(Expected, Actual)}]
        end,
    lists:map(F, Cases).
-endif.
