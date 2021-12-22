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
                        connect_timeout => 100,
                        domain_lookup_timeout => 100,
                        tls_handshake_timeout => 100,
                        http_opts => #{
                                       closing_timeout => 100,
                                       keepalive => 60 * 1000 %% 60 sec
                                      },
                        http2_opts => #{
                                        closing_timeout => 100,
                                        keepalive => 60 * 1000 %% 60 sec
                                       }
                       }).

%%
%% gen_server funs
%%
init(Args) ->
    ReqOpts = proplists:get_value(gun_opts, Args, ?DEFAULT_OPTS),
    {ok, #state{
            new_conn = fun(Host, Port, Opt) ->
                               case gun:open(Host, Port, maps:merge(ReqOpts, Opt)) of
                                   {ok, Conn} ->
                                       monitor(process, Conn),
                                       {ok, Conn};
                                   Err ->
                                       Err
                               end
                       end
           }}.

handle_call({checkout, HostInfo}, {Requester, _}, State) ->
    ?LOG_INFO("checkout a conn: ~p", [HostInfo]),
    {Ret, NextState} = conn_checkout(HostInfo, Requester, State),
    {reply, Ret, NextState};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(Req, From, State) ->
    ?LOG_WARNING("unhandled call (~p, ~p, ~p)", [Req, From ,State]),
    {reply, {error, no_handler}, State}.

handle_cast({checkin, Conn}, State) ->
    ?LOG_INFO("checkin a conn: ~p", [Conn]),
    NextState = conn_checkin(Conn, State),
    {noreply, NextState};
handle_cast(Req, State) ->
    ?LOG_WARNING("unhandled cast (~p, ~p)", [Req, State]),
    {noreply, State}.

handle_info({gun_up, Conn, Proto}, State) ->
    ?LOG_INFO("gun_up a conn: ~p (~p)", [Conn, Proto]),
    NextState = conn_up(Conn, {ok, Proto}, State),
    {noreply, NextState};
handle_info({gun_down, Conn, _, _, _}, State) ->
    ?LOG_INFO("gun_down a conn: ~p", [Conn]),
    NextState = conn_down(Conn, {error, gun_down}, State),
    {noreply, NextState};
handle_info({'DOWN', MRef, _, Conn, Reason}, State) ->
    ?LOG_INFO("conn has gone away: ~p (~p)", [Conn, Reason]),
    demonitor(MRef),
    NextState = conn_down(Conn, {error, Reason}, State),
    {noreply, NextState};
handle_info(Req, State) ->
    ?LOG_WARNING("unhandled info (~p, ~p)", [Req, State]),
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
                         State#state.new_conn
                        ),
    NextState = case Ret of
                    {ok, Conn} ->
                        State#state{
                          host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
                          conn_host = maps:put(Conn, HostInfo, State#state.conn_host)
                         };
                    {awaiting, Conn} ->
                        State#state{
                          host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
                          conn_host = maps:put(Conn, HostInfo, State#state.conn_host)
                         };
                    _ ->
                        State
                end,
    {Ret, NextState}.

conn_checkout_host_conns(
  {Host, Port, Opt},
  Requester,
  Conns,
  NewConnFun
 ) ->
    case Conns#connections.available of
        [Conn|_] ->
            {{ok, Conn},
             Conns#connections{
               available = lists:delete(Conn, Conns#connections.available),
               in_use = [Conn | Conns#connections.in_use]
              }};
        _ ->
            %% make a new conn since there's no conn available
            case NewConnFun(Host, Port, Opt) of
                {ok, Conn} ->
                    {{awaiting, Conn},
                     Conns#connections{
                       awaiting = [{Conn, Requester} | Conns#connections.awaiting]
                      }};
                Err ->
                    {Err, Conns}
            end
    end.

-spec conn_checkin(pid(), state()) -> state().
conn_checkin(Conn, State) ->
    case maps:find(Conn, State#state.conn_host) of
        {ok, HostInfo} ->
            NextConns = conn_checkin_host_conns(Conn, maps:find(HostInfo, State#state.host_conns)),
            State#state{
              host_conns = maps:put(HostInfo, NextConns, State#state.host_conns)
             };
        _ ->
            ?LOG_WARNING("unknown conn has checked-in: ~p", [Conn]),
            State
    end.

conn_checkin_host_conns(Conn, {ok, Conns}) ->
    Conns#connections{
      available = [Conn | Conns#connections.available],
      in_use = lists:delete(Conn, Conns#connections.in_use)
     };
conn_checkin_host_conns(Conn, _) ->
    #connections{
       available = [Conn]
      }.

-spec conn_up(pid(), term(), state()) -> state().
conn_up(Conn, Msg, State) ->
    case maps:find(Conn, State#state.conn_host) of
        {ok, HostInfo} ->
            NextConns = conn_up_host_conns(Conn, Msg, maps:find(HostInfo, State#state.host_conns)),
            State#state{
              host_conns = maps:put(HostInfo, NextConns, State#state.host_conns)
             };
        _ ->
            ?LOG_WARNING("unknown conn has gone up: ~p (~p)", [Conn, Msg]),
            State
    end.

conn_up_host_conns(Conn, Msg, {ok, Conns}) ->
    Conns#connections{
      in_use = [Conn | Conns#connections.in_use],
      awaiting = notify_and_filter_awaiting(Conn, Msg, Conns#connections.awaiting)
     };
conn_up_host_conns(_, _, _) ->
    #connections{}.

-spec conn_down(pid(), term(), state()) -> state().
conn_down(Conn, Msg, State) ->
    case maps:find(Conn, State#state.conn_host) of
        {ok, HostInfo} ->
            NextConns = conn_down_host_conns(Conn, Msg, maps:find(HostInfo, State#state.host_conns)),
            State#state{
              host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
              conn_host = maps:remove(Conn, State#state.conn_host)
             };
        _ ->
            ?LOG_WARNING("unknown conn has gone down: ~p", [Conn]),
            State
    end.

conn_down_host_conns(Conn, Msg, {ok, Conns}) ->
    Conns#connections{
      available = lists:delete(Conn, Conns#connections.available),
      in_use = lists:delete(Conn, Conns#connections.in_use),
      awaiting = notify_and_filter_awaiting(Conn, Msg, Conns#connections.awaiting)
     };
conn_down_host_conns(_, _, _) ->
    #connections{}.


-spec notify_and_filter_awaiting(pid(), term(), [{pid(), pid()}]) -> [{pid(), pid()}].
notify_and_filter_awaiting(Conn, Msg, Awaiting) ->
    notify_and_filter_awaiting(Conn, Msg, Awaiting, []).

notify_and_filter_awaiting(_, _, [], Acc) ->
    Acc;
notify_and_filter_awaiting(Conn, Msg, [{Conn, Requester}|T], Acc) ->
    Requester ! Msg,
    notify_and_filter_awaiting(Conn, Msg, T, Acc);
notify_and_filter_awaiting(Conn, Msg, [H|T], Acc) ->
    notify_and_filter_awaiting(Conn, Msg, T, [H|Acc]).
