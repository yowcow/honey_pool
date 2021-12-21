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

%%
%% gen_server funs
%%
init(_) ->
    {ok, #state{
            new_conn = fun(Host, Port, Opt) ->
                               case gun:open(Host, Port, Opt#{retry => 0}) of
                                   {ok, Pid} ->
                                       case gun:await_up(Pid) of
                                           {ok, _} ->
                                               {ok, Pid};
                                           Err ->
                                               Err
                                       end;
                                   Err ->
                                       Err
                               end
                       end
           }}.

handle_call({checkout, HostInfo}, _From, State) ->
    ?LOG_INFO("checkout a conn: ~p", [HostInfo]),
    {Ret, NextState} = checkout(HostInfo, State),
    {reply, Ret, NextState};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(Req, From, State) ->
    ?LOG_DEBUG("unhandled call (~p, ~p, ~p)", [Req, From ,State]),
    {reply, {error, no_handler}, State}.

handle_cast({checkin, Conn}, State) ->
    ?LOG_INFO("checkin a conn: ~p", [Conn]),
    NextState = checkin(Conn, State),
    {noreply, NextState};
handle_cast(Req, State) ->
    ?LOG_DEBUG("unhandled cast (~p, ~p)", [Req, State]),
    {noreply, State}.

handle_info({gun_down, Conn, _, _, _, _}, State) ->
    ?LOG_INFO("gun_down a conn: ~p", [Conn]),
    NextState = close(Conn, State),
    {noreply, NextState};
handle_info(Req, State) ->
    ?LOG_DEBUG("unhandled info (~p, ~p)", [Req, State]),
    {noreply, State}.



%%
%% private funs
%%
-spec checkout(hostinfo(), state()) -> {{ok, pid()} | {error, Reason::any()}, state()}.
checkout(HostInfo, State) ->
    {Ret, NextConns} = checkout_host_conn(
                         HostInfo,
                         maps:get(
                           HostInfo,
                           State#state.host_conns,
                           #connections{}
                          ),
                         State#state.new_conn
                        ),
    NextState = case Ret of
                    {ok, Conn} ->
                        State#state{
                          host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
                          conn_host = maps:put(Conn, HostInfo, State#state.conn_host)
                         };
                    _ ->
                        State
                end,
    {Ret, NextState}.

checkout_host_conn({Host, Port, Opt}, Conns, NewConnFun) ->
    case Conns#connections.available of
        [Conn|_] ->
            {{ok, Conn}, Conns#connections{
                           available = lists:delete(Conn, Conns#connections.available),
                           in_use = [Conn | Conns#connections.in_use]
                          }};
        _ ->
            %% make a new conn since there's no conn available
            case NewConnFun(Host, Port, Opt) of
                {ok, Conn} ->
                    {{ok, Conn}, Conns#connections{
                                   in_use = [Conn | Conns#connections.in_use]
                                  }};
                Err ->
                    ?LOG_WARNING("failed creating a new conn (~p, ~p, ~p): ~p", [Host, Port, Opt, Err]),
                    {Err, Conns}
            end
    end.

-spec checkin(pid(), state()) -> state().
checkin(Conn, State) ->
    NextState = case maps:find(Conn, State#state.conn_host) of
                    {ok, HostInfo} ->
                        NextConns = checkin_host_conn(Conn, maps:find(HostInfo, State#state.host_conns)),
                        State#state{
                          host_conns = maps:put(HostInfo, NextConns, State#state.host_conns)
                         };
                    _ ->
                        ?LOG_WARNING("unknown conn has checked-in: ~p", [Conn]),
                        State
                end,
    NextState.

checkin_host_conn(Conn, {ok, Conns}) ->
    Conns#connections{
      available = [Conn | Conns#connections.available],
      in_use = lists:delete(Conn, Conns#connections.in_use)
     };
checkin_host_conn(Conn, _) ->
    #connections{
       available = [Conn]
      }.

-spec close(pid(), state()) -> state().
close(Conn, State) ->
    NextState = case maps:find(Conn, State#state.conn_host) of
                    {ok, HostInfo} ->
                        NextConns = close_host_conn(Conn, maps:find(HostInfo, State#state.host_conns)),
                        State#state{
                          host_conns = maps:put(HostInfo, NextConns, State#state.host_conns),
                          conn_host = maps:remove(Conn, State#state.conn_host)
                         };
                    _ ->
                        ?LOG_WARNING("unknown conn has closed: ~p", [Conn]),
                        State
                end,
    NextState.

close_host_conn(Conn, {ok, Conns}) ->
    Conns#connections{
      available = lists:delete(Conn, Conns#connections.available),
      in_use = lists:delete(Conn, Conns#connections.in_use)
     };
close_host_conn(_, _) ->
    #connections{}.
