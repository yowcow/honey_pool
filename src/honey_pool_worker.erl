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
    ReqOpts = maps:merge(?DEFAULT_OPTS, proplists:get_value(gun_opts, Args, #{})),
    {ok, #state{
            new_conn = fun(Host, Port, Transport) ->
                               Ret = gun:open(Host, Port, ReqOpts#{transport => Transport}),
                               case Ret of
                                   {ok, Pid} ->
                                       MRef = monitor(process, Pid),
                                       {ok, {Pid, MRef}};
                                   Err ->
                                       Err
                               end
                       end,
            tabid = ets:new(?ETS_TABLE, [set])
           }}.

terminate(Reason, State) ->
    ?LOG_INFO("(~p) terminating worker: ~p", [self(), Reason]),
    Pids = ets:foldl(
             fun(V, Acc) ->
                     case V of
                         {{hostinfo, Pid}, _} ->
                             [Pid|Acc];
                         _ ->
                             Acc
                     end
             end, [], State#state.tabid),
    lists:map(fun(Pid) ->
                      gun:close(Pid)
              end, Pids),
    ok.

handle_call({checkout, HostInfo}, {Requester, _}, State) ->
    Ret = conn_checkout(HostInfo, Requester, State),
    ?LOG_DEBUG("(~p) checkout a conn: ~p -> ~p", [self(), HostInfo, Ret]),
    {reply, {self(), Ret}, State};
handle_call(dump_state, _From, State) ->
    {reply, ets_dump(State#state.tabid), State};
handle_call(tabid, _From, State) ->
    {reply, State#state.tabid, State};
handle_call(Req, From, State) ->
    ?LOG_WARNING("(~p) unhandled call (~p, ~p, ~p)", [self(), Req, From ,State]),
    {reply, {error, no_handler}, State}.

handle_cast(state, State) ->
    io:format("state: ~p~n", [State]),
    {noreply, State};
handle_cast(Req, State) ->
    ?LOG_WARNING("(~p) unhandled cast (~p, ~p)", [self(), Req, State]),
    {noreply, State}.

handle_info({checkin, Pid}, State) ->
    ?LOG_DEBUG("(~p) checkin a conn: ~p", [self(), Pid]),
    ok = conn_checkin(Pid, State),
    {noreply, State};
handle_info({gun_up, Pid, Proto}, State) ->
    ?LOG_DEBUG("(~p) gun_up a conn: ~p (~p)", [self(), Pid, Proto]),
    ok = conn_up(Pid, {ok, Proto}, State),
    {noreply, State};
handle_info({gun_down, Pid, _, _, _}, State) ->
    ?LOG_DEBUG("(~p) gun_down a conn: ~p", [self(), Pid]),
    ok = conn_down(Pid, {error, gun_down}, State),
    {noreply, State};
handle_info({'DOWN', MRef, _, Pid, Reason}, State) ->
    ?LOG_DEBUG("(~p) conn has gone away: ~p (~p)", [self(), Pid, Reason]),
    demonitor(MRef),
    ok = conn_down(Pid, {error, Reason}, State),
    {noreply, State};
handle_info(Req, State) ->
    ?LOG_WARNING("(~p) unhandled info (~p, ~p)", [self(), Req, State]),
    {noreply, State}.


%%
%% ets funs
%%
ets_shift_available_conns(TabId, HostInfo) ->
    case ets:lookup(TabId, {available_conns, HostInfo}) of
        [] ->
            none;
        [{_key, []}] ->
            none;
        [{_Key, [V|T]}] ->
            ets:insert(TabId, {{available_conns, HostInfo}, T}),
            {ok, V}
    end.

ets_unshift_available_conn(TabId, HostInfo, V) ->
    Conns = case ets:lookup(TabId, {available_conns, HostInfo}) of
                [] ->
                    [V];
                [{_Key, T}] ->
                    [V|T]
            end,
    ets:insert(TabId, {{available_conns, HostInfo}, Conns}).

ets_pickup_available_conn(TabId, HostInfo, Pid) ->
    Conns = case ets:lookup(TabId, {available_conns, HostInfo}) of
                [{_Key, T}] ->
                    T;
                _ ->
                    []
            end,
    case lists:partition(fun({P, _}) -> P =:= Pid end, Conns) of
        {[Conn|_], AvailableConns} ->
            ets:insert(TabId, {{available_conns, HostInfo}, AvailableConns}),
            {ok, Conn};
        _ ->
            none
    end.

ets_unshift_in_use_conn(TabId, HostInfo, V) ->
    Conns = case ets:lookup(TabId, {in_use_conns, HostInfo}) of
                [] ->
                    [V];
                [{_Key, T}] ->
                    [V|T]
            end,
    ets:insert(TabId, {{in_use_conns, HostInfo}, Conns}).

ets_pickup_in_use_conn(TabId, HostInfo, Pid) ->
    Conns = case ets:lookup(TabId, {in_use_conns, HostInfo}) of
                [{_Key, T}] ->
                    T;
                _ ->
                    []
            end,
    case lists:partition(fun({P, _}) -> P =:= Pid end, Conns) of
        {[Conn|_], InUseConns} ->
            ets:insert(TabId, {{in_use_conns, HostInfo}, InUseConns}),
            {ok, Conn};
        _ ->
            none
    end.

ets_unshift_awaiting_conn(TabId, HostInfo, V) ->
    Conns = case ets:lookup(TabId, {awaiting_conns, HostInfo}) of
                [] ->
                    [V];
                [{_Key, T}] ->
                    [V|T]
            end,
    ets:insert(TabId, {{awaiting_conns, HostInfo}, Conns}).

ets_pickup_awaiting_conn(TabId, HostInfo, Pid) ->
    Conns = case ets:lookup(TabId, {awaiting_conns, HostInfo}) of
                [{_Key, T}] ->
                    T;
                _ ->
                    []
            end,
    case lists:partition(fun({{P, _}, _}) -> P =:= Pid end, Conns) of
        {[Conn|_], AwaitingConns} ->
            ets:insert(TabId, {{awaiting_conns, HostInfo}, AwaitingConns}),
            {ok, Conn};
        _ ->
            none
    end.

ets_insert_conn_hostinfo(TabId, Pid, HostInfo) ->
    ets:insert(TabId, {{hostinfo, Pid}, HostInfo}).

ets_find_conn_hostinfo(TabId, Pid) ->
    case ets:lookup(TabId, {hostinfo, Pid}) of
        [] ->
            none;
        [{_, V}] ->
            {ok, V}
    end.

ets_delete_conn_hostinfo(TabId, Pid) ->
    ets:delete(TabId, {hostinfo, Pid}).

ets_dump(TabId) ->
    ets:foldl(
      fun ets_dump/2,
      #{
        host_conns => #{},
        conn_host => #{}
       },
      TabId).

ets_dump({{hostinfo, Pid}, HostInfo}, #{conn_host := M} = Acc) ->
    Acc#{conn_host => M#{Pid => HostInfo}};
ets_dump({{Group, HostInfo}, Conns}, #{host_conns := M} = Acc) ->
    case maps:find(HostInfo, M) of
        {ok, N} ->
            Acc#{host_conns => M#{HostInfo => N#{Group => Conns}}};
        _ ->
            Acc#{host_conns => M#{HostInfo => #{Group => Conns}}}
    end;
ets_dump(_V, Acc) ->
    Acc.


%%
%% private funs
%%
-spec conn_checkout(HostInfo::hostinfo(), Pid::pid(), State::state()) ->
    {ok, pid()} |
    {awaiting, pid()} |
    {error, Reason::any()}.
conn_checkout(
  {Host, Port, Transport} = HostInfo,
  Requester,
  State
 ) ->
    TabId = State#state.tabid,
    NewConnFun = State#state.new_conn,
    case ets_shift_available_conns(TabId, HostInfo) of
        {ok, {Pid, _} = Conn} ->
            %% conn available -> move to in_use_conns
            ets_unshift_in_use_conn(TabId, HostInfo, Conn),
            {ok, Pid};
        _ ->
            %% no conn available -> create a new conn
            case NewConnFun(Host, Port, Transport) of
                {ok, {Pid, _} = Conn} ->
                    ets_unshift_awaiting_conn(TabId, HostInfo, {Conn, Requester}),
                    ets_unshift_in_use_conn(TabId, HostInfo, Conn),
                    ets_insert_conn_hostinfo(TabId, Pid, HostInfo),
                    {awaiting, Pid};
                Err ->
                    Err
            end
    end.

-spec conn_up(pid(), term(), state()) -> ok.
conn_up(Pid, Msg, State) ->
    TabId = State#state.tabid,
    case ets_find_conn_hostinfo(TabId, Pid) of
        {ok, HostInfo} ->
            case ets_pickup_awaiting_conn(TabId, HostInfo, Pid) of
                {ok, {_, Requester}} ->
                    Requester ! Msg,
                    ok;
                _ ->
                    ?LOG_WARNING("(~p) conn is not awaiting: ~p (~p)", [self(), Pid, Msg]),
                    gun:close(Pid),
                    ok
            end;
        _ ->
            ?LOG_DEBUG("(~p) unknown conn has gone up: ~p (~p)", [self(), Pid, Msg]),
            gun:close(Pid),
            ok
    end.

-spec conn_checkin(pid(), state()) -> ok.
conn_checkin(Pid, State) ->
    TabId = State#state.tabid,
    case ets_find_conn_hostinfo(TabId, Pid) of
        {ok, HostInfo} ->
            case ets_pickup_in_use_conn(TabId, HostInfo, Pid) of
                {ok, Conn} ->
                    ets_unshift_available_conn(TabId, HostInfo, Conn),
                    ok;
                _ ->
                    ?LOG_WARNING("(~p) conn is not in use: ~p (~p)", [self(), Pid, HostInfo]),
                    gun:close(Pid),
                    ok
            end;
        _ ->
            ?LOG_DEBUG("(~p) unknown conn has checked-in: ~p", [self(), Pid]),
            gun:close(Pid),
            ok
    end.

-spec conn_down(pid(), term(), state()) -> ok.
conn_down(Pid, Msg, State) ->
    TabId = State#state.tabid,
    case ets_find_conn_hostinfo(TabId, Pid) of
        {ok, HostInfo} ->
            case ets_pickup_available_conn(TabId, HostInfo, Pid) of
                {ok, {_, MRef1}} ->
                    demonitor(MRef1, [flush]);
                _ ->
                    skip
            end,
            case ets_pickup_in_use_conn(TabId, HostInfo, Pid) of
                {ok, {_, MRef2}} ->
                    demonitor(MRef2, [flush]);
                _ ->
                    skip
            end,
            case ets_pickup_awaiting_conn(TabId, HostInfo, Pid) of
                {ok, {{_, MRef3}, Requester}} ->
                    Requester ! Msg,
                    demonitor(MRef3, [flush]);
                _ ->
                    skip
            end,
            ets_delete_conn_hostinfo(TabId, Pid),
            ok;
        _ ->
            ?LOG_DEBUG("(~p) unknown conn has gone down: ~p (~p)", [self(), Pid, Msg]),
            ok
    end.
