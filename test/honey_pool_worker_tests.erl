-module(honey_pool_worker_tests).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, honey_pool_worker_test_listener).

init(Req0, State) ->
    Req = cowboy_req:reply(
            200,
            #{<<"content-type">> => <<"text/plain">>},
            <<"Hello">>,
            Req0),
    {ok, Req, State}.

boot_server() ->
    {ok, Apps} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{"/", ?MODULE, []}]
                  }]),
    {ok, _} = cowboy:start_clear(
                ?LISTENER,
                [{port, 0}],
                #{env => #{dispatch => Dispatch}}),
    Port = ranch:get_port(?LISTENER),
    [{apps, Apps},
     {port, Port},
     {hostinfo, {"localhost",Port,tcp}}].

checkout_checkin_test_() ->
    {setup,
     fun() ->
             Config = boot_server(),
             {ok, Apps} = application:ensure_all_started(gun),
             {ok, Pid} = gen_server:start_link({local, ?MODULE}, honey_pool_worker, [], []),
             [{apps, lists:flatten(Apps, proplists:get_value(apps, Config))},
              {pid, Pid} |
              Config]
     end,
     fun(Config) ->
             ok = gen_server:stop(?MODULE),
             cowboy:stop_listener(?LISTENER),
             lists:map(fun(App) ->
                               error_logger:tty(false),
                               try application:stop(App)
                               after error_logger:tty(true)
                               end
                       end, proplists:get_value(apps, Config))
     end,
     fun(Config) ->
             HostInfo = proplists:get_value(hostinfo, Config),
             Cases = [
                      {
                       "initial checkout",
                       fun(Title) ->
                               %% checkout
                               {ReturnTo, {awaiting, Pid}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               Ret1 = receive
                                          V1 -> V1
                                      end,
                               #{conn_host := ConnHost1,
                                 host_conns := HostConns1} = gen_server:call(?MODULE, dump_state),
                               %% checkin
                               ReturnTo ! {checkin, Pid},
                               #{conn_host := ConnHost2,
                                 host_conns := HostConns2} = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": receive",
                                 ?_assertEqual({ok, http}, Ret1)},
                                {Title++": conn_host after checkout",
                                 ?_assertMatch(
                                    #{Pid := HostInfo},
                                    ConnHost1)},
                                {Title++": host_conns after checkout",
                                 ?_assertMatch(
                                    #{HostInfo := #{
                                                    awaiting_conns := [],
                                                    in_use_conns := [{Pid, _}]
                                                   }},
                                    HostConns1)},
                                {Title++": conn_host after checkin",
                                 ?_assertMatch(
                                    #{Pid := HostInfo},
                                    ConnHost2)},
                                {Title++": host_conns after checkin",
                                 ?_assertMatch(
                                    #{HostInfo := #{
                                                    awaiting_conns := [],
                                                    in_use_conns := [],
                                                    available_conns := [{Pid, _}]
                                                   }},
                                    HostConns2)}
                               ]
                       end
                      },
                      {
                       "second checkout",
                       fun(Title) ->
                               %% checkout
                               {ReturnTo, {ok, Pid}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               #{conn_host := ConnHost1,
                                 host_conns := HostConns1} = gen_server:call(?MODULE, dump_state),
                               %% checkin
                               ReturnTo ! {checkin, Pid},
                               #{conn_host := ConnHost2,
                                 host_conns := HostConns2} = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": conn_host after checkout",
                                 ?_assertMatch(
                                    #{Pid := HostInfo},
                                    ConnHost1)},
                                {Title++": host_conns after checkout",
                                 ?_assertMatch(
                                    #{HostInfo := #{
                                                    awaiting_conns := [],
                                                    in_use_conns := [{Pid, _}]
                                                   }},
                                    HostConns1)},
                                {Title++": conn_host after checkin",
                                 ?_assertMatch(
                                    #{Pid := HostInfo},
                                    ConnHost2)},
                                {Title++": host_conns after checkin",
                                 ?_assertMatch(
                                    #{HostInfo := #{
                                                    awaiting_conns := [],
                                                    in_use_conns := [],
                                                    available_conns := [{Pid, _}]
                                                   }},
                                    HostConns2)}
                               ]
                       end
                      },
                      {
                       "sudden down",
                       fun(Title) ->
                               %% checkout
                               {_ReturnTo, {ok, Pid}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               #{conn_host := ConnHost1,
                                 host_conns := HostConns1} = gen_server:call(?MODULE, dump_state),
                               %% closing conn
                               gun:close(Pid),
                               #{conn_host := ConnHost2,
                                 host_conns := HostConns2} = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": conn_host after checkout",
                                 ?_assertMatch(
                                    #{Pid := HostInfo},
                                    ConnHost1)},
                                {Title++": host_conns after checkout",
                                 ?_assertMatch(
                                    #{HostInfo := #{
                                                    awaiting_conns := [],
                                                    in_use_conns := [{Pid, _}]
                                                   }},
                                    HostConns1)},
                                {Title++": conn_host after close",
                                 ?_assertEqual(
                                    #{},
                                    ConnHost2)},
                                {Title++": host_conns after close",
                                 ?_assertEqual(
                                    #{HostInfo => #{
                                                    awaiting_conns => [],
                                                    in_use_conns => [],
                                                    available_conns => []
                                                   }},
                                    HostConns2)}
                               ]
                       end
                      }
                     ],
             F = fun({Title, Test}) ->
                         Test(Title)
                 end,
             lists:map(F, Cases)
     end}.
