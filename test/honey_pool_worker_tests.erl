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
     {hostinfo, {"localhost", Port, tcp}}].

checkout_checkin_test_() ->
    {setup,
     fun() ->
             Config = boot_server(),
             {ok, Apps} = application:ensure_all_started(gun),
             {ok, Pid} = gen_server:start_link({local, ?MODULE}, honey_pool_worker, [{idle_timeout, 500}], []),
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
                               {ok, {await_up, {ReturnTo, Pid}}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               Ret1 = receive
                                          V1 -> V1
                                      end,
                               #{await_up_conns := AwaitUpConns1,
                                 up_conns := UpConns1,
                                 host_conns := HostConns1
                                } = gen_server:call(?MODULE, dump_state),
                               %% checkin
                               honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
                               #{await_up_conns := AwaitUpConns2,
                                 up_conns := UpConns2,
                                 host_conns := HostConns2
                                } = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": receive",
                                 ?_assertEqual({gun_up, Pid, http}, Ret1)},
                                {Title++": up_conns is empty after checkout",
                                 ?_assertMatch(#{}, UpConns1)},
                                {Title++": await_up_conns is empty after checkout",
                                 ?_assertMatch(#{}, AwaitUpConns1)},
                                {Title++": host_conns is empty after checkout",
                                 ?_assertMatch(#{}, HostConns1)},
                                {Title++": up_conns has pid after checkin",
                                 ?_assertMatch(#{Pid := HostInfo}, UpConns2)},
                                {Title++": await_up_conns is empty after checkin",
                                 ?_assertMatch(#{}, AwaitUpConns2)},
                                {Title++": host_conns after checkin",
                                 ?_assertMatch(#{HostInfo := [{Pid, _, _tref}]}, HostConns2)}
                               ]
                       end
                      },
                      {
                       "second checkout",
                       fun(Title) ->
                               %% checkout
                               {ok, {up, {ReturnTo, Pid}}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               #{await_up_conns := AwaitUpConns1,
                                 up_conns := UpConns1,
                                 host_conns := HostConns1
                                } = gen_server:call(?MODULE, dump_state),
                               %% checkin
                               honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
                               #{await_up_conns := AwaitUpConns2,
                                 up_conns := UpConns2,
                                 host_conns := HostConns2
                                } = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": up_conns is empty after checkout",
                                 ?_assertMatch(#{}, UpConns1)},
                                {Title++": await_up_conns is empty after checkout",
                                 ?_assertMatch(#{}, AwaitUpConns1)},
                                {Title++": host_conns is empty after checkout",
                                 ?_assertMatch(#{}, HostConns1)},
                                {Title++": up_conns has pid after checkin",
                                 ?_assertMatch(#{Pid := HostInfo}, UpConns2)},
                                {Title++": await_up_conns is empty after checkin",
                                 ?_assertMatch(#{}, AwaitUpConns2)},
                                {Title++": host_conns after checkin",
                                 ?_assertMatch(#{HostInfo := [{Pid, _, _tref}]}, HostConns2)}
                               ]
                       end
                      },
                      {
                       "sudden down",
                       fun(Title) ->
                               %% checkout
                               {ok, {up, {_ReturnTo, Pid}}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               %% closing conn
                               gun:close(Pid),
                               #{await_up_conns := AwaitUpConns,
                                 up_conns := UpConns,
                                 host_conns := HostConns
                                } = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": up_conns is empty after down",
                                 ?_assertMatch(#{}, UpConns)},
                                {Title++": await_up_conns is empty after down",
                                 ?_assertMatch(#{}, AwaitUpConns)},
                                {Title++": host_conns is empty after down",
                                 ?_assertMatch(#{}, HostConns)}
                               ]
                       end
                      },
                      {
                       "after idle_timeout (500 ms)",
                       fun(Title) ->
                               %% checkout
                               {ok, {await_up, {ReturnTo, Pid}}} = gen_server:call(?MODULE, {checkout, HostInfo}),
                               {gun_up, Pid, http} = receive
                                                         V1 -> V1
                                                     end,
                               %% checkin
                               honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
                               #{await_up_conns := AwaitUpConns1,
                                 up_conns := UpConns1,
                                 host_conns := HostConns1
                                } = gen_server:call(?MODULE, dump_state),
                               %% idle_timeout (with extra 250 ms)
                               timer:sleep(750),
                               #{await_up_conns := AwaitUpConns2,
                                 up_conns := UpConns2,
                                 host_conns := HostConns2
                                } = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [
                                {Title++": up_conns has pid after checkin",
                                 ?_assertMatch(#{Pid := HostInfo}, UpConns1)},
                                {Title++": await_up_conns is empty after checkin",
                                 ?_assertMatch(#{}, AwaitUpConns1)},
                                {Title++": host_conns after checkin",
                                 ?_assertMatch(#{HostInfo := [{Pid, _, _tref}]}, HostConns1)},
                                {Title++": up_conns is empty after idle timeout",
                                 ?_assertMatch(#{}, UpConns2)},
                                {Title++": await_up_conns is empty after idle timeout",
                                 ?_assertMatch(#{}, AwaitUpConns2)},
                                {Title++": host_conns is empty after idle timeout",
                                 ?_assertMatch(#{}, HostConns2)}
                               ]
                       end
                      }
                     ],
             F = fun({Title, Test}) ->
                         Test(Title)
                 end,
             lists:map(F, Cases)
     end}.
