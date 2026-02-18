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
                 [{'_', [{"/", ?MODULE, []}]}]),
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
             {ok, Pid} = gen_server:start_link(
                           {local, ?MODULE}, honey_pool_worker, [{idle_timeout, 500}], []),
             [{apps, lists:flatten(Apps, proplists:get_value(apps, Config))},
              {pid, Pid} | Config]
     end,
     fun(Config) ->
             ok = gen_server:stop(?MODULE),
             cowboy:stop_listener(?LISTENER),
             lists:map(
               fun(App) ->
                       error_logger:tty(false),
                       try
                           application:stop(App)
                       after
                           error_logger:tty(true)
                       end
               end,
               proplists:get_value(apps, Config))
     end,
     fun(Config) ->
             HostInfo = proplists:get_value(hostinfo, Config),
             Cases = [{"initial checkout",
                       fun(Title) ->
                               %% checkout
                               {ok, {await_up, {ReturnTo, Pid}}} = gen_server:call(
                                                                     ?MODULE, {checkout, HostInfo}),
                               Ret1 =
                                   receive
                                       V1 -> V1
                                   end,
                               State1 = gen_server:call(?MODULE, dump_state),
                               %% checkin
                               honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
                               State2 = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [{Title ++ ": receive", ?_assertEqual({gun_up, Pid, http}, Ret1)},
                                {Title ++ ": dump_state 1",
                                 ?_assertEqual(
                                   #{
                                     await_up_conns => #{},
                                     checked_in_conns => #{},
                                     checked_out_conns => #{Pid => HostInfo},
                                     pool_conns => #{},
                                     cur_conns => 1,
                                     cur_pending_conns => 0,
                                     max_conns => infinity,
                                     max_pending_conns => infinity
                                    },
                                   State1)},
                                {Title ++ ": dump_state 2",
                                 ?_assertEqual(
                                   #{
                                     await_up_conns => #{},
                                     checked_in_conns => #{Pid => HostInfo},
                                     checked_out_conns => #{},
                                     pool_conns => #{HostInfo => [Pid]},
                                     cur_conns => 1,
                                     cur_pending_conns => 0,
                                     max_conns => infinity,
                                     max_pending_conns => infinity
                                    },
                                   State2)}]
                       end},
                      {"second checkout",
                       fun(Title) ->
                               %% checkout
                               {ok, {up, {ReturnTo, Pid}}} = gen_server:call(
                                                               ?MODULE, {checkout, HostInfo}),
                               State1 = gen_server:call(?MODULE, dump_state),
                               %% checkin
                               honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
                               State2 = gen_server:call(?MODULE, dump_state),
                               %% tests
                                                               [{Title ++ ": dump_state 1",
                                                                ?_assertEqual(
                                                                  #{
                                                                    await_up_conns => #{},
                                                                    checked_in_conns => #{},
                                                                    checked_out_conns => #{Pid => HostInfo},
                                                                    pool_conns => #{},
                                                                    cur_conns => 1,
                                                                    cur_pending_conns => 0,
                                                                    max_conns => infinity,
                                                                    max_pending_conns => infinity
                                                                   },
                                                                  State1)},
                                                               {Title ++ ": dump_state 2",
                                                                ?_assertEqual(
                                                                  #{
                                                                    await_up_conns => #{},
                                                                    checked_in_conns => #{Pid => HostInfo},
                                                                    checked_out_conns => #{},
                                                                    pool_conns => #{HostInfo => [Pid]},
                                                                    cur_conns => 1,
                                                                    cur_pending_conns => 0,
                                                                    max_conns => infinity,
                                                                    max_pending_conns => infinity
                                                                   },
                                                                  State2)}]
                               
                       end},
                      {"unexpected gun connection termination",
                       fun(Title) ->
                               %% checkout
                               {ok, {up, {_ReturnTo, Pid}}} = gen_server:call(
                                                                ?MODULE, {checkout, HostInfo}),
                               %% closing conn
                               gun:close(Pid),
                               State = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [{Title ++ ": conn is cleaned",
                                 ?_assertEqual(
                                   #{
                                     await_up_conns => #{},
                                     checked_in_conns => #{},
                                     checked_out_conns => #{},
                                     pool_conns => #{},
                                     cur_conns => 0,
                                     cur_pending_conns => 0,
                                     max_conns => infinity,
                                     max_pending_conns => infinity
                                    },
                                   State)}]
                       end},
                      {"after idle_timeout (500 ms)",
                       fun(Title) ->
                               %% checkout
                               {ok, {await_up, {ReturnTo, Pid}}} = gen_server:call(
                                                                     ?MODULE, {checkout, HostInfo}),
                               {gun_up, Pid, http} =
                                   receive
                                       V1 -> V1
                                   end,
                               %% checkin
                               honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
                               State1 = gen_server:call(?MODULE, dump_state),
                               %% idle_timeout (with extra 250 ms)
                               timer:sleep(750),
                               State2 = gen_server:call(?MODULE, dump_state),
                               %% tests
                               [{Title ++ ": before idle timeout",
                                 ?_assertEqual(
                                   #{
                                     await_up_conns => #{},
                                     checked_in_conns => #{Pid => HostInfo},
                                     checked_out_conns => #{},
                                     pool_conns => #{HostInfo => [Pid]},
                                     cur_conns => 1,
                                     cur_pending_conns => 0,
                                     max_conns => infinity,
                                     max_pending_conns => infinity
                                    },
                                   State1)},
                                {Title ++ ": after idle timeout",
                                 ?_assertEqual(
                                   #{
                                     await_up_conns => #{},
                                     checked_in_conns => #{},
                                     checked_out_conns => #{},
                                     pool_conns => #{},
                                     cur_conns => 0,
                                     cur_pending_conns => 0,
                                     max_conns => infinity,
                                     max_pending_conns => infinity
                                    },
                                   State2)}]
                       end}],
             F = fun({Title, Test}) ->
                         Test(Title)
                 end,
             lists:map(F, Cases)
     end}.
