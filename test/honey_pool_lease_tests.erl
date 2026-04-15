-module(honey_pool_lease_tests).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, honey_pool_lease_test_listener).
-define(LEASE_TIMEOUT, 200).


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
     {hostinfo, {"localhost", Port, tcp, #{}}}].


start_worker() ->
    start_worker(?LEASE_TIMEOUT).

start_worker(LeaseTimeout) ->
    {ok, _Apps} = application:ensure_all_started(gun),
    {ok, Pid} = gen_server:start_link(
                  honey_pool_worker,
                  [{idle_timeout, 5000},
                   {lease_timeout, LeaseTimeout}], []),
    Pid.


%% Test 1: lease_expired closes a checked_out connection
lease_expired_closes_checked_out_test_() ->
    {setup,
     fun() ->
             Config = boot_server(),
             Worker = start_worker(),
             [{worker, Worker} | Config]
     end,
     fun(Config) ->
             gen_server:stop(proplists:get_value(worker, Config)),
             cowboy:stop_listener(?LISTENER)
     end,
     fun(Config) ->
             Worker = proplists:get_value(worker, Config),
             HostInfo = proplists:get_value(hostinfo, Config),
             %% checkout (new connection)
             {ok, {await_up, {_ReturnTo, Pid}}} =
                 gen_server:call(Worker, {checkout, HostInfo}),
             {gun_up, Pid, http} = receive V1 -> V1 end,
             State1 = gen_server:call(Worker, dump_state),
             %% wait for lease to expire
             timer:sleep(?LEASE_TIMEOUT + 100),
             State2 = gen_server:call(Worker, dump_state),
             [{"checked_out before lease expires",
               ?_assertEqual(1, maps:size(maps:get(checked_out_conns, State1)))},
              {"connection returned to pool after lease expires",
               ?_assertEqual(1, maps:get(cur_conns, State2))},
              {"checked_out is empty after lease expires",
               ?_assertEqual(#{}, maps:get(checked_out_conns, State2))},
              {"checked_in after lease expires",
               ?_assertEqual(1, maps:size(maps:get(checked_in_conns, State2)))},
              {"in pool after lease expires",
               ?_assertEqual(1, maps:size(maps:get(pool_conns, State2)))}]
     end}.


%% Test 2: stale lease_expired after checkin must NOT destroy checked_in connection
%%
%% This simulates the race condition where erlang:cancel_timer/1 fails to
%% remove an already-fired {lease_expired, Pid} message from the mailbox.
%% We reproduce this by sending {lease_expired, Pid} directly to the worker
%% after checkin has been processed.
stale_lease_expired_must_not_destroy_checked_in_test_() ->
    {setup,
     fun() ->
             Config = boot_server(),
             %% Use infinity lease_timeout to prevent real timer from interfering;
             %% we manually inject the stale message.
             Worker = start_worker(infinity),
             [{worker, Worker} | Config]
     end,
     fun(Config) ->
             gen_server:stop(proplists:get_value(worker, Config)),
             cowboy:stop_listener(?LISTENER)
     end,
     fun(Config) ->
             Worker = proplists:get_value(worker, Config),
             HostInfo = proplists:get_value(hostinfo, Config),
             %% checkout (new connection)
             {ok, {await_up, {ReturnTo, Pid}}} =
                 gen_server:call(Worker, {checkout, HostInfo}),
             {gun_up, Pid, http} = receive V2 -> V2 end,
             %% checkin
             honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
             %% ensure checkin is processed
             _ = gen_server:call(Worker, dump_state),
             %% simulate stale lease_expired (timer fired after cancel_timer)
             Worker ! {lease_expired, Pid},
             %% ensure the message is processed
             StateAfterLease = gen_server:call(Worker, dump_state),
             %% connection must survive
             [{"connection survives stale lease_expired",
               ?_assertEqual(1, maps:get(cur_conns, StateAfterLease))},
              {"still checked_in after stale timer",
               ?_assertEqual(1, maps:size(maps:get(checked_in_conns, StateAfterLease)))},
              {"still in pool after stale timer",
               ?_assertEqual(1, maps:size(maps:get(pool_conns, StateAfterLease)))}]
     end}.


%% Test 3: connection is reusable after stale timer fires
stale_lease_expired_connection_reusable_test_() ->
    {setup,
     fun() ->
             Config = boot_server(),
             Worker = start_worker(infinity),
             [{worker, Worker} | Config]
     end,
     fun(Config) ->
             gen_server:stop(proplists:get_value(worker, Config)),
             cowboy:stop_listener(?LISTENER)
     end,
     fun(Config) ->
             Worker = proplists:get_value(worker, Config),
             HostInfo = proplists:get_value(hostinfo, Config),
             %% checkout (new connection)
             {ok, {await_up, {ReturnTo, Pid}}} =
                 gen_server:call(Worker, {checkout, HostInfo}),
             {gun_up, Pid, http} = receive V3 -> V3 end,
             %% checkin
             honey_pool:return_to(ReturnTo, Pid, {checkin, HostInfo, Pid}),
             _ = gen_server:call(Worker, dump_state),
             %% simulate stale lease_expired
             Worker ! {lease_expired, Pid},
             _ = gen_server:call(Worker, dump_state),
             %% re-checkout: connection should still be in pool
             {ok, {Status, {ReturnTo2, Pid2}}} =
                 gen_server:call(Worker, {checkout, HostInfo}),
             StateReCheckout = gen_server:call(Worker, dump_state),
             %% checkin again
             honey_pool:return_to(ReturnTo2, Pid2, {checkin, HostInfo, Pid2}),
             StateAfter = gen_server:call(Worker, dump_state),
             [{"re-checkout returns up (from pool, not new conn)",
               ?_assertEqual(up, Status)},
              {"re-checkout succeeds with same pid",
               ?_assertEqual(Pid, Pid2)},
              {"re-checkout is checked_out",
               ?_assertEqual(1, maps:size(maps:get(checked_out_conns, StateReCheckout)))},
              {"final state is checked_in",
               ?_assertEqual(1, maps:size(maps:get(checked_in_conns, StateAfter)))}]
     end}.
