-module(honey_pool_edge_tests).
-include_lib("eunit/include/eunit.hrl").


edge_test_() ->
    {setup,
     fun() ->
             {ok, _} = application:ensure_all_started(worker_pool),
             {ok, _} = application:ensure_all_started(gun),
             ok
     end,
     fun(_) ->
             application:stop(gun),
             application:stop(worker_pool),
             ok
     end,
     [{"ghost connection counter drift test",
       fun() ->
               %% Start worker
               {ok, Pid} = gen_server:start_link(honey_pool_worker, [], []),
               HostInfo = {"localhost", 1234, tcp},

               %% Spawn a dummy process to act as a connection
               DummyConn = spawn(fun() -> receive _ -> ok end end),

               %% Manually checkin this "ghost" connection
               gen_server:cast(Pid, {checkin, HostInfo, DummyConn}),

               %% Wait a bit for cast to process
               timer:sleep(50),

               %% Check state: cur_conns SHOULD be 1
               State1 = gen_server:call(Pid, dump_state),
               CurConns1 = maps:get(cur_conns, State1),

               %% Kill the dummy connection to trigger DOWN/gun_down
               exit(DummyConn, kill),
               timer:sleep(50),

               %% Check state: cur_conns SHOULD be 0
               State2 = gen_server:call(Pid, dump_state),
               CurConns2 = maps:get(cur_conns, State2),

               gen_server:stop(Pid),

               %% Assertions
               %% If bug exists: CurConns1 will be 0, and CurConns2 will be -1 (or 0 if clamped)
               ?assertEqual(1, CurConns1),
               ?assertEqual(0, CurConns2)
       end}]}.
