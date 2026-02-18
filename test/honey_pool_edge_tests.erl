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
     [{"manual checkin connection counter test",
       fun() ->
               %% Test that manual checkin (via {checkin, HostInfo, Pid} message)
               %% properly increments and decrements cur_conns.
               %% This is the legitimate way to add externally-created connections to the pool.
               
               %% Start worker
               {ok, Pid} = gen_server:start_link(honey_pool_worker, [], []),
               HostInfo = {"localhost", 1234, tcp},

               %% Spawn a dummy process to act as a connection
               DummyConn = spawn(fun() -> receive _ -> ok end end),

               %% Manually checkin this connection
               gen_server:cast(Pid, {checkin, HostInfo, DummyConn}),

               %% Wait a bit for cast to process
               timer:sleep(50),

               %% Check state: cur_conns SHOULD be 1 (connection was added)
               State1 = gen_server:call(Pid, dump_state),
               CurConns1 = maps:get(cur_conns, State1),

               %% Kill the dummy connection to trigger monitor notification
               exit(DummyConn, kill),
               timer:sleep(50),

               %% Check state: cur_conns SHOULD be 0 (connection was removed)
               State2 = gen_server:call(Pid, dump_state),
               CurConns2 = maps:get(cur_conns, State2),

               gen_server:stop(Pid),

               %% Assertions: verify counter incremented and decremented correctly
               ?assertEqual(1, CurConns1),
               ?assertEqual(0, CurConns2)
       end}]}.
