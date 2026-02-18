-module(honey_pool_limit_tests).
-include_lib("eunit/include/eunit.hrl").

-define(WORKER, honey_pool_worker).

limit_test_() ->
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
     [
      {"max_conns limit test",
       fun() ->
           %% Start worker with max_conns = 2
           {ok, Pid} = gen_server:start_link(honey_pool_worker, [{max_conns, 2}], []),
           HostInfo = {"localhost", 1234, tcp},

           %% 1st checkout (opens conn)
           {ok, {await_up, {Pid, C1}}} = gen_server:call(Pid, {checkout, HostInfo}),
           %% 2nd checkout (opens another)
           {ok, {await_up, {Pid, C2}}} = gen_server:call(Pid, {checkout, HostInfo}),
           
           %% 3rd checkout should fail due to max_conns
           Result = gen_server:call(Pid, {checkout, HostInfo}),
           ?assertEqual({error, {limit, max_conns}}, Result),

           %% Cleanup
           gun:close(C1),
           gun:close(C2),
           gen_server:stop(Pid)
       end},

      {"max_pending_conns limit test",
       fun() ->
           %% Start worker with max_pending_conns = 1
           {ok, Pid} = gen_server:start_link(honey_pool_worker, [{max_pending_conns, 1}], []),
           HostInfo = {"localhost", 1234, tcp},

           %% 1st checkout (becomes pending)
           {ok, {await_up, {Pid, C1}}} = gen_server:call(Pid, {checkout, HostInfo}),
           
           %% 2nd checkout should fail due to max_pending_conns
           Result = gen_server:call(Pid, {checkout, HostInfo}),
           ?assertEqual({error, {limit, max_pending_conns}}, Result),

           %% Cleanup
           gun:close(C1),
           gen_server:stop(Pid)
       end}
     ]}.
