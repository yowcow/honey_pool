-module(honey_pool_proc_tests).

-include_lib("eunit/include/eunit.hrl").

no_timeout_test_() ->
    {setup,
     fun() ->
             process_flag(trap_exit, true),
             ok
     end,
     fun(ok) ->
             process_flag(trap_exit, false),
             ok
     end,
     fun(_) ->
             {ok, Pid} = honey_pool_proc:start_link(hoge, 0),
             exit(Pid, hoge),
             Ret = receive
                       Msg -> Msg
                   end,
             [
              ?_assertMatch({'EXIT', Pid, hoge}, Ret)
             ]
     end}.

timeout_test_() ->
    {setup,
     fun() ->
             error_logger:tty(false),
             process_flag(trap_exit, true),
             ok
     end,
     fun(ok) ->
             error_logger:tty(true),
             process_flag(trap_exit, false),
             ok
     end,
     fun(_) ->
             {ok, Pid} = honey_pool_proc:start_link(hoge, 100),
             Ret = receive
                       Msg -> Msg
                   after
                       500 -> {error, timeout}
                   end,
             [
              ?_assertMatch({'EXIT', Pid, bye}, Ret)
             ]
     end}.
