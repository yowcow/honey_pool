-module(honey_pool_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/honey_pool.hrl").

checkout_test_() ->
    HostInfo = {"host", 123, #{}},
    State = #state{
               new_conn = fun(Host, Port, Opt) ->
                                  {ok,
                                   {pid, Host, Port, Opt}}
                          end
              },
    Cases = [
             {
              "no conns in the state",
              {checkout, HostInfo},
              State,
              {reply,
               {ok, {pid, "host", 123, #{}}},
               State#state{
                 host_conns = #{
                                HostInfo => #connections{
                                               in_use = [{pid, "host", 123, #{}}]
                                              }
                               },
                 conn_host = #{
                               {pid, "host", 123, #{}} => HostInfo
                              }
                }
              }
             },
             {
              "conn available in the state",
              {checkout, HostInfo},
              State#state{
                host_conns = #{
                               HostInfo => #connections{
                                              available = [pid1, pid2]
                                             }
                              },
                conn_host = #{
                              pid1 => HostInfo,
                              pid2 => HostInfo
                             }
               },
              {reply,
               {ok, pid1},
               State#state{
                 host_conns = #{
                                HostInfo => #connections{
                                               available = [pid2],
                                               in_use = [pid1]
                                              }
                               },
                 conn_host = #{
                               pid1 => HostInfo,
                               pid2 => HostInfo
                              }
                }
              }
             }
            ],
    F = fun({Title, Req, State0, Expected}) ->
                Actual = honey_pool_worker:handle_call(Req, from, State0),
                [{Title, ?_assertEqual(Expected, Actual)}]
        end,
    lists:map(F, Cases).

checkin_test_() ->
    HostInfo = {"host", 123, #{}},
    State = #state{
               new_conn = fun(Host, Port, Opt) ->
                                  {pid, Host, Port, Opt}
                          end,
               host_conns = #{
                              HostInfo => #connections{
                                             available = [pid2],
                                             in_use = [pid1]
                                            }
                             },
               conn_host = #{
                             pid1 => HostInfo,
                             pid2 => HostInfo
                            }
              },
    Cases = [
             {
              "checkin ok",
              {checkin, pid1},
              State,
              {noreply,
               State#state{
                 host_conns = #{
                                HostInfo => #connections{
                                               available = [pid1, pid2],
                                               in_use = []
                                              }
                               }
                }
              }
             },
             {
              "conn not in conn_host",
              {checkin, pid3},
              State,
              {noreply, State}
             },
             {
              "conn not in in_use",
              {checkin, pid3},
              State#state{
                conn_host = #{
                              pid3 => HostInfo
                             }
               },
              {noreply,
               State#state{
                 conn_host = #{
                               pid3 => HostInfo
                              },
                 host_conns = #{
                                HostInfo => #connections{
                                               available = [pid3, pid2],
                                               in_use = [pid1]
                                              }
                               }
                }
              }
             }
            ],
    F = fun({Title, Req, State0, Expected}) ->
                Actual = honey_pool_worker:handle_cast(Req, State0),
                [{Title, ?_assertEqual(Expected, Actual)}]
        end,
    lists:map(F, Cases).

close_test_() ->
    HostInfo = {"host", 123, #{}},
    State = #state{
               new_conn = fun(Host, Port, Opt) ->
                                  {pid, Host, Port, Opt}
                          end,
               host_conns = #{
                              HostInfo => #connections{
                                             available = [pid2],
                                             in_use = [pid1]
                                            }
                             },
               conn_host = #{
                             pid1 => HostInfo,
                             pid2 => HostInfo
                            }
              },
    Cases = [
             {
              "close conn in in_use",
              {gun_down, pid1, hoge, fuga, foo, bar},
              State,
              {noreply,
               State#state{
                 host_conns = #{
                                HostInfo => #connections{
                                               available = [pid2],
                                               in_use = []
                                              }
                               },
                 conn_host = #{
                               pid2 => HostInfo
                              }
                }
              }
             },
             {
              "close conn in available",
              {gun_down, pid2, hoge, fuga, foo, bar},
              State,
              {noreply,
               State#state{
                 host_conns = #{
                                HostInfo => #connections{
                                               available = [],
                                               in_use = [pid1]
                                              }
                               },
                 conn_host = #{
                               pid1 => HostInfo
                              }
                }
              }
             },
             {
              "conn not found in host_conns",
              {gun_down, pid3, hoge, fuga, foo, bar},
              State#state{
                conn_host = #{
                              pid3 => unknown_host
                             }
               },
              {noreply,
               State#state{
                 host_conns = #{
                                HostInfo => #connections{
                                               available = [pid2],
                                               in_use = [pid1]
                                              },
                                unknown_host => #connections{}
                               },
                 conn_host = #{}
                }
              }
             }
            ],
    F = fun({Title, Req, State0, Expected}) ->
                Actual = honey_pool_worker:handle_info(Req, State0),
                [{Title, ?_assertEqual(Expected, Actual)}]
        end,
    lists:map(F, Cases).
