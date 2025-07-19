-module(honey_pool_stress_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/2]).


stress_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun run/1}.


setup() ->
    Apps =
        lists:flatten(
          lists:map(fun(App) ->
                            {ok, Started} = application:ensure_all_started(App),
                            Started
                    end,
                    [cowboy, honey_pool])),
    Dispatch = cowboy_router:compile([{'_', [{"/", ?MODULE, []}]}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 0}], #{env => #{dispatch => Dispatch}}),
    #{
      apps => Apps,
      url => io_lib:format("http://localhost:~.10b", [ranch:get_port(?MODULE)])
     }.


cleanup(#{apps := Apps}) ->
    error_logger:tty(false),
    ok = cowboy:stop_listener(?MODULE),
    try
        lists:map(fun(App) -> application:stop(App) end, Apps)
    after
        error_logger:tty(true)
    end,
    ok.


-define(CONCURRENCY,          100).
-define(REQS_PER_CHILD,       100).
-define(CHILD_TIMEOUT,        5000).  % Timeout for child processes to finish
-define(RESPONSE_STATUS_CODE, 200).
-define(RESPONSE_MAX_DELAY,   20).
-define(RESPONSE_BODY,        <<"Hello">>).


run(#{url := Url}) ->
    [{"a request",
      fun() ->
              Resp = honey_pool:get(Url, [], 200),
              ?assertMatch({ok, {200, _, ?RESPONSE_BODY}}, Resp)
      end},
     {"many requests",
      fun() ->
              Parent = self(),
              Result =
                  lists:foldl(fun(#{ok := OkCount, error := ErrCount},
                                  Acc = #{ok := OkTotal, error := ErrTotal}) ->
                                      Acc#{ok => OkTotal + OkCount, error => ErrTotal + ErrCount}
                              end,
                              #{ok => 0, error => 0},
                              collect_from_children(spawn_children(Parent, Url))),
              ?assertEqual(#{ok => ?CONCURRENCY * ?REQS_PER_CHILD, error => 0}, Result)
      end}].


spawn_children(Parent, Url) ->
    lists:map(fun(_) -> spawn_link(fun() -> run_child(Parent, Url, ?REQS_PER_CHILD) end) end,
              lists:seq(1, ?CONCURRENCY)).


run_child(Parent, Url, Count) ->
    Result =
        lists:foldl(fun(_, #{ok := OkCount, error := ErrCount} = Acc) ->
                            case honey_pool:get(Url, [], 200) of
                                {ok, {?RESPONSE_STATUS_CODE, _, _}} -> Acc#{ok => OkCount + 1};
                                _ -> Acc#{error => ErrCount + 1}
                            end
                    end,
                    #{ok => 0, error => 0},
                    lists:seq(1, Count)),
    Parent ! {self(), Result},
    ok.


collect_from_children(Workers) ->
    collect_from_children(Workers, []).


collect_from_children([], Results) ->
    Results;
collect_from_children(Workers, Results) ->
    receive
        {Child, Result} ->
            collect_from_children(lists:delete(Child, Workers), [Result | Results])
    after
        ?CHILD_TIMEOUT ->
            ?LOG_ERROR("Timeout waiting for child processes to finish"),
            []
    end.


init(Req0, State) ->
    Delay = rand:uniform(?RESPONSE_MAX_DELAY),  % Random delay to simulate variability
    timer:sleep(Delay),
    Req = cowboy_req:reply(?RESPONSE_STATUS_CODE,
                           #{<<"content-type">> => <<"text/plain">>},
                           ?RESPONSE_BODY,
                           Req0),
    {ok, Req, State}.
