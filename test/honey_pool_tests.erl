-module(honey_pool_tests).

-export([
         init/2
        ]).

-include_lib("eunit/include/eunit.hrl").

init(Req0, State) ->
    StatusCode = binary_to_integer(cowboy_req:binding(status_code, Req0)),
    Delay = binary_to_integer(cowboy_req:binding(delay_millisec, Req0)),
    timer:sleep(Delay),
    Req = cowboy_req:reply(
            StatusCode,
            #{<<"content-type">> => <<"text/plain">>},
            <<"Hello">>,
            Req0),
    {ok, Req, State}.

request_test_() ->
    {setup,
     fun() ->
             Apps = lists:flatten(
                      lists:map(
                        fun(App) ->
                                {ok, Started} = application:ensure_all_started(App),
                                Started
                        end,
                        [cowboy, honey_pool])),
             Dispatch = cowboy_router:compile(
                          [{'_',
                            [{"/status/:status_code/delay/:delay_millisec", ?MODULE, []}]
                           }]),
             {ok, _} = cowboy:start_clear(
                         ?MODULE,
                         [{port, 0}],
                         #{env => #{dispatch => Dispatch}}),
             #{apps => Apps,
               url => io_lib:format(
                        "http://localhost:~.10b",
                        [ranch:get_port(?MODULE)])
              }
     end,
     fun(#{apps := Apps}) ->
             error_logger:tty(false),
             ok = cowboy:stop_listener(?MODULE),
             try lists:map(fun(App) ->
                                   application:stop(App)
                           end, Apps)
             after
                 error_logger:tty(true)
             end,
             ok
     end,
     fun(#{url := Url}) ->
             Cases = [
                      {"get: not found",
                       fun() ->
                               Actual = honey_pool:get([Url, "/foobar"]),
                               ?assertMatch(
                                  {ok, {404, _, _}},
                                  Actual)
                       end},
                      {"get: timeout=infinity",
                       fun() ->
                               Actual = honey_pool:get(
                                          [Url, "/status/200/delay/500"],
                                          [],
                                          infinity),
                               ?assertMatch(
                                  {ok, {200, _, _}},
                                  Actual)
                       end},
                      {"get: with query",
                       fun() ->
                               Actual = honey_pool:get(
                                          [Url, "/status/200/delay/50?foo=${FOO}&bar=][&buz=.."],
                                          [],
                                          infinity),
                               ?assertMatch(
                                  {ok, {200, _, _}},
                                  Actual)
                       end},
                      {"get: delay < timeout",
                       fun() ->
                               Actual = honey_pool:get(
                                          [Url, "/status/200/delay/50"],
                                          [],
                                          1000),
                               ?assertMatch(
                                  {ok, {200, _, _}},
                                  Actual)
                       end},
                      {"get: delay > timeout",
                       fun() ->
                               Actual = honey_pool:get(
                                          [Url, "/status/200/delay/100"],
                                          [],
                                          5),
                               ?assertMatch(
                                  {error, {timeout, await}},
                                  Actual)
                       end},
                      {"post: timeout=infinity",
                       fun() ->
                               Actual = honey_pool:post(
                                          [Url, "/status/200/delay/500"],
                                          [],
                                          <<"req data">>,
                                          infinity),
                               ?assertMatch(
                                  {ok, {200, _, _}},
                                  Actual)
                       end},
                      {"get: status=400",
                       fun() ->
                               Actual = honey_pool:post(
                                          [Url, "/status/400/delay/50"],
                                          [],
                                          <<"req data">>,
                                          infinity),
                               ?assertMatch(
                                  {ok, {400, _, no_data}},
                                  Actual)
                       end}
                     ],
             F = fun({Title, Test}) ->
                         [{Title, Test}]
                 end,
             lists:map(F, Cases)
     end}.
