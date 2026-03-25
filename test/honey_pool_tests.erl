-module(honey_pool_tests).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").


init(Req0, State) ->
    StatusCode = binary_to_integer(cowboy_req:binding(status_code, Req0)),
    Delay = binary_to_integer(cowboy_req:binding(delay_millisec, Req0)),
    timer:sleep(Delay),
    Req = cowboy_req:reply(StatusCode,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello">>,
                           Req0),
    {ok, Req, State}.


request_test_() ->
    {setup,
     fun() ->
             Apps =
                 lists:flatten(
                   lists:map(fun(App) ->
                                     {ok, Started} = application:ensure_all_started(App),
                                     Started
                             end,
                             [cowboy, honey_pool])),
             Dispatch =
                 cowboy_router:compile([{'_',
                                         [{"/status/:status_code/delay/:delay_millisec",
                                           ?MODULE,
                                           []}]}]),
             {ok, _} = cowboy:start_clear(?MODULE, [{port, 0}], #{env => #{dispatch => Dispatch}}),
             #{apps => Apps, url => io_lib:format("http://localhost:~.10b", [ranch:get_port(?MODULE)])}
     end,
     fun(#{apps := Apps}) ->
             error_logger:tty(false),
             ok = cowboy:stop_listener(?MODULE),
             try
                 lists:map(fun(App) -> application:stop(App) end, Apps)
             after
                 error_logger:tty(true)
             end,
             ok
     end,
     fun(#{url := Url}) ->
             Cases =
                 [{"get: not found",
                   fun() ->
                           Actual = honey_pool:get([Url, "/foobar"]),
                           ?assertMatch({ok, {404, _, _}}, Actual)
                   end},
                  {"get: timeout=infinity",
                   fun() ->
                           Actual = honey_pool:get([Url, "/status/200/delay/500"], [], infinity),
                           ?assertMatch({ok, {200, _, _}}, Actual)
                   end},
                  {"get: with query",
                   fun() ->
                           Actual =
                               honey_pool:get([Url, "/status/200/delay/50?foo=${FOO}&bar=][&buz=.."],
                                              [],
                                              infinity),
                           ?assertMatch({ok, {200, _, _}}, Actual)
                   end},
                  {"get: delay < timeout",
                   fun() ->
                           Actual = honey_pool:get([Url, "/status/200/delay/50"], [], 1000),
                           ?assertMatch({ok, {200, _, _}}, Actual)
                   end},
                  {"get: delay > timeout",
                   fun() ->
                           Actual = honey_pool:get([Url, "/status/200/delay/100"], [], 5),
                           ?assertMatch({error, {timeout, await}}, Actual)
                   end},
                  {"post: timeout=infinity",
                   fun() ->
                           Actual =
                               honey_pool:post([Url, "/status/200/delay/500"], [], <<"req data">>, infinity),
                           ?assertMatch({ok, {200, _, _}}, Actual)
                   end},
                  {"get: status=400",
                   fun() ->
                           Actual =
                               honey_pool:post([Url, "/status/400/delay/50"], [], <<"req data">>, infinity),
                           ?assertMatch({ok, {400, _, no_data}}, Actual)
                   end},
                  {"get: await_up timeout",
                   fun() ->
                           HttpsUrl = string:replace(Url, "http", "https", leading),
                           Actual = honey_pool:get(HttpsUrl, [], 10),
                           ?assertMatch({error, {checkout, {timeout, await_up}}}, Actual)
                   end},
                  {"get: with http2 prior knowledge",
                  fun() ->
                          %% This won't actually succeed because our cowboy test listener
                          %% is not configured for http2, but we can verify that the
                          %% protocols option is handled and a connection attempt is made.
                          ConnOpts1 = #{conn_opts => #{protocols => [http2]}},
                          ConnOpts2 = #{conn_opts => #{protocols => [http2], alt => true}},
                          Actual1 = honey_pool:get([Url, "/status/200/delay/10"], [], ConnOpts1, 1000),
                          ?assertMatch({ok, {200, _, _}}, Actual1),
                          Actual2 = honey_pool:get([Url, "/status/200/delay/10"], [], ConnOpts2, 1000),
                          ?assertMatch({ok, {200, _, _}}, Actual2),
                          %% Verify that different conn_opts values are pooled separately
                          State = honey_pool:dump_state(),
                          case State of
                              L when is_list(L) ->
                                  ?assert(length(L) >= 2);
                              M when is_map(M) ->
                                  ?assert(maps:size(M) >= 2);
                              _ ->
                                  ?assert(false)
                          end
                  end}],
             F = fun({Title, Test}) -> [{Title, Test}] end,
             lists:map(F, Cases)
     end}.
