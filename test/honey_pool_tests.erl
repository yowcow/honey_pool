-module(honey_pool_tests).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("honey_pool.hrl").


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
                  {"get: with legacy req_opts map (backward compat)",
                   fun() ->
                           %% A plain gun:req_opts() map (no conn_opts/req_opts keys) should
                           %% be treated as legacy request opts rather than silently ignored.
                           %% reply_to => self() is a valid gun req_opt and should be forwarded.
                           LegacyOpts = #{reply_to => self()},
                           Actual = honey_pool:get([Url, "/status/200/delay/50"], [], LegacyOpts, 1000),
                           ?assertMatch({ok, {200, _, _}}, Actual)
                   end},
                  {"get: with http2 prior knowledge",
                   fun() ->
                           %% Our cowboy test listener is configured with start_clear/3 (HTTP/1),
                           %% so we include an HTTP/1 fallback alongside http2. This verifies
                           %% that the protocols option is handled and that different conn_opts
                           %% values result in separate pooled connections.
                           ConnOpts1 = #{conn_opts => #{protocols => [http2, http], retry => 0}},
                           ConnOpts2 = #{conn_opts => #{protocols => [http2, http], retry => 1}},
                           Actual1 = honey_pool:get([Url, "/status/200/delay/10"], [], ConnOpts1, 1000),
                           ?assertMatch({ok, {200, _, _}}, Actual1),
                           Actual2 = honey_pool:get([Url, "/status/200/delay/10"], [], ConnOpts2, 1000),
                           ?assertMatch({ok, {200, _, _}}, Actual2),
                           %% Verify that different conn_opts values are pooled separately
                           {ok, #uri{port = Port}} = honey_pool_uri:parse(Url),
                           Key1 = {"localhost", Port, tcp, #{protocols => [http2, http], retry => 0}},
                           Key2 = {"localhost", Port, tcp, #{protocols => [http2, http], retry => 1}},
                           States = honey_pool:dump_state(),
                           CountConns =
                               fun(Key, SList) ->
                                       lists:foldl(fun(S, Acc) ->
                                                           Pool = maps:get(pool_conns, S),
                                                           Acc + length(maps:get(Key, Pool, []))
                                                   end, 0, SList)
                               end,
                           ?assertEqual(1, CountConns(Key1, States)),
                           ?assertEqual(1, CountConns(Key2, States))
                   end}],
             F = fun({Title, Test}) -> [{Title, Test}] end,
             lists:map(F, Cases)
     end}.
