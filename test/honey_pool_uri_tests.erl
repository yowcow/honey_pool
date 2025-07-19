-module(honey_pool_uri_tests).

-include_lib("eunit/include/eunit.hrl").

-include("honey_pool.hrl").


parse_uri_test_() ->
    Cases =
        [{"http://foobar.com",
          fun(Actual) ->
                  Expected =
                      {ok,
                       #uri{
                         host = "foobar.com",
                         path = "/",
                         query = "",
                         pathquery = "/",
                         port = 80,
                         transport = tcp
                        }},
                  ?_assertEqual(Expected, Actual)
          end},
         {"https://foobar.com/",
          fun(Actual) ->
                  Expected =
                      {ok,
                       #uri{
                         host = "foobar.com",
                         path = "/",
                         query = "",
                         pathquery = "/",
                         port = 443,
                         transport = tls
                        }},
                  ?_assertEqual(Expected, Actual)
          end},
         {"https://foobar.com:8443/hoge/fuga?foo=bar&bar=foo",
          fun({ok, Actual}) ->
                  Expected =
                      {ok,
                       #uri{
                         host = "foobar.com",
                         path = "/hoge/fuga",
                         query = "foo=bar&bar=foo",
                         pathquery = "/hoge/fuga?foo=bar&bar=foo",
                         port = 8443,
                         transport = tls
                        }},
                  Actual1 = Actual#uri{pathquery = lists:flatten(Actual#uri.pathquery)},
                  ?_assertEqual(Expected, {ok, Actual1})
          end},
         {<<"https://foobar.com:8443/hoge/fuga?foo=bar&bar=foo">>,
          fun({ok, Actual}) ->
                  Expected =
                      {ok,
                       #uri{
                         host = "foobar.com",
                         path = "/hoge/fuga",
                         query = "foo=bar&bar=foo",
                         pathquery = "/hoge/fuga?foo=bar&bar=foo",
                         port = 8443,
                         transport = tls
                        }},
                  ?_assertEqual(Expected,
                                {ok, Actual#uri{pathquery = lists:flatten(Actual#uri.pathquery)}})
          end},
         {<<"http://hogehoge/?hoge=${HOGE}&fuga=..&foo=]|">>,
          fun({ok, Actual}) ->
                  Expected =
                      {ok,
                       #uri{
                         host = "hogehoge",
                         path = "/",
                         query = "hoge=${HOGE}&fuga=..&foo=]|",
                         pathquery = "/?hoge=${HOGE}&fuga=..&foo=]|",
                         port = 80,
                         transport = tcp
                        }},
                  ?_assertEqual(Expected,
                                {ok, Actual#uri{pathquery = lists:flatten(Actual#uri.pathquery)}})
          end}],
    F = fun({Input, Test}) ->
                Actual = honey_pool_uri:parse(Input),
                [{Input, Test(Actual)}]
        end,
    lists:map(F, Cases).
