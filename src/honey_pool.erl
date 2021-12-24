-module(honey_pool).

-export([
         get/1, get/2, get/3, get/4,
         post/2, post/3, post/4, post/5,
         request/6
        ]).

-export([
         checkout/4,
         checkin/2
        ]).

-include_lib("kernel/include/logger.hrl").

-define(USER_AGENT, "honey-pool/0.1").
-define(WORKER, honey_pool_worker).
-define(DEFAULT_REQUEST_TIMEOUT, 1000). %% msec

-define(METHOD_GET, <<"GET">>).
-define(METHOD_POST, <<"POST">>).

-type method() :: binary().
-type url() :: string().
-type status() :: integer().

-type resp() :: {ok, {status(), gun:resp_headers(), binary() | no_data}} | {error, Reason::any()}.

-spec get(Url::url()) -> resp().
get(Url) ->
    get(Url, []).

-spec get(Url::url(), Headers::gun:req_headers()) -> resp().
get(Url, Headers) ->
    get(Url, Headers, #{}).

-spec get(Url::url(), Headers::gun:req_headers(), Opts::gun:opts()|Timeout::integer()) -> resp().
get(Url, Headers, Timeout) when is_integer(Timeout) ->
    get(Url, Headers, #{}, Timeout);
get(Url, Headers, Opt) ->
    get(Url, Headers, Opt, ?DEFAULT_REQUEST_TIMEOUT).

-spec get(Url::url(), Headers::gun:req_headers(), Opts::gun:opts(), Timeout::integer()) -> resp().
get(Url, Headers, Opt, Timeout) ->
    request(?METHOD_GET, Url, Headers, <<>>, Opt, Timeout).

-spec post(Url::url(), Headers::gun:req_headers()) -> resp().
post(Url, Headers) ->
    post(Url, Headers, <<>>).

-spec post(Url::url(), Headers::gun:req_headers(), Body::binary()) -> resp().
post(Url, Headers, Body) ->
    post(Url, Headers, Body, #{}).

-spec post(Url::url(), Headers::gun:req_headers(), Body::binary(), Opts::gun:opts()|Timeout::integer()) -> resp().
post(Url, Headers, Body, Timeout) when is_integer(Timeout) ->
    post(Url, Headers, Body, #{}, Timeout);
post(Url, Headers, Body, Opt) ->
    post(Url, Headers, Body, Opt, ?DEFAULT_REQUEST_TIMEOUT).

-spec post(Url::url(), Headers::gun:req_headers(), Body::binary(), Opts::gun:opts(), Timeout::integer()) -> resp().
post(Url, Headers, Body, Opt, Timeout) ->
    request(?METHOD_POST, Url, Headers, Body, Opt, Timeout).

-spec request(
        Method::method(),
        Url::url(),
        Headers::gun:req_headers(),
        Body::binary() | no_data,
        Opts::gun:opts(),
        Timeout0::integer()
       ) -> resp().
request(Method, Url, Headers, Body, Opts, Timeout0) ->
    U = parse_uri(Url),
    Host = maps:get(host, U),
    Port = maps:get(port, U),
    T0 = os:timestamp(),
    case checkout(Host, Port, #{transport => maps:get(transport, U)}, Timeout0) of
        {ok, {ReturnTo, Pid}} ->
            Ret = do_request(
                    Pid,
                    Method,
                    make_path(U),
                    Headers,
                    Body,
                    Opts,
                    Timeout0 - timestamp_interval(T0)
                   ),
            case Ret of
                {ok, {Status, _, _}} ->
                    ?LOG_INFO("(~p) ~p ~p -> ~p", [self(), Method, Url, Status]);
                Err ->
                    ?LOG_INFO("(~p) ~p ~p -> ~p", [self(), Method, Url, Err])
            end,
            checkin(ReturnTo, Pid),
            Ret;
        {error, Reason} ->
            {error, {checkout_error, Reason}}
    end.

-spec do_request(
        Pid::pid(),
        Method::method(),
        Path::url(),
        Headers::gun:req_headers(),
        Body::iodata(),
        Opts::gun:req_opts(),
        Timeout0::integer()
       ) -> resp().
do_request(Pid, Method, Path, Headers, Body, Opts, Timeout0) ->
    T0 = os:timestamp(),
    ReqHeaders = headers(Headers),
    StreamRef = gun:request(Pid, Method, Path, ReqHeaders, Body, Opts),
    Resp = case gun:await(Pid, StreamRef, Timeout0) of
               {response, fin, Status, RespHeaders} ->
                   {ok, {Status, RespHeaders, no_data}};
               {response, nofin, 200, RespHeaders} ->
                   case gun:await_body(
                          Pid,
                          StreamRef,
                          Timeout0 - timestamp_interval(T0)
                         ) of
                       {ok, RespBody} ->
                           {ok, {200, RespHeaders, RespBody}};
                       Err ->
                           Err
                   end;
               {response, nofin, Status, RespHeaders} ->
                   {ok, {Status, RespHeaders, no_data}};
               {error,{stream_error,{stream_error,protocol_error,'Content-length header received in a 204 response. (RFC7230 3.3.2)'}}} ->
                   %% some servers return content-length header
                   {ok, {204, [], no_data}};
               {error, Reason} ->
                   {error, Reason};
               V ->
                   {error, {unsupported, V}}
           end,
    gun:flush(StreamRef),
    gun:cancel(Pid, StreamRef),
    ?LOG_DEBUG("(~p) conn: ~p, request: ~p, response: ~p",
               [self(), Pid, {Method, Path, ReqHeaders, Body, Opts}, Resp]),
    Resp.

-spec make_path(map()) -> string().
make_path(UrlMap) ->
    Path = maps:get(path, UrlMap, "/"),
    case maps:get(query, UrlMap, "") of
        [] ->
            Path;
        Query ->
            Path ++ "?" ++ Query
    end.

-spec parse_uri(string() | binary()) -> map().
parse_uri(Uri) when is_binary(Uri) ->
    parse_uri(binary_to_list(Uri));
parse_uri(Uri) ->
    Parsed = uri_string:parse(Uri),
    M = case maps:find(scheme, Parsed) of
            {ok, "https"} ->
                #{ transport => tls };
            _ ->
                #{ transport => tcp }
        end,
    M#{
      host => maps:get(host, Parsed, ""),
      path => case maps:find(path, Parsed) of
                  {ok, ""} -> "/";
                  {ok, V} -> V;
                  _ -> "/"
              end,
      query => maps:get(query, Parsed, ""),
      port => maps:get(port, Parsed, case maps:get(transport, M) of
                                         tls -> 443;
                                         _ -> 80
                                     end)
     }.

-spec headers(gun:req_headers()) -> gun:req_headers().
headers(Headers) ->
    [
     {<<"user-agent">>, ?USER_AGENT}
     | [{string:lowercase(K), V} || {K, V} <- Headers]
    ].

-spec timestamp_msec({Mega::integer(), Sec::integer(), Micro::integer()}) -> integer().
timestamp_msec({Mega, Sec, Micro}) ->
    (Mega*1_000_000 + Sec)*1_000 + round(Micro/1000).

-spec timestamp_interval(T0::erlang:timestamp()) -> integer().
timestamp_interval(T0) ->
    timestamp_msec(os:timestamp()) - timestamp_msec(T0).

-spec checkout(
        Host::string(),
        Port::integer(),
        Opt::map(),
        Timeout::integer()
       ) -> {ok, {ReturnTo::pid(), Pid::pid()}} | {error, Reason::term()}.
checkout(Host, Port, Opt, Timeout0) ->
    T0 = os:timestamp(),
    case wpool:call(?WORKER, {checkout, {Host, Port, Opt}}, available_worker, Timeout0) of
        {ReturnTo, {ok, Pid}} ->
            {ok, {ReturnTo, Pid}};
        {ReturnTo, {awaiting, Pid}} ->
            Timeout1 = Timeout0 - timestamp_interval(T0),
            receive
                {ok, _} ->
                    {ok, {ReturnTo, Pid}};
                Err ->
                    gun:flush(Pid),
                    Err
            after
                Timeout1 ->
                    gun:flush(Pid),
                    {error, await_timeout}
            end
    end.

-spec checkin(ReturnTo::pid(), Pid::pid()) -> ok.
checkin(ReturnTo, Pid) ->
    gun:flush(Pid), %% flush before check-in
    ReturnTo ! {checkin, Pid},
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

timestamp_msec_test_() ->
    [
     ?_assertEqual(1_000_001_002, timestamp_msec({1, 1, 2000}))
    ].

timestamp_interval_test_() ->
    T0 = os:timestamp(),
    timer:sleep(100),
    Actual = timestamp_interval(T0),
    [
     ?_assert(Actual >= 100)
    ].

headers_test_() ->
    Input = [
             {<<"X-Hoge-Fuga">>, <<"Hoge">>},
             {"X-Hoge-Fuga", <<"Hoge">>}
            ],
    Expected = [
                {<<"user-agent">>, "honey-pool/0.1"},
                {<<"x-hoge-fuga">>, <<"Hoge">>},
                {"x-hoge-fuga", <<"Hoge">>}
               ],
    Actual = headers(Input),
    [?_assertEqual(Expected, Actual)].

parse_uri_test_() ->
    Cases = [
             {
              "http://foobar.com",
              #{
                host => "foobar.com",
                path => "/",
                query => "",
                port => 80,
                transport => tcp
               }
             },
             {
              "https://foobar.com/",
              #{
                host => "foobar.com",
                path => "/",
                query => "",
                port => 443,
                transport => tls
               }
             },
             {
              "https://foobar.com:8443/hoge/fuga?foo=bar&bar=foo",
              #{
                host => "foobar.com",
                path => "/hoge/fuga",
                query => "foo=bar&bar=foo",
                port => 8443,
                transport => tls
               }
             },
             {
              <<"https://foobar.com:8443/hoge/fuga?foo=bar&bar=foo">>,
              #{
                host => "foobar.com",
                path => "/hoge/fuga",
                query => "foo=bar&bar=foo",
                port => 8443,
                transport => tls
               }
             }
            ],
    F = fun({Input, Expected}) ->
                Actual = parse_uri(Input),
                [{Input, ?_assertEqual(Expected, Actual)}]
        end,
    lists:map(F, Cases).
-endif.
