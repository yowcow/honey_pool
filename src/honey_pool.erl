-module(honey_pool).

-export([
         get/1, get/2, get/3,
         post/2, post/3, post/4,
         request/5
        ]).

-export([
         checkout/3,
         checkin/2
        ]).

-include_lib("kernel/include/logger.hrl").

-define(USER_AGENT, "honey-pool/0.1").
-define(WORKER, honey_pool_worker).

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

-spec get(Url::url(), Headers::gun:req_headers(), Opts::gun:opts()) -> resp().
get(Url, Headers, Opt) ->
    request(?METHOD_GET, Url, Headers, <<>>, Opt).

-spec post(Url::url(), Headers::gun:req_headers()) -> resp().
post(Url, Headers) ->
    post(Url, Headers, <<>>).

-spec post(Url::url(), Headers::gun:req_headers(), Body::binary()) -> resp().
post(Url, Headers, Body) ->
    post(Url, Headers, Body, #{}).

-spec post(Url::url(), Headers::gun:req_headers(), Body::binary(), Opts::gun:opts()) -> resp().
post(Url, Headers, Body, Opt) ->
    request(?METHOD_POST, Url, Headers, Body, Opt).

-spec request(
        Method::method(),
        Url::url(),
        Headers::gun:req_headers(),
        Body::binary() | no_data,
        Opts::gun:opts()
       ) -> resp().
request(Method, Url, Headers, Body, Opts) ->
    U = parse_uri(Url),
    case checkout(
             maps:get(host, U),
             maps:get(port, U),
             #{ transport => maps:get(transport, U) }
            ) of
        {ReturnTo, Conn} ->
            Ret = try do_request(
                        Conn,
                        Method,
                        make_path(U),
                        Headers,
                        Body,
                        Opts
                       )
                  catch
                      _:Err ->
                          checkin(ReturnTo, Conn),
                          throw({error, Err})
                  end,
            checkin(ReturnTo, Conn),
            Ret
    end.

-spec do_request(
        Conn::pid(),
        Method::method(),
        Path::url(),
        Headers::gun:req_headers(),
        Body::iodata(),
        Opts::gun:req_opts()
       ) -> resp().
do_request(Conn, Method, Path, Headers, Body, Opts) ->
    ReqHeaders = [
                  {<<"user-agent">>, ?USER_AGENT}
                  | Headers
                 ],
    StreamRef = gun:request(Conn, Method, Path, ReqHeaders, Body, Opts),
    case gun:await(Conn, StreamRef) of
        {response, fin, Status, RespHeaders} ->
            {ok, {Status, RespHeaders, no_data}};
        {response, nofin, Status, RespHeaders} ->
            {ok, RespBody} = gun:await_body(Conn, StreamRef),
            {ok, {Status, RespHeaders, RespBody}}
    end.

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
      path => maps:get(path, Parsed, "/"),
      query => maps:get(query, Parsed, ""),
      port => maps:get(port, Parsed, case maps:get(transport, M) of
                                         tls -> 443;
                                         _ -> 80
                                     end)
     }.

-spec checkout(Host::string(), Port::integer(), Opt::map()) -> {pid(), {ok, pid()}}.
checkout(Host, Port, Opt) ->
    case wpool:call(?WORKER, {checkout, {Host, Port, Opt}}) of
        {ReturnTo, {ok, Conn}} ->
            {ReturnTo, Conn};
        {ReturnTo, {awaiting, Conn}} ->
            receive X -> X end,
            {ReturnTo, Conn};
        Err ->
            throw(Err)
    end.

-spec checkin(pid(), pid()) -> ok.
checkin(ReturnTo, Conn) ->
    gun:flush(Conn), %% flush before check-in
    ReturnTo ! {checkin, Conn}.
    %wpool:cast(?WORKER, {checkin, Conn}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_uri_test_() ->
    Cases = [
             {
              "http://foobar.com",
              #{
                host => "foobar.com",
                path => "",
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
