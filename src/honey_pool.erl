-module(honey_pool).

-export([
         request/5,
         checkout/3,
         checkin/1
        ]).

-include_lib("kernel/include/logger.hrl").

-define(WORKER, honey_pool_worker).

-type method() :: get | post.
-type url() :: string().
-type status() :: integer().

-spec request(
        Method::method(),
        Url::url(),
        ReqHeaders::gun:req_headers(),
        Body::binary() | no_data,
        ReqOpts::gun:req_opts()
       ) ->
    {ok, {status(), gun:resp_headers(), binary() | no_data}}
    | {error, Reason::any()}.
request(Method, Url, ReqHeaders, Body, ReqOpts) ->
    U = parse_uri(Url),
    case checkout(
             maps:get(host, U),
             maps:get(port, U),
             #{ transport => maps:get(transport, U) }
            ) of
        {ok, Conn} ->
            Ret = try do_request(
                        Conn,
                        Method,
                        make_path(U),
                        ReqHeaders,
                        Body,
                        ReqOpts
                       )
                  catch
                      _:Err ->
                          {error, Err}
                  end,
            checkin(Conn),
            Ret
    end.

-spec do_request(
        Conn::pid(),
        Method::method(),
        Path::url(),
        ReqHeaders::gun:req_headers(),
        Body::binary() | no_body,
        ReqOpts::gun:req_opts()
       ) ->
    {ok, {status(), gun:resp_headers(), binary() | no_data}}
    | {error, Reason::any()}.
do_request(Conn, get, Path, ReqHeaders, _, ReqOpts) ->
    StreamRef = gun:get(Conn, Path, ReqHeaders, ReqOpts),
    case gun:await(Conn, StreamRef) of
        {response, fin, Status, Headers} ->
            {ok, {Status, Headers, no_data}};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(Conn, StreamRef),
            {ok, {Status, Headers, Body}}
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

-spec checkout(Host::string(), Port::integer(), Opt::map()) -> {ok, pid()}.
checkout(Host, Port, Opt) ->
    wpool:call(?WORKER, {checkout, {Host, Port, Opt}}).

-spec checkin(pid()) -> ok.
checkin(Conn) ->
    wpool:cast(?WORKER, {checkin, Conn}).


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
