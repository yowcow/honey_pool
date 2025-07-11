-module(honey_pool_uri).

-export([parse/1]).

-include("include/honey_pool.hrl").


-spec parse(string() | binary()) -> {ok, uri()} | {error, term()}.
parse(Uri) when is_binary(Uri) ->
    parse(binary_to_list(Uri));
parse(Uri) ->
    {UriWithoutQuery, Query} =
        case string:split(Uri, "?") of
            [UriPart, QueryPart] ->
                {UriPart, QueryPart};
            [UriPart] ->
                {UriPart, ""}
        end,
    try
        Parsed = uri_string:parse(UriWithoutQuery),
        Transport =
            case maps:find(scheme, Parsed) of
                {ok, "https"} -> tls;
                _ -> tcp
            end,
        Path =
            case maps:find(path, Parsed) of
                {ok, ""} -> "/";
                {ok, V} -> V;
                _ -> "/"
            end,
        Port = maps:get(
                 port,
                 Parsed,
                 case Transport of
                     tls -> 443;
                     _ -> 80
                 end),
        {ok, #uri{
               host = maps:get(host, Parsed, ""),
               path = Path,
               query = Query,
               pathquery =
                   case Query of
                       [] -> Path;
                       _ -> [Path, "?", Query]
                   end,
               port = Port,
               transport = Transport
              }}
    catch
        _:Err ->
            {error, {Err, UriWithoutQuery}}
    end.
