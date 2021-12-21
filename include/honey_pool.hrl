-record(connections, {
          available = [] :: [pid()],
          in_use = [] :: [pid()]
         }).

-record(state, {
          new_conn :: fun((Host::string(), Port::integer(), Opt::map()) -> {ok, pid()} | {error, Reason::any()}),
          host_conns = #{} :: host_conns(),
          conn_host = #{} :: conn_host()
         }).

-type hostinfo() :: {Host::string(), Port::integer(), Opt::map()}.
-type connections() :: #connections{}.
-type host_conns() :: #{ hostinfo() => connections() }.
-type conn_host() :: #{ pid() => hostinfo() }.
-type state() :: #state{}.
