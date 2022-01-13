-record(connections, {
          available = [] :: [available_conn()],
          in_use = [] :: [conn()],
          awaiting = [] :: [awaiting_conn()]
         }).

-record(state, {
          new_conn :: fun((Host::string(), Port::integer(), Opt::map())
                          -> {ok, conn()} | {error, Reason::any()}),
          inactivity_timeout = infinity :: timeout(),
          host_conns = #{} :: host_conns(),
          conn_host = #{} :: conn_host()
         }).

-type conn() :: {pid(), reference()}.
-type awaiting_conn() :: {conn(), pid()}.
-type available_conn() :: {conn(), reference()|no_ref}.

-type hostinfo() :: {Host::string(), Port::integer(), Opt::map()}.
-type connections() :: #connections{}.
-type host_conns() :: #{ hostinfo() => connections() }.
-type conn_host() :: #{ pid() => hostinfo() }.
-type state() :: #state{}.
