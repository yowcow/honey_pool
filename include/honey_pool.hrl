-record(uri, {
          host = "" :: string(),
          path = "" :: string(),
          query = "" :: string(),
          pathquery = "" :: string(),
          port = 80 :: integer(),
          transport = tcp :: transport()
         }).

-record(connections, {
          available = [] :: [active_conn()],
          in_use = [] :: [active_conn()],
          awaiting = [] :: [awaiting_conn()]
         }).

-record(state, {
          new_conn :: fun((Host::string(), Port::integer(), Transport::transport())
                          -> {ok, conn()} | {error, Reason::any()}),
          tabid :: ets:tid(),
          idle_timeout = infinity :: timeout()
         }).

-type conn() :: {pid(), monitor_ref()}.
-type active_conn() :: {conn(), timer_ref()}.
-type awaiting_conn() :: {conn(), pid()}.

-type monitor_ref() :: reference().
-type timer_ref() :: reference()|no_ref.

-type transport() :: tcp | tls.
-type hostinfo() :: {Host::string(), Port::integer(), Transport::transport()}.
-type connections() :: #connections{}.
-type host_conns() :: #{ hostinfo() => connections() }.
-type state() :: #state{}.
-type uri() :: #uri{}.
