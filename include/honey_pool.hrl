-record(uri, {
          host = "" :: string(),
          path = "" :: string(),
          query = "" :: string(),
          pathquery = "" :: string(),
          port = 80 :: integer(),
          transport = tcp :: transport()
         }).

-record(connections, {
          available = [] :: [conn()],
          in_use = [] :: [conn()],
          awaiting = [] :: [awaiting_conn()]
         }).

-record(state, {
          new_conn :: fun((Host::string(), Port::integer(), Transport::transport())
                          -> {ok, conn()} | {error, Reason::any()}),
          tabid :: ets:tid()
         }).

-type conn() :: {pid(), reference()}.
-type awaiting_conn() :: {conn(), pid()}.

-type transport() :: tcp | tls.
-type hostinfo() :: {Host::string(), Port::integer(), Transport::transport()}.
-type connections() :: #connections{}.
-type host_conns() :: #{ hostinfo() => connections() }.
-type state() :: #state{}.
-type uri() :: #uri{}.
