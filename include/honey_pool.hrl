-doc "Represents a parsed URI.".
-record(uri, {
          host = "" :: string(),
          path = "" :: string(),
          query = "" :: string(),
          pathquery = "" :: string(),
          port = 80 :: integer(),
          transport = tcp :: transport()
         }).

-doc "Represents the state of a honey_pool_worker.".
-record(state, {
          tabid :: ets:tid(),
          gun_opts = #{} :: gun_opts(),
          idle_timeout = infinity :: timeout(),
          await_up_timeout = 5000 :: timeout()
         }).

-doc "Gun options.".
-type gun_opts() :: gun:opts().

-doc "Gun request options.".
-type gun_req_opts() :: gun:req_opts().

-doc "A gun connection.".
-type conn() :: {pid(), monitor_ref()}.

-doc "A monitor reference.".
-type monitor_ref() :: reference().

-doc "The transport protocol.".
-type transport() :: tcp | tls.

-doc "Information about a host.".
-type hostinfo() :: {Host :: string(), Port :: integer(), Transport :: transport()}.

-doc "The state of a honey_pool_worker.".
-type state() :: #state{}.

-doc "A parsed URI.".
-type uri() :: #uri{}.
