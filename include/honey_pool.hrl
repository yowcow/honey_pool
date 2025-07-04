-record(uri, {
          host = "" :: string(),
          path = "" :: string(),
          query = "" :: string(),
          pathquery = "" :: string(),
          port = 80 :: integer(),
          transport = tcp :: transport()
         }).

-record(state, {
          tabid :: ets:tid(),
          gun_opts = #{} :: gun_opts(),
          idle_timeout = infinity :: timeout()
         }).

-type gun_opts() :: gun:opts().
-type gun_req_opts() :: gun:req_opts().

-type conn() :: {pid(), monitor_ref()}.
-type monitor_ref() :: reference().

-type transport() :: tcp | tls.
-type hostinfo() :: {Host::string(), Port::integer(), Transport::transport()}.
-type state() :: #state{}.
-type uri() :: #uri{}.

%% Constants
-define(DEFAULT_KEEPALIVE_TIMEOUT, 30000).  % 30 seconds
-define(HTTP_STATUS_OK, 200).
-define(HTTP_STATUS_NO_CONTENT, 204).
