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
          idle_timeout = infinity :: timeout(),
          await_up_timeout = 5000 :: timeout()
         }).

-type gun_opts() :: gun:opts().
-type gun_req_opts() :: gun:req_opts().

-type conn() :: {pid(), monitor_ref()}.
-type monitor_ref() :: reference().

-type transport() :: tcp | tls.
-type hostinfo() :: {Host :: string(), Port :: integer(), Transport :: transport()}.
-type state() :: #state{}.
-type uri() :: #uri{}.
