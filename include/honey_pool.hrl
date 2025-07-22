%% @doc Represents a parsed URI.
-record(uri, {
          host = "" :: string(),  %% The host part of the URI.
          path = "" :: string(),  %% The path part of the URI.
          query = "" :: string(),  %% The query string part of the URI.
          pathquery = "" :: string(),  %% The path and query string combined.
          port = 80 :: integer(),  %% The port number of the URI.
          transport = tcp :: transport()  %% The transport protocol (tcp or tls).
         }).

%% @doc Represents the state of a honey_pool_worker.
-record(state, {
          tabid :: ets:tid(),  %% The ETS table ID for storing connection data.
          gun_opts = #{} :: gun_opts(),  %% Default options for gun connections.
          idle_timeout = infinity :: timeout(),  %% Timeout for idle connections.
          await_up_timeout = 5000 :: timeout()  %% Timeout for waiting for a connection to be established.
         }).

%% @doc Gun options for connection settings.
-type gun_opts() :: gun:opts().

%% @doc Gun request options.
-type gun_req_opts() :: gun:req_opts().

%% @doc A gun connection, represented by its process ID and a monitor reference.
-type conn() :: {pid(), monitor_ref()}.

%% @doc A monitor reference for a process.
-type monitor_ref() :: reference().

%% @doc The transport protocol for the connection.
-type transport() :: tcp | tls.

%% @doc Information about a host, including host, port, and transport protocol.
-type hostinfo() :: {Host :: string(), Port :: integer(), Transport :: transport()}.

%% @doc The state of a honey_pool_worker.
-type state() :: #state{}.

%% @doc A parsed URI.
-type uri() :: #uri{}.
