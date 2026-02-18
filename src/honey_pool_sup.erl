%%%-------------------------------------------------------------------
%% @doc honey_pool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(honey_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


%% @doc Starts the honey_pool supervisor.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 0,
                 period => 1
                },
    GunOpts = application:get_env(honey_pool, gun_opts, #{}),
    IdleTimeout = application:get_env(honey_pool, idle_timeout, infinity),
    AwaitUpTimeout = application:get_env(honey_pool, await_up_timeout, 5000),
    MaxConns = application:get_env(honey_pool, max_conns, infinity),
    MaxPendingConns = application:get_env(honey_pool, max_pending_conns, infinity),
    WpoolUserConfig = application:get_env(honey_pool, wpool, []),
    WpoolDefaultConfig =
        #{
          workers => 2,
          overrun_warning => 300,
          worker =>
              {honey_pool_worker,
               [{gun_opts, GunOpts},
                {idle_timeout, IdleTimeout},
                {await_up_timeout, AwaitUpTimeout},
                {max_conns, MaxConns},
                {max_pending_conns, MaxPendingConns}]}
         },
    WpoolConfig =
        maps:to_list(
          maps:merge(WpoolDefaultConfig, maps:from_list(WpoolUserConfig))),
    ChildSpecs = [#{
                    id => honey_pool_workers,
                    start => {wpool, start_pool, [honey_pool_worker, WpoolConfig]},
                    restart => permanent,
                    type => worker
                   }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
