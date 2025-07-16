%%%-------------------------------------------------------------------
%% @doc honey_pool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(honey_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


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
    WpoolUserConfig = application:get_env(honey_pool, wpool, []),
    WpoolDefaultConfig =
        #{workers => 2,
          overrun_warning => 300,
          worker =>
              {honey_pool_worker,
               [{gun_opts, GunOpts},
                {idle_timeout, IdleTimeout}]}},
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
