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
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    WpoolConfig = application:get_env(honey_pool, wpool, []),
    GunOpt = application:get_env(honey_pool, gun_opt, #{}),
    ChildSpecs = [
                  #{
                    id => honey_pool_workers,
                    start => {wpool, start_pool,
                              [honey_pool_worker, [WpoolConfig ++
                                                   [{workers, 1},
                                                    {overrun_warning, 300},
                                                    {worker, {honey_pool_worker, [GunOpt]}}
                                                   ]]
                             },
                    restart => permanent,
                    type => worker
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
