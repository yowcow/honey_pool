%%%-------------------------------------------------------------------
%% @doc honey_pool public API
%% @end
%%%-------------------------------------------------------------------

-module(honey_pool_app).

-behaviour(application).

-export([start/2, stop/1]).


%% @doc Starts the honey_pool application.
start(_StartType, _StartArgs) ->
    honey_pool_sup:start_link().


%% @doc Stops the honey_pool application.
stop(_State) ->
    ok.

%% internal functions
