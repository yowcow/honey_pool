%%%-------------------------------------------------------------------
%% @doc honey_pool public API
%% @end
%%%-------------------------------------------------------------------

-module(honey_pool_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    honey_pool_sup:start_link().


stop(_State) ->
    ok.

%% internal functions
