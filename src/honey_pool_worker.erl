-module(honey_pool_worker).
-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
         init/1,
         handle_cast/2,
         handle_call/3
        ]).

-record(pool, {
          available = [] :: list(pid()),
          checkedout = [] :: list(pid())
         }).

-type generator_fun() :: fun((Arg::term()) -> {ok, pid()}).
-type pool() :: #pool{}.

-record(state, {
          generator :: generator_fun(),
          pool = #pool{} :: pool()
         }).

init(Args) ->
    Fun = proplists:get_value(generator, Args),
    {ok, #state{
            generator = Fun,
            pool = #pool{}
           }}.

handle_cast(dump_state, State) ->
    Pool = State#state.pool,
    ?LOG_INFO("pool: ~p", [
                           #{
                             available => Pool#pool.available,
                             checkedout => Pool#pool.checkedout
                            }
                          ]),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, unhandled, State}.
