-module(honey_pool_proc).
-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2
        ]).

-export([
         start_link/2
        ]).

-record(state, {
          name :: atom()
         }).

init(Args) ->
    Name = proplists:get_value(name, Args),
    case proplists:get_value(timeout, Args) of
        0 ->
            ok;
        Timeout ->
            timer:send_after(Timeout, timeout)
    end,
    {ok, #state{name = Name}}.

handle_cast(Req, State) ->
    ?LOG_ALERT("unhandled cast (~p, ~p)", [Req, State]),
    {noreply, State}.

handle_call(Req, From, State) ->
    ?LOG_ALERT("unhandled call (~p, ~p, ~p)", [Req, From, State]),
    {reply, unhandled, State}.

handle_info(timeout, State) ->
    ?LOG_ALERT("received timeout", []),
    {stop, bye, State};
handle_info(Req, State) ->
    ?LOG_ALERT("unhandled info (~p, ~p)", [Req, State]),
    {noreply, State}.

start_link(Name, Timeout) ->
    gen_server:start_link(
      {local, Name},
      ?MODULE,
      [{name, Name}, {timeout, Timeout}],
      []
     ).
