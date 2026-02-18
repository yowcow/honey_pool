-module(honey_pool_limit_tests).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-define(WORKER, honey_pool_worker).
-define(LISTENER, honey_pool_limit_test_listener).


init(Req0, State) ->
    Req = cowboy_req:reply(
            200,
            #{<<"content-type">> => <<"text/plain">>},
            <<"Hello">>,
            Req0),
    {ok, Req, State}.


boot_server() ->
    {ok, Apps} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/", ?MODULE, []}]}]),
    {ok, _} = cowboy:start_clear(
                ?LISTENER,
                [{port, 0}],
                #{env => #{dispatch => Dispatch}}),
    Port = ranch:get_port(?LISTENER),
    [{apps, Apps},
     {port, Port},
     {hostinfo, {"localhost", Port, tcp}}].


limit_test_() ->
    {setup,
     fun() ->
             Config = boot_server(),
             {ok, Apps} = application:ensure_all_started(worker_pool),
             {ok, GunApps} = application:ensure_all_started(gun),
             [{apps, lists:flatten(Apps, lists:flatten(GunApps, proplists:get_value(apps, Config)))} | Config]
     end,
     fun(Config) ->
             cowboy:stop_listener(?LISTENER),
             lists:map(
               fun(App) ->
                       error_logger:tty(false),
                       try
                           application:stop(App)
                       after
                           error_logger:tty(true)
                       end
               end,
               proplists:get_value(apps, Config))
     end,
     fun(Config) ->
             HostInfo = proplists:get_value(hostinfo, Config),
             [{"max_conns limit test",
               fun() ->
                       %% Start worker with max_conns = 2
                       {ok, Pid} = gen_server:start_link(honey_pool_worker, [{max_conns, 2}], []),

                       %% 1st checkout (opens conn)
                       {ok, {await_up, {Pid, C1}}} = gen_server:call(Pid, {checkout, HostInfo}),
                       %% 2nd checkout (opens another)
                       {ok, {await_up, {Pid, C2}}} = gen_server:call(Pid, {checkout, HostInfo}),

                       %% 3rd checkout should fail due to max_conns
                       Result = gen_server:call(Pid, {checkout, HostInfo}),
                       ?assertEqual({error, {limit, max_conns}}, Result),

                       %% Cleanup
                       gun:close(C1),
                       gun:close(C2),
                       gen_server:stop(Pid)
               end},

              {"max_pending_conns limit test",
               fun() ->
                       %% Start worker with max_pending_conns = 1
                       {ok, Pid} = gen_server:start_link(honey_pool_worker, [{max_pending_conns, 1}], []),

                       %% 1st checkout (becomes pending)
                       {ok, {await_up, {Pid, C1}}} = gen_server:call(Pid, {checkout, HostInfo}),

                       %% 2nd checkout should fail due to max_pending_conns
                       Result = gen_server:call(Pid, {checkout, HostInfo}),
                       ?assertEqual({error, {limit, max_pending_conns}}, Result),

                       %% Cleanup
                       gun:close(C1),
                       gen_server:stop(Pid)
               end}]
     end}.
