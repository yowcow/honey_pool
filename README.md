![master branch](https://github.com/yowcow/honey_pool/actions/workflows/ci.yml/badge.svg?branch=master)

honey_pool
=====

honey_pool is yet another gun connection pool based on worker pool.

**Alpha Quality**: Things can change.


WHY Honey Pool
--------------

Consider using honey_pool when your hackney_pool hangs while sending massive amount of HTTP reqs.
honey_pool starts multiple connection pools using worker pool, and manages gun HTTP connections in the pools.


HOW TO USE
----------

In the rebar.config deps, have:

```
{honey_pool, {git, "git://github.com/yowcow/honey_pool.git", {branch, "master"}}}
```

and start application `honey_pool` in your app, then try in shell:

```
1> logger:set_primary_config(level, info).
ok

2> honey_pool:get("https://example.com/").
{ok,{200,
     [{<<"accept-ranges">>,<<"bytes">>},
      {<<"age">>,<<"104009">>},
      {<<"cache-control">>,<<"max-age=604800">>},
      {<<"content-type">>,<<"text/html; charset=UTF-8">>},
      {<<"date">>,<<"Mon, 17 Jan 2022 07:45:39 GMT">>},
      {<<"etag">>,<<"\"3147526947\"">>},
      {<<"expires">>,<<"Mon, 24 Jan 2022 07:45:39 GMT">>},
      {<<"last-modified">>,<<"Thu, 17 Oct 2019 07:18:26 GMT">>},
      {<<"server">>,<<"ECS (oxr/836E)">>},
      {<<"vary">>,<<"Accept-Encoding">>},
      {<<"x-cache">>,<<"HIT">>},
      {<<"content-length">>,<<"1256">>}],
     <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n  "...>>}}

3> honey_pool:summarize_state().
[#{host_conns =>
       #{{"example.com",443,tls} =>
             #{available_conns => 1,awaiting_conns => 0,in_use_conns => 0}},
   total_conns => 1},
 #{host_conns => #{},total_conns => 0}]
```


HOW TO CONFIGURE
----------------

In the sys.config, have:

```
{honey_pool, [
              %% honey pool configurations
              {idle_timeout, 60000}, %% close connection after 60 sec of idle
              %% worker pool configurations (see worker pool docs for details)
              {wpool, [
                       {workers, 10}, %% start 10 connection pools
                       {overrun_warning, 50} %% warn when a pool takes over 50 msec to respond
                      ]},
              %% gun configurations (see gun docs for details)
              {gun_opt, #{
                          retry => 0, %% let go dead connections
                          connect_timeout => 1000 %% give up connecting after 1000 msec
                         }}
             ]}
```
