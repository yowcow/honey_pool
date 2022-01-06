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

and start application `honey_pool` in your app, then:

```erlang
{ok, {StatusCode, RespStatus, RespBody}} = honey_pool:get("http://example.com/path/to/endpoint"),
```


HOW TO CONFIGURE
----------------

In the sys.config, have:

```
{honey_pool, [
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
