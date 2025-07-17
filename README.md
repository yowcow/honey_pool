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

2> honey_pool:dump_state().
[#{await_up_conns => #{},host_conns => #{},up_conns => #{}},
 #{await_up_conns => #{},host_conns => #{},up_conns => #{}},
 #{await_up_conns => #{},host_conns => #{},up_conns => #{}}]

3> honey_pool:get("http://example.com/", 100).
{error,{checkout,{timeout,await_up}}}

4> honey_pool:dump_state().
[#{await_up_conns => #{},
   host_conns =>
       #{{"example.com",80,tcp} =>
             [{<0.195.0>,#Ref<0.485877779.3106144257.39748>,
               #Ref<0.485877779.3106144257.39749>}]},
   up_conns => #{<0.195.0> => {"example.com",80,tcp}}},
 #{await_up_conns => #{},host_conns => #{},up_conns => #{}},
 #{await_up_conns => #{},host_conns => #{},up_conns => #{}}]

5> honey_pool:summarize_state().
[#{host_conns => [{{"example.com",80,tcp},1}],
   total_conns => #{await_up => 0,up => 1}},
 #{host_conns => [],total_conns => #{await_up => 0,up => 0}},
 #{host_conns => [],total_conns => #{await_up => 0,up => 0}}]
```


HOW TO CONFIGURE
----------------

In the sys.config, have:

```
{honey_pool, [
              %% honey pool configurations
              {idle_timeout, 60000}, %% close connection after 60 sec of idle
              {await_up_timeout, 5000}, %% max. time (in milliseconds) to wait for a newly opened connection to become available.
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
