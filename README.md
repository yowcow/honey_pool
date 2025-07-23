![master branch](https://github.com/yowcow/honey_pool/actions/workflows/ci.yml/badge.svg?branch=master)

# honey_pool

`honey_pool` is a robust Erlang connection pool for `gun`, designed to manage HTTP connections efficiently and reliably. It leverages a worker pool to handle multiple `gun` connection pools, providing a stable solution for applications with high volumes of concurrent HTTP requests.

**Stable Release**: This library is stable and suitable for production environments. The API is stable and will maintain backward compatibility for future versions.

## Why Use honey_pool?

`honey_pool` addresses common issues encountered with other connection pooling solutions (like `hackney_pool`) when dealing with massive amounts of HTTP requests, such as hangs or instability. By distributing and managing `gun` HTTP connections across multiple worker pools, `honey_pool` ensures smoother operation and improved resilience under heavy load.

## Features

- Efficient management of `gun` HTTP connections.
- Utilizes `worker_pool` for robust concurrency.
- Configurable connection timeouts and worker pool settings.
- Provides state dumping and summarization for monitoring.

## Getting Started

### Add to Your Project

Include `honey_pool` in your `rebar.config` dependencies:

```erlang
{honey_pool, {git, "git://github.com/yowcow/honey_pool.git", {branch, "master"}}}
```

### Start the Application

Ensure `honey_pool` is started in your application's `.app` file or by calling `application:ensure_all_started(honey_pool)`.

### Basic Usage

Here's how you can interact with `honey_pool` in the Erlang shell:

```erlang
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
               #Ref<0.485877779.3106144257.39749>}]}},
   up_conns => #{<0.195.0> => {"example.com",80,tcp}}},
 #{await_up_conns => #{},host_conns => #{},up_conns => #{}},
 #{await_up_conns => #{},host_conns => #{},up_conns => #{}}]

5> honey_pool:summarize_state().
[#{host_conns => [{{"example.com",80,tcp},1}],
   total_conns => #{await_up => 0,up => 1}},
 #{host_conns => [],total_conns => #{await_up => 0,up => 0}},
 #{host_conns => [],total_conns => #{await_up => 0,up => 0}}]
```

## Configuration

Configure `honey_pool` in your `sys.config` file:

```erlang
{honey_pool, [
              %% honey_pool configurations
              {idle_timeout, 60000},    %% Close connection after 60 seconds of idle time.
              {await_up_timeout, 5000}, %% Max. time (in milliseconds) to wait for a newly opened connection to become available.

              %% worker_pool configurations (see worker_pool documentation for details)
              {wpool, [
                       {workers, 10},          %% Start 10 connection pools.
                       {overrun_warning, 50}   %% Warn when a pool takes over 50 msec to respond.
                      ]},

              %% gun configurations (see gun documentation for details)
              {gun_opt, #{
                          retry => 0,          %% Let go dead connections.
                          connect_timeout => 1000 %% Give up connecting after 1000 msec.
                         }}
             ]}
```
