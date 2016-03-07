## A simple and easy to use generic pooling library for Erlang

An OTP application/gen_server to provide a simple way for pooling objects with heavy initialization,
e.g. database connections, gen_server instances, channels or something else.

[![Build Status](https://api.travis-ci.org/eugenehr/erlypool.svg?branch=master)](https://travis-ci.org/eugenehr/erlypool)

## Usage

### 1. Standalone pool

#### example.erl
```erlang
%% erlypool:start(PoolName, PoolOpts, CreateOpts) - create a new pool
{ok, _Pid} = erlypool:start(db_pool, 
	[% Pool options
	 {min_size, 3},      % Minimum number of active connections
     {max_size, 50},     % Maximum number of active connections
     {module, db_conn}], % Erlang module that implements erlypool_object behaviour
    [% db_conn:create() options
     {hostname, "127.0.0.1"},
     {database, "db1"},
     {username, "user"},
     {password, "pswd"}
    ]),
%% erlypool:borrow(PoolRef) - borrow an object from the pool
Conn = erlypool:borrow(db_pool),
...
%% erlypool:release(PoolRef, Object) - release the object
erlypool:release(db_pool, Conn)
```


#### db_conn.erl
```erlang
-behaviour(erlypool_object).
-export([create/1, test/1, reset/1, close/1]).

%% Create a new connection to the database
create(CreateOpts) ->
    Hostname = proplists:get_value(hostname, CreateOpts),
    Database = proplists:get_value(database, CreateOpts, "db1"),
    Username = proplists:get_value(username, CreateOpts, "user"),
    Password = proplists:get_value(database, CreateOpts, "pa$$wd"), 
    % Connect to database
    {ok, Conn}.
    
%% Test the connection before it will be returned to caller
test(Conn) ->
    case is_alive(Conn) of
        true -> ok;
           _ -> {error, "Connection is broken"}
    end.
    
%% Reset the connection before it will be returned back to the pool
reset(Conn) ->
    case in_transaction(Conn) of
    	true -> rollback(Conn);
           _ -> ok
    end,
    ok.
    
%% Close the connection
close(Conn) ->
    % Close the connection
    ok.
```

### 2. Run as application

#### example.config

```erlang
[{erlypool, [{pools, [
    {pool1,
        [{min_size, 3}, {max_size, 50}, {strategy, lifo}, {module, erlypool_testpool}],
        [{hostname, "127.0.0.1"}, 
         {database, "db1"}, 
         {username, "user1"}, 
         {password, "pa$$wd"}
        ]
    },
    {pool2,
        [{min_size, 3}, {max_size, 10}, {strategy, fifo}, {module, erlypool_testpool}],
        [{hostname, "127.0.0.1"}, 
         {database, "db2"}, 
         {username, "user2"}, 
         {password, "pa$$wd"}
        ]
    },
}].    
````
#### Run

```bash
erl -config example
```

### Pool options

|  Parameter       |   Default  |  Description 
|------------------|:----------:|--------------
| `min_size`       |    0       | The minimum number of active connections 
| `max_size`       |`unlimited` | The maximum number of active connections or `unlimited` atom
| `strategy`       |`lifo`      | How objects should be placed first or last in the pool. 'lifo' or 'fifo'
| `module`         | -          | The name of the Erlang module which callbacks will be used to create and destroy objects. This parameter is required
| `test_on_borrow` | `true`     | `true` to call test-callback before object will be returned from the pool


## License

Erlypool is licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
