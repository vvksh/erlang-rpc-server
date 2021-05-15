# Erlang RPC server

This is a toy exercise to learn Erlang. I will write a rpc server in Erlang.
With this server, users can send "module:func(args)" commands to a tcp socket, 
the server will execute the command and reply with the result

## How to run

### Compile
```shell script
erlc src/tr_server.erl
```

### Run
- Open erlang shell
```shell script
erl

```

- Start the server
```shell script
Erlang/OTP 23 [erts-11.1.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Eshell V11.1.3  (abort with ^G)
1> tr_server:start_link(1055).
```
Note that it will not be ready unless a client connects over TCP although it returns.
It is waiting for a client to open a tcp connection.

- Execute rpc
```shell script
viveks:~/ telnet localhost 1055
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
math.pi(). # sample rpc command
3.141592653589793
init:stop().  # send stop command to the server
ok
```

When you execute `tr_server:get_count().` in erlang shell,
you will get updated counts.

## Install Erlang
```shell script
brew install erlang
```


## IDE
I am using IntelliJ with erlang plugin.