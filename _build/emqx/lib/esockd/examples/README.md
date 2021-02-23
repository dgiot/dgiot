## active_n 

Benchmark the `{active, N}` and `async_recv`.

```
recv_eprof:start(Port = 6000, async_recv, 10000).

recv_eprof:start(Port = 5000, active_n, 10000).
```

## async_recv

## client

TCP echo client.

## dtls

The DTLS echo server which listens on port 5000.

Start a DTLS client:

```
openssl s_client -dtls1 -connect 127.0.0.1:5000 -debug
```

## dtls_psk

The DTLS echo server which listens on port 5000.

Start a DTLS client:
```
Client = dtls_psk_client:connect().
dtls_psk_client:send(Client, <<"hi">>).
```

## gen_server

## plain

## proxy_protocol

## simple

## ssl

Start the SSL echo server which listens on port 5000:

```
ssl_echo_server:start(5000).
```

Start a SSL client:
```
openssl s_client -tls1 -debug -connect 127.0.0.1:5000
```

## tcp_window

`rebar3 shell` and run `tcp_window` test:

```
tcp_window:start(5000, sync).
tcp_window:start(5000, async).
tcp_window:start(5000, async_force).
tcp_window:start(5000, async_nosuspend).
```

TCP Window Full:

Send Function                           | Socket Options           | TCP Window full
----------------------------------------|--------------------------|-------------------------------------------
gen_tcp:send(Socket, Data)              | {send_timeout, infinity} | Block forever.
port_command(Socket, Data)              | {send_timeout, infinity} | Block forever.
port_command(Socket, Data, [force])     | {send_timeout, infinity} | Return true always. Write to TCP Stack.
port_command(Socket, Data, [nosuspend]) | {send_timeout, infinity} | Return false always. Drop the packets silently.
gen_tcp:send(Socket, Data)              | {send_timeout, 5000}     | Return {error, timeout}.
port_command(Socket, Data)              | {send_timeout, 5000}     | Return true always. Pause 5 seconds and Drop packets silently.
port_command(Socket, Data, [force])     | {send_timeout, 5000}     | Return true always. Write to TCP Stack.
port_command(Socket, Data, [nosuspend]) | {send_timeout, 5000}     | Return false first, and true after timeout. Drop the packets silently.

Conclusions:

1. Should set the `send_timeout` option if using `gen_tcp:send`
2. Should not set the `nosuspend` option if using `port_command`, for the busy sock will be killed at once.

## udp

Start the UDP echo server which listens on port 5000.

```
udp_echo_server:start(5000).
```

