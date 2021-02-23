# emqtt

Erlang MQTT v5.0 Client compatible with MQTT v3.0

## Build

    $ make

### Getting started

#### As a command line tool

```sh
$ ./emqtt_cli sub --help
Usage: emqtt_cli sub [-h [<host>]] [-p [<port>]] [-V [<protocol_version>]]
                     [-u <username>] [-P <password>] [-C <clientid>]
                     [-k [<keepalive>]] [-t <topic>] [-q [<qos>]]
                     [--help <help>] [--ifaddr <ifaddr>]
                     [--will-topic <will_topic>]
                     [--will-payload <will_payload>]
                     [--will-qos [<will_qos>]]
                     [--will-retain [<will_retain>]]
                     [--enable-websocket [<enable_websocket>]]
                     [--enable-ssl [<enable_ssl>]]
                     [--cacertfile <cacertfile>] [--certfile <certfile>]
                     [--keyfile <keyfile>]
                     [--retain-as-publish [<retain_as_publish>]]
                     [--retain-handling [<retain_handling>]]

  -h, --host              mqtt server hostname or IP address [default:
                          localhost]
  -p, --port              mqtt server port number [default: 1883]
  -V, --protocol-version  mqtt protocol version: 3 | 4 | 5 [default: 5]
  -u, --username          username for connecting to server
  -P, --password          password for connecting to server
  -C, --clientid          client identifier
  -k, --keepalive         keep alive in seconds [default: 300]
  -t, --topic             mqtt topic on which to publish the message
  -q, --qos               maximum qos level at which the server can send
                          application messages to the client [default: 0]
  --help                  help information
  --ifaddr                local ipaddress or interface address
  --will-topic            topic in will message
  --will-payload          payload in will message
  --will-qos              qos in will message [default: 0]
  --will-retain           retain in will message [default: false]
  --enable-websocket      enable websocket transport or not [default:
                          false]
  --enable-ssl            enable ssl/tls or not [default: false]
  --cacertfile            path to a file containing pem-encoded ca
                          certificates
  --certfile              path to a file containing the user certificate
                          on pem format
  --keyfile               path to the file containing the user's private
                          pem-encoded key
  --retain-as-publish     retain as publih option in subscription options
                          [default: false]
  --retain-handling       retain handling option in subscription options
                          [default: 0]
```

```sh
$ ./emqtt_cli pub --help
Usage: emqtt_cli pub [-h [<host>]] [-p [<port>]] [-V [<protocol_version>]]
                     [-u <username>] [-P <password>] [-C <clientid>]
                     [-k [<keepalive>]] [-q [<qos>]] [-r [<retain>]]
                     [-t <topic>] [--help <help>] [--ifaddr <ifaddr>]
                     [--will-topic <will_topic>]
                     [--will-payload <will_payload>]
                     [--will-qos [<will_qos>]]
                     [--will-retain [<will_retain>]]
                     [--enable-websocket [<enable_websocket>]]
                     [--enable-ssl [<enable_ssl>]]
                     [--cacertfile <cacertfile>] [--certfile <certfile>]
                     [--keyfile <keyfile>] [--payload <payload>]

  -h, --host              mqtt server hostname or IP address [default:
                          localhost]
  -p, --port              mqtt server port number [default: 1883]
  -V, --protocol-version  mqtt protocol version: 3 | 4 | 5 [default: 5]
  -u, --username          username for connecting to server
  -P, --password          password for connecting to server
  -C, --clientid          client identifier
  -k, --keepalive         keep alive in seconds [default: 300]
  -q, --qos               qos level of assurance for delivery of an
                          application message [default: 0]
  -r, --retain            retain message or not [default: false]
  -t, --topic             mqtt topic to subscribe to
  --help                  help information
  --ifaddr                local ipaddress or interface address
  --will-topic            topic in will message
  --will-payload          payload in will message
  --will-qos              qos in will message [default: 0]
  --will-retain           retain in will message [default: false]
  --enable-websocket      enable websocket transport or not [default:
                          false]
  --enable-ssl            enable ssl/tls or not [default: false]
  --cacertfile            path to a file containing pem-encoded ca
                          certificates
  --certfile              path to a file containing the user certificate
                          on pem format
  --keyfile               path to the file containing the user's private
                          pem-encoded key
  --payload               application message that is being published.
```

Subscribing to the topic named 'a' and maximum QoS level is 0. The client will wait for PUBLISH packet that matches the topic and output the payload of packet to the console:

```sh
./emqtt_cli sub -C subscriber -t a -q 0
Client subscriber sent CONNECT
Client subscriber subscribed to a
```

Publishing a application message to topic 'a':

```sh
./emqtt_cli pub -C publisher -t a -q 0 --payload hello
Client publisher sent CONNECT
Client publisher sent PUBLISH (Q0, R0, D0, Topic=a, Payload=...(5 bytes))
Client publisher sent DISCONNECT
```

#### As a library

rebar.config

```erlang
{deps, [{emqtt, {git, "https://github.com/emqx/emqtt", {tag, "v1.0.0"}}}]}.
```

Here is the simple usage of `emqtt` library.

```erlang
ClientId = <<"test">>.
{ok, ConnPid} = emqtt:start_link([{clientid, ClientId}]).
{ok, _Props} = emqtt:connect(ConnPid).
Topic = <<"guide/#">>.
QoS = 1.
{ok, _Props, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, QoS}).
{ok, _PktId} = emqtt:publish(ConnPid, <<"guide/1">>, <<"Hello World!">>, QoS).
%% If the qos of publish packet is 0, `publish` function would not return packetid.
ok = emqtt:publish(ConnPid, <<"guide/2">>, <<"Hello World!">>, 0).

%% Recursively get messages from mail box.
Y = fun (Proc) -> ((fun (F) -> F(F) end)((fun(ProcGen) -> Proc(fun() -> (ProcGen(ProcGen))() end) end))) end.
Rec = fun(Receive) -> fun()-> receive {publish, Msg} -> io:format("Msg: ~p~n", [Msg]), Receive(); _Other -> Receive() after 5 -> ok end end end.
(Y(Rec))().

%% If you don't like y combinator, you can also try named function to recursively get messages in erlang shell.
Receive = fun Rec() -> receive {publish, Msg} -> io:format("Msg: ~p~n", [Msg]), Rec(); _Other -> Rec() after 5 -> ok end end.
Receive().

{ok, _Props, _ReasonCode} = emqtt:unsubscribe(ConnPid, <<"guide/#">>).

ok = emqtt:disconnect(ConnPid).
```

Not only the `clientid` can be passed as parameter, but also a lot of other options
 can be passed as parameters.

Here is the options which could be passed into emqtt:start_link/1.

```erlang
-type(option() :: {name, atom()}
                | {owner, pid()}
                | {msg_handler, msg_handler()}
                | {host, host()}
                | {hosts, [{host(), inet:port_number()}]}
                | {port, inet:port_number()}
                | {tcp_opts, [gen_tcp:option()]}
                | {ssl, boolean()}
                | {ssl_opts, [ssl:ssl_option()]}
                | {ws_path, string()}
                | {connect_timeout, pos_integer()}
                | {bridge_mode, boolean()}
                | {clientid, iodata()}
                | {clean_start, boolean()}
                | {username, iodata()}
                | {password, iodata()}
                | {proto_ver, v3 | v4 | v5}
                | {keepalive, non_neg_integer()}
                | {max_inflight, pos_integer()}
                | {retry_interval, timeout()}
                | {will_topic, iodata()}
                | {will_payload, iodata()}
                | {will_retain, boolean()}
                | {will_qos, qos()}
                | {will_props, properties()}
                | {auto_ack, boolean()}
                | {ack_timeout, pos_integer()}
                | {force_ping, boolean()}
                | {properties, properties()}).
```

## License

Apache License Version 2.0

## Author

EMQ X Team.

