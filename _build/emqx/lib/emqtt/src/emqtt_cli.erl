-module(emqtt_cli).

-include("emqtt.hrl").

-export([ main/1
        ]).

-import(proplists, [get_value/2]).

-define(HELP_OPT,
        [{help, undefined, "help", boolean,
          "help information"}
        ]).

-define(CONN_SHORT_OPTS,
        [{host, $h, "host", {string, "localhost"},
          "mqtt server hostname or IP address"},
         {port, $p, "port", {integer, 1883},
          "mqtt server port number"},
         {protocol_version, $V, "protocol-version", {integer, 5},
          "mqtt protocol version: 3 | 4 | 5"},
         {username, $u, "username", string,
          "username for connecting to server"},
         {password, $P, "password", string,
          "password for connecting to server"},
         {clientid, $C, "clientid", string,
          "client identifier"},
         {keepalive, $k, "keepalive", {integer, 300},
          "keep alive in seconds"}
        ]).

-define(CONN_LONG_OPTS,
        [{ifaddr, undefined, "ifaddr", string,
          "local ipaddress or interface address"},
         {will_topic, undefined, "will-topic", string,
          "topic in will message"},
         {will_payload, undefined, "will-payload", string,
          "payload in will message"},
         {will_qos, undefined, "will-qos", {integer, 0},
          "qos in will message"},
         {will_retain, undefined, "will-retain", {boolean, false},
          "retain in will message"},
         {enable_websocket, undefined, "enable-websocket", {boolean, false},
          "enable websocket transport or not"},
         {enable_ssl, undefined, "enable-ssl", {boolean, false},
          "enable ssl/tls or not"},
         {cacertfile, undefined, "cacertfile", string,
          "path to a file containing pem-encoded ca certificates"},
         {certfile, undefined, "certfile", string,
          "path to a file containing the user certificate on pem format"},
         {keyfile, undefined, "keyfile", string,
          "path to the file containing the user's private pem-encoded key"}
        ]).

-define(PUB_OPTS, ?CONN_SHORT_OPTS ++
        [{qos, $q, "qos", {integer, 0},
          "qos level of assurance for delivery of an application message"},
         {retain, $r, "retain", {boolean, false},
          "retain message or not"},
         {topic, $t, "topic", string,
          "mqtt topic to subscribe to"}
        ] ++ ?HELP_OPT ++ ?CONN_LONG_OPTS ++
        [{payload, undefined, "payload", string,
          "application message that is being published."}
        ]).

-define(SUB_OPTS, ?CONN_SHORT_OPTS ++
        [{topic, $t, "topic", string,
          "mqtt topic on which to publish the message"},
         {qos, $q, "qos", {integer, 0},
          "maximum qos level at which the server can send application messages to the client"}
        ] ++ ?HELP_OPT ++ ?CONN_LONG_OPTS ++
        [{retain_as_publish, undefined, "retain-as-publish", {boolean, false},
          "retain as publih option in subscription options"},
         {retain_handling, undefined, "retain-handling", {integer, 0},
          "retain handling option in subscription options"}
        ]).

main(["sub" | Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?SUB_OPTS, Argv),
    ok = maybe_help(sub, Opts),
    ok = check_required_args(sub, [topic], Opts),
    main(sub, Opts);

main(["pub" | Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?PUB_OPTS, Argv),
    ok = maybe_help(pub, Opts),
    ok = check_required_args(pub, [topic, payload], Opts),
    main(pub, Opts);

main(_Argv) ->
    ScriptPath = escript:script_name(),
    Script = filename:basename(ScriptPath),
    io:format("Usage: ~s pub | sub [--help]~n", [Script]).

main(PubSub, Opts) ->
    application:ensure_all_started(emqtt),
    NOpts = enrich_opts(parse_cmd_opts(Opts)),
    {ok, Client} = emqtt:start_link(NOpts),
    ConnRet = case proplists:get_bool(enable_websocket, NOpts) of
                  true  -> emqtt:ws_connect(Client);
                  false -> emqtt:connect(Client)
              end,
    case ConnRet of
        {ok, Properties} ->
            io:format("Client ~s sent CONNECT~n", [get_value(clientid, NOpts)]),
            case PubSub of
                pub ->
                    publish(Client, NOpts),
                    disconnect(Client, NOpts);
                sub ->
                    subscribe(Client, NOpts),
                    KeepAlive = maps:get('Server-Keep-Alive', Properties, get_value(keepalive, NOpts)) * 1000,
                    timer:send_interval(KeepAlive, ping),
                    main_loop(Client)
            end;
        {error, Reason} ->
            io:format("Client ~s failed to sent CONNECT due to ~p~n", [get_value(clientid, NOpts), Reason])
    end.

publish(Client, Opts) ->
    case emqtt:publish(Client, get_value(topic, Opts), get_value(payload, Opts), Opts) of
        {error, Reason} ->
            io:format("Client ~s failed to sent PUBLISH due to ~p~n", [get_value(clientid, Opts), Reason]);
        {error, _PacketId, Reason} ->
            io:format("Client ~s failed to sent PUBLISH due to ~p~n", [get_value(clientid, Opts), Reason]);
        _ ->
            io:format("Client ~s sent PUBLISH (Q~p, R~p, D0, Topic=~s, Payload=...(~p bytes))~n",
                      [get_value(clientid, Opts),
                       get_value(qos, Opts),
                       i(get_value(retain, Opts)),
                       get_value(topic, Opts),
                       length(binary_to_list(get_value(payload, Opts)))])
    end.

subscribe(Client, Opts) ->
    case emqtt:subscribe(Client, get_value(topic, Opts), Opts) of
        {ok, _, [ReasonCode]} when 0 =< ReasonCode andalso ReasonCode =< 2 ->
            io:format("Client ~s subscribed to ~s~n", [get_value(clientid, Opts), get_value(topic, Opts)]);
        {ok, _, [ReasonCode]} ->
            io:format("Client ~s failed to subscribe to ~s due to ~s~n", [get_value(clientid, Opts),
                                                                          get_value(topic, Opts),
                                                                          emqtt:reason_code_name(ReasonCode)]);
        {error, Reason} ->
            io:format("Client ~s failed to send SUBSCRIBE due to ~p~n", [get_value(clientid, Opts), Reason])
    end.

disconnect(Client, Opts) ->
    case emqtt:disconnect(Client) of
        ok ->
            io:format("Client ~s sent DISCONNECT~n", [get_value(clientid, Opts)]);
        {error, Reason} ->
            io:format("Client ~s failed to send DISCONNECT due to ~p~n", [get_value(clientid, Opts), Reason])
    end.

maybe_help(PubSub, Opts) ->
    case proplists:get_value(help, Opts) of
        true ->
            usage(PubSub),
            halt(0);
        _ -> ok
    end.

usage(PubSub) ->
    ScriptPath = escript:script_name(),
    Script = filename:basename(ScriptPath),
    Opts = case PubSub of
               pub -> ?PUB_OPTS;
               sub -> ?SUB_OPTS
           end,
    getopt:usage(Opts, Script ++ " " ++ atom_to_list(PubSub)).

check_required_args(PubSub, Keys, Opts) ->
    lists:foreach(fun(Key) ->
        case lists:keyfind(Key, 1, Opts) of
            false ->
                io:format("Error: '~s' required~n", [Key]),
                usage(PubSub),
                halt(1);
            _ -> ok
        end
    end, Keys).

parse_cmd_opts(Opts) ->
    parse_cmd_opts(Opts, []).

parse_cmd_opts([], Acc) ->
    Acc;
parse_cmd_opts([{host, Host} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{host, Host} | Acc]);
parse_cmd_opts([{port, Port} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{port, Port} | Acc]);
parse_cmd_opts([{ifaddr, IfAddr} | Opts], Acc) ->
    {ok, IpAddr} = inet_parse:address(IfAddr),
    parse_cmd_opts(Opts, maybe_append(tcp_opts, {ip, IpAddr}, Acc));
parse_cmd_opts([{protocol_version, 3} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{proto_ver, v3} | Acc]);
parse_cmd_opts([{protocol_version, 4} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{proto_ver, v4} | Acc]);
parse_cmd_opts([{protocol_version, 5} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{proto_ver, v5} | Acc]);
parse_cmd_opts([{username, Username} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{username, list_to_binary(Username)} | Acc]);
parse_cmd_opts([{password, Password} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{password, list_to_binary(Password)} | Acc]);
parse_cmd_opts([{clientid, Clientid} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{clientid, list_to_binary(Clientid)} | Acc]);
parse_cmd_opts([{will_topic, Topic} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{will_topic, list_to_binary(Topic)} | Acc]);
parse_cmd_opts([{will_payload, Payload} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{will_payload, list_to_binary(Payload)} | Acc]);
parse_cmd_opts([{will_qos, Qos} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{will_qos, Qos} | Acc]);
parse_cmd_opts([{will_retain, Retain} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{will_retain, Retain} | Acc]);
parse_cmd_opts([{keepalive, I} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{keepalive, I} | Acc]);
parse_cmd_opts([{enable_websocket, EnableWebsocket} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{enable_websocket, EnableWebsocket} | Acc]);
parse_cmd_opts([{enable_ssl, EnableSSL} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{ssl, EnableSSL} | Acc]);
parse_cmd_opts([{cacertfile, CacertFile} | Opts], Acc) ->
    parse_cmd_opts(Opts, maybe_append(ssl_opts, {cacertfile, CacertFile}, Acc));
parse_cmd_opts([{certfile, CertFile} | Opts], Acc) ->
    parse_cmd_opts(Opts, maybe_append(ssl_opts, {certfile, CertFile}, Acc));
parse_cmd_opts([{keyfile, KeyFile} | Opts], Acc) ->
    parse_cmd_opts(Opts, maybe_append(ssl_opts, {keyfile, KeyFile}, Acc));
parse_cmd_opts([{qos, QoS} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{qos, QoS} | Acc]);
parse_cmd_opts([{retain_as_publish, RetainAsPublish} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{rap, RetainAsPublish} | Acc]);
parse_cmd_opts([{retain_handling, RetainHandling} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{rh, RetainHandling} | Acc]);
parse_cmd_opts([{retain, Retain} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{retain, Retain} | Acc]);
parse_cmd_opts([{topic, Topic} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{topic, list_to_binary(Topic)} | Acc]);
parse_cmd_opts([{payload, Payload} | Opts], Acc) ->
    parse_cmd_opts(Opts, [{payload, list_to_binary(Payload)} | Acc]);
parse_cmd_opts([_ | Opts], Acc) ->
    parse_cmd_opts(Opts, Acc).

maybe_append(Key, Value, TupleList) ->
    case lists:keytake(Key, 1, TupleList) of
        {value, {Key, OldValue}, NewTupleList} ->
            [{Key, [Value | OldValue]} | NewTupleList];
        false ->
            [{Key, [Value]} | TupleList]
    end.

enrich_opts(Opts) ->
    case lists:keyfind(clientid, 1, Opts) of
        false -> [{clientid, emqtt:random_client_id()} | Opts];
        _ -> Opts
    end.

main_loop(Client) ->
    receive
        {publish, #{payload := Payload}} ->
            io:format("~s~n", [Payload]),
            main_loop(Client);
        ping ->
            emqtt:ping(Client),
            main_loop(Client);
        _Other ->
            main_loop(Client)
    end.

i(true)  -> 1;
i(false) -> 0.
