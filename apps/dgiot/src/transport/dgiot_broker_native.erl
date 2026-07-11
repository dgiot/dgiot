%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_broker_native).
-behaviour(dgiot_broker).

-export([init/0]).
-export([has_routes/1, subscribe_mgmt/2, unsubscribe_mgmt/2,
         subscribe_self/3, subscribe_client/2, unsubscribe_self/1,
         unsubscribe_client/2, publish/3, safe_publish/1, message_make/4,
         shared_sub/3, shared_unsub/3, lookup_subpid/1, register_sub/2,
         reclaim_seq/1, get_sub_shard/2, inc_actions_success/1, inc_msg/1]).

-define(SUB_TAB, dgiot_native_sub).
-define(SHARED_TAB, dgiot_native_shared).
-define(MSG_TAB, dgiot_native_msg).
-define(SHARD_TAB, dgiot_native_shard).
-define(SEQ_TAB, dgiot_native_seq).
-define(TOPIC_MAX_LEN, 4096).
-define(CLIENTID_MAX_LEN, 256).

init() ->
    init_table(?SUB_TAB, [named_table, public, bag, {write_concurrency, true}]),
    init_table(?SHARED_TAB, [named_table, public, set, {write_concurrency, true}]),
    init_table(?MSG_TAB, [named_table, public, set, {write_concurrency, true}]),
    init_table(?SHARD_TAB, [named_table, public, set, {write_concurrency, true}]),
    init_table(?SEQ_TAB, [named_table, public, set, {write_concurrency, true}]),
    dgiot_broker_trie:new(),
    start_mqtt_listener(),
    ok.

start_mqtt_listener() ->
    try
        esockd:start(),
        MqttOpts = [{backlog,512},{keepalive,true},{send_timeout,15000},
                    {send_timeout_close,true},{nodelay,true},{reuseaddr,true},
                    binary,{packet,raw},{exit_on_close,true}],
        Opts = [{tcp_options,MqttOpts},{acceptors,8},{max_connections,1024000},
                {max_conn_rate,{1000,1}}],
        {ok, _} = esockd:open(external, 1883, Opts, {dgiot_broker_native, handle_connect, []}),
        io:format("[BROKER] MQTT TCP listener started on :1883~n")
    catch _:E ->
        io:format("[BROKER] MQTT listener start failed: ~p~n", [E])
    end.

%% MQTT connection handler (esockd callback: start_link/3)
start_link(_Transport, Socket, []) ->
    {ok, {IP, _Port}} = esockd_transport:peername(Socket),
    io:format("[MQTT] New connection from ~s~n", [inet:ntoa(IP)]),
    {ok, spawn_link(fun() -> mqtt_loop(Socket) end)}.

mqtt_loop(Socket) ->
    receive
        {tcp, Socket, Data} -> mqtt_loop(Socket);
        {tcp_closed, Socket} -> io:format("[MQTT] Connection closed~n");
        _ -> mqtt_loop(Socket)
    end.

init_table(Name, Opts) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, Opts);
        _ -> ok
    end.

has_routes(Topic) ->
    case dgiot_broker_trie:subscribers(Topic) of
        [] ->
            case ets:match(?SHARED_TAB, {{'$1', Topic}, '_'}, 1) of
                {[], _} -> false;
                '$end_of_table' -> false;
                _ -> true
            end;
        _ -> true
    end.

subscribe_mgmt(ClientId, Topic) ->
    validate_topic(Topic),
    validate_clientid(ClientId),
    dgiot_broker_trie:insert(Topic, ClientId),
    ets:insert(?SUB_TAB, {Topic, ClientId}),
    ok.

unsubscribe_mgmt(ClientId, Topic) ->
    dgiot_broker_trie:delete(Topic, ClientId),
    ets:delete_object(?SUB_TAB, {Topic, ClientId}),
    ok.

subscribe_self(Topic, SubId, _Options) ->
    Pid = self(),
    dgiot_broker_trie:insert(Topic, Pid),
    ets:insert(?SUB_TAB, {Topic, Pid, SubId}),
    ok.

subscribe_client(TopicFilter, _ClientId) ->
    Pid = self(),
    dgiot_broker_trie:insert(TopicFilter, Pid),
    ets:insert(?SUB_TAB, {TopicFilter, Pid}),
    ok.

unsubscribe_self(Topic) ->
    Pid = self(),
    dgiot_broker_trie:delete(Topic, Pid),
    ets:match_delete(?SUB_TAB, {Topic, Pid, '_'}),
    ok.

unsubscribe_client(_ClientId, TopicFilter) ->
    Pid = self(),
    dgiot_broker_trie:delete(TopicFilter, Pid),
    ets:delete_object(?SUB_TAB, {TopicFilter, Pid}),
    ok.

publish(_Client, Topic, Payload) ->
    validate_topic(Topic),
    check_acl(publish, _Client, Topic),
    Subs = dgiot_broker_trie:subscribers(Topic),
    [deliver(S, Topic, Payload) || S <- Subs],
    publish_shared(Topic, Payload),
    ok.

publish_shared(Topic, Payload) ->
    SharedSubs = ets:match(?SHARED_TAB, {{'$1', Topic}, '$2'}),
    Grouped = group_shared(SharedSubs),
    maps:foreach(fun(Group, Pids) ->
        Pid = pick_shared(Group, Pids),
        deliver(Pid, Topic, Payload)
    end, Grouped),
    ok.

group_shared(Matches) ->
    lists:foldl(fun([G, P], Acc) ->
        maps:update_with(G, fun(V) -> [P | V] end, [P], Acc)
    end, #{}, Matches).

pick_shared(Group, Pids) ->
    case ets:lookup(?SHARED_TAB, {Group, rr}) of
        [] -> I = 0;
        [{_, I}] -> ok
    end,
    Next = (I + 1) rem length(Pids),
    ets:insert(?SHARED_TAB, {{Group, rr}, Next}),
    lists:nth(I + 1, Pids).

deliver(Pid, Topic, Payload) when is_pid(Pid) ->
    Msg = #{id => dgiot_guid:gen(),
            topic => Topic,
            payload => Payload,
            timestamp => erlang:system_time(millisecond)},
    Pid ! {deliver, Topic, Msg};
deliver(_ClientId, _Topic, _Payload) ->
    ok.

safe_publish(Msg) when is_map(Msg) ->
    Topic = maps:get(topic, Msg, <<>>),
    Payload = maps:get(payload, Msg, <<>>),
    publish(<<"dgiot">>, Topic, Payload).

message_make(Client, QoS, Topic, Payload) ->
    #{id => dgiot_guid:gen(),
      qos => QoS,
      from => dgiot_utils:to_binary(Client),
      flags => #{dup => false, retain => false},
      headers => #{},
      topic => Topic,
      payload => Payload,
      timestamp => erlang:system_time(millisecond)}.

shared_sub(Group, Topic, SubPid) ->
    dgiot_broker_trie:insert(<<"$share/", Group/binary, "/", Topic/binary>>, SubPid),
    ets:insert(?SHARED_TAB, {{Group, Topic}, SubPid}),
    ok.

shared_unsub(Group, Topic, SubPid) ->
    dgiot_broker_trie:delete(<<"$share/", Group/binary, "/", Topic/binary>>, SubPid),
    ets:delete_object(?SHARED_TAB, {{Group, Topic}, SubPid}),
    ok.

lookup_subpid(ClientId) ->
    Match3 = ets:match(?SUB_TAB, {'$1', ClientId, '$2'}, 1),
    Match2 = ets:match(?SUB_TAB, {'$1', ClientId}, 1),
    case Match3 of
        {[[_Topic, _SubId]], _} -> self();
        _ -> case Match2 of
                {[[_Topic]], _} -> self();
                _ -> undefined
             end
    end.

register_sub(SubPid, SubId) ->
    ets:insert(?SHARD_TAB, {SubPid, SubId}),
    ok.

reclaim_seq(Topic) ->
    case ets:lookup(?SEQ_TAB, Topic) of
        [] -> ok;
        [{Topic, N}] when N > 0 ->
            ets:insert(?SEQ_TAB, {Topic, N - 1})
    end,
    ok.

get_sub_shard(SubPid, _Topic) ->
    erlang:phash2(SubPid, 64).

inc_actions_success(ActId) ->
    dgiot_metrics_adapter:inc({actions, success, ActId}, 1).

inc_msg(_Msg) ->
    dgiot_metrics_adapter:inc({msg, total}, 1).

%%------------------------------------------------------------------------------
%% Input validation
%%------------------------------------------------------------------------------

validate_topic(Topic) when is_binary(Topic), byte_size(Topic) > 0,
                            byte_size(Topic) =< ?TOPIC_MAX_LEN ->
    case binary:match(Topic, [<<0>>, <<"\n">>, <<"\r">>]) of
        nomatch -> ok;
        _ -> error({invalid_topic, Topic})
    end.

validate_clientid(ClientId) when is_binary(ClientId),
                                  byte_size(ClientId) > 0,
                                  byte_size(ClientId) =< ?CLIENTID_MAX_LEN ->
    case is_sane_clientid(ClientId) of
        true -> ok;
        false -> error({invalid_clientid, ClientId})
    end.

is_sane_clientid(<<>>) -> true;
is_sane_clientid(<<H, Rest/binary>>) when
    H >= $0, H =< $9;
    H >= $a, H =< $z;
    H >= $A, H =< $Z;
    H =:= $_; H =:= $-; H =:= $. ->
    is_sane_clientid(Rest);
is_sane_clientid(_) -> false.

%% ACL check hook — extensible via dgiot_hooks
check_acl(Action, ClientId, Topic) ->
    case catch dgiot_hooks:run('broker.acl', [{Action, ClientId, Topic}]) of
        {'EXIT', _} -> ok;
        ok -> ok;
        _ -> error({acl_denied, Action, Topic})
    end.
