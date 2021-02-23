%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_mgmt).

-include("emqx_mgmt.hrl").

-include_lib("stdlib/include/qlc.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").

-import(proplists, [get_value/2]).

%% Nodes and Brokers API
-export([ list_nodes/0
        , lookup_node/1
        , list_brokers/0
        , lookup_broker/1
        , node_info/1
        , broker_info/1
        ]).

%% Metrics and Stats
-export([ get_metrics/0
        , get_metrics/1
        , get_stats/0
        , get_stats/1
        ]).

%% Clients, Sessions
-export([ lookup_client/2
        , lookup_client/3
        , kickout_client/1
        , list_acl_cache/1
        , list_acl_cache/2
        , clean_acl_cache/1
        , clean_acl_cache/2
        ]).

%% Subscriptions
-export([ list_subscriptions/1
        , lookup_subscriptions/1
        , lookup_subscriptions/2
        ]).

%% Routes
-export([ list_routes/0
        , lookup_routes/1
        ]).

%% PubSub
-export([ subscribe/2
        , publish/1
        , unsubscribe/2
        ]).

%% Plugins
-export([ list_plugins/0
        , list_plugins/1
        , load_plugin/2
        , unload_plugin/2
        , reload_plugin/2
        ]).

%% Listeners
-export([ list_listeners/0
        , list_listeners/1
        ]).

%% Alarms
-export([ get_alarms/1
        , get_alarms/2
        ]).

%% Banned
-export([ create_banned/1
        , delete_banned/1
        ]).

%% Common Table API
-export([ count/1
        , query_handle/1
        , item/2
        , max_row_limit/0
        ]).

-define(MAX_ROW_LIMIT, 10000).

-define(APP, emqx_management).

%%--------------------------------------------------------------------
%% Node Info
%%--------------------------------------------------------------------

list_nodes() ->
    Running = mnesia:system_info(running_db_nodes),
    Stopped = mnesia:system_info(db_nodes) -- Running,
    DownNodes = lists:map(fun stopped_node_info/1, Stopped),
    [{Node, node_info(Node)} || Node <- Running] ++ DownNodes.

lookup_node(Node) -> node_info(Node).

node_info(Node) when Node =:= node() ->
    Memory  = emqx_vm:get_memory(),
    Info = maps:from_list([{K, list_to_binary(V)} || {K, V} <- emqx_vm:loads()]),
    BrokerInfo = emqx_sys:info(),
    Info#{name              => node(),
          otp_release       => iolist_to_binary(otp_rel()),
          memory_total      => get_value(allocated, Memory),
          memory_used       => get_value(used, Memory),
          process_available => erlang:system_info(process_limit),
          process_used      => erlang:system_info(process_count),
          max_fds           => get_value(max_fds, lists:usort(lists:flatten(erlang:system_info(check_io)))),
          connections       => ets:info(emqx_conn, size),
          node_status       => 'Running',
          uptime            => iolist_to_binary(proplists:get_value(uptime, BrokerInfo)),
          version           => iolist_to_binary(proplists:get_value(version, BrokerInfo))
          };
node_info(Node) ->
    rpc_call(Node, node_info, [Node]).

stopped_node_info(Node) ->
    #{name => Node, node_status => 'Stopped'}.

%%--------------------------------------------------------------------
%% Brokers
%%--------------------------------------------------------------------

list_brokers() ->
    [{Node, broker_info(Node)} || Node <- ekka_mnesia:running_nodes()].

lookup_broker(Node) ->
    broker_info(Node).

broker_info(Node) when Node =:= node() ->
    Info = maps:from_list([{K, iolist_to_binary(V)} || {K, V} <- emqx_sys:info()]),
    Info#{otp_release => iolist_to_binary(otp_rel()), node_status => 'Running'};

broker_info(Node) ->
    rpc_call(Node, broker_info, [Node]).

%%--------------------------------------------------------------------
%% Metrics and Stats
%%--------------------------------------------------------------------

get_metrics() ->
    [{Node, get_metrics(Node)} || Node <- ekka_mnesia:running_nodes()].

get_metrics(Node) when Node =:= node() ->
    emqx_metrics:all();
get_metrics(Node) ->
    rpc_call(Node, get_metrics, [Node]).

get_stats() ->
    [{Node, get_stats(Node)} || Node <- ekka_mnesia:running_nodes()].

get_stats(Node) when Node =:= node() ->
    emqx_stats:getstats();
get_stats(Node) ->
    rpc_call(Node, get_stats, [Node]).

%%--------------------------------------------------------------------
%% Clients
%%--------------------------------------------------------------------

lookup_client({clientid, ClientId}, FormatFun) ->
    lists:append([lookup_client(Node, {clientid, ClientId}, FormatFun) || Node <- ekka_mnesia:running_nodes()]);

lookup_client({username, Username}, FormatFun) ->
    lists:append([lookup_client(Node, {username, Username}, FormatFun) || Node <- ekka_mnesia:running_nodes()]).

lookup_client(Node, {clientid, ClientId}, FormatFun) when Node =:= node() ->
    FormatFun(ets:lookup(emqx_channel, ClientId));

lookup_client(Node, {clientid, ClientId}, FormatFun) ->
    rpc_call(Node, lookup_client, [Node, {clientid, ClientId}, FormatFun]);

lookup_client(Node, {username, Username}, FormatFun) when Node =:= node() ->
    MatchSpec = [{{'$1', #{clientinfo => #{username => '$2'}}, '_'}, [{'=:=','$2', Username}], ['$1']}],
    FormatFun(ets:select(emqx_channel_info, MatchSpec));

lookup_client(Node, {username, Username}, FormatFun) ->
    rpc_call(Node, lookup_client, [Node, {username, Username}, FormatFun]).

kickout_client(ClientId) ->
    Results = [kickout_client(Node, ClientId) || Node <- ekka_mnesia:running_nodes()],
    case lists:any(fun(Item) -> Item =:= ok end, Results) of
        true  -> ok;
        false -> lists:last(Results)
    end.

kickout_client(Node, ClientId) when Node =:= node() ->
    emqx_cm:kick_session(ClientId);

kickout_client(Node, ClientId) ->
    rpc_call(Node, kickout_client, [Node, ClientId]).

list_acl_cache(ClientId) ->
    Results = lists:append([list_acl_cache(Node, ClientId) || Node <- ekka_mnesia:running_nodes()]),
    Expected = lists:filter(fun({error, _}) -> false;
                               (_) -> true
                            end, Results),
    case Expected of
        [] -> case Results of
                  [] -> [];
                  _ -> lists:last(Results)
              end;
        _ -> Expected
    end.

list_acl_cache(Node, ClientId) when Node =:= node() ->
    case emqx_cm:lookup_channels(ClientId) of
        [] ->
            [{error, not_found}];
        Pids when is_list(Pids) ->
            Pid = lists:last(Pids),
            case emqx_cm:get_chan_info(ClientId, Pid) of
                #{conninfo := #{conn_mod := emqx_connection}} ->
                    gen_server:call(Pid, list_acl_cache);
                #{conninfo := #{conn_mod := emqx_ws_connection}} ->
                    emqx_ws_connection:call(Pid, list_acl_cache);
                undefined -> [{error, not_found}]
            end
    end;
list_acl_cache(Node, ClientId) ->
    rpc_call(Node, list_acl_cache, [Node, ClientId]).

clean_acl_cache(ClientId) ->
    Results = [clean_acl_cache(Node, ClientId) || Node <- ekka_mnesia:running_nodes()],
    case lists:any(fun(Item) -> Item =:= ok end, Results) of
        true  -> ok;
        false -> lists:last(Results)
    end.

clean_acl_cache(Node, ClientId) when Node =:= node() ->
    case emqx_cm:lookup_channels(ClientId) of
        [] ->
            {error, not_found};
        Pids when is_list(Pids) ->
            erlang:send(lists:last(Pids), clean_acl_cache),
            ok
    end;
clean_acl_cache(Node, ClientId) ->
    rpc_call(Node, clean_acl_cache, [Node, ClientId]).

%%--------------------------------------------------------------------
%% Subscriptions
%%--------------------------------------------------------------------

list_subscriptions(Node) when Node =:= node() ->
    case check_row_limit([mqtt_subproperty]) of
        false -> throw(max_row_limit);
        ok    -> [item(subscription, Sub) || Sub <- ets:tab2list(mqtt_subproperty)]
    end;

list_subscriptions(Node) ->
    rpc_call(Node, list_subscriptions, [Node]).

lookup_subscriptions(Key) ->
    lists:append([lookup_subscriptions(Node, Key) || Node <- ekka_mnesia:running_nodes()]).

lookup_subscriptions(Node, Key) when Node =:= node() ->
    ets:match_object(emqx_suboption, {{Key, '_'}, '_'});

lookup_subscriptions(Node, Key) ->
    rpc_call(Node, lookup_subscriptions, [Node, Key]).

%%--------------------------------------------------------------------
%% Routes
%%--------------------------------------------------------------------

list_routes() ->
    case check_row_limit(emqx_route) of
        false -> throw(max_row_limit);
        ok    -> lists:append([ets:tab2list(Tab) || Tab <- emqx_route])
    end.

lookup_routes(Topic) ->
    emqx_router:lookup_routes(Topic).

%%--------------------------------------------------------------------
%% PubSub
%%--------------------------------------------------------------------

subscribe(ClientId, TopicTables) ->
    case ets:lookup(emqx_channel, ClientId) of
        [] -> {error, channel_not_found};
        [{_, Pid}] ->
            Pid ! {subscribe, TopicTables}
    end.

%%TODO: ???
publish(Msg) -> emqx:publish(Msg).

unsubscribe(ClientId, Topic) ->
    case ets:lookup(emqx_channel, ClientId) of
        [] -> {error, channel_not_found};
        [{_, Pid}] ->
            Pid ! {unsubscribe, [Topic]}
    end.

%%--------------------------------------------------------------------
%% Plugins
%%--------------------------------------------------------------------

list_plugins() ->
    [{Node, list_plugins(Node)} || Node <- ekka_mnesia:running_nodes()].

list_plugins(Node) when Node =:= node() ->
    emqx_plugins:list();
list_plugins(Node) ->
    rpc_call(Node, list_plugins, [Node]).

load_plugin(Node, Plugin) when Node =:= node() ->
    emqx_plugins:load(Plugin);
load_plugin(Node, Plugin) ->
    rpc_call(Node, load_plugin, [Node, Plugin]).

unload_plugin(Node, Plugin) when Node =:= node() ->
    emqx_plugins:unload(Plugin);
unload_plugin(Node, Plugin) ->
    rpc_call(Node, unload_plugin, [Node, Plugin]).

reload_plugin(Node, Name) when Node =:= node() ->
    try
        Config = gen_config(Name),
        Plugin = emqx_plugins:find_plugin(Name),
        case Plugin#plugin.active of
            false ->
                load_plugin_with_config(Name, Config);
            true ->
                case emqx_plugins:unload(Name) of
                    ok ->
                        load_plugin_with_config(Name, Config);
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    catch _ : Error : Stacktrace ->
        logger:error("[MGMT] Reload plugin crashed by error ~p, stacktrace: ~p~n", [Error, Stacktrace]),
        {error, parse_config_file_failed}
    end;

reload_plugin(Node, Name) ->
    rpc_call(Node, reload_plugin, [Node, Name]).

gen_config(App) ->
    Schema = cuttlefish_schema:files([filename:join([code:priv_dir(App), App]) ++ ".schema"]),
    Conf = cuttlefish_conf:file(filename:join([emqx:get_env(plugins_etc_dir), App]) ++ ".conf"),
    Configs = cuttlefish_generator:map(Schema, Conf),
    proplists:get_value(App, Configs, []).

load_plugin_with_config(Plugin, Config) ->
    lists:foreach(fun({Key, _}) -> application:unset_env(Plugin, Key) end, application:get_all_env(Plugin)),
    lists:foreach(fun({Key, Val}) -> application:set_env(Plugin, Key, Val) end, Config),
    emqx_plugins:load(Plugin).

%%--------------------------------------------------------------------
%% Listeners
%%--------------------------------------------------------------------

list_listeners() ->
    [{Node, list_listeners(Node)} || Node <- ekka_mnesia:running_nodes()].

list_listeners(Node) when Node =:= node() ->
    Tcp = lists:map(fun({{Protocol, ListenOn}, Pid}) ->
        #{protocol        => Protocol,
          listen_on       => ListenOn,
          acceptors       => esockd:get_acceptors(Pid),
          max_conns       => esockd:get_max_connections(Pid),
          current_conns   => esockd:get_current_connections(Pid),
          shutdown_count  => esockd:get_shutdown_count(Pid)}
    end, esockd:listeners()),
    Http = lists:map(fun({Protocol, Opts}) ->
        #{protocol        => Protocol,
          listen_on       => proplists:get_value(port, Opts),
          acceptors       => maps:get(num_acceptors, proplists:get_value(transport_options, Opts, #{}), 0),
          max_conns       => proplists:get_value(max_connections, Opts),
          current_conns   => proplists:get_value(all_connections, Opts),
          shutdown_count  => []}
    end, ranch:info()),
    Tcp ++ Http;

list_listeners(Node) ->
    rpc_call(Node, list_listeners, [Node]).

%%--------------------------------------------------------------------
%% Get Alarms
%%--------------------------------------------------------------------

get_alarms(Type) ->
    [{Node, get_alarms(Node, Type)} || Node <- ekka_mnesia:running_nodes()].

get_alarms(Node, Type) when Node =:= node() ->
    emqx_alarm_handler:get_alarms(Type);
get_alarms(Node, Type) ->
    rpc_call(Node, get_alarms, [Node, Type]).

%%--------------------------------------------------------------------
%% Banned API
%%--------------------------------------------------------------------

create_banned(Banned) ->
    emqx_banned:create(Banned).

delete_banned(Who) ->
    emqx_banned:delete(Who).

%%--------------------------------------------------------------------
%% Common Table API
%%--------------------------------------------------------------------

count(conns) ->
    table_size(emqx_conn);

count(sessions) ->
    table_size(emqx_session);

count(subscriptions) ->
    table_size(emqx_suboption);

count(routes) ->
    lists:sum([table_size(Tab) || Tab <- emqx_route]).

query_handle(conns) ->
    qlc:q([Client || Client <- ets:table(emqx_conn)]);

query_handle(sessions) ->
    qlc:q([Session || Session <- ets:table(emqx_session)]);

query_handle(subscriptions) ->
    qlc:q([E || E <- ets:table(emqx_suboption)]);

query_handle(routes) ->
    qlc:append([qlc:q([E || E <- ets:table(Tab)]) || Tab <- emqx_route]).

item(client, {ClientId, ChanPid}) ->
    Attrs = case emqx_cm:get_chan_info(ClientId, ChanPid) of
                undefined -> #{};
                Attrs0 -> Attrs0
            end,
    Stats = case emqx_cm:get_chan_stats(ClientId, ChanPid) of
                undefined -> #{};
                Stats0 -> maps:from_list(Stats0)
            end,
    ClientInfo = maps:get(clientinfo, Attrs, #{}),
    ConnInfo = maps:get(conninfo, Attrs, #{}),
    Session = maps:get(session, Attrs, #{}),
    Connected = case maps:get(conn_state, Attrs) of
                    connected -> true;
                    _ -> false
                end,
    NStats = Stats#{max_subscriptions => maps:get(subscriptions_max, Stats, 0),
                    max_inflight => maps:get(inflight_max, Stats, 0),
                    max_awaiting_rel => maps:get(awaiting_rel_max, Stats, 0),
                    max_mqueue => maps:get(mqueue_max, Stats, 0),
                    inflight => maps:get(inflight_cnt, Stats, 0),
                    awaiting_rel => maps:get(awaiting_rel_cnt, Stats, 0)},
    lists:foldl(fun(Items, Acc) ->
                    maps:merge(Items, Acc)
                end, #{connected => Connected},
                [maps:with([ subscriptions_cnt, max_subscriptions,
                             inflight, max_inflight, awaiting_rel,
                             max_awaiting_rel, mqueue_len, mqueue_dropped,
                             max_mqueue, heap_size, reductions, mailbox_len,
                             recv_cnt, recv_msg, recv_oct, recv_pkt, send_cnt,
                             send_msg, send_oct, send_pkt], NStats),
                 maps:with([clientid, username, is_bridge, zone], ClientInfo),
                 maps:with([clean_start, keepalive, expiry_interval, proto_name,
                            proto_ver, peername, connected_at, disconnected_at], ConnInfo),
                 maps:with([created_at], Session)]);

item(subscription, {{Topic, ClientId}, Options}) ->
    #{topic => Topic, clientid => ClientId, options => Options};

item(route, #route{topic = Topic, dest = Node}) ->
    #{topic => Topic, node => Node};
item(route, {Topic, Node}) ->
    #{topic => Topic, node => Node}.

%%--------------------------------------------------------------------
%% Internel Functions.
%%--------------------------------------------------------------------

rpc_call(Node, Fun, Args) ->
    case rpc:call(Node, ?MODULE, Fun, Args) of
        {badrpc, Reason} -> {error, Reason};
        Res -> Res
    end.

otp_rel() ->
    lists:concat(["R", erlang:system_info(otp_release), "/", erlang:system_info(version)]).

check_row_limit(Tables) ->
    check_row_limit(Tables, max_row_limit()).

check_row_limit([], _Limit) ->
    ok;
check_row_limit([Tab|Tables], Limit) ->
    case table_size(Tab) > Limit of
        true  -> false;
        false -> check_row_limit(Tables, Limit)
    end.

max_row_limit() ->
    application:get_env(?APP, max_row_limit, ?MAX_ROW_LIMIT).

table_size(Tab) -> ets:info(Tab, size).
