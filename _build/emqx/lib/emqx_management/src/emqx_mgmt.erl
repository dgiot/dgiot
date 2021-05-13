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
        , get_all_topic_metrics/0
        , get_topic_metrics/1
        , get_topic_metrics/2
        , register_topic_metrics/1
        , register_topic_metrics/2
        , unregister_topic_metrics/1
        , unregister_topic_metrics/2
        , unregister_all_topic_metrics/0
        , unregister_all_topic_metrics/1
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
        , list_subscriptions_via_topic/2
        , list_subscriptions_via_topic/3
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

%% Modules
-export([ list_modules/0
        , list_modules/1
        , load_module/2
        , unload_module/2
        , reload_module/2
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

%% Export/Import
-export([ export_rules/0
        , export_resources/0
        , export_blacklist/0
        , export_applications/0
        , export_users/0
        , export_auth_clientid/0
        , export_auth_username/0
        , export_auth_mnesia/0
        , export_acl_mnesia/0
        , export_schemas/0
        , import_rules/1
        , import_resources/1
        , import_blacklist/1
        , import_applications/1
        , import_users/1
        , import_auth_clientid/1
        , import_auth_username/1
        , import_auth_mnesia/1
        , import_acl_mnesia/1
        , import_schemas/1
        , to_version/1
        ]).

%% Common Table API
-export([ item/2
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
    Info#{node              => node(),
          otp_release       => iolist_to_binary(otp_rel()),
          memory_total      => get_value(allocated, Memory),
          memory_used       => get_value(used, Memory),
          process_available => erlang:system_info(process_limit),
          process_used      => erlang:system_info(process_count),
          max_fds           => get_value(max_fds, lists:usort(lists:flatten(erlang:system_info(check_io)))),
          connections       => ets:info(emqx_channel, size),
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
    Info#{node => Node, otp_release => iolist_to_binary(otp_rel()), node_status => 'Running'};

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

get_all_topic_metrics() ->
    lists:foldl(fun(Topic, Acc) ->
                    case get_topic_metrics(Topic) of
                        {error, _Reason} ->
                            Acc;
                        Metrics ->
                            [#{topic => Topic, metrics => Metrics} | Acc]
                    end
                end, [], emqx_mod_topic_metrics:all_registered_topics()).

get_topic_metrics(Topic) ->
    lists:foldl(fun(Node, Acc) ->
                    case get_topic_metrics(Node, Topic) of
                        {error, _Reason} ->
                            Acc;
                        Metrics ->
                            case Acc of
                                [] -> Metrics;
                                _ ->
                                    lists:foldl(fun({K, V}, Acc0) ->
                                                    [{K, V + proplists:get_value(K, Metrics, 0)} | Acc0]
                                                end, [], Acc)
                            end
                    end
                end, [], ekka_mnesia:running_nodes()).

get_topic_metrics(Node, Topic) when Node =:= node() ->
    emqx_mod_topic_metrics:metrics(Topic);
get_topic_metrics(Node, Topic) ->
    rpc_call(Node, get_topic_metrics, [Node, Topic]).

register_topic_metrics(Topic) ->
    Results = [register_topic_metrics(Node, Topic) || Node <- ekka_mnesia:running_nodes()],
    case lists:any(fun(Item) -> Item =:= ok end, Results) of
        true  -> ok;
        false -> lists:last(Results)
    end.

register_topic_metrics(Node, Topic) when Node =:= node() ->
    emqx_mod_topic_metrics:register(Topic);
register_topic_metrics(Node, Topic) ->
    rpc_call(Node, register_topic_metrics, [Node, Topic]).

unregister_topic_metrics(Topic) ->
    Results = [unregister_topic_metrics(Node, Topic) || Node <- ekka_mnesia:running_nodes()],
    case lists:any(fun(Item) -> Item =:= ok end, Results) of
        true  -> ok;
        false -> lists:last(Results)
    end.

unregister_topic_metrics(Node, Topic) when Node =:= node() ->
    emqx_mod_topic_metrics:unregister(Topic);
unregister_topic_metrics(Node, Topic) ->
    rpc_call(Node, unregister_topic_metrics, [Node, Topic]).

unregister_all_topic_metrics() ->
    Results = [unregister_all_topic_metrics(Node) || Node <- ekka_mnesia:running_nodes()],
    case lists:any(fun(Item) -> Item =:= ok end, Results) of
        true  -> ok;
        false -> lists:last(Results)
    end.

unregister_all_topic_metrics(Node) when Node =:= node() ->
    emqx_mod_topic_metrics:unregister_all();
unregister_all_topic_metrics(Node) ->
    rpc_call(Node, unregister_topic_metrics, [Node]).

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

list_subscriptions_via_topic(Topic, FormatFun) ->
    lists:append([list_subscriptions_via_topic(Node, Topic, FormatFun) || Node <- ekka_mnesia:running_nodes()]).

list_subscriptions_via_topic(Node, Topic, FormatFun) when Node =:= node() ->
    MatchSpec = [{{{'_', '$1'}, '_'}, [{'=:=','$1', Topic}], ['$_']}],
    FormatFun(ets:select(emqx_suboption, MatchSpec));

list_subscriptions_via_topic(Node, {topic, Topic}, FormatFun) ->
    rpc_call(Node, list_subscriptions_via_topic, [Node, {topic, Topic}, FormatFun]).

lookup_subscriptions(ClientId) ->
    lists:append([lookup_subscriptions(Node, ClientId) || Node <- ekka_mnesia:running_nodes()]).

lookup_subscriptions(Node, ClientId) when Node =:= node() ->
    case ets:lookup(emqx_subid, ClientId) of
        [] -> [];
        [{_, Pid}] ->
            ets:match_object(emqx_suboption, {{Pid, '_'}, '_'})
    end;

lookup_subscriptions(Node, ClientId) ->
    rpc_call(Node, lookup_subscriptions, [Node, ClientId]).

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
            Pid ! {force_subscribe, TopicTables}
    end.

%%TODO: ???
publish(Msg) -> emqx:publish(Msg).

unsubscribe(ClientId, Topic) ->
    case ets:lookup(emqx_channel, ClientId) of
        [] -> {error, channel_not_found};
        [{_, Pid}] ->
            Pid ! {force_unsubscribe, [Topic]}
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

reload_plugin(Node, Plugin) when Node =:= node() ->
    emqx_plugins:reload(Plugin);
reload_plugin(Node, Plugin) ->
    rpc_call(Node, reload_plugin, [Node, Plugin]).


%%--------------------------------------------------------------------
%% Modules
%%--------------------------------------------------------------------

list_modules() ->
    [{Node, list_modules(Node)} || Node <- ekka_mnesia:running_nodes()].

list_modules(Node) when Node =:= node() ->
    emqx_modules:list();
list_modules(Node) ->
    rpc_call(Node, list_modules, [Node]).

load_module(Node, Module) when Node =:= node() ->
    emqx_modules:load(Module);
load_module(Node, Module) ->
    rpc_call(Node, load_module, [Node, Module]).

unload_module(Node, Module) when Node =:= node() ->
    emqx_modules:unload(Module);
unload_module(Node, Module) ->
    rpc_call(Node, unload_module, [Node, Module]).

reload_module(Node, Module) when Node =:= node() ->
    emqx_modules:reload(Module);
reload_module(Node, Module) ->
    rpc_call(Node, reload_module, [Node, Module]).
%%--------------------------------------------------------------------
%% Listeners
%%--------------------------------------------------------------------

list_listeners() ->
    [{Node, list_listeners(Node)} || Node <- ekka_mnesia:running_nodes()].

list_listeners(Node) when Node =:= node() ->
    Tcp = lists:map(fun({{Protocol, ListenOn}, _Pid}) ->
        #{protocol        => Protocol,
          listen_on       => ListenOn,
          acceptors       => esockd:get_acceptors({Protocol, ListenOn}),
          max_conns       => esockd:get_max_connections({Protocol, ListenOn}),
          current_conns   => esockd:get_current_connections({Protocol, ListenOn}),
          shutdown_count  => esockd:get_shutdown_count({Protocol, ListenOn})}
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
%% Data Export and Import
%%--------------------------------------------------------------------

export_rules() ->
    lists:map(fun({_, RuleId, _, RawSQL, _, _, _, _, _, _, Actions, Enabled, Desc}) ->
                    [{id, RuleId},
                      {rawsql, RawSQL},
                      {actions, actions_to_prop_list(Actions)},
                      {enabled, Enabled},
                      {description, Desc}]
               end, emqx_rule_registry:get_rules()).

export_resources() ->
    lists:foldl(fun({_, Id, Type, Config, CreatedAt, Desc}, Acc) ->
                    NCreatedAt = case CreatedAt of
                                     undefined -> null;
                                     _ -> CreatedAt
                                 end,
                    [[{id, Id},
                      {type, Type},
                      {config, maps:to_list(Config)},
                      {created_at, NCreatedAt},
                      {description, Desc}] | Acc]
               end, [], emqx_rule_registry:get_resources()).

export_blacklist() ->
    lists:foldl(fun(#banned{who = Who, by = By, reason = Reason, at = At, until = Until}, Acc) ->
                    NWho = case Who of
                               {peerhost, Peerhost} -> {peerhost, inet:ntoa(Peerhost)};
                               _ -> Who
                           end,
                    [[{who, [NWho]}, {by, By}, {reason, Reason}, {at, At}, {until, Until}] | Acc]
                end, [], ets:tab2list(emqx_banned)).

export_applications() ->
    lists:foldl(fun({_, AppID, AppSecret, Name, Desc, Status, Expired}, Acc) ->
                    [[{id, AppID}, {secret, AppSecret}, {name, Name}, {desc, Desc}, {status, Status}, {expired, Expired}] | Acc]
                end, [], ets:tab2list(mqtt_app)).

export_users() ->
    lists:foldl(fun({_, Username, Password, Tags}, Acc) ->
                    [[{username, Username}, {password, base64:encode(Password)}, {tags, Tags}] | Acc]
                end, [], ets:tab2list(mqtt_admin)).

export_auth_clientid() ->
    case ets:info(emqx_auth_clientid) of
        undefined -> [];
        _ ->
            lists:foldl(fun({_, ClientId, Password}, Acc) ->
                            [[{clientid, ClientId}, {password, Password}] | Acc]
                        end, [], ets:tab2list(emqx_auth_clientid))
    end.

export_auth_username() ->
    case ets:info(emqx_auth_username) of
        undefined -> [];
        _ ->
            lists:foldl(fun({_, Username, Password}, Acc) ->
                            [[{username, Username}, {password, Password}] | Acc]
                        end, [], ets:tab2list(emqx_auth_username))
    end.

export_auth_mnesia() ->
    case ets:info(emqx_user) of
        undefined -> [];
        _ -> 
            lists:foldl(fun({_, Login, Password, IsSuperuser}, Acc) ->
                            [[{login, Login}, {password, Password}, {is_superuser, IsSuperuser}] | Acc]
                        end, [], ets:tab2list(emqx_user))
    end.

export_acl_mnesia() ->
    case ets:info(emqx_user) of
        undefined -> [];
        _ ->
            lists:foldl(fun({_, Login, Topic, Action, Allow}, Acc) ->
                            [[{login, Login}, {topic, Topic}, {action, Action}, {allow, Allow}] | Acc]
                        end, [], ets:tab2list(emqx_acl))
    end.

export_schemas() ->
    case ets:info(emqx_schema) of
        undefined -> [];
        _ ->
            [emqx_schema_api:format_schema(Schema) || Schema <- emqx_schema_registry:get_all_schemas()]
    end.

import_rules(Rules) ->
    lists:foreach(fun(#{<<"id">> := RuleId,
                        <<"rawsql">> := RawSQL,
                        <<"actions">> := Actions,
                        <<"enabled">> := Enabled,
                        <<"description">> := Desc}) ->
                      Rule = #{
                        id => RuleId,
                        rawsql => RawSQL,
                        actions => map_to_actions(Actions),
                        enabled => Enabled,
                        description => Desc
                      },
                      try emqx_rule_engine:create_rule(Rule)
                      catch throw:{resource_not_initialized, _ResId} ->
                          emqx_rule_engine:create_rule(Rule#{enabled => false})
                      end
                  end, Rules).

import_resources(Reources) ->
    lists:foreach(fun(#{<<"id">> := Id,
                        <<"type">> := Type,
                        <<"config">> := Config,
                        <<"created_at">> := CreatedAt,
                        <<"description">> := Desc}) ->
                      NCreatedAt = case CreatedAt of
                                       null -> undefined;
                                       _ -> CreatedAt
                                   end,
                      emqx_rule_engine:create_resource(#{id => Id,
                                                         type => any_to_atom(Type),
                                                         config => Config,
                                                         created_at => NCreatedAt,
                                                         description => Desc})
                  end, Reources).

import_blacklist(Blacklist) ->
    lists:foreach(fun(#{<<"who">> := Who,
                        <<"by">> := By,
                        <<"reason">> := Reason,
                        <<"at">> := At,
                        <<"until">> := Until}) ->
                      NWho = case Who of
                                 #{<<"peerhost">> := Peerhost} ->
                                     {ok, NPeerhost} = inet:parse_address(Peerhost),
                                     {peerhost, NPeerhost};
                                 #{<<"clientid">> := ClientId} -> {clientid, ClientId};
                                 #{<<"username">> := Username} -> {username, Username}
                             end,
                     emqx_banned:create(#banned{who = NWho, by = By, reason = Reason, at = At, until = Until})
                  end, Blacklist).

import_applications(Apps) ->
    lists:foreach(fun(#{<<"id">> := AppID,
                        <<"secret">> := AppSecret,
                        <<"name">> := Name,
                        <<"desc">> := Desc,
                        <<"status">> := Status,
                        <<"expired">> := Expired}) ->
                      NExpired = case is_integer(Expired) of
                                     true -> Expired;
                                     false -> undefined
                                 end,
                      emqx_mgmt_auth:force_add_app(AppID, Name, AppSecret, Desc, Status, NExpired)
                  end, Apps).

import_users(Users) ->
    lists:foreach(fun(#{<<"username">> := Username,
                        <<"password">> := Password,
                        <<"tags">> := Tags}) ->
                      NPassword = base64:decode(Password),
                      emqx_dashboard_admin:force_add_user(Username, NPassword, Tags)
                  end, Users).

import_auth_clientid(Lists) ->
    case ets:info(emqx_auth_clientid) of
        undefined -> ok;
        _ ->
            [ mnesia:dirty_write({emqx_auth_clientid, ClientId, Password}) || #{<<"clientid">> := ClientId, 
                                                                               <<"password">> := Password} <- Lists ]
    end.

import_auth_username(Lists) ->
    case ets:info(emqx_auth_username) of
        undefined -> ok;
        _ ->
            [ mnesia:dirty_write({emqx_auth_username, Username, Password}) || #{<<"username">> := Username, 
                                                                               <<"password">> := Password} <- Lists ]
    end.

import_auth_mnesia(Auths) ->
    case ets:info(emqx_acl) of
        undefined -> ok;
        _ -> 
            [ mnesia:dirty_write({emqx_user, Login, Password, IsSuperuser}) || #{<<"login">> := Login,
                                                                                 <<"password">> := Password,
                                                                                 <<"is_superuser">> := IsSuperuser} <- Auths ]
    end.

import_acl_mnesia(Acls) ->
    case ets:info(emqx_acl) of
        undefined -> ok;
        _ -> 
            [ mnesia:dirty_write({emqx_acl ,Login, Topic, Action, Allow}) || #{<<"login">> := Login, 
                                                                               <<"topic">> := Topic,
                                                                               <<"action">> := Action,
                                                                               <<"allow">> := Allow} <- Acls ]
    end.

import_schemas(Schemas) -> 
    case ets:info(emqx_schema) of
        undefined -> ok;
        _ -> [emqx_schema_registry:add_schema(emqx_schema_api:make_schema_params(Schema)) || Schema <- Schemas]
    end.

any_to_atom(L) when is_list(L) -> list_to_atom(L);
any_to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
any_to_atom(A) when is_atom(A) -> A.

to_version(Version) when is_integer(Version) ->
    integer_to_list(Version);
to_version(Version) when is_binary(Version) ->
    binary_to_list(Version);
to_version(Version) when is_list(Version) ->
    Version.



%%--------------------------------------------------------------------
%% Common Table API
%%--------------------------------------------------------------------

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

map_to_actions(Maps) ->
    [map_to_action(M) || M <- Maps].

map_to_action(Map = #{<<"id">> := ActionInstId, <<"name">> := Name, <<"args">> := Args}) ->
    #{id => ActionInstId,
      name => any_to_atom(Name),
      args => Args,
      fallbacks => map_to_actions(maps:get(<<"fallbacks">>, Map, []))}.

actions_to_prop_list(Actions) ->
    [action_to_prop_list(Act) || Act <- Actions].

action_to_prop_list({action_instance, ActionInstId, Name, FallbackActions, Args}) ->
    [{id, ActionInstId},
     {name, Name},
     {fallbacks, actions_to_prop_list(FallbackActions)},
     {args, Args}].
