%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_mqtt_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"MQTT">>).
-author("johnliu").
-record(state, {id, auth = <<"ProductSecret"/utf8>>, devaddr, deviceId}).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"MQTT采集通道"/utf8>>
    },
    description => #{
        zh => <<"MQTT采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"auth">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"ProductSecret"/utf8>>,
        enum => [<<"ProductSecret"/utf8>>, <<"DeviceSecret"/utf8>>, <<"DeviceCert"/utf8>>],
        title => #{
            zh => <<"设备授权"/utf8>>
        },
        description => #{
            zh => <<"设备授权：一型一密:ProductSecret 一机一密: DeviceSecret 设备证书：DeviceCert "/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/MQTT.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).


start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"product">> := Products,
    <<"auth">> := Auth}) ->
%%    io:format("Products = ~p.~n", [Products]),
    lists:map(fun(X) ->
        case X of
            {ProductId, #{<<"ACL">> := Acl, <<"thing">> := Thing}} ->
                dgiot_data:insert({mqttd, ProductId}, {Acl, maps:get(<<"properties">>, Thing, [])}),
%%              创建连接规则
                ConRawsql = <<"SELECT clientid, connected_at FROM \"$events/client_connected\" WHERE username = '", ProductId/binary, "'">>,
                create_rules(<<"rule:connected_", ProductId/binary>>, ChannelId, <<"创建连接规则"/utf8>>, ConRawsql, <<"/${productid}/#">>),
%%              创建断开连接规则
                DisRawsql = <<"SELECT clientid, disconnected_at FROM \"$events/client_disconnected\" WHERE username = '", ProductId/binary, "'">>,
                create_rules(<<"rule:disconnected_", ProductId/binary>>, ChannelId, <<"断开连接规则"/utf8>>, DisRawsql, <<"/${productid}/#">>),
                %%              创建上传数据规则
                MetaRawsql = <<"SELECT payload.msg as msg,clientid,'", ProductId/binary, "' as productid FROM \"/", ProductId/binary, "/#\" WHERE username = '", ProductId/binary, "'">>,
                create_rules(<<"rule:metadata_", ProductId/binary>>, ChannelId, <<"派生物模型上报规则"/utf8>>, MetaRawsql, <<"/${productid}/#">>);
            _ ->
                io:format("~s ~p X = ~p.~n", [?FILE, ?LINE, X]),
                pass
        end
              end, Products),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        auth = Auth
    },
    dgiot_rule_handler:sysc_rules(),
    emqx_rule_engine_api:list_rules(#{}, []),
    {ok, State};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}}.

handle_init(State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_event('client.connected', {rule, #{clientid := DeviceId, connected_at := _ConnectedAt, peername := PeerName}, _Context}, #state{id = _ChannelId} = State) ->
    [DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
    updat_device(DeviceId, DTUIP, <<"ONLINE">>),
    dgiot_device:online(DeviceId),
    {ok, State};


handle_event('client.disconnected', {rule, #{clientid := DeviceId, disconnected_at := _DisconnectedAt, peername := PeerName}, _Context}, State) ->
    [DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
    updat_device(DeviceId, DTUIP, <<"OFFLINE">>),
    dgiot_device:offline(DeviceId),
    {ok, State};

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message({rule, _Msg, _Context}, State) ->
    {ok, State};

handle_message(_Message, State) ->
    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

%% 更新设备
updat_device(DeviceId, DTUIP, Status) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, _Result} ->
            Body = #{
                <<"ip">> => DTUIP,
                <<"status">> => Status},
            dgiot_parse:update_object(<<"Device">>, DeviceId, Body);
        _R ->
            pass
    end.

create_rules(RuleID, ChannelId, Description, Rawsql, Target_topic) ->
    emqx_rule_engine_api:create_resource(#{},
        [
            {<<"id">>, <<"resource:", ChannelId/binary>>},
            {<<"type">>, <<"dgiot_resource">>},
            {<<"config">>, [{<<"channel">>, ChannelId}]},
            {<<"description">>, <<"resource:", ChannelId/binary>>}
        ]),
    Params = #{
        <<"actions">> => [#{<<"name">> => <<"dgiot">>, <<"fallbacks">> => [],
            <<"params">> => #{
                <<"$resource">> => <<"resource:", ChannelId/binary>>,
                <<"channel">> => <<"数蛙物联网通道"/utf8>>,
                <<"payload_tmpl">> => <<"${payload}">>,
                <<"target_qos">> => 0,
                <<"target_topic">> => Target_topic
            }}],
        <<"enabled">> => true,
        <<"ctx">> => #{
            <<"clientid">> => <<"c_swqx">>,
            <<"payload">> => <<"{\"msg\":\"hello\"}">>,
            <<"qos">> => 1,
            <<"topic">> => <<"t/a">>,
            <<"username">> => <<"u_swqx">>
        },
        <<"description">> => Description,
        <<"for">> => <<"[\"t/#\"]">>,
        <<"rawsql">> => Rawsql
    },
    ObjectId = dgiot_parse_id:get_dictid(RuleID, <<"ruleengine">>, <<"Rule">>, <<"Rule">>),
    case dgiot_parse:get_object(<<"Dict">>, ObjectId) of
        {ok, _} ->
            dgiot_rule_handler:save_rule_to_dict(RuleID, Params);
        _ ->
            R = emqx_rule_engine_api:create_rule(#{}, maps:to_list(Params)),
            case R of
                {ok, #{data := #{id := EmqxRuleId}}} ->
                    dgiot_data:delete(?DGIOT_RUlES, EmqxRuleId),
                    emqx_rule_engine_api:delete_rule(#{id => EmqxRuleId}, []),
                    dgiot_rule_handler:save_rule_to_dict(RuleID, Params);
                _ -> pass
            end
    end.
