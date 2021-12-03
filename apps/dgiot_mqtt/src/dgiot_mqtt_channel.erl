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
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/MQTT.png">>,
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
                dgiot_data:insert({mqttd, ProductId}, {Acl, maps:get(<<"properties">>, Thing, [])});
%%            创建连接规则
%%            创建断开连接规则
%%            创建上传数据规则
            _ ->
                io:format("dgiot_mqtt_channel:init 87 X = ~p.~n", [X]),
                pass
        end
              end, Products),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        auth = Auth
    },
%%    dgiot_matlab_tcp:start(Port, State)
    {ok, State};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}}.

handle_init(State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_event('client.connected', {rule, #{clientid := DevId, connected_at := _ConnectedAt, peername := PeerName}, _Context}, #state{id = _ChannelId} = State) ->
    [DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
    updat_device(DevId, DTUIP, <<"ONLINE">>),
    {ok, State};


handle_event('client.disconnected', {rule, #{clientid := DeviceId, disconnected_at := _DisconnectedAt, peername := PeerName}, _Context}, State) ->
    [DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
    updat_device(DeviceId, DTUIP, <<"OFFLINE">>),
    {ok, State};

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    io:format("_EventId = ~p.~n", [_EventId]),
    io:format("_Event = ~p.~n", [_Event]),
    {ok, State}.

handle_message({rule, #{clientid := _DeviceId, username := ProductId, payload := Payload, topic := Topic, peerhost := Peerhost} = Msg, _Context}, State) ->
    io:format("Msg = ~p.~n", [Msg]),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
%%                     /ecfd3a227c/6C4B909AF64A/metadata/derived  派生物模型上报
                [<<>>, ProductId, DtuAddr, <<"metadata">>, <<"derived">>] ->
                    create_device(ProductId, DtuAddr, Peerhost),
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"timestamp">> := _Timestamp,
                            <<"metadata">> := Metadata,
                            <<"all">> := _All} when is_map(Metadata) ->
                            io:format("Metadata = ~p.~n", [Metadata]),
                            dgiot_tdengine_adapter:save(ProductId, DtuAddr, Metadata);
                        _ ->
                            pass
                    end;
                _ ->
                    pass
            end;
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
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

%%新设备
create_device(ProductId, DtuAddr, DTUIP) ->
    {Acl, _Properties} = dgiot_data:get({mqttd, ProductId}),
    Requests = #{
        <<"devaddr">> => DtuAddr,
        <<"name">> => <<"MATLAB_", DtuAddr/binary>>,
        <<"ip">> => DTUIP,
        <<"isEnable">> => true,
        <<"product">> => ProductId,
        <<"ACL">> => Acl,
        <<"status">> => <<"ONLINE">>,
        <<"brand">> => <<"MATLAB", DtuAddr/binary>>,
        <<"devModel">> => <<"MATLAB">>
    },
    dgiot_device:create_device(Requests).
