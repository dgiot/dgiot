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
-record(state, {id, auth = <<"ProductSecret"/utf8>>}).
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
        enum => [<<"ProductSecret">>, <<"DeviceSecret"/utf8>>, <<"DeviceCert"/utf8>>],
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
    <<"port">> := Port,
    <<"product">> := Products,
    <<"auth">> := Auth}) ->
    lists:map(fun(X) ->
        case X of
            {ProductId, #{<<"ACL">> := Acl, <<"nodeType">> := 1,<<"thing">> := Thing}} ->
                dgiot_data:insert({mqttd, ChannelId}, {ProductId, Acl, maps:get(<<"properties">>,Thing,[])});
            _ ->
                ?LOG(info,"X ~p", [X]),
                pass
        end
              end, Products),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        auth = Auth
    },
    {ok, State, dgiot_matlab_tcp:start(Port, State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := DtuAddr, connected_at := _ConnectedAt}, #{peername := PeerName} = _Context}, #state{id = ChannelId} = State) ->
    ?LOG(error,"DtuAddr ~p PeerName ~p",[DtuAddr,PeerName] ),
    DTUIP = dgiot_utils:get_ip(PeerName),
    dgiot_matlab:create_matlab(DtuAddr, ChannelId, DTUIP),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, disconnected_at := _DisconnectedAt}, _Context}, State) ->
    ?LOG(error,"DevAddr ~p ",[DevAddr] ),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := _Topic}, _Msg}, #state{id = ChannelId} = State) ->
    ?LOG(error,"DevAddr ~p Payload ~p ChannelId ~p",[DevAddr,Payload,ChannelId] ),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
