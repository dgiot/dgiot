%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_location_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_location.hrl").
-define(TYPE, <<"LOCATION">>).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?FEATURE_CHL,
    title => #{
        zh => <<"LOCATION应用通道"/utf8>>
    },
    description => #{
        zh => <<"LOCATION应用通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"search">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"IP"/utf8>>,
        enum => [<<"IP"/utf8>>, <<"BTS"/utf8>>, <<"WIFI"/utf8>>, <<"GPS"/utf8>>],
        title => #{
            zh => <<"定位模式"/utf8>>
        },
        description => #{
            zh => <<"定位模式:IP|BTS|WIFI|GPS"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/%E4%BD%8D%E7%BD%AE%E5%AE%9A%E4%BD%8D%E9%80%9A%E9%81%93.jpg">>,
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
    <<"search">> := Search}) ->
    lists:map(fun(X) ->
        case X of
            {ProductId, #{<<"ACL">> := Acl, <<"nodeType">> := 1, <<"thing">> := Thing}} ->
                dgiot_data:insert({dtu, ChannelId}, {ProductId, Acl, maps:get(<<"properties">>, Thing, [])});
            {ProductId, #{<<"ACL">> := Acl, <<"thing">> := Thing}} ->
                dgiot_data:insert({meter, ChannelId}, {ProductId, Acl, maps:get(<<"properties">>, Thing, [])});
            _ ->
                pass
        end
              end, Products),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        search = Search
    },
    {ok, State, []};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
%%SELECT username as productid, clientid, connected_at FROM "$events/client_connected" WHERE username = 'bffb6a3a27'
handle_event('client.connected', {rule, #{peername := PeerName}, #{<<"clientid">> := DtuAddr, <<"productid">> := ProductId} = _Select}, State) ->
    [DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
    DeviceId = dgiot_parse:get_deviceid(ProductId, DtuAddr),
    case dgiot_device:lookup(DeviceId) of
        {ok, _V} ->
            dgiot_device:put(#{<<"objectId">> => DeviceId});
        _ ->
            dgiot_location:create_dtu(mqtt, DtuAddr, ProductId, DTUIP)
    end,
    {ok, State};

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error, "EventId ~p Event ~p", [EventId, Event]),
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := DevAddr, disconnected_at := _DisconnectedAt}, _Context}, State) ->
    ?LOG(error, "DevAddr ~p ", [DevAddr]),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := _Topic}, _Msg}, #state{id = ChannelId} = State) ->
    ?LOG(error, "DevAddr ~p Payload ~p ChannelId ~p", [DevAddr, Payload, ChannelId]),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
