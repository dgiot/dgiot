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


-module(dgiot_grpc_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_grpc.hrl").
-define(TYPE, <<"GRPC_ClIENT">>).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{

    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"GRPC资源通道"/utf8>>
    },
    description => #{
        zh => <<"GRPC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"scheme">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"http">>,
        title => #{
            zh => <<"通讯协议"/utf8>>
        },
        description => #{
            zh => <<"通讯协议"/utf8>>
        }
    },
    <<"host">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"127.0.0.1">>,
        title => #{
            zh => <<"IP"/utf8>>
        },
        description => #{
            zh => <<"grpc服务器IP"/utf8>>
        }
    },
    <<"port">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 7000,
        title => #{
            zh => <<"port"/utf8>>
        },
        description => #{
            zh => <<"grpc服务器端口"/utf8>>
        }
    },
    <<"model">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"both"/utf8>>,
        enum => [<<"both">>, <<"client">>, <<"server">>],
        title => #{
            zh => <<"启动模式"/utf8>>
        },
        description => #{
            zh => <<"启动模式:both|client|server"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/grpc-logo.png">>,
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
init(?TYPE, ChannelId, Env) ->
    State = #state{
        id = ChannelId,
        env = Env
    },
    Port = maps:get(<<"port">>, Env),
%%    Scheme = maps:get(<<"scheme">>, Env),
%%    Host = dgiot_utils:to_list(maps:get(<<"host">>, Env)),
    %%Opts0 = [{scheme, dgiot_utils:to_atom(Scheme)}, {host, Host}, {port, Port}],
    case maps:get(<<"model">>, Env) of
        <<"both">> ->
            dgiot_grpc_server:start(ChannelId, Port, []),
            application:ensure_all_started(emqx_hooks);
            %%emqx_exhook:enable(ChannelId, Opts0);
        <<"client">> ->
            application:ensure_all_started(emqx_hooks);
            %%emqx_exhook:enable(ChannelId, Opts0);
        <<"server">> ->
            dgiot_grpc_server:start(ChannelId, Port, [])
    end,
    {ok, State, []}.

handle_init(State) ->
    erlang:send_after(5000, self(), start),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
%%SELECT username as productid, clientid, connected_at FROM "$events/client_connected" WHERE username = 'bffb6a3a27'
handle_event('client.connected', {rule, #{peername := PeerName}, #{<<"clientid">> := DtuAddr, <<"productid">> := ProductId} = _Select}, State) ->
    [_DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
    DeviceId = dgiot_parse:get_deviceid(ProductId, DtuAddr),
    case dgiot_device:lookup(DeviceId) of
        {ok, _V} ->
            dgiot_device:put(#{<<"objectId">> => DeviceId});
        _ ->
            pass
    end,
    {ok, State};

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error, "EventId ~p Event ~p", [EventId, Event]),
    {ok, State}.

handle_message(start, State) ->
    {ok, State};

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

stop(_ChannelType, ChannelId, _State) ->
    emqx_exhook:disable(ChannelId),
    ok.
