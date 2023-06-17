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
-module(dgiot_mqttc_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"MQTTC">>).
-author("kenneth").
-record(state, {id, client = disconnect, env = #{}}).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-dgiot_data("ets").
-export([init_ets/0, send/4]).

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"MQTT桥接通道"/utf8>>
    },
    description => #{
        zh => <<"MQTT桥接通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"address">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1">>,
        title => #{
            zh => <<"主机地址"/utf8>>
        },
        description => #{
            zh => <<"主机地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 1883,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"ssl">> => #{
        order => 5,
        type => boolean,
        required => true,
        default => false,
        title => #{
            zh => <<"SSL"/utf8>>
        },
        description => #{
            zh => <<"是否使用SSL"/utf8>>
        }
    },
    <<"clean_start">> => #{
        order => 6,
        type => boolean,
        required => true,
        default => false,
        title => #{
            zh => <<"清除会话"/utf8>>
        },
        description => #{
            zh => <<"是否清除会话"/utf8>>
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

init_ets() ->
    dgiot_data:init(?DGIOT_MQTT_WORK).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, ChannelArgs) ->
    State = #state{
        id = ChannelId,
        env = ChannelArgs
    },
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 10, dgiot_datetime:now_secs() + 120),
    {ok, State, dgiot_mqttc_worker:childSpec(ChannelId, ChannelArgs)}.

%% 初始化池子
handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(start_client, #state{id = ChannelId, env = ChannelArgs} = State) ->
    case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
            dgiot_mqttc_worker:start(ChannelId, ChannelArgs),
            dgiot_data:insert({start_client, ChannelId}, ChannelId);
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, ChannelId, _State) ->
    dgiot_data:delete({start_client, ChannelId}),
    ok.

send(ProductId, DevAddr, Topic, Payload) ->
    Clientid = <<ProductId/binary, "_", DevAddr/binary>>,
    case dgiot_data:get(?DGIOT_MQTT_WORK, ProductId) of
        not_find ->
            pass;
        ChannelId ->
            dgiot_client:send(ChannelId, Clientid, Topic, Payload)
    end.












