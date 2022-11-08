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
-record(state, {id}).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"MQTT模拟设备通道"/utf8>>
    },
    description => #{
        zh => <<"MQTT模拟设备通道"/utf8>>
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
    <<"username">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"anonymous"/utf8>>,
        title => #{
            zh => <<"用户名"/utf8>>
        },
        description => #{
            zh => <<"用户名"/utf8>>
        }
    },
    <<"password">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"test"/utf8>>,
        title => #{
            zh => <<"密码"/utf8>>
        },
        description => #{
            zh => <<"密码"/utf8>>
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

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, ChannelArgs) ->
    State = #state{id = ChannelId},
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"profile">>]),
    ChildSpecs = dlink_mqttc:childspec(ChannelId, ChannelArgs),
    {ok, State, ChildSpecs}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Device">>, #{<<"profile">> := #{<<"mock">> := #{<<"enable">> := true}}, <<"objectId">> := DeviceId}},
        #state{id = ChannelId} = State) ->
    dgiot_client:start(ChannelId, DeviceId, #{}),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Device">>, #{<<"profile">> := #{<<"mock">> := #{<<"enable">> := false}}, <<"objectId">> := DeviceId}},
        #state{id = ChannelId} = State) ->
    dgiot_client:stop(ChannelId, DeviceId),
    {ok, State};

handle_message(_Message, State) ->
%%    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

