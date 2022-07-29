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
-record(state, {id, auth = <<"ProductSecret"/utf8>>, count, devaddr, deviceId}).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"MQTT压测通道"/utf8>>
    },
    description => #{
        zh => <<"MQTT压测通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"auth">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"ProductSecret"/utf8>>,
        type => string,
        required => false,
        default => #{<<"value">> => <<"ProductSecret">>, <<"label">> => <<"一型一密"/utf8>>},
        enum => [
            #{<<"value">> => <<"ProductSecret">>, <<"label">> => <<"一型一密"/utf8>>},
            #{<<"value">> => <<"DeviceSecret">>, <<"label">> => <<"一机一密"/utf8>>},
            #{<<"value">> => <<"DeviceCert">>, <<"label">> => <<"设备证书"/utf8>>}
        ],
        title => #{
            zh => <<"设备授权"/utf8>>
        },
        description => #{
            zh => <<"设备授权"/utf8>>
        }
    },
    <<"count">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 10,
        title => #{
            zh => <<"压测数量"/utf8>>
        },
        description => #{
            zh => <<"压测数量：数量为-1时, 从设备档案数据库获取Device, 否则自动生成device"/utf8>>
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
    <<"product">> := [{_ProductId, _Product} |_],
    <<"auth">> := Auth,
    <<"count">> := Count}) ->
    State = #state{
        id = ChannelId,
        auth = Auth,
        count = Count
    },
%%    io:format("~s ~p ProductId ~p ~n",[?FILE, ?LINE, ProductId]),
    {ok, State};

init(?TYPE, _ChannelId, _Args) ->
%%    io:format("~s ~p _ChannelId ~p ~n",[?FILE, ?LINE, _ChannelId]),
    {ok, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(_Message, State) ->
%%    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
