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

-module(dgiot_dlink_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"DLINK">>).
-author("johnliu").
-record(state, {id}).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"Dlink采集通道"/utf8>>
    },
    description => #{
        zh => <<"Dlink采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/dgiot_dlink_channel.png">>,
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
init(?TYPE, ChannelId, _ChannelArgs) ->
    State = #state{id = ChannelId},
    {ok, State}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message({mqtt_login, do_after, ProductId, DeviceAddr, Ip}, State) ->
    io:format("~s ~p DeviceAddr ~p ~n",[?FILE, ?LINE, DeviceAddr]),
    dgiot_device:create_device(ProductId, DeviceAddr, Ip),
    {ok, State};

handle_message(_Message, State) ->
%%    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.


