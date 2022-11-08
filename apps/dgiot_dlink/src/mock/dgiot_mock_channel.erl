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

-module(dgiot_mock_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"MOCK">>).
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
        zh => <<"MOCK模拟设备通道"/utf8>>
    },
    description => #{
        zh => <<"MOCK模拟设备通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/dgiot_mock_channel.png">>,
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
    ChildSpecs = dgiot_mock_mqtt:childspec(ChannelId, ChannelArgs),
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

