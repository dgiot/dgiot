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
-module(dgiot_common_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"COMMON">>).
-record(state, {id, env, superchannel}).
%% API
-export([
    start/2
]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

-channel(?TYPE).
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"TCPC资源通道"/utf8>>
    },
    description => #{
        zh => <<"TCPC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/TcpIcon.jpeg">>,
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
init(?TYPE, ChannelId, Args) ->
    SuperChannel = maps:get(<<"superchannel">>, Args, ChannelId),
    State = #state{id = ChannelId, env = Args, superchannel = SuperChannel},
    StartTime = maps:get(<<"starttime">>, Args, dgiot_datetime:now_secs() + 5),
    NewStartTime =
        case dgiot_datetime:now_secs() > StartTime of
            true ->
                dgiot_datetime:now_secs() + 5;
            _ ->
                StartTime
        end,
    EndTime = dgiot_utils:to_int(maps:get(<<"endtime">>, Args, dgiot_datetime:now_secs() + 1000)),
    io:format("~s ~p SuperChannel = ~p NewStartTime ~p EndTime= ~p ~n", [?FILE, ?LINE, SuperChannel,NewStartTime,  EndTime]),
    dgiot_client:add_clock(ChannelId, NewStartTime, EndTime),
    ChildSpecs = maps:get(<<"childspecs">>, Args, []),
    {ok, State, ChildSpecs}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message(start_client, #state{id = ChannelId} = State) ->
    io:format("~s ~p ChannelId = ~p.~n", [?FILE, ?LINE, ChannelId]),
    case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
            dgiot_client:start_que(ChannelId);
        _ ->
            pass
    end,
    {ok, State};

handle_message(stop_client, #state{id = ChannelId, superchannel = SuperChannel} = State) ->
    io:format("~s ~p stop_client ChannelId = ~p. SuperChannel ~p ~n", [?FILE, ?LINE, ChannelId, SuperChannel]),
    dgiot_channelx:do_message(dgiot_utils:to_binary(SuperChannel), {stop_client, ChannelId}),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    dgiot_client:stop(ChannelId),
    ?LOG(warning, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.
