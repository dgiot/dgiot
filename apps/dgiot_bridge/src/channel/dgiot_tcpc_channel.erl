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
-module(dgiot_tcpc_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"TCPC">>).
-record(state, {id, env}).
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
    <<"ip">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1"/utf8>>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 8080,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/tcpc_channel.png">>,
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
    State = #state{id = ChannelId, env = Args},
    StartTime = maps:get(<<"startTime">>, Args, dgiot_datetime:now_secs() + 5),
    EndTime = maps:get(<<"endTime">>, Args, dgiot_datetime:now_secs() + 1000000),
    Mod = maps:get(<<"mod">>, Args, dgiot_tcpc_worker),
    io:format("~s ~p StartTime = ~p. EndTime =  ~p~n", [?FILE, ?LINE, StartTime, EndTime]),
    dgiot_client:add_clock(ChannelId, StartTime, EndTime),
    NewArgs = Args#{<<"channel">> => ChannelId, <<"mod">> => Mod, <<"count">> => 3, <<"freq">> => 10},
    {ok, State, dgiot_client:register(ChannelId, tcp_client_sup, NewArgs)}.

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
            dgiot_client:start(ChannelId, dgiot_utils:to_binary(<<"AF000001">>));
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    dgiot_client:stop(ChannelId),
    ?LOG(warning, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.
