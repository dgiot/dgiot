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

-module(dgiot_uav_tcp_channel).
-behaviour(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

-define(TYPE, <<"UAV_TCP">>).
-record(state, {
    id,
    port,
    product_id = <<>>,
    env = #{}
}).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{zh => <<"无人机TCP通道"/utf8>>},
    description => #{zh => <<"无人机TCP通道，处理无人机DTU的TCP会话，集成工位发现功能"/utf8>>}
}).

-params(#{
    <<"port">> => #{
        order => 1, type => integer, required => true, default => 20000,
        title => #{zh => <<"端口"/utf8>>},
        description => #{zh => <<"侦听端口，调试使用20000端口"/utf8>>}
    },
    <<"product_id">> => #{
        order => 2, type => string, required => false, default => <<"">>,
        title => #{zh => <<"产品ID"/utf8>>},
        description => #{zh => <<"无人机产品ID"/utf8>>}
    },
    <<"ico">> => #{
        order => 102, type => string, required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/uav_tcp_channel.png">>,
        title => #{en => <<"channel ICO">>, zh => <<"通道ICO"/utf8>>},
        description => #{en => <<"channel ICO">>, zh => <<"通道ICO"/utf8>>}
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

init(?TYPE, ChannelId, #{<<"port">> := Port, <<"product_id">> := ProductId} = Args) ->
    State = #state{
        id = ChannelId,
        port = Port,
        product_id = ProductId,
        env = maps:get(<<"env">>, Args, #{})
    },
    ?LOG(error, "无人机TCP通道启动: ~p, 端口: ~p", [ChannelId, Port]),
    {ok, State, dgiot_uav_tcp_worker:child_spec(Port, State)}.

handle_init(State) ->
    ?LOG(info, "无人机TCP通道初始化完成: ~p", [State#state.id]),
    {ok, State}.

handle_event(_EventId, Event, State) ->
    ?LOG(info, "通道事件: ~p", [Event]),
    {ok, State}.

handle_message(Message, State) ->
    ?LOG(debug, "收到通道消息: ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "通道停止: ~p, ~p", [ChannelType, ChannelId]),
    ok.