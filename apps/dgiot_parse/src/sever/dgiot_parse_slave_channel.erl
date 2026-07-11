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

-module(dgiot_parse_slave_channel).
-define(TYPE1, <<"PARSESLAVE">>).
-author("kenneth").
-include("dgiot_parse.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_channelx).

-export([start/2, init/3, handle_init/1, handle_event/3, handle_message/2, stop/3, handle_save/1]).

-record(state, {channel, cfg}).

%% 注册通道类型
-channel(?TYPE1).
-channel_type(#{
    cType => ?TYPE1,
    type => ?BACKEND_CHL,
    priority => 1,
    title => #{
        zh => <<"Parse 数据主备同步通道"/utf8>>
    },
    description => #{
        zh => <<"Parse 数据主备同步通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"issync">> => #{
        order => 4,
        type => boolean,
        required => true,
        default => true,
        title => #{
            zh => <<"是否同步"/utf8>>
        },
        description => #{
            zh => <<"是否同步"/utf8>>
        }
    },
    <<"interval">> => #{
        order => 1,
        type => integer,
        required => false,
        default => 180,
        title => #{
            zh => <<"同步间隔/秒"/utf8>>
        },
        description => #{
            zh => <<"从库同步主库间隔/秒"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/parse_slave_channel.png">>,
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

start(Channel, Cfg) ->
    dgiot_channelx:add(?TYPE1, Channel, ?MODULE, Cfg#{
        <<"Size">> => 100
    }).

%% 通道初始化
init(?TYPE1, Channel, Cfg) ->
    State = #state{channel = Channel, cfg = Cfg},
%%    dgiot_parse:sync_parse(),
%%    dgiot_parse:sync_user(),
%%    dgiot_parse:sync_role(),
    {ok, State}.

%% 初始化池子
handle_init(State) ->
    {ok, State}.

handle_message(export, #state{cfg = Cfg} = State) ->
    {reply, {ok, Cfg}, State};

handle_message(config, #state{cfg = Cfg} = State) ->
    {reply, {ok, Cfg}, State};

handle_message(_Message, State) ->
    {ok, State}.

handle_event(_EventId, _Event, _State) ->
    ok.

handle_save(Channel) ->
    dgiot_parse_cache:do_save(Channel).

stop(_ChannelType, _ChannelId, _State) ->
    ok.






















