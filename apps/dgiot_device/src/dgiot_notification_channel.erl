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
-module(dgiot_notification_channel).
-behavior(dgiot_channelx).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"NOTIFICATION">>).
-record(state, {id, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    title => #{
        zh => <<"NOTIFICATION通道"/utf8>>
    },
    description => #{
        zh => <<"NOTIFICATION通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"msgtype">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"小程序"/utf8>>,
        enum => [
            #{<<"value">> => <<"wechat">>, <<"label">> => <<"小程序"/utf8>>},
            #{<<"value">> => <<"email">>, <<"label">> => <<"邮件"/utf8>>},
            #{<<"value">> => <<"sms">>, <<"label">> => <<"短信"/utf8>>},
            #{<<"value">> => <<"umeng">>, <<"label">> => <<"友盟通知"/utf8>>}
        ],
        title => #{
            zh => <<"通知方式"/utf8>>
        },
        description => #{
            zh => <<"通知方式:wechat|email|sms|umeng"/utf8>>
        }
    },
    <<"defultname">> => #{
        order => 3,
        type => string,
        required => false,
        default => <<"dgiot_notification">>,
        title => #{
            zh => <<"默认通道名"/utf8>>
        },
        description => #{
            zh => <<"默认通道名,有则覆盖手动填入的通道名称"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/modbus.png">>,
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
    State = #state{
        id = ChannelId,
        env = Args
    },
%%    dgiot_parse_id:get_channelid(?BACKEND_CHL, ?TYPE, <<"dgiot_notification">>),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := _DevAddr, connected_at := _ConnectedAt} = Msg, _Context}, State) ->
    io:format("~s ~p  Msg = ~p.~n", [?FILE, ?LINE, Msg]),
    io:format("~s ~p  _Context = ~p.~n", [?FILE, ?LINE, _Context]),
    {ok, State};

handle_message({rule, #{clientid := _DevAddr, disconnected_at := _DisconnectedAt} = Msg, _Context}, State) ->
    io:format("~s ~p  Msg = ~p.~n", [?FILE, ?LINE, Msg]),
    io:format("~s ~p  _Context = ~p.~n", [?FILE, ?LINE, _Context]),
    {ok, State};

handle_message({rule, #{clientid := _DevAddr, payload := _Payload, topic := _Topic} = Msg, _Context}, State) ->
    io:format("~s ~p  Msg = ~p.~n", [?FILE, ?LINE, Msg]),
    io:format("~s ~p  _Context = ~p.~n", [?FILE, ?LINE, _Context]),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
