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

-module(dgiot_tcp_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"TCP">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {
    id,
    buff_size = 1024000,
    devaddr = <<>>,
    heartcount = 0,
    head = "xxxxxx0eee",
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    env = #{},
    dtutype = <<>>
}).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"TCP采集通道"/utf8>>
    },
    description => #{
        zh => <<"TCP采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 18110,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"login">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"9C-A5-**-**-**-**">>,
        title => #{
            zh => <<"登录报文帧头"/utf8>>
        },
        description => #{
            zh => <<"填写正则表达式匹配login报文, 9C-A5标识设备类型，**-**-**-**为设备地址,中杆会自动去除"/utf8>>
        }
    },
    <<"dtutype">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"dgiot">>,
        title => #{
            zh => <<"控制器厂商"/utf8>>
        },
        description => #{
            zh => <<"控制器厂商与设备类型一起组合生成产品ID"/utf8>>
        }
    },
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
init(?TYPE, ChannelId, #{
    <<"port">> := Port,
    <<"login">> := Login,
    <<"dtutype">> := Dtutype
} = Args) ->
    {Header, Len} = get_header(Login),
    State = #state{
        id = ChannelId,
        head = Header,
        len = Len,
        dtutype = Dtutype,
        buff_size = maps:get(<<"buff_size">>, Args, 1024000)
    },
    {ok, State, dgiot_tcp_worker:child_spec(Port, State)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.

handle_message(Message, State) ->
    ?LOG(info, "Channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.

get_header(Regular) ->
    lists:foldl(fun(X, {Header, Len}) ->
        case X of
            "**" -> {Header, Len + length(X)};
            "*" -> {Header, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
                end, {[], 0},
        re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).
