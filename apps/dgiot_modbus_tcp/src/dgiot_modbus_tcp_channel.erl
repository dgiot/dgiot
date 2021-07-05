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
-module(dgiot_modbus_tcp_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include("dgiot_modbus_tcp.hrl").
-define(TYPE, <<"MODBUS_TCP">>).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"MODBUS_TCP通道"/utf8>>
    },
    description => #{
        zh => <<"MODBUS_TCP通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 20110,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"regtype">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"上传Mac"/utf8>>,
        title => #{
            zh => <<"注册类型"/utf8>>
        },
        description => #{
            zh => <<"上传Mac"/utf8>>
        }
    },
    <<"regular">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"9C-A5-25-**-**-**">>,
        title => #{
            zh => <<"登录报文帧头"/utf8>>
        },
        description => #{
            zh => <<"填写正则表达式匹配login"/utf8>>
        }
    },
    <<"DTUTYPE">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"usr">>,
        title => #{
            zh => <<"控制器厂商"/utf8>>
        },
        description => #{
            zh => <<"控制器厂商"/utf8>>
        }
    },
    <<"heartbeat">> => #{
        order => 5,
        type => integer,
        required => true,
        default => 10,
        title => #{
            zh => <<"心跳周期"/utf8>>
        },
        description => #{
            zh => <<"心跳周期"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/MODBUS_TCP%E9%80%9A%E9%81%93.jpg">>,
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
    <<"heartbeat">> := Heartbeat,
    <<"regtype">> := Type,
    <<"regular">> := Regular,
    <<"product">> := Products
} = _Args) ->
    [{ProdcutId, App} | _] = get_app(Products),
    {Header, Len} = get_header(Regular),
    State = #state{
        id = ChannelId,
        regtype = Type,
        head = Header,
        len = Len,
        app = App,
        product = ProdcutId
    },

    dgiot_data:insert({ChannelId, heartbeat}, {Heartbeat, Port}),
    {ok, State, dgiot_modbus_tcp:start(Port, State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

get_app(Products) ->
    lists:map(fun({ProdcutId, #{<<"ACL">> := Acl}}) ->
        Predicate = fun(E) ->
            case E of
                <<"role:", _/binary>> -> true;
                _ -> false
            end
                    end,
        [<<"role:", App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
        {ProdcutId, App}
              end, Products).



get_header(Regular) ->
    lists:foldl(fun(X, {Header, Len}) ->
        case X of
            "**" -> {Header, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
                end, {[], 0},
        re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).
