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
-module(dgiot_modbus_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"MODBUS">>).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"MODBUS通道"/utf8>>
    },
    description => #{
        zh => <<"MODBUS通道"/utf8>>
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
            zh => <<"填写正则表达式匹配login报文, 9C-A5标识设备类型，**-**-**-**为设备地址,中杆会自动去除"/utf8>>
        }
    },
    <<"modbustype">> => #{
        order => 4,
        type => string,
        required => true,
        default => #{<<"value">> => <<"modbusrtu">>, <<"label">> => <<"Modbus RTU">>},
        enum => [
            #{<<"value">> => <<"modbusrtu">>, <<"label">> => <<"Modbus RTU">>},
            #{<<"value">> => <<"modbustcp">>, <<"label">> => <<"Modbus TCP">>}
        ],
        title => #{
            zh => <<"Modbus种类"/utf8>>
        },
        description => #{
            zh => <<"Modbus种类"/utf8>>
        }
    },
    <<"dtutype">> => #{
        order => 5,
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
init(?TYPE, ChannelId, #{
    <<"port">> := Port,
    <<"regtype">> := Type,
    <<"regular">> := Regular,
    <<"product">> := Products,
    <<"dtutype">> := Dtutype
} = Args) ->
    {ProdcutId, App} =
        case get_app(Products) of
            [{ProdcutId1, App1} | _] ->
                {ProdcutId1, App1};
            [] ->
                {<<>>, <<>>};
            _ ->
                {<<>>, <<>>}
        end,
    {Header, Len} = get_header(Regular),
    State = #state{
        id = ChannelId,
        regtype = Type,
        head = Header,
        len = Len,
        app = App,
        product = ProdcutId,
        dtutype = Dtutype
    },
%%    dgiot_data:insert({ChannelId, heartbeat}, {Heartbeat, Port}),
    case maps:get(<<"modbustype">>, Args, <<"modbusrtu">>) of
        <<"modbustcp">> ->
            {ok, State, dgiot_modbustcp_tcp:start(Port, State)};
        _ ->
            {ok, State, dgiot_modbusrtu_tcp:start(Port, State)}
    end;


init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := DevAddr, connected_at := _ConnectedAt}, #{peername := PeerName} = _Context}, State) ->
    ?LOG(error, "DevAddr ~p PeerName ~p", [DevAddr, PeerName]),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, disconnected_at := _DisconnectedAt}, _Context}, State) ->
    ?LOG(error, "DevAddr ~p ", [DevAddr]),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := _Topic}, _Msg}, #state{id = ChannelId} = State) ->
    ?LOG(error, "DevAddr ~p Payload ~p ChannelId ~p", [DevAddr, Payload, ChannelId]),
    {ok, State};

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
            "*" -> {Header, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
                end, {[], 0},
        re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).
