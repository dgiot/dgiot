%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%%#  用户场景
%%从用户到设备，主要需要解决如下几个问题：
%%+ 人与设备的关系，User基于流动性，权限系统里一般不会直接绑定User与Device的权限关系，中间会通过Department(Role)来间接绑定ACL
%%+ 设备与设备的关系，设备与设备之间有可能存在真实的关系，例如DTU与Meter，也可能只有一种虚拟关系，例如Group与DTU，属于因工程需要，临时拉群
%%+ 对具体设备来说，1、需要一个UID来表征身份；2、需要一个route来表征关系；3、需要多个tag来表征特征
%%+ 重点讨论Meter、DTU和TCP Server的交互过程
%%
%%| No.|名称|   Meter         |   DTU                  | TCP Server                 |  说明      |
%%| --| ----   | -------      | ------                 | -----------               |-----------|
%%|1 |连接     |               | send ->  [IP]           | ack <-- [IP]             | 必选      |
%%|2 |登陆     |               | send ->  [DtuAddr]      | ack <-- [DtuAddr]        | 可选，可用IP代替|
%%|3 |扫串口   | ack-> [485]   | send <-- [search 485]   | send <--[search 485]    | 可选，有档案可免 |
%%|4 |扫modbus | ack-> [modbus]| send <-- [search modbus]   | send <--[search modbus] |可选，有档案可免 |
%%|5 |扫表 | ack-> [Meter Addr]| send <-- [search meter]   | send <--[search meter] |可选，有档案可免 |
%%|6 |抄表 | ack-> [Meter Data]| send <-- [read meter]   | send <--[read meter] |必选 |
%%|7 |写表 | ack-> [Meter Control]| send <-- [write meter]   | send <--[write meter] |可选 |
%%|8 |登出 |       |  send -> [DtuAddr] |  ack <-- [DtuAddr]        |可选 |
%%|9 |断开 |     |  send -> [IP]      |  do_something        |必选 |
%%

-module(dgiot_matlab_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_matlab.hrl").
-define(TYPE, <<"MATLAB">>).
-define(MAX_BUFF_SIZE, 1024).
-define(SECS, [5, 5 * 60]).
-define(JIAOSHI, 60 * 10 * 1000).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"MATLAB采集通道"/utf8>>
    },
    description => #{
        zh => <<"MATLAB采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 61888,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"search">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"quick"/utf8>>,
        enum => [<<"nosearch">>, <<"quick">>, <<"normal">>],
        title => #{
            zh => <<"搜表模式"/utf8>>
        },
        description => #{
            zh => <<"搜表模式:nosearch|quick|normal"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/matlab.jpg">>,
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
    <<"product">> := Products,
    <<"search">> := Search}) ->
    lists:map(fun(X) ->
        case X of
            {ProductId, #{<<"ACL">> := Acl, <<"nodeType">> := 1}} ->
                {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} = dgiot_device:lookup_prod(ProductId),
                dgiot_data:insert({matlab, ChannelId}, {ProductId, Acl, Properties});
            _ ->
                ?LOG(info,"X ~p", [X]),
                pass
        end
              end, Products),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        search = Search
    },
    {ok, State, dgiot_matlab_tcp:start(Port, State)};

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
handle_message({rule, #{clientid := DtuAddr, connected_at := _ConnectedAt}, #{peername := PeerName} = _Context}, #state{id = ChannelId} = State) ->
    ?LOG(error,"DtuAddr ~p PeerName ~p",[DtuAddr,PeerName] ),
    DTUIP = dgiot_utils:get_ip(PeerName),
    dgiot_matlab:create_matlab(DtuAddr, ChannelId, DTUIP),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, disconnected_at := _DisconnectedAt}, _Context}, State) ->
    ?LOG(error,"DevAddr ~p ",[DevAddr] ),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := _Topic}, _Msg}, #state{id = ChannelId} = State) ->
    ?LOG(error,"DevAddr ~p Payload ~p ChannelId ~p",[DevAddr,Payload,ChannelId] ),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
