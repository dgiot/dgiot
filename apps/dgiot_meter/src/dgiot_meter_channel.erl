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
%%扫串口、扫描modbus、扫表是一个费时费流量的操作，例如扫表一般至少需要扫256次，一般只会在物联网工程施工阶段进行，并完成相应的自动注册功能，形成设备档案，正常运行中不进行这些操作。
%%
%%这也是为什么电力抄表非常强调电表档案建设，如果没有档案，每一次DTU掉线都需要重新进行非常耗时耗流量的扫描任务
%%
%%整体交互流程如下
%%
%%```
%%---------------------------------------------------------------------------------------------------------------------
%%|             物理层                                      |   连接层                 |      虚拟层            | 应用层|
%%---------------------------------------------------------------------------------------------------------------------
%%User1(Session)                User2(Session)
%%|                              |
%%API                            API             <--http--> dgiot_rest  --+
%%|                              |                                       |
%%+                              +                                       |
%%Department1(Role)             Department2(Role)                             |
%%|                              |                                       |
%%ACL                            ACL            <--parse--> shuaw_parse --+
%%+                              +                                       |              +-- 时序数据--+
%%Group(Device)                 Group(Device)                                |              |            |
%%|                              |                                       | === 流计算==> 物理层镜像    +--> 批量计算
%%+--------+-------+                      +                                       |              |            |
%%|                |                      |                                       |              +-- 关系数据--+
%%DTU1(Device1)    DTU2(Device)           DTU3(Device)        <--tcp-->  tcp_server ---+
%%|                |                      |                                       |
%%modbus             modbus                modbus            <--modbus-->  proctol ---+
%%|                |                      |                                       |
%%+                +                      +                                       |
%%485              485                     485             <--485-->    proctol  --+
%%|                |                      |                                       |
%%+                +             +--------+--------+                              |
%%|                |             |                 |                              |
%%Meter1(Device) Meter2(Device) Meter4(Device）Meter5(Device）<--DLT/645--> proctol --+
%%---------------------------------------------------------------------------------------------------------------------
-module(dgiot_meter_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_meter.hrl").
-define(TYPE, <<"METER">>).
-define(MAX_BUFF_SIZE, 1024).
-define(SECS, [5, 5 * 60]).
-define(JIAOSHI, 60 * 10 * 1000).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{

    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"METER采集通道"/utf8>>
    },
    description => #{
        zh => <<"METER采集通道"/utf8>>
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
        enum => [
            #{<<"value">> => <<"nosearch">>, <<"label">> => <<"nosearch"/utf8>>},
            #{<<"value">> => <<"quick">>, <<"label">> => <<"quick"/utf8>>},
            #{<<"value">> => <<"normal">>, <<"label">> => <<"normal"/utf8>>}
        ],
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
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/meter.jpg">>,
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
            {ProductId, #{<<"ACL">> := Acl, <<"nodeType">> := 1, <<"thing">> := Thing}} ->
                Props = maps:get(<<"properties">>, Thing, []),
                dgiot_data:insert({dtu, ChannelId}, {ProductId, Acl, Props}),
                lists:map(fun(Prop) ->
                    case Prop of
                        #{<<"identifier">> := Identifier, <<"dataSource">> := #{<<"da">> := Da, <<"dt">> := Dt}} ->
                            dgiot_data:insert({protocol, <<Da/binary, Dt/binary>>, ProductId}, Identifier);
                        _ ->
                            pass
                    end
                          end, Props);
            {ProductId, #{<<"ACL">> := Acl, <<"nodeType">> := 3, <<"thing">> := Thing}} ->
                dgiot_data:insert({dtu, ChannelId}, {ProductId, Acl, maps:get(<<"properties">>, Thing, [])});
            {ProductId, #{<<"ACL">> := Acl, <<"thing">> := Thing}} ->
                Props = maps:get(<<"properties">>, Thing, []),
                dgiot_data:insert({meter, ChannelId}, {ProductId, Acl, Props}),
                lists:map(fun(Prop) ->
                    case Prop of
                        #{<<"identifier">> := Identifier, <<"dataSource">> := #{<<"di">> := Di}} ->
                            dgiot_data:insert({protocol, Di, ProductId}, Identifier);
                        #{<<"identifier">> := Identifier, <<"dataSource">> := #{<<"da">> := Da, <<"dt">> := Dt}} ->
                            dgiot_data:insert({protocol, <<Da/binary, Dt/binary>>, ProductId}, Identifier);
                        _ ->
                            pass
                    end
                          end, Props);
            _ ->
                pass
        end
              end, Products),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        search = Search
    },
    {ok, State, dgiot_meter_tcp:start(Port, State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%%%% 通道消息处理,注意：进程池调用
%%%%SELECT username as productid, clientid, connected_at FROM "$events/client_connected" WHERE username = 'bffb6a3a27'
%%handle_event('client.connected', {rule, #{peername := PeerName}, #{<<"clientid">> := DtuAddr, <<"productid">> := ProductId} = _Select}, State) ->
%%    [DTUIP, _] = binary:split(PeerName, <<$:>>, [global, trim]),
%%    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
%%    case dgiot_device:lookup(DeviceId) of
%%        {ok, _V} ->
%%            dgiot_device:put(#{<<"objectId">> => DeviceId});
%%        _ ->
%%            dgiot_meter:create_dtu(mqtt, DtuAddr, ProductId, DTUIP)
%%    end,
%%    {ok, State};

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error, "EventId ~p Event ~p", [EventId, Event]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
