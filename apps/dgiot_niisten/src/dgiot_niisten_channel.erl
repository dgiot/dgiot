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

-module(dgiot_niisten_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include("dgiot_niisten.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"NIISTEN">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {
    id,
    devaddr = <<>>,
    heartcount = 0,
    regtype = <<>>,
    head = "xxxxxx0eee",
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    scale = 10,
    temperature = 0,
    env = <<>>
}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

%% 注册通道类型
-channel_type(#{

    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"NIISTEN通道"/utf8>>
    },
    description => #{
        zh => <<"NIISTEN通道"/utf8>>
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
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/niisten.jpg">>,
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

%%http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/niisten.jpg

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
    {ok, State, dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error,"EventId ~p Event ~p",[EventId, Event] ),
    {ok, State}.


% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := DevAddr, connected_at := _ConnectedAt}, #{peername := PeerName} = _Context}, State) ->
    ?LOG(error,"DevAddr ~p PeerName ~p",[DevAddr,PeerName] ),
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

%% =======================
%% {ok, State} | {stop, Reason}
%%init(TCPState) ->
%%    erlang:send_after(5 * 1000, self(), login),
%%    {ok, TCPState}.

init(#tcp{state = #state{id = ChannelId}} = TCPState) ->
    ?LOG(info, "ChannelId ~p", [ChannelId]),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, _ProductIds} ->
            {ok, TCPState};
        {error, not_find} ->
            {stop, not_find_channel}
    end.

%% 9C A5 25 CD 00 DB
%% 11 04 02 06 92 FA FE
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, head = Head, len = Len, product = ProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "DTU revice from  ~p", [dgiot_utils:binary_to_hex(Buff)]),
    DTUIP = dgiot_utils:get_ip(Socket),
    DtuAddr = dgiot_utils:binary_to_hex(Buff),
    List = dgiot_utils:to_list(DtuAddr),
    List1 = dgiot_utils:to_list(Buff),
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DtuAddr}),
    case re:run(DtuAddr, Head, [{capture, first, list}]) of
        {match, [Head]} when length(List) == Len ->
            {DevId, Devaddr} =
                case create_device(DeviceId, ProductId, DtuAddr, DTUIP) of
                    {<<>>, <<>>} ->
                        {<<>>, <<>>};
                    {DevId1, Devaddr1} ->
                        {DevId1, Devaddr1}
                end,
            {noreply, TCPState#tcp{buff = <<>>, state = State#state{devaddr = Devaddr, deviceId = DevId}}};
        _Error ->
            case re:run(Buff, Head, [{capture, first, list}]) of
                {match, [Head]} when length(List1) == Len ->
                    create_device(DeviceId, ProductId, Buff, DTUIP),
                    {noreply, TCPState#tcp{buff = <<>>, state = State#state{devaddr = Buff}}};
                Error1 ->
                    ?LOG(info, "Error1 ~p Buff ~p ", [Error1, dgiot_utils:to_list(Buff)]),
                    {noreply, TCPState#tcp{buff = <<>>}}
            end
    end;

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = _DtuAddr, env = #{product := _ProductId, pn := _Pn, di := _Di}, product = _DtuProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "revice from  ~p", [dgiot_utils:binary_to_hex(Buff)]),
%%    case modbus_rtu:parse_frame(Buff, #{}, #{
%%        <<"dtuproduct">> => ProductId,
%%        <<"channel">> => ChannelId,
%%        <<"dtuaddr">> => DtuAddr,
%%        <<"slaveId">> => dgiot_utils:to_int(Pn),
%%        <<"address">> => Di}) of
%%        {_, Things} ->
%%            NewTopic = <<"thing/", DtuProductId/binary, "/", DtuAddr/binary, "/post">>,
%%            dgiot_bridge:send_log(ChannelId, "end to_task: ~p: ~p ~n", [NewTopic, jsx:encode(Things)]),
%%            dgiot_mqtt:publish(DtuAddr, NewTopic, jsx:encode(Things));
%%        Other ->
%%            ?LOG(info, "Other ~p", [Other]),
%%            pass
%%    end,
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}};

handle_info({deliver, Topic, Msg}, #tcp{state = #state{id = ChannelId, product = DtuProductId} = _State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    dgiot_bridge:send_log(ChannelId, "begin from_task: ~ts: ~ts ", [unicode:characters_to_list(Topic), unicode:characters_to_list(Payload)]),
    case jsx:is_json(Payload) of
        true ->
            jsx:decode(Payload, [{labels, binary}, return_maps]),
            case binary:split(Topic, <<$/>>, [global, trim]) of
                %%接收task采集指令
                [<<"thing">>, DtuProductId, _DtuAddr] ->
                    {noreply, TCPState};
                %%接收task汇聚过来的整个dtu物模型采集的数据
                _ ->
                    {noreply, TCPState}
            end;
        false ->
            {noreply, TCPState}
    end;

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    ?LOG(info, "TCPState ~p", [TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

get_header(Regular) ->
    lists:foldl(fun(X, {Header, Len}) ->
        case X of
            "**" -> {Header, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
                end, {[], 0},
        re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).


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

create_device(DeviceId, ProductId, DTUMAC, DTUIP) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType}} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"devaddr">> := _GWAddr}} ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ip">> => DTUIP, <<"status">> => <<"ONLINE">>}),
                    dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC),
                    create_instruct(Acl, ProductId, DeviceId),
                    {DeviceId, DTUMAC};
                _ ->
                    dgiot_device:create_device(#{
                        <<"devaddr">> => DTUMAC,
                        <<"name">> => <<"USRDTU", DTUMAC/binary>>,
                        <<"ip">> => DTUIP,
                        <<"isEnable">> => true,
                        <<"product">> => ProductId,
                        <<"ACL">> => Acl,
                        <<"status">> => <<"ONLINE">>,
                        <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441},
                        <<"brand">> => <<"USRDTU">>,
                        <<"devModel">> => DevType
                    }),
                    dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC),
                    create_instruct(Acl, ProductId, DeviceId),
                    {DeviceId, DTUMAC}
            end;
        Error2 ->
            ?LOG(info, "Error2 ~p ", [Error2]),
            {<<>>, <<>>}
    end.

create_instruct(ACL, DtuProductId, DtuDevId) ->
    case dgiot_product:lookup_prod(DtuProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
            lists:map(fun(Y) ->
                case Y of
                    #{<<"dataForm">> := #{<<"slaveid">> := 256}} ->   %%不做指令
                        pass;
                    #{<<"dataForm">> := #{<<"slaveid">> := SlaveId}} ->
                        Pn = dgiot_utils:to_binary(SlaveId),
%%                        ?LOG(info,"DtuProductId ~p DtuDevId ~p Pn ~p ACL ~p", [DtuProductId, DtuDevId, Pn, ACL]),
%%                        ?LOG(info,"Y ~p", [Y]),
                        dgiot_instruct:create(DtuProductId, DtuDevId, Pn, ACL, <<"all">>, #{<<"properties">> => [Y]});
                    _ -> pass
                end
                      end, Properties);
        _ -> pass
    end.
