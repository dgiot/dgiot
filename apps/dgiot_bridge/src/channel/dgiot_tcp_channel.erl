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
%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

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
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/tcp%E5%9B%BE%E6%A0%87.jpeg">>,
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
    {ok, State, dgiot_tcp_server:child_spec(?MODULE, Port, State)}.

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

%% =======================
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            NewTcpState = TCPState#tcp{
                log = log_fun(ChannelId)
            },
            do_product(init, [ChannelId], NewTcpState#tcp{
                state = State#state{
                    env = #{},
                    product = ProductIds
                }
            });
        {error, not_find} ->
            {stop, not_find_channel}
    end.

handle_info({deliver, _, Msg}, TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"thing">>, ProductId, DevAddr, <<"tcp">>, <<"hex">>] ->
            DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
            dgiot_device:save_log(DeviceId, Payload, ['tcp_send']),
            dgiot_tcp_server:send(TCPState, dgiot_utils:hex_to_binary(dgiot_utils:trim_string(Payload))),
            {noreply, TCPState};
        _ ->
            case jsx:is_json(Payload) of
                true ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"cmd">> := <<"send">>} = Cmd ->
                            handle_info(Cmd, TCPState);
                        Info ->
                            handle_info({mqtt, Topic, Info}, TCPState)
                    end;
                false ->
                    handle_info({mqtt, Topic, Payload}, TCPState)
            end
    end;

%% 对于TCP转一道，向下发的命令
handle_info(#{<<"cmd">> := <<"send">>} = Cmd, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    case do_product(to_frame, [maps:without([<<"cmd">>], Cmd)], TCPState) of
        {ok, NewTCPState} ->
            {noreply, NewTCPState};
        {reply, ProductId, Payload, NewTCPState} ->
            case dgiot_tcp_server:send(TCPState, Payload) of
                ok ->
                    ok;
                {error, Reason} ->
                    dgiot_bridge:send_log(ChannelId, ProductId, "Send Fail, ~p, CMD:~p", [Cmd, Reason])
            end,
            {noreply, NewTCPState}
    end;

handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, head = Head, len = Len, product = Products, dtutype = Dtutype} = State} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    case check_login(Head, Len, Buff) of
        <<>> ->
            {noreply, TCPState#tcp{buff = <<>>}};
        DtuAddr ->
            NewProductId = get_productid(ChannelId, Products, Head, Dtutype),
            DeviceId = dgiot_parse:get_deviceid(NewProductId, DtuAddr),
            case create_device(DeviceId, NewProductId, DtuAddr, DTUIP, Dtutype) of
                {<<>>, <<>>} ->
                    {noreply, TCPState#tcp{buff = <<>>}};
                {_, _} ->
                    NewProducts = dgiot_utils:unique_1(Products ++ [NewProductId]),
                    dgiot_bridge:send_log(ChannelId, NewProductId, DtuAddr, "DeviceId ~p DTU revice from  ~p", [DeviceId, DtuAddr]),
                    {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId,
                        state = State#state{devaddr = DtuAddr, product = NewProducts, deviceId = DeviceId}}}
            end
    end;

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, product = Products, deviceId = DeviceId}} = TCPState) ->
    dgiot_device:save_log(DeviceId, dgiot_utils:binary_to_hex(Buff), ['tcp_receive']),
    case decode(Buff, Products, TCPState) of
        {ok, [], NewTCPState} ->
            {noreply, NewTCPState#tcp{buff = <<>>}};
        {ok, Frames, #tcp{state = #state{product = ProductId}} = NewTCPState} ->
            dgiot_bridge:send_log(ChannelId, ProductId, "~s", [jsx:encode(Frames)]),
%%            Module:do(ChannelId, ProductId, Frames),
            handle_frames(Frames, NewTCPState),
            {noreply, NewTCPState#tcp{buff = <<>>}};
        {stop, _Reason} ->
            {noreply, TCPState#tcp{buff = <<>>}}
    end;

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(Info, TCPState) ->
    case do_product(handle_info, [Info], TCPState) of
        {ok, NewTCPState} ->
            {noreply, NewTCPState};
        {stop, Reason, NewTCPState} ->
            {stop, Reason, NewTCPState}
    end.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.


%% =======================

log_fun(ChannelId) ->
    fun(Type, Buff) ->
        Data =
            case Type of
                <<"ERROR">> -> Buff;
                _ -> <<<<Y>> || <<X:4>> <= Buff, Y <- integer_to_list(X, 16)>>
            end,
        dgiot_bridge:send_log(ChannelId, "~s", [<<Type/binary, " ", Data/binary>>])
    end.

send_fun(TCPState) ->
    fun(Payload) ->
        dgiot_tcp_server:send(TCPState, Payload)
    end.

%% 如果是多个产品，先用报文依次解析，只要有一个能解开，则认为此连接为此产品的，
%% 否则一直解析下去，如果一个产品都没有解析成功，则缓存到下一次解析，直到缓存
%% 如果超过了BuffSize，TCP断开，
decode(Payload, [], _TCPState) when byte_size(Payload) > ?MAX_BUFF_SIZE ->
    {stop, buff_size_limit};
decode(Payload, [], TCPState) ->
    {ok, [], TCPState#tcp{buff = Payload}};
decode(Payload, [ProductId | Products], #tcp{state = #state{env = Env} = State} = TCPState) ->
    case dgiot_bridge:parse_frame(ProductId, Payload, Env) of
        {error, function_not_exported} ->
            decode(Payload, Products, TCPState);
        {error, Reason} ->
            {stop, Reason};
        {Rest, Messages, NewEnv} when length(Messages) > 0 ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{env = NewEnv, product = ProductId}}};
        {Rest, Messages} when length(Messages) > 0 ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{product = ProductId}}};
        _ ->
            decode(Payload, Products, TCPState)
    end;

decode(Payload, ProductId, #tcp{state = #state{env = Env} = State} = TCPState) ->
    case dgiot_bridge:parse_frame(ProductId, Payload, Env) of
        {error, Reason} ->
            {stop, Reason};
        {Rest, Messages} ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{product = ProductId}}};
        {Rest, Messages, NewEnv} ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{env = NewEnv, product = ProductId}}}
    end.

do_product(Fun, Args, #tcp{state = #state{product = ProductIds, id = ChannelId, env = Env}} = TCPState) ->
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, Fun, Args, Env#{<<"send">> => send_fun(TCPState)}) of
        {ok, NewEnv} ->
            {ok, update_state(NewEnv, TCPState)};
        {stop, Reason, NewEnv} ->
            {stop, Reason, update_state(NewEnv, TCPState)};
        {reply, ProductId, Reply, NewEnv} ->
            {reply, ProductId, Reply, update_state(NewEnv, TCPState)}
    end.

handle_frames([], TCPState) ->
    {noreply, TCPState};
handle_frames([Frame | Frames], TCPState) ->
    case do_product(handle_info, [{message, Frame}], TCPState) of
        {ok, NewTCPState} ->
            handle_frames(Frames, NewTCPState);
        {stop, Reason, NewTCPState} ->
            {stop, Reason, NewTCPState}
    end.

update_state(Env, #tcp{state = State} = TCPState) ->
    TCPState#tcp{state = State#state{env = maps:without([<<"send">>], Env)}}.

create_device(DeviceId, ProductId, DTUMAC, DTUIP, Dtutype) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType}} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"devaddr">> := _GWAddr}} ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ip">> => DTUIP, <<"status">> => <<"ONLINE">>}),
                    dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC),
                    create_instruct(Acl, ProductId, DeviceId);
                _ ->
                    dgiot_device:create_device(#{
                        <<"devaddr">> => DTUMAC,
                        <<"name">> => <<Dtutype/binary, DTUMAC/binary>>,
                        <<"ip">> => DTUIP,
                        <<"isEnable">> => true,
                        <<"product">> => ProductId,
                        <<"ACL">> => Acl,
                        <<"status">> => <<"ONLINE">>,
                        <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441},
                        <<"brand">> => Dtutype,
                        <<"devModel">> => DevType
                    }),
                    dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC),
                    create_instruct(Acl, ProductId, DeviceId)
            end,
            Productname =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := Productname1}} ->
                        Productname1;
                    _ ->
                        <<"">>
                end,
            ?MLOG(info, #{<<"deviceid">> => DeviceId, <<"devaddr">> => DTUMAC, <<"productid">> => ProductId, <<"productname">> => Productname, <<"devicename">> => <<Dtutype/binary, DTUMAC/binary>>}, ['online']),
            dgiot_device:sub_topic(DeviceId, <<"tcp/hex">>),
            dgiot_device:save_log(DeviceId, dgiot_utils:binary_to_hex(DTUMAC), ['tcp_receive']),
            {DeviceId, DTUMAC};
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
                        dgiot_instruct:create(DtuProductId, DtuDevId, Pn, ACL, <<"all">>, #{<<"properties">> => [Y]});
                    _ -> pass
                end
                      end, Properties);
        _ -> pass
    end.


get_header(Regular) ->
    lists:foldl(fun(X, {Header, Len}) ->
        case X of
            "**" -> {Header, Len + length(X)};
            "*" -> {Header, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
                end, {[], 0},
        re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).

get_productid(ChannelId, Products, Head, Dtutype) ->
    NewProductId = dgiot_parse:get_productid(<<"DGIOTHUB"/utf8>>, dgiot_utils:to_binary(Head), dgiot_utils:to_binary(Dtutype)),
    case lists:member(NewProductId, Products) of
        false ->
            {ok, Acl} = dgiot_bridge:get_acl(ChannelId),
            case dgiot_parse:get_object(<<"Product">>, NewProductId) of
                {ok, _} ->
                    pass;
                _ ->
                    Product = #{
                        <<"name">> => dgiot_utils:to_binary(Dtutype),
                        <<"devType">> => dgiot_utils:to_binary(Head),
                        <<"category">> => <<"DGIOTHUB"/utf8>>,
                        <<"ACL">> => Acl,
                        <<"netType">> => <<"NB-IOT">>,
                        <<"nodeType">> => 3,
                        <<"config">> => #{},
                        <<"thing">> => #{},
                        <<"productSecret">> => dgiot_utils:random()
                    },
                    dgiot_parse:create_object(<<"Product">>, Product),
                    pass
            end;
        true ->
            pass
    end,
    NewProductId.

check_login(Head, Len, Addr) ->
    HexAddr = dgiot_utils:binary_to_hex(Addr),
    HexList = dgiot_utils:to_list(HexAddr),
    List = dgiot_utils:to_list(Addr),
    case re:run(HexAddr, Head, [{capture, first, list}]) of
        {match, [Head]} when length(HexList) == Len ->
            HexAddr;
        _Error ->
            case re:run(Addr, Head, [{capture, first, list}]) of
                {match, [Head]} when length(List) == Len ->
                    Addr;
                _Error1 ->
                    <<>>
            end
    end.
