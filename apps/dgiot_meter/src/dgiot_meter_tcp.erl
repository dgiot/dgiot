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
-module(dgiot_meter_tcp).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_meter.hrl").
%% API
-export([start/2]).

%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

%% =======================
%% tcp server start
%% {ok, State} | {stop, Reason}
init(TCPState) ->
    dgiot_metrics:inc(dgiot_meter, <<"dtu_login">>, 1),
    {ok, TCPState}.

%%设备登录报文，登陆成功后，开始搜表
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = <<>>, search = Search} = State} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    NewBuff =
        case is_binary(Buff) of
            true -> dgiot_utils:binary_to_hex(Buff);
            false -> Buff
        end,
    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
    dgiot_bridge:send_log(ChannelId, DtuProductId, NewBuff, "(登录) ~p ", [NewBuff]),
    {Protocol, DtuAddr} =
        case Buff of
            <<16#68, _:4/bytes, 16#68, _A1:8/bytes, _Rest/binary>> ->
                {_, [Acc | _]} = dlt376_decoder:parse_frame(Buff, []),  %% NewBuff
                #{<<"msgtype">> := Protocol1, <<"con">> := Con, <<"addr">> := MeterAddr} = Acc,
                Concentrator = maps:get(<<"concentrator">>, Acc, <<16#31, 16#07, 16#5F, 16#81, 16#00>>),
                case Con of
                    1 ->
                        Frame1 = maps:get(<<"frame">>, Acc, <<>>),
                        dgiot_tcp_server:send(TCPState, Frame1);
                    _ -> pass
                end,
                dgiot_meter:create_meter4G(MeterAddr, ChannelId, DTUIP),
                Frame2 = dlt376_decoder:to_frame(#{<<"command">> => 16#4B,    %%读取集中器保存的电表信息，包括测量点号、表地址等
                    <<"concentrator">> => Concentrator,
                    <<"afn">> => ?AFN_READ_PARAM,
                    <<"di">> => <<"00000201080001000200030004000500060007000800">>}),  %%读取测量点号0001~0008的电表信息
                dgiot_tcp_server:send(TCPState, Frame2),
                {Protocol1, MeterAddr};
            _ ->
                {?DLT645, Buff}
        end,
    case Protocol of
        ?DLT376 ->
            {ProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
            Topic = <<"thing/", ProductId/binary, "/", DtuAddr/binary>>,
            dgiot_mqtt:subscribe(Topic),  %为这个集中器订阅一个mqtt
            Topic2 = <<"profile/", ProductId/binary, "/", DtuAddr/binary>>,
            dgiot_mqtt:subscribe(Topic2),
            ?LOG(info, "Topic2 ~p", [Topic2]),
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "from dev ~p (登录)", [dgiot_utils:binary_to_hex(Buff)]),
            {NewRef, NewStep} = {undefined, read_meter},
            DtuId = dgiot_parse:get_deviceid(DtuProductId, DtuAddr),
            case Search of
                <<"nosearch">> ->
                    lists:map(fun(X) ->
                        case X of
                            #{<<"product">> := #{<<"objectId">> := MeterProductid}, <<"devaddr">> := Meteraddr, <<"route">> := Route} ->
                                dgiot_bridge:send_log(ChannelId, MeterProductid, Meteraddr, "save taskque MeterProductid ~p  Meteraddr ~p", [MeterProductid, Meteraddr]),
                                dgiot_data:insert({concentrator, MeterProductid, Meteraddr}, Route),
                                dgiot_task:save_pnque(DtuProductId, DtuAddr, MeterProductid, Meteraddr);
                            _ ->
                                pass
                        end
                              end, dgiot_meter:get_sub_device(DtuAddr));
                _ ->
                    pass
            end,
            dgiot_metrics:inc(dgiot_meter, <<"dtu_online">>, 1),
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DtuId, state = State#state{dtuaddr = DtuAddr, protocol = ?DLT376, ref = NewRef, step = NewStep}}};
        ?DLT645 ->
            dgiot_meter:create_dtu(DtuAddr, ChannelId, DTUIP),
            {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
            Topic = <<"profile/", DtuProductId/binary, "/", DtuAddr/binary>>,
            dgiot_mqtt:subscribe(Topic),
            {NewRef, NewStep} =
                case Search of
                    <<"nosearch">> ->
                        lists:map(fun(X) ->
                            case X of
                                #{<<"product">> := #{<<"objectId">> := MeterProductid}, <<"devaddr">> := Meteraddr} ->
                                    dgiot_bridge:send_log(ChannelId, MeterProductid, Meteraddr, "save taskque Meteraddr ~p", [Meteraddr]),
                                    dgiot_task:save_pnque(DtuProductId, DtuAddr, MeterProductid, Meteraddr);
                                _ ->
                                    pass
                            end
                                  end, dgiot_meter:get_sub_device(DtuAddr)),
                        {undefined, read_meter};
                    <<"quick">> ->
                        dgiot_meter:search_meter(tcp, undefined, TCPState, 0),
                        {undefined, search_meter};
                    _ ->
                        {Ref, Step, _Payload} = dgiot_meter:search_meter(tcp, undefined, TCPState, 1),
                        {Ref, Step}
                end,
            dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "from dev ~p (登录)", [dgiot_utils:binary_to_hex(DtuAddr)]),
            DtuId = dgiot_parse:get_deviceid(DtuProductId, DtuAddr),
            dgiot_metrics:inc(dgiot_meter, <<"dtu_online">>, 1),
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DtuId, state = State#state{dtuaddr = DtuAddr, protocol = ?DLT645, ref = NewRef, step = NewStep}}}
    end;

%%定时器触发搜表
handle_info(search_meter, #tcp{state = #state{ref = Ref, protocol = ?DLT645} = State} = TCPState) ->
    {NewRef, Step, _Payload} = dgiot_meter:search_meter(tcp, Ref, TCPState, 1),
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{ref = NewRef, step = Step}}};

%%ACK报文触发搜表
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = DtuAddr, protocol = ?DLT645, ref = Ref, step = search_meter, search = Search} = State} = TCPState) ->
    dgiot_metrics:inc(dgiot_meter, <<"search_meter">>, 1),
    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
    ?LOG(info, "Buff ~p", [Buff]),
    dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "from dev ~p (搜表成功)", [dgiot_utils:binary_to_hex(Buff)]),
    {Rest, Frames} = dgiot_meter:parse_frame(?DLT645, Buff, []),
    lists:map(fun(X) ->
        case X of
            #{<<"addr">> := Addr} ->
                DTUIP = dgiot_utils:get_ip(Socket),
                dgiot_meter:create_meter(dgiot_utils:binary_to_hex(Addr), ChannelId, DTUIP, DtuAddr);
            Other ->
                ?LOG(info, "Other ~p", [Other]),
                pass %%异常报文丢弃
        end
              end, Frames),
    case Search of
        <<"normal">> ->
            {NewRef, Step, _Payload} = dgiot_meter:search_meter(tcp, Ref, TCPState, 1),
            {noreply, TCPState#tcp{buff = Rest, state = State#state{ref = NewRef, step = Step}}};
        _ ->
            case length(Frames) > 0 of
                true ->
                    {noreply, TCPState#tcp{buff = Rest, state = State#state{ref = undefined, step = read_meter}}};
                false ->
                    {noreply, TCPState#tcp{buff = Rest, state = State#state{ref = undefined, step = search_meter}}}
            end
    end;

handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, protocol = Protocol, step = _Step}} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    case Protocol of
        ?DLT376 ->
            dgiot_bridge:send_log(ChannelId, "from_dev:  ~p ", [dgiot_utils:binary_to_hex(Buff)]),
            {Rest, Frames} = dgiot_meter:parse_frame(?DLT376, Buff, []),
            ?LOG(info, "Frames ~p~n", [Frames]), %[#{<<"addr">> => <<"330100480000">>,<<"di">> => <<0,1,0,0>>,<<"value">> => #{<<"00010000">> => 0}}]
            case Frames of
                [#{<<"con">> := 1, <<"frame">> := Frame} | _] ->
                    dgiot_tcp_server:send(TCPState, Frame);  %%回复确认
                [#{<<"afn">> := 16#0A, <<"di">> := <<16#00, 16#00, 16#02, 16#01>>} | _] ->
                    dlt376_decoder:process_message(Frames, ChannelId, DTUIP);  %%注册或更新电表信
                _ ->
                    dlt376_decoder:process_message(Frames, ChannelId)
            end,
            {noreply, TCPState#tcp{buff = Rest}};
        ?DLT645 ->
            {Rest, Frames} = dgiot_meter:parse_frame(?DLT645, Buff, []),
            dgiot_bridge:send_log(ChannelId, "from_dev: Rest ~p Frames ~p ", [Rest, Frames]),
            dlt645_decoder:process_message(Frames, ChannelId),
            {noreply, TCPState#tcp{buff = Rest}};
        _ ->
            {noreply, TCPState#tcp{buff = <<0, 0, 0, 0, 0, 0, 0, 0>>}}
    end;

handle_info({retry, Concentrator}, TCPState) ->
    Crc1 = dgiot_utils:get_parity(<<16#4B, Concentrator/binary, 16#0C, 16#60, 16#01, 16#01, 16#01, 16#10>>),
    Frame1 = <<16#68, 16#32, 16#00, 16#32, 16#00, 16#68, 16#4B, Concentrator/binary, 16#0C, 16#60, 16#01, 16#01, 16#01, 16#10, Crc1:8, 16#16>>,
    dgiot_tcp_server:send(TCPState, Frame1),          %16#27,16#03,16#00,16#72,16#05,16#53
%%    UserData = <<16#4B,Concentrator/binary,16#10,16#60,16#00,16#00,16#01,16#01,16#02,16#01,16#00,16#00,16#00,16#00,16#00,16#00,16#68,16#27,16#02,16#11,16#07,16#15,16#01,16#00,16#00,16#01,16#00>>,
%%    Crc2 = dgiot_utils:get_parity(UserData),
%%    Frame2 = <<16#68,16#7E,16#00,16#7E,16#00,16#68, UserData/binary,Crc2:8,16#16>>,
%%    dgiot_tcp_server:send(TCPState, Frame2),
    erlang:send_after(20000, self(), {retry, Concentrator}),
    {noreply, TCPState};

%%接受抄表任务命令抄表(下发指令)
handle_info({deliver, _Topic, Msg}, #tcp{state = #state{id = _ChannelId, protocol = Protocol}} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
%%    ?LOG(info, "Payload  ~p ~n~n", [Payload]),
    dgiot_metrics:inc(dgiot_meter, <<"mqtt_revc">>, 1),
    case jsx:is_json(Payload) of
        true ->
%%            Load = binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]),
%%            ?LOG(info, "Load ~p ~n", [Load]),    [<<\"profile\">>,<<\"ecfbf67dd7\">>,<<\"330100480000\">>]
            case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
                [<<"thing">>, ProductId, DevAddr] ->
%%                    #tcp{state = #state{protocol = Protocol}} = TCPState,
                    case Protocol of
                        ?DLT376 ->
                            Route = dgiot_data:get({concentrator, ProductId, DevAddr}),
                            [#{<<"thingdata">> := ThingData} | _] = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                            Payload1 = dgiot_meter:to_frame(ThingData),  %%DLT645协议，需要透传转发
                            ?LOG(info, "Route ~p     DevAddr:~p     Payload1:~p ~n~n", [Route, DevAddr, dgiot_utils:binary_to_hex(Payload1)]),
                            HexPayload = dgiot_utils:binary_to_hex(Payload1),
                            Payload2 = dlt376_decoder:to_frame(#{<<"afn">> => 16#10,
                                <<"command">> => 16#4A,
                                <<"concentrator">> => <<16#01, 16#33, 16#48, 16#00, 16#00>>,
                                <<"di">> => <<"00000100">>,
                                <<"data">> => <<"026B81801000", HexPayload/binary, "00000000000000000000000000000000">>}),
                            ?LOG(info, "Payload2:~p ~n~n", [dgiot_utils:binary_to_hex(Payload2)]),
                            dgiot_tcp_server:send(TCPState, Payload2);
                        ?DLT645 ->
                            [#{<<"thingdata">> := ThingData} | _] = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                            Payload1 = dgiot_meter:to_frame(ThingData),
%%                            io:format("~s ~p DLT645 Payload1 = ~p.~n", [?FILE, ?LINE, Payload1]),
                            dgiot_tcp_server:send(TCPState, Payload1)
                    end;
                [<<"profile">>, _ProductId, DevAddr] ->   %%[<<"profile">>,<<"ecfbf67dd7">>,<<"330100480000">>
%%                            io:format("~s ~p ProData ~p  Protocol ~p~n", [?FILE, ?LINE, Payload, Protocol]),
                    case Protocol of
                        ?DLT376 ->
                            Payload2 = dlt376_decoder:frame_write_param(#{<<"concentrator">> => DevAddr, <<"payload">> => jsx:decode(Payload)}),
                            ?LOG(info, "Payload2:~p ~n~n", [dgiot_utils:binary_to_hex(Payload2)]),
                            dgiot_tcp_server:send(TCPState, Payload2);
                        ?DLT645 ->
                            Payload1 = dlt645_decoder:frame_write_param(#{<<"meter">> => DevAddr, <<"payload">> => jsx:decode(Payload)}),
                            ?LOG(info, "DLT645 Payload1 :~p ~n~n", [dgiot_utils:binary_to_hex(Payload1)]),
                            dgiot_tcp_server:send(TCPState, Payload1)
                    end;
                [<<"thingctrl">>, _ProductId, _DevAddr] ->
                    #tcp{state = #state{protocol = Protocol}} = TCPState,
                    case Protocol of
                        ?DLT376 ->
                            [#{<<"thingdata">> := ThingData} | _] = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                            case ThingData of
                                % 远程控制电表拉闸、合闸
                                #{<<"devaddr">> := DevAddr, <<"ctrlflag">> := CtrlFlag, <<"devpass">> := DevPass, <<"apiname">> := <<"get_meter_ctrl">>} ->
                                    ThingData1 = #{<<"devaddr">> => DevAddr, <<"ctrlflag">> => CtrlFlag, <<"devpass">> => DevPass, <<"protocol">> => Protocol, <<"apiname">> => get_meter_ctrl},
                                    Payload1 = dgiot_meter:to_frame(ThingData1),
                                    dgiot_tcp_server:send(TCPState, Payload1);
                                % 获取上次拉闸、合闸的时间（一次发送两条查询指令）
                                #{<<"devaddr">> := DevAddr, <<"ctrlflag">> := CtrlFlag, <<"apiname">> := <<"get_meter_ctrl_status">>} ->
                                    % 上次合闸时间
                                    ThingData1 = #{<<"devaddr">> => DevAddr, <<"ctrlflag">> => CtrlFlag, <<"protocol">> => Protocol, <<"apiname">> => get_meter_ctrl_status},
                                    Payload1 = dgiot_meter:to_frame(ThingData1),
                                    dgiot_tcp_server:send(TCPState, Payload1);
                                _ ->
                                    pass
                            end;
                        ?DLT645 ->
                            [#{<<"thingdata">> := ThingData} | _] = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                            case ThingData of
                                % 远程控制电表拉闸、合闸
                                #{<<"devaddr">> := DevAddr, <<"ctrlflag">> := CtrlFlag, <<"devpass">> := DevPass, <<"apiname">> := <<"get_meter_ctrl">>} ->
                                    % ?LOG(info, "GGM 212 dgiot_meter_tcp, handle_info9 ~p,~p,~p,~p",[DevAddr,CtrlFlag,DevPass,Protocol]),
                                    ThingData1 = #{<<"devaddr">> => DevAddr, <<"ctrlflag">> => CtrlFlag, <<"devpass">> => DevPass, <<"protocol">> => Protocol, <<"apiname">> => get_meter_ctrl},
                                    Payload1 = dgiot_meter:to_frame(ThingData1),
                                    dgiot_tcp_server:send(TCPState, Payload1);
                                % 获取上次拉闸、合闸的时间（一次发送两条查询指令）
                                #{<<"devaddr">> := DevAddr, <<"ctrlflag">> := CtrlFlag, <<"apiname">> := <<"get_meter_ctrl_status">>} ->
                                    ThingData1 = #{<<"devaddr">> => DevAddr, <<"ctrlflag">> => CtrlFlag, <<"protocol">> => Protocol, <<"apiname">> => get_meter_ctrl_status},
                                    Payload1 = dgiot_meter:to_frame(ThingData1),
                                    dgiot_tcp_server:send(TCPState, Payload1);
                                _ ->
                                    pass
                            end
                    end;
                _ ->
                    pass
            end;
        false -> pass
    end,
    {noreply, TCPState};




%%handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = <<>>, search = Search} = State} = TCPState) ->
%%    DTUIP = dgiot_utils:get_ip(Socket),
%%    NewBuff =
%%        case is_binary(Buff) of
%%            true -> dgiot_utils:binary_to_hex(Buff);
%%            false -> Buff
%%        end,
%%    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
%%    dgiot_bridge:send_log(ChannelId, DtuProductId, NewBuff, "(登录) ~p ", [NewBuff]),
%%    {Protocol, DtuAddr, NewRef, NewStep} = frame(Buff, DTUIP, DtuProductId, TCPState),
%%    DtuId = dgiot_parse:get_deviceid(DtuProductId, DtuAddr),
%%    {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DtuId,
%%        state = State#state{dtuaddr = DtuAddr, protocol = Protocol, ref = NewRef, step = NewStep}}};


%% 异常报文丢弃
%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(Info, TCPState) ->
    ?LOG(info, "Meter Rev Buff========ERROR========= dgiot_meter_tcp, handle_info, ~p,~p~n~n~n ", [Info, TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    dgiot_metrics:dec(dgiot_meter, <<"dtu_online">>, 1),
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.


%%frame(Buff, DTUIP, DtuProductId, #tcp{state = #state{id = ChannelId, dtuaddr = <<>>, search = Search}} = TCPState) ->
%%    {Protocol, DtuAddr} =
%%        case Buff of
%%            <<16#68, _:4/bytes, 16#68, _A1:8/bytes, _Rest/binary>> ->
%%                {_, [Acc | _]} = dlt376_decoder:parse_frame(Buff, []),  %% NewBuff
%%                #{<<"msgtype">> := Protocol1, <<"con">> := Con, <<"addr">> := MeterAddr} = Acc,
%%                Concentrator = maps:get(<<"concentrator">>, Acc, <<16#31,16#07,16#5F,16#81,16#00>>),
%%                case Con of
%%                    1 ->
%%                        Frame1 = maps:get(<<"frame">>, Acc, <<>>),
%%                        dgiot_tcp_server:send(TCPState, Frame1),
%%                        erlang:send_after(10000, self(), {retry, Concentrator});
%%                    _ -> pass
%%                end,
%%                Frame2 = dlt376_decoder:to_frame(#{<<"command">> => <<16#4B>>,    %%读取集中器保存的电表信息，包括测量点号、表地址等
%%                    <<"concentrator">> => Concentrator,
%%                    <<"afn">> => ?AFN_READ_PARAM,
%%                    <<"di">> => <<"00000201">>}),
%%                dgiot_tcp_server:send(TCPState, Frame2),
%%                dgiot_meter:create_meter4G(MeterAddr, ChannelId, DTUIP),
%%                {Protocol1, MeterAddr};
%%            _ ->
%%                {?DLT645, Buff}
%%        end,
%%    case Protocol of
%%        ?DLT376 ->
%%            {ProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
%%            Topic = <<"thing/", ProductId/binary, "/", DtuAddr/binary>>,
%%            dgiot_mqtt:subscribe(Topic),  %为这个设备订阅一个mqtt
%%            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "from dev ~p (登录)", [dgiot_utils:binary_to_hex(Buff)]),
%%            {NewRef, NewStep} = {undefined, read_meter},
%%            DtuId = dgiot_parse:get_deviceid(DtuProductId, DtuAddr),
%%            case Search of
%%                <<"nosearch">> ->
%%                    lists:map(fun(X) ->
%%                        case X of
%%                            #{<<"product">> := #{<<"objectId">> := MeterProductid}, <<"devaddr">> := Meteraddr,<<"route">> := Route} ->
%%                                dgiot_bridge:send_log(ChannelId, MeterProductid, Meteraddr, "save taskque MeterProductid ~p  Meteraddr ~p", [MeterProductid, Meteraddr]),
%%                                dgiot_data:insert({concentrator, MeterProductid, Meteraddr}, Route),
%%                                dgiot_task:save_pnque(DtuProductId, DtuAddr, MeterProductid, Meteraddr);
%%                            _ ->
%%                                pass
%%                        end
%%                              end, dgiot_meter:get_sub_device(DtuAddr));
%%                _ ->
%%                    pass
%%            end,
%%            dgiot_metrics:inc(dgiot_meter, <<"dtu_online">>, 1),
%%            {Protocol, DtuAddr, NewRef, NewStep};
%%        ?DLT645 ->
%%            dgiot_meter:create_dtu(DtuAddr, ChannelId, DTUIP),
%%            {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
%%            Topic = <<"profile/", DtuProductId/binary, "/", DtuAddr/binary>>,
%%            dgiot_mqtt:subscribe(Topic),
%%            {NewRef, NewStep} =
%%                case Search of
%%                    <<"nosearch">> ->
%%                        lists:map(fun(X) ->
%%                            case X of
%%                                #{<<"product">> := #{<<"objectId">> := MeterProductid}, <<"devaddr">> := Meteraddr} ->
%%                                    dgiot_bridge:send_log(ChannelId, MeterProductid, Meteraddr, "save taskque Meteraddr ~p", [Meteraddr]),
%%                                    dgiot_task:save_pnque(DtuProductId, DtuAddr, MeterProductid, Meteraddr);
%%                                _ ->
%%                                    pass
%%                            end
%%                                  end, dgiot_meter:get_sub_device(DtuAddr)),
%%                        {undefined, read_meter};
%%                    <<"quick">> ->
%%                        dgiot_meter:search_meter(tcp, undefined, TCPState, 0),
%%                        {undefined, search_meter};
%%                    _ ->
%%                        {Ref, Step, _Payload} = dgiot_meter:search_meter(tcp, undefined, TCPState, 1),
%%                        {Ref, Step}
%%                end,
%%            dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "from dev ~p (登录)", [dgiot_utils:binary_to_hex(DtuAddr)]),
%%            DtuId = dgiot_parse:get_deviceid(DtuProductId, DtuAddr),
%%            dgiot_metrics:inc(dgiot_meter, <<"dtu_online">>, 1),
%%            {Protocol, DtuAddr, NewRef, NewStep}
%%    end.
