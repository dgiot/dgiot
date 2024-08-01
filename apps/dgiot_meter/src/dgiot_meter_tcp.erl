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
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = <<>>, search = Search, len = Len} = State} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
    {Protocol, DtuAddr} =
        case Buff of
            <<16#68, _:4/bytes, 16#68, _A1:8/bytes, _Rest/binary>> ->
                dgiot_bridge:send_log(ChannelId, "~s ~p DLT376 login   ~p ", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
                {_, [Acc | _]} = dlt376_decoder:parse_frame(Buff, []),  %% NewBuff
                #{<<"msgtype">> := Protocol1, <<"con">> := Con, <<"addr">> := MeterAddr} = Acc,
                Concentrator = maps:get(<<"concentrator">>, Acc, <<16#31, 16#07, 16#5F, 16#81, 16#00>>),
                case Con of
                    1 ->
                        Frame1 = maps:get(<<"frame">>, Acc, <<>>),
                        dgiot_bridge:send_log(ChannelId, " ~s ~p DLT376 login response: ~p ", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Frame1)]),
%%                        io:format("~s ~p DLT376 login Buff = ~p.~n", [?FILE, ?LINE, NewBuff]),
%%                        io:format("~s ~p DLT376 response login Buff = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Frame1)]),
                        dgiot_tcp_server:send(TCPState, Frame1);
                    _ -> pass
                end,
                dgiot_meter:create_meter4G(MeterAddr, ChannelId, DTUIP),
                Frame2 = dlt376_decoder:to_frame(#{<<"command">> => 16#4B,    %%读取集中器保存的电表信息，包括测量点号、表地址等
                    <<"addr">> => Concentrator,
                    <<"afn">> => ?AFN_READ_PARAM,
                    <<"di">> => <<"0000020120000100020003000400050006000700080009000A000B000C000D000E000F0010001100120013001400150016001700180019001A001B001C001D001E001F002000">>}),  %%读取测量点号0001~0020的电表信息
%%                io:format("~s ~p DLT376 (集中器搜表) = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Frame2)]),
                dgiot_bridge:send_log(ChannelId, "~s ~p DLT376 (集中器搜表): ~p ", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Frame2)]),
                dgiot_tcp_server:send(TCPState, Frame2),
                {Protocol1, MeterAddr};
            _ ->
                BinBuff = dgiot_utils:binary_to_hex(Buff),
                List = dgiot_utils:to_list(BinBuff),
                List1 = dgiot_utils:to_list(Buff),
                NewBuff =
                    case length(List) of
                        Len ->
                            BinBuff;
                        _ ->
                            case length(List1) of
                                Len ->
                                    Buff;
                                _ ->
                                    <<>>
                            end
                    end,
                dgiot_bridge:send_log(ChannelId, "~s ~p DLT645 dtu login ~p", [?FILE, ?LINE, NewBuff]),
                {?DLT645, NewBuff}
        end,
    case Protocol of
        ?DLT376 ->
            {ProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
            Topic = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/properties">>,
            dgiot_mqtt:subscribe(Topic),  %为这个集中器订阅一个mqtt
            %%                    $dg/device/{productId}/{deviceAddr}/profile
            Topic2 = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/profile">>,
            dgiot_mqtt:subscribe(Topic2),
            {NewRef, NewStep} = {undefined, read_meter},
            DtuId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
            dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"online">>),
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
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DtuId, state = State#state{product = ProductId, dtuaddr = DtuAddr, protocol = ?DLT376, ref = NewRef, step = NewStep}}};
        ?DLT645 ->
            case DtuAddr of
                <<>> ->
                    {noreply, TCPState#tcp{buff = <<>>}};
                _ ->
                    dgiot_meter:create_dtu(DtuAddr, ChannelId, DTUIP),
                    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
                    %%                    $dg/device/{productId}/{deviceAddr}/profile
                    Topic = <<"$dg/device/", DtuProductId/binary, "/", DtuAddr/binary, "/profile">>,
                    dgiot_mqtt:subscribe(Topic),
                    {NewRef, NewStep} =
                        case Search of
                            <<"nosearch">> ->
                                lists:map(fun(X) ->
                                    case X of
                                        #{<<"product">> := #{<<"objectId">> := MeterProductid}, <<"devaddr">> := Meteraddr} ->
                                            dgiot_bridge:send_log(ChannelId, MeterProductid, Meteraddr, "save taskque Meteraddr ~p", [Meteraddr]),
                                            dgiot_task:save_pnque(DtuProductId, DtuAddr, MeterProductid, Meteraddr),
                                            ProfileTopic = <<"$dg/device/", MeterProductid/binary, "/", Meteraddr/binary, "/profile">>,
                                            dgiot_mqtt:subscribe(ProfileTopic);
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
                    DtuId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
                    dgiot_metrics:inc(dgiot_meter, <<"dtu_online">>, 1),
                    {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DtuId, state = State#state{product = DtuProductId, dtuaddr = DtuAddr, protocol = ?DLT645, ref = NewRef, step = NewStep}}}
            end
    end;

%%定时器触发搜表
handle_info(search_meter, #tcp{state = #state{ref = Ref, protocol = ?DLT645} = State} = TCPState) ->
    {NewRef, Step, _Payload} = dgiot_meter:search_meter(tcp, Ref, TCPState, 1),
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{ref = NewRef, step = Step}}};

%%ACK报文触发搜表
handle_info({tcp, Buff}, #tcp{socket = Socket, clientid = DtuId, state = #state{id = ChannelId, dtuaddr = DtuAddr, protocol = ?DLT645, ref = Ref, step = search_meter, search = Search} = State} = TCPState) ->
%%    io:format("~s ~p tcp Buff = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
    dgiot_metrics:inc(dgiot_meter, <<"search_meter">>, 1),
    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
    {Rest, Frames} = dgiot_meter:parse_frame(<<"DLT645">>, Buff, []),
    lists:map(fun(X) ->
        case X of
            #{<<"addr">> := Addr} ->
                DTUIP = dgiot_utils:get_ip(Socket),
                dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "~s ~p from dtu:~p (搜表成功) Buff:~p => MeterAddr:~p", [?FILE, ?LINE, DtuAddr, dgiot_utils:binary_to_hex(Buff), dgiot_utils:binary_to_hex(Addr)]),
                dgiot_meter:create_meter(dgiot_utils:binary_to_hex(Addr), ChannelId, DTUIP, DtuId, DtuAddr);
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


handle_info({tcp, Buff}, #tcp{socket = Socket, clientid = DtuId, state = #state{id = ChannelId, protocol = Protocol, step = _Step}} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    case Protocol of
        ?DLT376 ->
            dgiot_bridge:send_log(ChannelId, "~s ~p DLT376 from_dev: ~p ", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
            {Rest, Frames} = dgiot_meter:parse_frame(?DLT376, Buff, []),
            case Frames of
                [#{<<"con">> := 1, <<"frame">> := Frame} | _] ->
                    dgiot_bridge:send_log(ChannelId, "~s ~p DLT376 response: ~p ", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Frame)]),
                    dgiot_tcp_server:send(TCPState, Frame);  %%回复确认
                [#{<<"afn">> := 16#0A, <<"di">> := <<16#00, 16#00, 16#02, 16#01>>} | _] ->
                    dlt376_decoder:process_message(Frames, ChannelId, DTUIP, DtuId);  %%注册或更新电表信
                _ ->
                    case dlt376_decoder:process_message(?DLT376, Frames, ChannelId) of
                        % 拉闸，返回
                        #{<<"productid">> := ProductId, <<"addr">> := Addr, <<"di">> := Di, <<"value">> := Value} ->
                            DevAddr = dgiot_utils:binary_to_hex(Addr),
                            dgiot_meter:send_mqtt(ProductId, DevAddr, Di, Value);
                        % 抄表数据返回
                        #{<<"productid">> := ProductId, <<"addr">> := DevAddr, <<"value">> := Value, <<"childvalues">> := ChildValues} ->
                            Topic = dgiot_meter:send_task(ProductId, DevAddr, DtuId, Value),
                            dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "~s ~p Dtu to task ~p ~ts ", [?FILE, ?LINE, Topic, unicode:characters_to_list(dgiot_json:encode(Value))]),
                            timer:sleep(1 * 1000),
                            lists:foldl(fun(#{<<"productid">> := ChildProductId, <<"addr">> := ChildDevAddr, <<"value">> := ChildValue}, _Acc) ->
                                dgiot_device:save_log(ChildProductId, ChildDevAddr, ChildValue, <<"reportProperty">>),
                                ChildTopic = dgiot_meter:send_task(ChildProductId, ChildDevAddr, DtuId, ChildValue),
                                dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "~s ~p DtuChild to task ~p ~ts ", [?FILE, ?LINE, ChildTopic, unicode:characters_to_list(dgiot_json:encode(ChildValue))]),
                                timer:sleep(1 * 1000)
                                        end, #{}, ChildValues);
                        % 抄表数据返回
                        #{<<"productid">> := ProductId, <<"addr">> := Addr, <<"value">> := Value} ->
                            DevAddr = dgiot_utils:binary_to_hex(Addr),
                            dgiot_device:save_log(ProductId, DevAddr, Value, <<"reportProperty">>),
                            Topic = dgiot_meter:send_task(ProductId, DevAddr, DtuId, Value),
                            dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "~s ~p Dtu to task ~p ~ts ", [?FILE, ?LINE, Topic, unicode:characters_to_list(dgiot_json:encode(Value))]);
                        Error ->
                            dgiot_bridge:send_log(ChannelId, "~s ~p process_message error ~ts ", [?FILE, ?LINE, unicode:characters_to_list(dgiot_json:encode(Error))]),
                            pass
                    end
            end,
            {noreply, TCPState#tcp{buff = Rest}};
        ?DLT645 ->
            dgiot_bridge:send_log(ChannelId, "~s ~p DLT645 dev recv ~p ", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
%%            io:format("~s ~p Buff = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
            {Rest, Frames} = dgiot_meter:parse_frame(?DLT645, Buff, []),
            case dlt645_decoder:process_message(Frames, ChannelId) of
                % 拉闸，返回
                #{<<"productid">> := ProductId, <<"addr">> := Addr, <<"di">> := Di, <<"value">> := Value} ->
                    DevAddr = dgiot_utils:binary_to_hex(Addr),
                    dgiot_meter:send_mqtt(ProductId, DevAddr, Di, Value);
                % 抄表数据返回
                #{<<"productid">> := ProductId, <<"addr">> := Addr, <<"value">> := Value} ->
                    DevAddr = dgiot_utils:binary_to_hex(Addr),
                    dgiot_device:save_log(ProductId, DevAddr, Value, <<"reportProperty">>),
                    Topic = dgiot_meter:send_task(ProductId, DevAddr, DtuId, Value),
                    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "~s ~p to task ~p ~ts ", [?FILE, ?LINE, Topic, unicode:characters_to_list(dgiot_json:encode(Value))]);
                Error ->
                    dgiot_bridge:send_log(ChannelId, "~s ~p process_message error ~ts ", [?FILE, ?LINE, unicode:characters_to_list(dgiot_json:encode(Error))]),
                    pass
            end,
            {noreply, TCPState#tcp{buff = Rest}};
        _ ->
            {noreply, TCPState#tcp{buff = <<0, 0, 0, 0, 0, 0, 0, 0>>}}
    end;

handle_info({retry, Concentrator}, TCPState) ->
    Crc1 = dgiot_utils:get_parity(<<16#4B, Concentrator/binary, 16#0C, 16#60, 16#01, 16#01, 16#01, 16#10>>),
    Frame1 = <<16#68, 16#32, 16#00, 16#32, 16#00, 16#68, 16#4B, Concentrator/binary, 16#0C, 16#60, 16#01, 16#01, 16#01, 16#10, Crc1:8, 16#16>>,
    dgiot_tcp_server:send(TCPState, Frame1),
    erlang:send_after(20000, self(), {retry, Concentrator}),
    {noreply, TCPState};

%%接受抄表任务命令抄表(下发指令)
handle_info({deliver, _Topic, Msg}, #tcp{state = #state{id = ChannelId, protocol = Protocol} = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    dgiot_metrics:inc(dgiot_meter, <<"mqtt_revc">>, 1),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"properties">>] ->
                    case Protocol of
                        ?DLT376 ->
                            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
                            case dgiot_data:get({metertda, DeviceId}) of
                                not_find ->
                                    DataSource = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                                    Payload1 = dgiot_meter:to_frame(#{<<"devaddr">> => DevAddr, <<"dataSource">> => DataSource}),
                                    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, " ~s ~p DLT376 send to DevAddr ~p => ~p", [?FILE, ?LINE, DevAddr, dgiot_utils:binary_to_hex(Payload1)]),
                                    dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload1), <<"readProperty">>),
                                    dgiot_tcp_server:send(TCPState, Payload1);
                                {Da, Dtuaddr} ->
                                    DA = dgiot_utils:binary_to_hex(dlt376_decoder:pn_to_da(dgiot_utils:to_int(Da))),
                                    DataSource = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                                    Payload1 = dgiot_meter:to_frame(#{<<"protocol">> => Protocol, <<"devaddr">> => Dtuaddr, <<"dataSource">> => DataSource#{<<"da">> => DA}}),
                                    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, " ~s ~p DLT376 send to DevAddr ~p => ~p", [?FILE, ?LINE, DevAddr, dgiot_utils:binary_to_hex(Payload1)]),
                                    dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload1), <<"readProperty">>),
                                    dgiot_tcp_server:send(TCPState, Payload1)
                            end;
                        ?DLT645 ->
                            DataSource = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                            ThingData = #{
                                <<"devaddr">> => DevAddr,
                                <<"protocol">> => ?DLT645,
                                <<"dataSource">> => DataSource
                            },
                            Payload1 = dgiot_meter:to_frame(ThingData),
                            dgiot_bridge:send_log(ChannelId, "~s ~p DLT645 to dev => ~p", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Payload1)]),
                            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload1), <<"readProperty">>),
                            dgiot_tcp_server:send(TCPState, Payload1)
                    end,
                    {noreply, TCPState};
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"profile">>] ->
                    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
                    case Protocol of
                        ?DLT376 ->
                            Payload2 = dlt376_decoder:frame_write_param(#{<<"concentrator">> => DevAddr, <<"payload">> => ProfilePayload}),
                            dgiot_bridge:send_log(ChannelId, " ~s ~p DLT376(下发) send to DevAddr ~p => ~p", [?FILE, ?LINE, DevAddr, dgiot_utils:binary_to_hex(Payload2)]),
                            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload2), <<"device_operationlog">>),
                            dgiot_tcp_server:send(TCPState, Payload2);
                        ?DLT645 ->
%%                            dgiot_umeng:send_message_to3D(ProductId, DevAddr, ProfilePayload),
                            Payload1 = dlt645_decoder:frame_write_param(#{<<"meter">> => DevAddr, <<"payload">> => ProfilePayload}),
                            ?LOG(info, "DLT645 Payload1 :~p ~n~n", [dgiot_utils:binary_to_hex(Payload1)]),
                            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload1), <<"device_operationlog">>),
                            dgiot_tcp_server:send(TCPState, Payload1)
                    end,
                    {noreply, TCPState#tcp{state = State#state{env = #{product => ProductId, devaddr => DevAddr}}}};
                _ ->
                    {noreply, TCPState}
            end;
        false -> {noreply, TCPState}
    end;


%% 异常报文丢弃
%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
%%    io:format("~s ~p Error Info = ~p.~n", [?FILE, ?LINE, Info]),
%%    io:format("~s ~p Error TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{state = #state{dtuaddr = DtuAddr, product = ProductId}} = _TCPState) ->
    dgiot_metrics:dec(dgiot_meter, <<"dtu_online">>, 1),
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_task:del_pnque(DeviceId),
    dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"unregister">>),
    dgiot_client:stop(Taskchannel, DeviceId),
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.
