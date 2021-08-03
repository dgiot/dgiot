%%%-------------------------------------------------------------------
%%% @author liuxiaodong
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 6月 2021 11:02
%%%-------------------------------------------------------------------
-module(dgiot_xinchuangwei_tcp).
-author("liuxiaodong").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include("dgiot_xinchuangwei.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([start/2]).

%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

%% =======================
%% tcp server start
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId}} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, _ProductIds} ->
            {ok, TCPState};
        {error, not_find} ->
            {stop, not_find_channel}
    end.

%% dtu 登录报文
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, dtuproduct = ProductId} = State} = TCPState) when byte_size(Buff) == 15 ->
    dgiot_bridge:send_log(ChannelId, "DTU revice from  ~p", [dgiot_utils:binary_to_hex(Buff)]),
    DTUIP = dgiot_utils:get_ip(Socket),
    DtuAddr = Buff,
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DtuAddr}),
    dgiot_xinchuangwei:create_device(DeviceId, ProductId, DtuAddr, DTUIP),
    Topic = <<"setXinchuangwei/", ProductId/binary, "/", DtuAddr/binary>>,
    dgiot_mqtt:subscribe(Topic),
    erlang:send_after(18 * 1000, self(), readparams),
%%    erlang:send_after(10 * 1000, self(), readstatus),
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{devaddr = DtuAddr}}};

%% 接收 dtu 返回报文
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DevAddr, dtuproduct = DtuProductId, env = Env} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "xinchuangwei revice from  ~p", [dgiot_utils:binary_to_hex(Buff)]),
    case dgiot_xinchuangwei_decoder:parse_frame(Buff, State) of
        {status, Ack} ->
            ?LOG(info, "Ack ~p", [Ack]),
            NewTopic = <<"thing/", DtuProductId/binary, "/", DevAddr/binary, "/post">>,
            dgiot_bridge:send_log(ChannelId, "to_task: ~p: ~p ~n", [NewTopic, jsx:encode(Ack)]),
            dgiot_mqtt:publish(DevAddr, NewTopic, jsx:encode(Ack));
        {error, _} ->
            #{product := ProductId, pn := Pn, di := Di} = Env,
            <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Di)),
            <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Pn)),
            case modbus_rtu:parse_frame(Buff, [], #{
                <<"dtuproduct">> => ProductId,
                <<"channel">> => ChannelId,
                <<"dtuaddr">> => DevAddr,
                <<"slaveId">> => Sh * 256 + Sl,
                <<"address">> => H * 256 + L}) of
                {_, Things} ->
                    ?LOG(info, "Things ~p", [Things]),
                    NewTopic = <<"thing/", DtuProductId/binary, "/", DevAddr/binary, "/post">>,
                    dgiot_bridge:send_log(ChannelId, "end to_task: ~p: ~p ~n", [NewTopic, jsx:encode(Things)]),
                    dgiot_mqtt:publish(DevAddr, NewTopic, jsx:encode(Things));
                Other ->
                    ?LOG(info, "Other ~p", [Other]),
                    pass
            end;
        _ ->
            pass
    end,
    {noreply, TCPState};

handle_info(login, #tcp{state = #state{id = ChannelId, dtuproduct = ProductId}} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "ChannelId ~p ProductId ~p", [ChannelId, ProductId]),
    {noreply, TCPState#tcp{buff = <<>>}};

%% 一次读取所有状态
%% 55 03 00 13 6B
handle_info(readstatus, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    Heartbeat =
        case dgiot_data:get({ChannelId, heartbeat}) of
            not_find ->
                <<"5">>;
            Heartbeat1 ->
                Heartbeat1
        end,
    Payload = <<16#55, 16#03, 16#00, 16#13, 16#6B>>,
    dgiot_tcp_server:send(TCPState, Payload),
    dgiot_bridge:send_log(ChannelId, "readstatus to_dtu  ~p", [dgiot_utils:binary_to_hex(Payload)]),
    erlang:send_after(Heartbeat * 1000, self(), readstatus),
    {noreply, TCPState};

%% 一次读取所有参数
%% 55 02 00 29 80
handle_info(readparams, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    Payload = <<16#55, 16#02, 16#00, 16#29, 16#80>>,
    dgiot_tcp_server:send(TCPState, Payload),
    dgiot_bridge:send_log(ChannelId, "readparams to_dtu  ~p", [dgiot_utils:binary_to_hex(Payload)]),
    {noreply, TCPState};

%% 设置参数
handle_info({deliver, _Topic, Msg}, #tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
                [<<"setXinchuangwei">>, _ProductId, _DtuAddr] ->
%%                    设置参数
                    lists:map(fun(X) ->
                        dgiot_tcp_server:send(TCPState, X),
                        timer:sleep(1500)
                              end, Payload),
                    erlang:send_after(1000, self(), readparams),
                    {noreply, TCPState};
                [<<"profile">>, ProductId, _DtuAddr] ->
                    case Payload of
                        #{<<"_dgiotprotocol">> := <<"xinchuangwei">>} ->
                            Payloads = dgiot_xinchuangwei_decoder:set_params(maps:without([<<"_dgiotprotocol">>], Payload), ProductId),
                            lists:map(fun(X) ->
                                dgiot_tcp_server:send(TCPState, X)
                                      end, Payloads),
                            erlang:send_after(1000, self(), readparams);
                        _ ->
                            pass
                    end,
                    {noreply, TCPState};
                [<<"thing">>, _DtuProductId, _DtuAddr] ->
                    [#{<<"thingdata">> := ThingData} | _] = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
%%                    ?LOG(error, "ThingData ~p", [ThingData]),
                    case ThingData of
                        #{<<"command">> := <<"r">>,
                            <<"data">> := Value,
                            <<"di">> := Di,
                            <<"pn">> := SlaveId,
                            <<"product">> := ProductId,
                            <<"protocol">> := <<"modbus">>
                        } ->
                            Datas = modbus_rtu:to_frame(#{
                                <<"addr">> => SlaveId,
                                <<"value">> => Value,
                                <<"productid">> => ProductId,
                                <<"di">> => Di}),
                            lists:map(fun(X) ->
                                dgiot_bridge:send_log(ChannelId, "to_device: ~p ", [dgiot_utils:binary_to_hex(X)]),
                                dgiot_tcp_server:send(TCPState, X)
                                      end, Datas),
                            {noreply, TCPState#tcp{state = State#state{env = #{product => ProductId, pn => SlaveId, di => Di}}}};
                        #{<<"command">> := <<"rw">>,
                            <<"data">> := Value,
                            <<"di">> := Di,
                            <<"pn">> := SlaveId,
                            <<"product">> := ProductId,
                            <<"protocol">> := <<"modbus">>
                        } ->
                            Datas = modbus_rtu:to_frame(#{
                                <<"addr">> => SlaveId,
                                <<"value">> => Value,
                                <<"productid">> => ProductId,
                                <<"di">> => Di}),
                            lists:map(fun(X) ->
                                dgiot_bridge:send_log(ChannelId, "to_device: ~p ", [dgiot_utils:binary_to_hex(X)]),
                                dgiot_tcp_server:send(TCPState, X)
                                      end, Datas),
                            {noreply, TCPState#tcp{state = State#state{env = #{product => ProductId, pn => SlaveId, di => Di}}}};
                        _Ot ->
%%                            task采集指令  状态
                            StatusPayload = <<16#55, 16#03, 16#00, 16#13, 16#6B>>,
                            dgiot_tcp_server:send(TCPState, StatusPayload),
                            dgiot_bridge:send_log(ChannelId, "readstatus to_dtu  ~p", [dgiot_utils:binary_to_hex(StatusPayload)]),
                            {noreply, TCPState}
                    end;
                _Other ->
                    ?LOG(info, "_Other ~p ", [_Other]),
                    {noreply, TCPState}
            end;
        false ->
            case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
                [<<"setXinchuangwei">>, _ProductId, _DtuAddr] ->
%%                    设置参数
                    lists:map(fun(X) ->
                        dgiot_tcp_server:send(TCPState, X),
                        timer:sleep(1000)
                              end, Payload),
                    erlang:send_after(1000, self(), readparams),
                    {noreply, TCPState};
                [<<"profile">>, ProductId, _DtuAddr] ->
                    %%                    设置参数
                    case Payload of
                        #{<<"_dgiotprotocol">> := <<"xinchuangwei">>} ->
                            Payloads = dgiot_xinchuangwei_decoder:set_params(maps:without([<<"_dgiotprotocol">>], Payload), ProductId),
                            lists:map(fun(X) ->
                                dgiot_tcp_server:send(TCPState, X)
                                      end, Payloads),
                            erlang:send_after(1000, self(), readparams);
                        _ ->
                            pass
                    end,
                    {noreply, TCPState};
                _ ->
                    {noreply, TCPState}
            end
    end;

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{state = #state{devaddr = _DevAddr, dtuproduct = _ProductId}} = _TCPState) ->
%%    case DevAddr =/= <<>> of
%%        true ->
%%            #{<<"objectId">> := DeviceId} = dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
%%            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"OFFLINE">>});
%%        false ->
%%            pass
%%    end,
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.


