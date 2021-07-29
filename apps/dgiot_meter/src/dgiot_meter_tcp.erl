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
handle_info({tcp, DtuAddr}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = <<>>, search = Search} = State} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    HexDtuAddr = dgiot_utils:binary_to_hex(DtuAddr),
    dgiot_meter:create_dtu(HexDtuAddr, ChannelId, DTUIP),
    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
    {NewRef, NewStep} =
        case Search of
            <<"nosearch">> ->
                [dgiot_task:save_pnque(DtuProductId, DtuAddr, Meterproductid, Meteraddr) || #{<<"product">> := Meterproductid, <<"devaddr">> := Meteraddr}
                    <- dgiot_meter:get_sub_device(DtuAddr)],
                {undefined, read_meter};
            <<"quick">> ->
                dgiot_meter:search_meter(tcp, undefined, TCPState, 0),
                {undefined, search_meter};
            _ ->
                {Ref, Step, _Payload} = dgiot_meter:search_meter(tcp, undefined, TCPState, 1),
                {Ref, Step}
        end,
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{dtuaddr = HexDtuAddr, ref = NewRef, step = NewStep}}};


%%定时器触发搜表
handle_info(search_meter, #tcp{state = #state{ref = Ref} = State} = TCPState) ->
    {NewRef, Step, _Payload} = dgiot_meter:search_meter(tcp, Ref, TCPState, 1),
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{ref = NewRef, step = Step}}};

%%ACK报文触发搜表
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = DtuAddr, ref = Ref, step = search_meter, search = Search} = State} = TCPState) ->
    ?LOG(info, "from_dev: search_meter Buff ~p", [dgiot_utils:binary_to_hex(Buff)]),
    ?LOG(info, "from_dev: parse_frame Buff ~p", [dgiot_meter:parse_frame(dlt645, Buff, [])]),
    {Rest, Frames} = dgiot_meter:parse_frame(dlt645, Buff, []),
    lists:map(fun(X) ->
        case X of
            #{<<"addr">> := Addr} ->
                ?LOG(info, "from_dev: search_meter Addr ~p", [Addr]),
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

%%接受抄表任务命令抄表
handle_info({deliver, _Topic, Msg}, #tcp{state = #state{id = ChannelId, step = read_meter}} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    dgiot_bridge:send_log(ChannelId, "Topic ~p Msg  ~p", [dgiot_mqtt:get_topic(Msg), Payload]),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
                [<<"thing">>, _ProductId, _DevAddr] ->
                    [#{<<"thingdata">> := ThingData} | _] = jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
                    Payload = dgiot_meter:to_frame(ThingData),
                    dgiot_bridge:send_log(ChannelId, "from_task: ~ts:  ~ts ", [_Topic, unicode:characters_to_list(dgiot_mqtt:get_payload(Msg))]),
                    ?LOG(info, "task->dev: Payload ~p", [dgiot_utils:binary_to_hex(Payload)]),
                    dgiot_tcp_server:send(TCPState, Payload);
                [<<"profile">>, _ProductId, _DtuAddr] ->
                    case Payload of
                        #{<<"_dgiotprotocol">> := <<"hex">>} ->
                            maps:fold(fun(_K, V, Acc) ->
                                dgiot_tcp_server:send(TCPState, dgiot_utils:hex_to_binary(V)),
                                Acc
                                      end, #{}, maps:without([<<"_dgiotprotocol">>], Payload));
                        _ ->
                            pass
                    end;
                _ ->
                    pass
            end;
        false -> pass
    end,
    {noreply, TCPState};

%% 接收抄表任务的ACK报文
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, step = read_meter}} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "from_dev:  ~p ", [dgiot_utils:binary_to_hex(Buff)]),
    ?LOG(info, "Buff ~p", [dgiot_utils:binary_to_hex(Buff)]),
    {Rest, Frames} = dgiot_meter:parse_frame(dlt645, Buff, []),
    case Frames of
        [#{<<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
                    dgiot_mqtt:publish(DevAddr, Topic, jsx:encode(Value));
                _ -> pass
            end;
        _ -> pass
    end,
    {noreply, TCPState#tcp{buff = Rest}};

%% 异常报文丢弃
%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    dgiot_metrics:dec(dgiot_meter, <<"dtu_login">>, 1),
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.
