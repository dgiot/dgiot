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
-module(dgiot_modbusc_tcp).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
%% API
-export([init/1, handle_info/2, terminate/2]).
-export([start_connect/1]).
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot.hrl").
-define(MAX_BUFF_SIZE, 10 * 1024).
-include_lib("dgiot/include/logger.hrl").

start_connect(_Opts =
    #{
        <<"auto_reconnect">> := Recon,
        <<"reconnect_times">> := ReTimes,
        <<"port">> := Port,
        <<"ip">> := Ip,
        <<"productid">> := ProductId,
        <<"channelid">> := ChannelId,
        <<"hb">> := HB
    }) ->
    State = #state{
        product = ProductId,
        id = ChannelId,
        hb = HB
    },
%%    dgiot_tcp_client:start_link(dgiot_modbusc_tcp, "192.168.1.5", 8989, 10, 3, #{productid => <<"c38905f64d">>,devaddr => <<"0622e8ca1355">>,hb => 60}).
    dgiot_tcp_client:start_link(?MODULE, Ip, Port, Recon, ReTimes, State).

init(TCPState) ->
    {ok, TCPState}.

handle_info(connection_ready, TCPState) ->
    io:format("47 ~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), login),
%%    dgiot_tcp_client:send(TCPState, <<"D3F100000009011000040001020021">>),
    {noreply, TCPState};

handle_info(tcp_closed, TCPState) ->
    {noreply, TCPState};

handle_info(login, #tcp{state = #state{hb = Hb}} = TCPState) ->
    io:format("~s ~p Hb = ~p.~n", [?FILE, ?LINE, Hb]),
    erlang:send_after(Hb * 1000, self(), save_pnque),
    {noreply, TCPState};

handle_info(save_pnque, #tcp{state = #state{product = ProductId} = State} = TCPState) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"product">> => ProductId}, <<"limit">> => 1}) of
        {ok, #{<<"results">> := []}} ->
            {noreply, TCPState};
        {ok, #{<<"results">> := [#{<<"devaddr">> := Devaddr}]}} ->
            dgiot_task:save_pnque(ProductId, Devaddr, ProductId, Devaddr),
            {noreply, TCPState#tcp{buff = <<>>, state = State#state{product = ProductId, devaddr = Devaddr}}};
        _ ->
            {noreply, TCPState}
    end;

handle_info(heartbeat, #tcp{state = #state{hb = Hb} = _State} = TCPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    io:format("~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    dgiot_tcp_client:send(TCPState, <<"heartbeat">>),
    {noreply, TCPState};

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = #{product := ProductId, pn := Pn, di := Di}, product = DtuProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "[DtuAddr:~p] returns [~p] to Channel", [DtuAddr, dgiot_utils:binary_to_hex(Buff)]),
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_tcp:is16(Di)),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_tcp:is16(Pn)),
    case modbus_tcp:parse_frame(Buff, #{}, #{
        <<"dtuproduct">> => ProductId,
        <<"channel">> => ChannelId,
        <<"dtuaddr">> => DtuAddr,
        <<"slaveId">> => Sh * 256 + Sl,
        <<"address">> => H * 256 + L}) of
        {_, Things} ->
%%            ?LOG(info, "Things ~p", [Things]),
            NewTopic = <<"thing/", DtuProductId/binary, "/", DtuAddr/binary, "/post">>,
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "Channel sends [~p] to [task:~p]", [jsx:encode(Things), NewTopic]),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            dgiot_mqtt:publish(DeviceId, NewTopic, jsx:encode(Things));
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            pass
    end,
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}};

handle_info({deliver, _, Msg}, #tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"profile">>] ->
%%                    设置参数
                    Payloads = modbus_tcp:set_params(jsx:decode(Payload), ProductId, DevAddr),
                    lists:map(fun(X) ->
                        dgiot_tcp_server:send(TCPState, X)
                              end, Payloads),
                    {noreply, TCPState};
                [<<"thing">>, ProductId, DevAddr] ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        [#{<<"thingdata">> := ThingData} | _] ->
                            case ThingData of
                                #{<<"command">> := <<"r">>,
                                    <<"product">> := ProductId,
                                    <<"protocol">> := <<"MODBUSTCP">>,
                                    <<"dataSource">> := #{
                                        <<"slaveid">> := SlaveId,
                                        <<"address">> := Address} = DataSource
                                } ->
                                    Data = modbus_tcp:to_frame(DataSource#{<<"productid">> => ProductId}),
                                    % io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Data)]),
                                    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "Channel sends [~p] to [DtuAddr:~p]", [dgiot_utils:binary_to_hex(Data), DevAddr]),
                                    dgiot_tcp_server:send(TCPState, Data),
                                    {noreply, TCPState#tcp{state = State#state{env = #{product => ProductId, pn => SlaveId, di => Address}}}};
                                #{<<"command">> := <<"rw">>,
                                    <<"product">> := ProductId,
                                    <<"protocol">> := <<"MODBUSTCP">>,
                                    <<"dataSource">> := #{
                                        <<"slaveid">> := SlaveId,
                                        <<"address">> := Address} = DataSource
                                } ->
                                    Datas = modbus_tcp:to_frame(DataSource#{<<"productid">> => ProductId}),
                                    lists:map(fun(X) ->
                                        dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "Channel sends [~p] to [DtuAddr:~p]", [dgiot_utils:binary_to_hex(X), DevAddr]),
                                        dgiot_tcp_server:send(TCPState, X),
                                        timer:sleep(1000)
                                              end, Datas),
                                    {noreply, TCPState#tcp{state = State#state{env = #{product => ProductId, pn => SlaveId, di => Address}}}};
                                _Ot ->
                                    io:format("~s ~p _Ot = ~p.~n", [?FILE, ?LINE, _Ot]),
                                    {noreply, TCPState}
                            end;
                        _a ->
                            io:format("~s ~p _Ot = ~p.~n", [?FILE, ?LINE, _a]),
                            {noreply, TCPState}
                    end;
                _Other ->
                    io:format("~s ~p _Other = ~p.~n", [?FILE, ?LINE, _Other]),
                    {noreply, TCPState}
            end;
        false ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"profile">>] ->
                    %% 设置参数
                    Payloads = modbus_tcp:set_params(jsx:decode(Payload), ProductId, DevAddr),
                    lists:map(fun(X) ->
                        dgiot_tcp_server:send(TCPState, X)
                              end, Payloads),
                    {noreply, TCPState};
                _ ->
                    {noreply, TCPState}
            end
    end;

handle_info(_Info, TCPState) ->
    io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
    io:format("~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

