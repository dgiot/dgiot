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
-module(dgiot_modbusrtu_tcp).
-author("stoneliu").
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(MAX_BUFF_SIZE, 1024).

-export([
    get_deviceid/2,
    start/2
]).

%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

%% =======================
%% {ok, State} | {stop, Reason}
%%init(TCPState) ->
%%    erlang:send_after(5 * 1000, self(), login),.
%%    {ok, TCPState}.

init(#tcp{state = #state{id = ChannelId}} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, _ProductIds} ->
            {ok, TCPState};
        {error, not_find} ->
            {stop, not_find_channel}
    end.


%% 9C A5 25 CD 00 DB
%% 11 04 02 06 92 FA FE
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, head = Head, len = Len, product = ProductId, dtutype = Dtutype} = State} = TCPState) ->
    DTUIP = dgiot_utils:get_ip(Socket),
    DtuAddr = dgiot_utils:binary_to_hex(Buff),
    List = dgiot_utils:to_list(DtuAddr),
    List1 = dgiot_utils:to_list(Buff),
    case re:run(DtuAddr, Head, [{capture, first, list}]) of
        {match, [Head]} when length(List) == Len ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            create_device(DeviceId, ProductId, DtuAddr, DTUIP, Dtutype),
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p DTU login DtuAddr:~p", [?FILE, ?LINE, DtuAddr]),
            Topic = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/profile">>,
            dgiot_mqtt:subscribe(Topic),
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}};
        _Error ->
            case re:run(Buff, Head, [{capture, first, list}]) of
                {match, [Head]} when length(List1) == Len ->
                    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Buff),
                    create_device(DeviceId, ProductId, Buff, DTUIP, Dtutype),
                    Topic = <<"$dg/device/", ProductId/binary, "/", Buff/binary, "/profile">>,
                    dgiot_bridge:send_log(ChannelId, ProductId, Buff, "~s ~p DTU login DtuAddr:~p", [?FILE, ?LINE, Buff]),
                    dgiot_mqtt:subscribe(Topic),
                    {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = Buff}}};
                Error1 ->
                    ?LOG(info, "Error1 ~p Buff ~p ", [Error1, dgiot_utils:to_list(Buff)]),
                    {noreply, TCPState#tcp{buff = <<>>}}
            end
    end;

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = #{product := ProductId, pn := Pn, di := Di}, product = DtuProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~p ~s ~p DTU ~p recv ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ?FILE, ?LINE, DtuAddr, dgiot_utils:binary_to_hex(Buff)]),
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Di)),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Pn)),
    case modbus_rtu:parse_frame(Buff, #{}, #{
        <<"dtuproduct">> => ProductId,
        <<"channel">> => ChannelId,
        <<"dtuaddr">> => DtuAddr,
        <<"slaveId">> => Sh * 256 + Sl,
        <<"address">> => H * 256 + L}) of
        {_, Things} ->
            NewTopic = <<"$dg/thing/", DtuProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p to task ~p ~ts ", [?FILE, ?LINE, NewTopic, unicode:characters_to_list(dgiot_json:encode(Things))]),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
            dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things);
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            pass
    end,
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}};

%% 主动上报 Buff = <<"01 03 0000 000C45CF 0103184BC73E373AB53E361BFD3E4100000000000000000000000021AC">>.
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = <<>>, product = DtuProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "~p ~s ~p DTU ~p recv ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ?FILE, ?LINE, DtuAddr, dgiot_utils:binary_to_hex(Buff)]),
    case modbus_rtu:dealwith(Buff) of
        {ok, #{<<"buff">> := NewBuff, <<"slaveId">> := SlaveId, <<"address">> := Address}} ->
            case modbus_rtu:parse_frame(NewBuff, #{}, #{
                <<"dtuproduct">> => DtuProductId,
                <<"channel">> => ChannelId,
                <<"dtuaddr">> => DtuAddr,
                <<"slaveId">> => SlaveId,
                <<"address">> => Address}) of
                {_, Things} ->
                    NewTopic = <<"$dg/thing/", DtuProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
                    dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "~s ~p to task ~p ~ts~n ", [?FILE, ?LINE, NewTopic, unicode:characters_to_list(dgiot_json:encode(Things))]),
                    DeviceId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
                    Taskchannel = dgiot_product_channel:get_taskchannel(DtuProductId),
                    dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things);
                Other ->
                    ?LOG(info, "Other ~p", [Other]),
                    pass
            end;
        _ ->
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
                    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
                    Payloads = modbus_rtu:set_params(ProfilePayload, ProductId, DevAddr),
                    lists:map(fun(X) ->
                        timer:sleep(100),
                        dgiot_tcp_server:send(TCPState, X)
                              end, Payloads),
                    {noreply, TCPState};
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"properties">>] ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"_dgiotTaskFreq">> := Freq, <<"slaveid">> := SlaveId, <<"address">> := Address} = DataSource ->
                            Data = modbus_rtu:to_frame(DataSource),
%%                            io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, dgiot_utils:to_hex(Data)]),
                            dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "Channel sends ~p to DTU ~p", [dgiot_utils:binary_to_hex(Data), DevAddr]),
                            dgiot_tcp_server:send(TCPState, Data),
                            {noreply, TCPState#tcp{state = State#state{hb = Freq, env = #{product => ProductId, pn => SlaveId, di => Address}}}};
                        _ ->
                            {noreply, TCPState}
                    end;
                _Other ->
                    ?LOG(error, "_Other ~p", [_Other]),
                    {noreply, TCPState}
            end;
        false ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"profile">>] ->
                    %% 设置参数
                    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
                    Payloads = modbus_rtu:set_params(ProfilePayload, ProductId, DevAddr),
                    lists:map(fun(X) ->
                        timer:sleep(100),
                        dgiot_tcp_server:send(TCPState, X)
                              end, Payloads),
                    {noreply, TCPState};
                _ ->
                    {noreply, TCPState}
            end
    end;
%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
%%    io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
%%    io:format("~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{state = #state{id = _ChannelId, devaddr = DtuAddr, product = ProductId}} = _TCPState) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_task:del_pnque(DeviceId),
    dgiot_client:stop(Taskchannel, DeviceId),
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

get_deviceid(ProdcutId, DevAddr) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse_id:get_objectid(<<"Device">>, #{<<"product">> => ProdcutId, <<"devaddr">> => DevAddr}),
    DeviceId.

create_device(DeviceId, ProductId, DTUMAC, DTUIP, Dtutype) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType}} ->
            dgiot_device:create_device(#{
                <<"devaddr">> => DTUMAC,
                <<"name">> => <<Dtutype/binary, "_", DTUMAC/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => Dtutype,
                <<"devModel">> => DevType
            }),
            dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC),
            Productname =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := Productname1}} ->
                        Productname1;
                    _ ->
                        <<"">>
                end,
            ?MLOG(info, #{<<"clientid">> => DeviceId, <<"devaddr">> => DTUMAC, <<"productid">> => ProductId, <<"productname">> => Productname, <<"devicename">> => <<Dtutype/binary, DTUMAC/binary>>, <<"status">> => <<"上线"/utf8>>}, ['device_statuslog']),
            {DeviceId, DTUMAC};
        _Error2 ->
%%            ?LOG(info, "Error2 ~p ", [Error2]),
            {<<>>, <<>>}
    end.
