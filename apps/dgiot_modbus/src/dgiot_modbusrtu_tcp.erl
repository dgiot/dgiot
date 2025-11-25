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
-include_lib("dgiot_device/include/dgiot_device.hrl").

-define(MAX_BUFF_SIZE, 1024).

-export([
    start/2
]).

%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).
-export([send_aggregated_device_report/5]).

start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

%% =======================
%% {ok, State} | {stop, Reason}
%%init(TCPState) ->
%%    erlang:send_after(5 * 1000, self(), login),.
%%    {ok, TCPState}.

init(#tcp{socket = Socket, state = #state{id = ChannelId, dtutype = Dtutype} = State} = TCPState) ->
    DtuAddr = dgiot_utils:get_ip(Socket),
    %io:format("~s ~p  DtuAddr: ~p   ~n", [?FILE, ?LINE, DtuAddr]),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, [ProductId | _ProductIds]} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            case dgiot_device:lookup(DeviceId) of
                {ok, _DeviceItem} ->
                    dgiot_modbus:register_client(ChannelId, ProductId, DtuAddr, DtuAddr, Dtutype),
                    {ok, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}};
                _ ->
                    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
                        {ok, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                            dgiot_modbus:register_client(ChannelId, ProductId, DtuAddr, DtuAddr, Dtutype),
                            {ok, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}};
                        _ ->
                            {ok, TCPState}
                    end
            end;
        {error, not_find} ->
           % io:format("~s ~p not_find_channel ~p~n", [?FILE, ?LINE, ChannelId]),
            {stop, not_find_channel}
    end.

%% 9C A5 25 CD 00 DB
%% 11 04 02 06 92 FA FE
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, head = Head, len = Len, product = ProductId, dtutype = Dtutype} = State} = TCPState) ->
    DtuIp = dgiot_utils:get_ip(Socket),
    DtuAddr = dgiot_utils:binary_to_hex(Buff),
    List = dgiot_utils:to_list(DtuAddr),
    List1 = dgiot_utils:to_list(Buff),
    % io:format("~s ~p Buff:~p DtuAddr:~p Dtutype:~p ~n", [?FILE, ?LINE, Buff, DtuAddr, Dtutype]),
    case re:run(DtuAddr, Head, [{capture, first, list}]) of
        {match, [Head]} when length(List) == Len ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            dgiot_modbus:register_client(ChannelId, ProductId, Buff, DtuIp, Dtutype),
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}};
        _Error ->
            case re:run(Buff, Head, [{capture, first, list}]) of
                {match, [Head]} when length(List1) == Len ->
                    dgiot_bridge:send_log(ChannelId, ProductId, Buff, "~s ~p DTU login DtuAddr:~p", [?FILE, ?LINE, Buff]),
                    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Buff),
                    dgiot_modbus:register_client(ChannelId, ProductId, Buff, DtuIp, Dtutype),
                    {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = Buff}}};
                Error1 ->
                    % io:format("~s ~p Error1:~p Buff:~p~n", [?FILE, ?LINE, Error1, dgiot_utils:to_list(Buff)]),
                    ?LOG(info, "Error1 ~p Buff ~p ", [Error1, dgiot_utils:to_list(Buff)]),
                    {noreply, TCPState#tcp{buff = <<>>}}
            end
    end;
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = #{product := ProductId, pn := Pn, di := Di}, product = DtuProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~p ~s ~p DTU ~p recv ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ?FILE, ?LINE, DtuAddr, dgiot_utils:binary_to_hex(Buff)]),
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Di)),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Pn)),
    dgiot_device:save_log(ProductId, DtuAddr, dgiot_utils:binary_to_hex(Buff), <<"tcp_receive">>),
    case modbus_rtu:parse_frame(Buff, #{}, #{<<"dtuproduct">> => ProductId, <<"channel">> => ChannelId,
        <<"dtuaddr">> => DtuAddr,<<"slaveId">> => Sh * 256 + Sl,<<"address">> => H * 256 + L})  of
        {_, Things} ->
            timer:sleep(1000),
            % 使用封装的函数发送聚合设备报告
            send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, DtuProductId);
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            pass
    end,
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}};
%% 主动上报 Buff = <<"01 03 0000 000C45CF 0103184BC73E373AB53E361BFD3E4100000000000000000000000021AC">>.
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = <<>>, product = DtuProductId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "~p ~s ~p DTU ~p recv ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ?FILE, ?LINE, DtuAddr, dgiot_utils:binary_to_hex(Buff)]),
    dgiot_device:save_log(DtuProductId, DtuAddr, dgiot_utils:binary_to_hex(Buff), <<"other">>),
    case modbus_rtu:dealwith(Buff) of
        {ok, #{<<"buff">> := NewBuff, <<"slaveId">> := SlaveId, <<"address">> := Address}} ->
            case
                modbus_rtu:parse_frame(NewBuff, #{}, #{
                    <<"dtuproduct">> => DtuProductId,
                    <<"channel">> => ChannelId,
                    <<"dtuaddr">> => DtuAddr,
                    <<"slaveId">> => SlaveId,
                    <<"address">> => Address
                })
            of
                {_, Things} ->
                    NewTopic = <<"$dg/thing/", DtuProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
                    dgiot_bridge:send_log(ChannelId, DtuProductId, DtuAddr, "~s ~p to task ~p ~ts~n ", [?FILE, ?LINE, NewTopic, unicode:characters_to_list(dgiot_json:encode(Things))]),
                    DeviceId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
                    % ParentId = dgiot_device:get_parent_id(DeviceId),
                    % io:format("~s ~p ParentId:~p~n", [?FILE, ?LINE, ParentId]),
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
                    lists:map(
                        fun(X) ->
                            timer:sleep(100),
                            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(X), <<"device_operationlog">>),
                            dgiot_tcp_server:send(TCPState, X)
                        end,
                        Payloads
                    ),
                    {noreply, TCPState};
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"properties">>] ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"_dgiotTaskFreq">> := Freq, <<"slaveid">> := SlaveId, <<"address">> := Address} = DataSource ->
                            Data = modbus_rtu:to_frame(DataSource),
                            %%                            io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, dgiot_utils:to_hex(Data)]),
                            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Data), <<"readProperty">>),
                            dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "Channel sends ~p to DTU ~p", [dgiot_utils:binary_to_hex(Data), DevAddr]),
                            dgiot_tcp_server:send(TCPState, Data),
                            {noreply, TCPState#tcp{state = State#state{hb = Freq, env = #{product => ProductId, pn => SlaveId, di => Address}}}};
                        _ ->
                            {noreply, TCPState}
                    end;
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"debug">>] ->
                    %% 设备调试
                    dgiot_tcp_server:send(TCPState, Payload),
                    dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload), <<"device_debug">>),
                    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "Channel device_debug ~p to DTU ~p", [dgiot_utils:binary_to_hex(Payload), DevAddr]),
                    {noreply, TCPState};
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
                    lists:map(
                        fun(X) ->
                            timer:sleep(100),
                            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(X), <<"device_operationlog">>),
                            dgiot_tcp_server:send(TCPState, X)
                        end,
                        Payloads
                    ),
                    {noreply, TCPState};
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"debug">>] ->
                    %% 设备调试
                    dgiot_tcp_server:send(TCPState, Payload),
                    dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Payload), <<"device_debug">>),
                    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "Channel device_debug ~p to DTU ~p", [dgiot_utils:binary_to_hex(Payload), DevAddr]),
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
    % ParentId = dgiot_device:get_parent_id(DeviceId),
    % io:format("~s ~p ParentId:~p~n", [?FILE, ?LINE, ParentId]),
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_task:del_pnque(DeviceId),
    dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"offline">>),
    dgiot_client:stop(Taskchannel, DeviceId),
    ok;
terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

%% 发送聚合设备报告消息，支持父设备消息汇聚
send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, _) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    
    % 获取父设备信息
    ParentInfo = dgiot_device_cache:get_parent_info(DeviceId),
    ParentId = maps:get(deviceid, ParentInfo, <<"">>),
    ParentProductId = maps:get(productid, ParentInfo, <<"">>),
    ParentDevAddr = maps:get(devaddr, ParentInfo, <<"">>),
    
    % 发送子设备消息
    ChildTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p to task ~p ~ts ", [?FILE, ?LINE, ChildTopic, unicode:characters_to_list(dgiot_json:encode(Things))]),
    dgiot_device:save_log(ProductId, DtuAddr, Things, <<"reportProperty">>),
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_client:send(Taskchannel, DeviceId, ChildTopic, Things),
    
    % 如果父设备存在，发送父设备消息
    case ParentId of
        <<"">> -> ok;
        _ ->
            ParentTopic = <<"$dg/thing/", ParentProductId/binary, "/", ParentDevAddr/binary, "/properties/report">>,
            dgiot_bridge:send_log(ChannelId, ParentProductId, ParentDevAddr, "~s ~p to parent task ~p ~ts ", [?FILE, ?LINE, ParentTopic, unicode:characters_to_list(dgiot_json:encode(Things))]),
            dgiot_device:save_log(ParentProductId, ParentDevAddr, Things, <<"reportProperty">>),
            ParentTaskchannel = dgiot_product_channel:get_taskchannel(ParentProductId),
            dgiot_client:send(ParentTaskchannel, ParentId, ParentTopic, Things)
    end,
    ok.
