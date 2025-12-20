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
-module(dgiot_modbusxrtu_tcp).
-author("stoneliu").
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_device/include/dgiot_device.hrl").

-export([
    get_deviceid/2,
    start/2
]).

-export([get_header/1]).

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
    io:format("~s ~p Device Connected~n", [?FILE, ?LINE]),
    % io:format("~s ~p ~p~n", [?FILE, ?LINE, TCPState]),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, _ProductIds} ->
            io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, _TYPE, _ProductIds]),
            {ok, TCPState};
        {error, not_find} ->
            io:format("~s ~p not_find_channel ~p~n", [?FILE, ?LINE, ChannelId]),
            {stop, not_find_channel}
    end.

%% 根据dtu头，找到对应的产品
find_product(_DtuHeader, []) -> not_found;
find_product(DtuHeader, [OuterMap | Tail]) ->
    % io:format("~s ~p Checking OuterMap: ~p~n", [?FILE, ?LINE, OuterMap]),
    % 使用 maps:to_list/1 将外部Map转换为键值对列表，然后提取第一个（也是唯一一个）键值对
    case OuterMap of
        {ProductId, _DetailMap} ->
            % io:format("~s ~p ProductId ~p DtuHeader: ~p DetailMap1: ~p~n", [?FILE, ?LINE, ProductId, DtuHeader, DetailMap]),
            case dgiot_product:local(ProductId) of
                {ok, ProductItem} ->
                    io:format("~s ~p ProductItem found ~n", [?FILE, ?LINE]),
                    case ProductItem of
                        #{<<"content">> := #{<<"head">> := TmpHeader}} ->
                            io:format("~s ~p TmpHeader:~p DtuHeader:~p~n", [?FILE, ?LINE, TmpHeader, DtuHeader]),

                            {Header, Len} = get_header(TmpHeader),
                            io:format("~s ~p Header:~p Len:~p~n", [?FILE, ?LINE, Header, Len]),

                            case re:run(DtuHeader, Header, [{capture, first, list}]) of
                                {match, [_DtuAddr]} when byte_size(DtuHeader) =:= Len ->
                                    io:format("~s ~p ~p ~p ~p Match found! Returning OuterMap.~n", [?FILE, ?LINE, TmpHeader, DtuHeader, _DtuAddr]),
                                    ProductItem; % 匹配成功，返回整个ProductItem
                                _ ->
                                    io:format("~s ~p Head not match. Continue searching.~n", [?FILE, ?LINE]),
                                find_product(DtuHeader, Tail) % 不匹配，继续遍历尾部
                            end;
                        _ ->
                            io:format("~s ~p Head not match or structure invalid. Continue searching.~n", [?FILE, ?LINE]),
                            find_product(DtuHeader, Tail) % 不匹配，继续遍历尾部
                    end;
                _ ->
                    io:format("~s ~p ProductId:~p not found~n", [?FILE, ?LINE, ProductId]),
                    find_product(DtuHeader, Tail) % 找不到合适的ProductItem，继续遍历尾部
            end;
        _ ->
            io:format("~s ~p not matched~n", [?FILE, ?LINE]),
            not_found
    end;

find_product(_, _) -> 
    io:format("~s ~p not_found, badarg  ~n", [?FILE, ?LINE]),
    not_found.

%% 9C A5 25 CD 00 DB
%% 11 04 02 06 92 FA FE
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, head = _Head, len = _Len, product = Products, dtutype = Dtutype} = State} = TCPState) ->
    io:format("~s ~p ChannelId: ~p Buff:~p Head:~p Len:~p Dtutype:~p ~n", [?FILE, ?LINE, ChannelId, Buff, _Head, _Len, Dtutype]),
    % io:format("~s ~p TCPState:~p~n", [?FILE, ?LINE, TCPState]),
    DTUIP = dgiot_utils:get_ip(Socket),
    DtuAddr = dgiot_utils:binary_to_hex(Buff),
    io:format("~s ~p DTUIP:~p Buff:~p DtuAddr:~p ~n", [?FILE, ?LINE, DTUIP, Buff, DtuAddr]),

    List = dgiot_utils:to_list(DtuAddr),
    io:format("~s ~p Buff:~p DtuAddr:~p List:~p ~n", [?FILE, ?LINE, Buff, DtuAddr, List]),

    List1 = dgiot_utils:to_list(Buff),
    io:format("~s ~p Buff:~p DtuAddr:~p List:~p List1:~p ~n", [?FILE, ?LINE, Buff, DtuAddr, List, List1]),

    DtuHeader = Buff,
    io:format("~s ~p DtuHeader: ~p DtuAddr: ~p Buff: ~p ~n", [?FILE, ?LINE, DtuHeader, DtuAddr, Buff]),
    ProductItem = find_product(DtuHeader, Products),

    % io:format("~s ~p ProductItem:~p Buff:~p DtuAddr:~p ~n", [?FILE, ?LINE, ProductItem, Buff, DtuAddr]),

    case ProductItem of
        not_found ->
            io:format("~s ~p not_found:~p  ~n", [?FILE, ?LINE, DtuAddr]),
            {noreply, TCPState#tcp{buff = <<>>}};
        #{<<"productId">> := ProductId} ->
            io:format("~s ~p Buff:~p ProductId:~p DtuAddr:~p ~n", [?FILE, ?LINE, Buff, ProductId, DtuAddr]),

            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            case dgiot_device:lookup(DeviceId) of
                {ok, _DeviceItem} ->
                    io:format("~s ~p Device found, DeviceId:~p ~n", [?FILE, ?LINE, DeviceId]),
                    dgiot_task:save_pnque(ProductId, DtuAddr, ProductId, DtuAddr);
                _ ->
                    io:format("~s ~p Device not found, DeviceId:~p ~n", [?FILE, ?LINE, DeviceId]),
                    Icon = maps:get(<<"icon">>, ProductItem, <<"">>),
                    create_device(DeviceId, ProductId, DtuAddr, DTUIP, Dtutype, Icon)
            end,

            dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"online">>),
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p DTU login DtuAddr:~p", [?FILE, ?LINE, DtuAddr]),
            Topic = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/profile">>,
            Topic1 = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/debug">>,
            dgiot_mqtt:subscribe(Topic),
            dgiot_mqtt:subscribe(Topic1),
            io:format("~s ~p Topic:~p Topic1:~p ~n", [?FILE, ?LINE, Topic, Topic1]),
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}}
    end;

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = #{product := ProductId, pn := Pn, di := Di}, product = _Products} = State} = TCPState) ->
    HexBuff = dgiot_utils:binary_to_hex(Buff),
    io:format("~s ~p ChannelId:~p ProductId:~p Buff:~p HexBuff:~p DtuAddr:~p Pn:~p Di:~p ~n", [?FILE, ?LINE, ChannelId, ProductId, Buff, HexBuff, DtuAddr, Pn, Di]),
    % io:format("~s ~p ProductId:~p Products:~p ~n", [?FILE, ?LINE, ProductId, _Products]),
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~p ~s ~p DTU ~p recv ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ?FILE, ?LINE, DtuAddr, HexBuff]),
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Di)),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(Pn)),
    dgiot_device:save_log(ProductId, DtuAddr, dgiot_utils:binary_to_hex(Buff), <<"tcp_receive">>),
    io:format("~s ~p H:~p L:~p Sh:~p Sl:~p ~n", [?FILE, ?LINE, H, L, Sh, Sl]),
    case modbus_rtu:parse_frame(Buff, #{}, #{
        <<"dtuproduct">> => ProductId,
        <<"channel">> => ChannelId,
        <<"dtuaddr">> => DtuAddr,
        <<"slaveId">> => Sh * 256 + Sl,
        <<"address">> => H * 256 + L}) of
        {_, Things} ->
            io:format("~s ~p ~p ~p ~p ~n", [?FILE, ?LINE, ChannelId, ProductId, DtuAddr]),
            timer:sleep(1000),
            NewTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
            io:format("~s ~p Things:~p~n", [?FILE, ?LINE, Things]),
            ThingsStr = unicode:characters_to_list(dgiot_json:encode(Things)),
            io:format("~s ~p ThingsStr:~p~n", [?FILE, ?LINE, ThingsStr]),
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p to task ~p ~ts ", [?FILE, ?LINE, NewTopic, ThingsStr]),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),

            ParentId = dgiot_device_cache:get_parent_id(DeviceId),
            io:format("~s ~p DeviceId:~p ParentId:~p~n", [?FILE, ?LINE, DeviceId, ParentId]),

            begin
                {ok, #{<<"results">> := Results}} = dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"objectId">> => DeviceId}}),
                case Results of
                    [] ->
                        pass;
                    [DeviceItem | _] ->
                        dgiot_data:insert(?DGIOT_DEVICE, DeviceId, DeviceItem)
                end
            end,

            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
            dgiot_device:save_log(ProductId, DtuAddr, Things, <<"reportProperty">>),
            dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things);
        Other ->
            io:format("~s ~p Buff:~p~n", [?FILE, ?LINE, Buff]),
            ?LOG(info, "Other ~p", [Other]),
            pass
    end,
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}};

%% 主动上报 Buff = <<"01 03 0000 000C45CF 0103184BC73E373AB53E361BFD3E4100000000000000000000000021AC">>.
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, env = <<>>, product = Products} = State} = TCPState) ->
    io:format("~s ~p ChannelId:~p Buff:~p DtuAddr:~p ~n", [?FILE, ?LINE, ChannelId, Buff, DtuAddr]),
    % io:format("~s ~p TCPState:~p~n", [?FILE, ?LINE, TCPState]),
    DtuHeader = DtuAddr,
    ProductItem = find_product(DtuHeader, Products),

    case ProductItem of
        not_found ->
            io:format("~s ~p not_found:~p  ~n", [?FILE, ?LINE, DtuHeader]),
            % {noreply, TCPState#tcp{buff = <<>>}};
            pass;
        #{<<"productId">> := ProductId} ->
            io:format("~s ~p Buff:~p ProductId:~p DtuHeader:~p DtuAddr:~p ~n", [?FILE, ?LINE, Buff, ProductId, DtuHeader, DtuAddr]),

            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~p ~s ~p DTU ~p recv ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ?FILE, ?LINE, DtuAddr, dgiot_utils:binary_to_hex(Buff)]),
            dgiot_device:save_log(ProductId, DtuAddr, dgiot_utils:binary_to_hex(Buff), <<"other">>),

            case modbus_rtu:dealwith(Buff) of
                {ok, #{<<"buff">> := NewBuff, <<"slaveId">> := SlaveId, <<"address">> := Address}} ->
                    io:format("~s ~p Buff:~p~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
                    case modbus_rtu:parse_frame(NewBuff, #{}, #{
                        <<"dtuproduct">> => ProductId,
                        <<"channel">> => ChannelId,
                        <<"dtuaddr">> => DtuAddr,
                        <<"slaveId">> => SlaveId,
                        <<"address">> => Address}) of
                        {_, Things} ->
                            NewTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
                            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p to task ~p ~ts~n ", [?FILE, ?LINE, NewTopic, unicode:characters_to_list(dgiot_json:encode(Things))]),
                            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
                            ParentId = dgiot_device_cache:get_parent_id(DeviceId),
                            io:format("~s ~p ParentId:~p~n", [?FILE, ?LINE, ParentId]),
                            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
                            dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things);
                        Other ->
                            ?LOG(info, "Other ~p", [Other]),
                            pass
                    end;
                _ ->
                    io:format("~s ~p Buff:~p~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
                    pass
            end
            % {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, state = State#state{devaddr = DtuHeader, deviceId = DeviceId}}}
    end,

    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}};

handle_info({tcp, Buff}, #tcp{socket = _Socket, state = #state{id = _ChannelId} = _State} = _TCPState) ->
    io:format("~s ~p Buff = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)])
    ;

handle_info({deliver, _, Msg}, #tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    io:format("~s ~p ~n", [?FILE, ?LINE]),
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
                        dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(X), <<"device_operationlog">>),
                        dgiot_tcp_server:send(TCPState, X)
                              end, Payloads),
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
                    lists:map(fun(X) ->
                        timer:sleep(100),
                        dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(X), <<"device_operationlog">>),
                        dgiot_tcp_server:send(TCPState, X)
                              end, Payloads),
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
    % io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
%%    io:format("~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{clientid = DeviceId, state = #state{id = _ChannelId, devaddr = DtuAddr, deviceId = DeviceId2, product = _Products}} = _TCPState) ->
    io:format("~s ~p ChannelId:~p _Reason:~p DtuAddr:~p DeviceId:~p DeviceId2:~p ~n", [?FILE, ?LINE, _ChannelId, _Reason, DtuAddr, DeviceId, DeviceId2]),
    % io:format("~s ~p ~p ~n", [?FILE, ?LINE, _TCPState]),
    % DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    ParentId = dgiot_device_cache:get_parent_id(DeviceId),
    io:format("~s ~p ParentId:~p~n", [?FILE, ?LINE, ParentId]),
    case dgiot_device:get_productid(DeviceId) of
        not_find ->
            io:format("~s ~p not_find~n", [?FILE, ?LINE]),
            dgiot_task:del_pnque(DeviceId),
            ok;
        ProductId ->
            io:format("~s ~p ProductId:~p~n", [?FILE, ?LINE, ProductId]),
            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
            dgiot_task:del_pnque(DeviceId),
            dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"offline">>),
            dgiot_client:stop(Taskchannel, DeviceId)
    end,
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

get_deviceid(ProdcutId, DevAddr) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse_id:get_objectid(<<"Device">>, #{<<"product">> => ProdcutId, <<"devaddr">> => DevAddr}),
    DeviceId.

create_device(DeviceId, ProductId, DTUMAC, DTUIP, Dtutype, Icon) ->
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
                <<"devModel">> => DevType,
                <<"icon">> => Icon
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

get_header(Regular) ->
    lists:foldl(fun(X, {Header, Len}) ->
        % io:format("~s ~p X = ~p Header: ~p, Len: ~p .~n", [?FILE, ?LINE, X, Header, Len]),
        case X of
            "**" -> {Header, Len + length(X)};
            "*" -> {Header, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
    end, {[], 0},
    re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).