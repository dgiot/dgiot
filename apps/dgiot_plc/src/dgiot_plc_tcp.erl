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
-module(dgiot_plc_tcp).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
%% API
-export([init/1, handle_info/2, terminate/2, get_dbque/7, save_data/2]).
-include_lib("dgiot/include/dgiot.hrl").
-define(MAX_BUFF_SIZE, 10 * 1024).
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% tcp client  callback
init(#dclient{child = _ChildState} = Dclient) ->
%%    io:format("~s ~p Dclient =~p.~n", [?FILE, ?LINE, Dclient]),
    {ok, Dclient};

init(_Dclient) ->
    {ok, #{}}.

handle_info(connection_ready, #dclient{channel = ChannelId, client = ClientId, child = ChildState} = Dclient) ->
    io:format("~s ~p connection_ready ChannelId = ~p. ClientId = ~p.~n", [?FILE, ?LINE, dgiot_utils:to_binary(ChannelId), ClientId]),
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), request_connection),
    {noreply, Dclient#dclient{channel = dgiot_utils:to_binary(ChannelId), child = ChildState#{<<"devaddr">> => <<>>}}};

handle_info(tcp_closed, #dclient{child = ChildState} = Dclient) ->
    io:format("~s ~p tcp_closed Dclient =~p.~n", [?FILE, ?LINE, Dclient]),
    {noreply, Dclient#dclient{child = ChildState}};

%% COTP申请连接
handle_info(request_connection, #dclient{channel = ChannelId, client = ClientId, child = #{<<"product">> := ProductId, <<"decoder">> := Decoder, <<"s7_type">> := S7_type} = _ChildState} = Dclient) ->
    Data = Decoder:encode(S7_type, request_connection),
    dgiot_tcp_client:send(ChannelId, ClientId, Data),
    dgiot_bridge:send_log(ChannelId, "~p sends request_connection ~p to PLC", [ProductId, dgiot_utils:binary_to_hex(Data)]),
    {noreply, Dclient};

handle_info({tcp, Buff}, #dclient{channel = ChannelId, client = ClientId, child =
#{<<"ip">> := IP, <<"devaddr">> := <<>>, <<"product">> := ProductId, <<"dtuType">> := DtuType, <<"decoder">> := Decoder, <<"s7_type">> := S7_type} = ChildState} = Dclient) ->
%%    io:format("~s ~p ~p Buff = ~p.~n", [?FILE, ?LINE, ProductId, dgiot_utils:binary_to_hex(Buff)]),
    case Decoder:parse_tpkt(Buff, ChildState) of
        {_, [#{<<"type">> := <<"response_connection">>, <<"pdutype">> := 208} = _Frame | _]} ->
%%            申请连接返回 发送确定正式连接
            ConfData = Decoder:encode(S7_type, confirm_connection),
            dgiot_tcp_client:send(ChannelId, ClientId, ConfData),
            dgiot_bridge:send_log(ChannelId, "~p sends confirm_connection ~p to PLC", [ProductId, dgiot_utils:binary_to_hex(ConfData)]),
            {noreply, Dclient};
        {_, [#{<<"param">> := #{<<"function">> := 240}, <<"datalen">> := 0, <<"rosctr">> := 3} = _Frame | _]} ->
%%            确定正式连接返回
            Data = Decoder:encode(S7_type, read_plc_sn),
            dgiot_tcp_client:send(ChannelId, ClientId, Data),
            dgiot_bridge:send_log(ChannelId, "~p sends read_plc_sn ~p to PLC", [ProductId, dgiot_utils:binary_to_hex(Data)]),
            {noreply, Dclient};
        {_, [#{<<"data">> := #{<<"sn">> := SN}, <<"errorcode">> := 0, <<"param">> := #{<<"function">> := 4}} = _Frame | _] = _FArg} ->
%%            读取SN返回
            create_device(ProductId, SN, DtuType, IP),
            Topic = <<"$dg/device/", ProductId/binary, "/", SN/binary, "/profile">>,
            dgiot_mqtt:subscribe(Topic),
            dgiot_bridge:send_log(ChannelId, "recv ~p SN ~p", [ProductId, SN]),
            erlang:send_after(1 * 1000, self(), read_block),
            {noreply, Dclient#dclient{child = ChildState#{<<"devaddr">> => SN}}};
        {_, [#{<<"data">> := #{<<"cputype">> := Cputype}, <<"param">> := #{<<"errorcode">> := 0}} = _Frame | _]} ->
%%            读取cputype返回
            create_device(ProductId, Cputype, DtuType, IP),
            Topic = <<"$dg/device/", ProductId/binary, "/", Cputype/binary, "/profile">>,
            dgiot_mqtt:subscribe(Topic),
            dgiot_bridge:send_log(ChannelId, "recv ~p plc type ~p", [ProductId, Cputype]),
            erlang:send_after(1 * 1000, self(), read_block),
            {noreply, Dclient#dclient{child = ChildState#{<<"devaddr">> => Cputype}}};
        R ->
            io:format("~s ~p R = ~p.~n", [?FILE, ?LINE, R]),
            {noreply, Dclient}
    end;

handle_info(read_block, #dclient{channel = ChannelId, child = #{<<"product">> := ProductId, <<"devaddr">> := Devaddr, <<"freq">> := Freq} = ChildState} = Dclient) ->
    dgiot_data:insert({check_connection, ChannelId, ProductId}, dgiot_datetime:now_secs()),
    DbqueMap =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(fun
                                (#{<<"dataSource">> := #{<<"address">> := Address, <<"originaltype">> := Originaltype}}, Acc) ->
                                    case s7_protocol:analysis_address(Address, 1) of
                                        #{<<"block_type">> := Block_type, <<"address">> := Addr, <<"db_block">> := Db_block} ->
%%                                                DB块
                                            BinDb_block = dgiot_utils:to_binary(Db_block),
                                            Block_address = <<Block_type/binary, BinDb_block/binary, ".">>,
                                            dgiot_plc_tcp:get_dbque(ProductId, Devaddr, Address, Block_address, Addr, Originaltype, Acc);
                                        #{<<"block">> := Block_address, <<"address">> := Addr} ->
%%                                                其他
                                            dgiot_plc_tcp:get_dbque(ProductId, Devaddr, Address, Block_address, Addr, Originaltype, Acc);
                                        _ ->
                                            Acc
                                    end;
                                (_, Acc) ->
                                    Acc
                            end, #{}, Props);
            _ ->
                #{}
        end,
    Dbque =
        maps:fold(fun(K, V, Acc) ->
            Acc ++ [{K, V}]
                  end, [], DbqueMap),
    erlang:send_after(Freq * 1000, self(), read),
    {noreply, Dclient#dclient{child = ChildState#{data => <<>>, dbque => Dbque, dique => Dbque}}};

%% 任务开始
handle_info(read, #dclient{channel = ChannelId, client = ClientId,
    child = #{dbque := Dbque, dique := [{Block_address, {Startaddr, Minaddr, Maxaddr, Len}} | Que] = Dique} = ChildState} = Dclient) when length(Dique) == length(Dbque) ->
    Start_time = dgiot_datetime:now_ms(),
    SecondQue = send_data(ChannelId, ClientId, Block_address, Maxaddr, Minaddr, Startaddr, Len, <<"start">>),
    {noreply, Dclient#dclient{child = ChildState#{dique => SecondQue ++ Que, current_que => {Block_address, {Startaddr, Minaddr, Maxaddr, Len}}, start_time => Start_time}}};

handle_info(read, #dclient{channel = ChannelId, client = ClientId, child = #{dique := [{Block_address, {Startaddr, Minaddr, Maxaddr, Len}} | Que]} = ChildState} = Dclient) ->
    SecondQue = send_data(ChannelId, ClientId, Block_address, Maxaddr, Minaddr, Startaddr, Len, <<"middle">>),
    {noreply, Dclient#dclient{child = ChildState#{dique => SecondQue ++ Que, current_que => {Block_address, {Startaddr, Minaddr, Maxaddr, Len}}}}};

%% dique空了 本轮任务结束
handle_info({tcp, Buff}, #dclient{channel = ChannelId, child = #{<<"product">> := ProductId, <<"devaddr">> := DevAddr, current_que := {Block_address, {Startaddr, Minaddr, Maxaddr, Len}} = Current_que,
    <<"freq">> := Freq, dbque := Dbque, dique := Que, start_time := Start_time} = ChildState} = Dclient) when length(Que) == 0 ->
    dgiot_data:insert({check_connection, ChannelId, ProductId}, dgiot_datetime:now_secs()),
    save_db_data(ProductId, DevAddr, Current_que, Buff),
    Data = save_data(ProductId, DevAddr),
    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "recv ~p ~p plc ~p block end ~p ~p ~p ~p ~p ~n~ts~n", [Startaddr, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), dgiot_datetime:now_ms() - Start_time, ProductId, Block_address, Minaddr, Maxaddr, Len, unicode:characters_to_list(dgiot_json:encode(Data))]),
%%    删除db_data
    delete_db_data(ProductId, DevAddr, Dbque),
    next_task(Freq * 1000, Start_time),
    {noreply, Dclient#dclient{child = ChildState#{dique => Dbque}}};

handle_info({tcp, Buff}, #dclient{channel = ChannelId,
    child = #{<<"product">> := ProductId, <<"devaddr">> := DevAddr, start_time := Start_time, current_que := {Block_address, {Startaddr, Minaddr, Maxaddr, Len}} = Current_que}} = Dclient) ->
    save_db_data(ProductId, DevAddr, Current_que, Buff),
    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "recv ~p ~p plc ~p block ~p ~p ~p ~p ~p", [Startaddr, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), dgiot_datetime:now_ms() - Start_time, ProductId, Block_address, Minaddr, Maxaddr, Len]),
    erlang:send_after(5, self(), read),
    {noreply, Dclient};

handle_info({deliver, _, Msg}, #dclient{channel = ChannelId, client = ClientId, child = #{<<"product">> := ProductId, <<"decoder">> := Decoder} = ChildState} = Dclient) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"profile">>] ->
%%                    设置参数
                    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
                    Payloads = Decoder:set_params(ProfilePayload, ProductId, DevAddr),
                    lists:map(fun(X) ->
                        dgiot_tcp_client:send(ChannelId, ClientId, X),
                        dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "Channel ~p sends to PLC ~p ~p", [unicode:characters_to_list(Payload), DevAddr, dgiot_utils:binary_to_hex(X)])
                              end, Payloads),
                    {noreply, Dclient#dclient{child = ChildState#{<<"redtype">> => write}}};
                _Other ->
%%                    ?LOG(error, "_Other ~p", [_Other]),
                    {noreply, Dclient}
            end;
        false ->
            {noreply, Dclient}
    end;

handle_info(_Info, #dclient{child = _ChildState} = Dclient) ->
%%    io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
%%    io:format("~s ~p Dclient = ~p.~n", [?FILE, ?LINE, Dclient]),
%%    io:format("~s ~p ChildState = ~p.~n", [?FILE, ?LINE, _ChildState]),
    {noreply, Dclient}.

terminate(_Reason, #dclient{channel = _ChannelId, child = #{<<"product">> := _ProductId}} = _Dclient) ->
    ok.

create_device(ProductId, Dtuaddr, DtuType, IP) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Dtuaddr),
    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"name">> := Name, <<"ACL">> := Acl, <<"devType">> := DevType}} ->
            dgiot_device:create_device(#{
                <<"devaddr">> => Dtuaddr,
                <<"name">> => <<Name/binary, "_", IP/binary>>,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"ip">> => IP,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => DtuType,
                <<"devModel">> => DevType
            }),
            dgiot_task:save_pnque(ProductId, Dtuaddr, ProductId, Dtuaddr),
            {DeviceId, Dtuaddr};
        _Error2 ->
            {<<>>, <<>>}
    end.
%% dgiot_plugin:compile(dgiot_plc). ProductId = <<"bd94b9b484">>, Devaddr = <<"3212">>, DevId = dgiot_parse_id:get_deviceid(ProductId, Devaddr).
get_dbque(ProductId, Devaddr, Address, Block_address, Addr, Originaltype, Acc) ->
    DevId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
    {_, IntLen} = s7_protocol:get_len(Originaltype, 1),
%%    dgiot_data:get({DevId, <<"MW226">>}).
    dgiot_data:delete({DevId, Address}),
    dgiot_data:insert({DevId, Address}, {Block_address, Addr, IntLen}),
    NewAddr = trunc(dgiot_utils:to_float(Addr)),
    case maps:get(Block_address, Acc, not_find) of
        not_find ->
            Acc#{Block_address => {NewAddr, NewAddr, NewAddr, IntLen}};
        {_, OldMin, OldMax, _} when NewAddr > OldMax ->
            Acc#{Block_address => {lists:min([OldMin, NewAddr]), lists:min([OldMin, NewAddr]), NewAddr, IntLen}};
        {_, OldMin, OldMax, OldIntLen} ->
            Acc#{Block_address => {lists:min([OldMin, NewAddr]), lists:min([OldMin, NewAddr]), OldMax, OldIntLen}}
    end.

send_data(ChannelId, ClientId, Block_address, Maxaddr, Minaddr, Startaddr, Len, Type) ->
    BinAddress = dgiot_utils:to_binary(Startaddr),
    Length = (Maxaddr - Startaddr) + Len,
    {Data, SecondQue} =
        case Length > 450 of
            true ->
                {s7_protocol:build_read_command(byte, s7_protocol:analysis_address(<<Block_address/binary, BinAddress/binary>>, 450)), [{Block_address, {Startaddr + 450, Minaddr, Maxaddr, Len}}]};
            _ ->
                {s7_protocol:build_read_command(byte, s7_protocol:analysis_address(<<Block_address/binary, BinAddress/binary>>, Length)), []}
        end,
    dgiot_tcp_client:send(ChannelId, ClientId, Data),
    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "send ~p ~p ~p read ~p ~p ~p ~p ~p ~p to DTU",
        [Startaddr, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), dgiot_datetime:now_ms(), Type, <<Block_address/binary, BinAddress/binary>>, Minaddr, Maxaddr, Length, dgiot_utils:binary_to_hex(Data)]),
    SecondQue.

save_db_data(ProductId, DevAddr, {Block_address, {_, Minaddr, _, _}}, Buff) ->
    Now = dgiot_datetime:now_ms(),
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    case dgiot_data:get(dgiot_dbque, {DeviceId, Block_address}) of
        {_, _, OldBuff} ->
            NewBuff = s7_decoder:parse_frame(Buff),
            dgiot_data:insert(dgiot_dbque, {DeviceId, Block_address}, {Now, Minaddr, <<OldBuff/binary, NewBuff/binary>>});
        _ ->
            dgiot_data:insert(dgiot_dbque, {DeviceId, Block_address}, {Now, Minaddr, s7_decoder:parse_frame(Buff)})
    end.

delete_db_data(ProductId, DevAddr, Dbque) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    lists:foldl(fun({Block_address, _}, _) ->
        dgiot_data:delete(dgiot_dbque, {DeviceId, Block_address})
                end, [], Dbque).

next_task(Freq, Start_time) ->
    Consum_time = dgiot_datetime:now_ms() - Start_time,
    case Consum_time > Freq of
        true ->
            erlang:send_after(1, self(), read);
        _ ->
            erlang:send_after(Freq - Consum_time, self(), read)
    end.

save_data(ProductId, DevAddr) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Block_Data =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"identifier">> := Identifier,
                            <<"dataForm">> := #{
                                <<"protocol">> := <<"S7">>},
                            <<"dataSource">> := #{
                                <<"address">> := Address,
                                <<"originaltype">> := Originaltype}
                        } ->
                            case s7_protocol:get_block_value(DeviceId, Address, Originaltype) of
                                {Time, Value} ->
                                    Acc#{<<"createdat">> => Time, Identifier => Value};
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end
                            end, #{}, Props),

            %%            计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], Block_Data, Props),
            %%            计算计算值
            AllData = dgiot_task:get_calculated(ProductId, DevAddr, Collection, Props),

            RealData = dgiot_device_card:get_card(ProductId, [AllData], DeviceId, #{}, dgiot_data:get({shard_storage, ProductId})),
            dgiot_data:insert({last_data, DeviceId}, RealData),

%%            Pubtopic = <<"$dg/user/realtimecard/", DeviceId/binary, "/report">>,
%%            dgiot_mqtt:publish(self(), Pubtopic, RealData),
%%            io:format("~s ~p Pubtopic = ~p, RealData = ~p.~n", [?FILE, ?LINE, Pubtopic, base64:encode(dgiot_json:encode(#{<<"data">> => RealData}))]),
            Sql = dgiot_tdengine:format_sql(ProductId, DevAddr, [AllData]),
            dgiot_tdengine_adapter:save_sql(ProductId, Sql),
            dgiot_device:online(DeviceId),
            AllData;
        _ ->
            #{}
    end.




%%handle_info({tcp, Buff}, #dclient{channel = ChannelId,
%%    child = #{<<"product">> := ProductId, <<"devaddr">> := DtuAddr, <<"freq">> := Freq, minaddr := MinAddr, maxaddr := Maxaddr, di := Address, data := OldData, step := Step} = ChildState} = Dclient) ->
%%    Data = s7_decoder:parse_frame(Buff),
%%    case Address + Step >= Maxaddr of
%%        true ->
%%            erlang:send_after(Freq * 1000, self(), read),
%%            EndData = <<OldData/binary, Data/binary>>,
%%            Now = dgiot_datetime:now_secs(),
%%            case s7_decoder:parse_frame(EndData, ChildState#{<<"redtype">> => block_read}) of
%%                {ok, Things} ->
%%                    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
%%                    %%            是否有缓存
%%                    CacheData = dgiot_task:merge_cache_data(DeviceId, Things, -1),
%%                    %%            获取物模型
%%                    Props = dgiot_task:get_props(ProductId),
%%                    %%            计算上报值
%%                    Collection = dgiot_task:get_collection(ProductId, [], CacheData, Props),
%%                    %%            计算计算值
%%                    AllData = dgiot_task:get_calculated(ProductId, DtuAddr, Collection, Props),
%%                    dgiot_task:save_cache_data(ProductId, AllData),
%%
%%                    RealData = dgiot_device_card:get_card(ProductId, [AllData#{<<"createdat">> => Now * 1000}], DeviceId, #{}, dgiot_data:get({shard_storage, ProductId})),
%%                    dgiot_data:insert({last_data, DeviceId}, RealData),
%%
%%                    Pubtopic = <<"$dg/user/realtimecard/", DeviceId/binary, "/report">>,
%%                    dgiot_mqtt:publish(self(), Pubtopic, base64:encode(dgiot_json:encode(#{<<"data">> => RealData}))),
%%
%%                    Sql = dgiot_tdengine:format_sql(ProductId, DtuAddr, [AllData#{<<"createdat">> => Now * 1000}]),
%%                    dgiot_tdengine_adapter:save_sql(ProductId, Sql),
%%                    dgiot_device:online(DeviceId),
%%%%                    dgiot_task:save_td(<<"b574dbae8e">>, <<"S C-P9GW11172022	">>, Things#{<<"createdat">> => dgiot_datetime:now_ms()}, #{}),
%%                    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~p ~p block recv => ~ts~n ", [dgiot_datetime:format(Now, "YYYY-MM-DD HH:NN:SS"), ProductId, unicode:characters_to_list(dgiot_json:encode(Things))]);
%%                _ ->
%%                    pass
%%            end,
%%            {noreply, Dclient#dclient{child = ChildState#{di => MinAddr, data => <<>>}}};
%%        _ ->
%%            erlang:send_after(20, self(), read),
%%            {noreply, Dclient#dclient{child = ChildState#{di => Address + Step, data => <<OldData/binary, Data/binary>>}}}
%%    end;

%%handle_info({tcp, Buff}, #dclient{channel = ChannelId, child =
%%#{<<"product">> := ProductId, <<"identifier">> := _Identifier, <<"devaddr">> := DtuAddr, <<"decoder">> := Decoder} = ChildState} = Dclient) ->
%%%%    io:format("~s ~p Buff = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
%%    case Decoder:parse_frame(Buff, ChildState) of
%%        {ok, Things} ->
%%            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
%%%%            CacheAck = dgiot_task:merge_cache_data(DeviceId, Things, -1),
%%%%            dgiot_task:save_cache_data(ProductId, CacheAck),
%%            NewTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
%%            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
%%            dgiot_data:insert({check_connection, ChannelId, ProductId}, dgiot_datetime:now_secs()),
%%            dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things),
%%            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~p ~p  recv => ~ts~n ", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), ProductId, unicode:characters_to_list(dgiot_json:encode(Things))]);
%%        {write, _Msg} ->
%%%%            dgiot_bridge:send_log(ChannelId, "Channel write to PLC ~p ~p", [DtuAddr, Msg]);
%%            pass;
%%        _R ->
%%%%            io:format("~s ~p R = ~p.~n~n", [?FILE, ?LINE, _R]),
%%            pass
%%    end,
%%    {noreply, Dclient};

%%handle_info({deliver, _, Msg}, #dclient{channel = ChannelId, client = ClientId, child = #{<<"product">> := ProductId, <<"decoder">> := Decoder} = ChildState} = Dclient) ->
%%    Payload = dgiot_mqtt:get_payload(Msg),
%%    Topic = dgiot_mqtt:get_topic(Msg),
%%    case jsx:is_json(Payload) of
%%        true ->
%%            case binary:split(Topic, <<$/>>, [global, trim]) of
%%                [<<"$dg">>, <<"device">>, ProductId, _DevAddr, <<"profile">>] ->
%%%%                    设置参数
%%%%                    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
%%%%                    Payloads = Decoder:set_params(ProfilePayload, ProductId, DevAddr),
%%%%                    lists:map(fun(X) ->
%%%%%%                        dgiot_tcp_client:send(ChannelId, ClientId, X),
%%%%                        dgiot_bridge:send_log(ChannelId, "Channel ~p sends to PLC ~p ~p", [unicode:characters_to_list(Payload), DevAddr, dgiot_utils:binary_to_hex(X)])
%%%%                              end, Payloads),
%%                    {noreply, Dclient#dclient{child = ChildState#{<<"redtype">> => write}}};
%%                [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"properties">>] ->
%%                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
%%                        #{<<"identifier">> := Identifier, <<"address">> := Address, <<"originaltype">> := Originaltype} = DataSource ->
%%                            case Decoder:to_frame(DataSource) of
%%                                {_, not_frame} ->
%%                                    {noreply, Dclient};
%%                                {Type, Data} ->
%%                                    dgiot_bridge:send_log(ChannelId, "~p read ~p ~p => ~p to PLC ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), Identifier, Type, dgiot_utils:binary_to_hex(Data), DevAddr]),
%%%%                                  io:format("~s ~p Channel  ~p ~p ~p sends => ~p to PLC ~p.~n", [?FILE, ?LINE,ProductId, Identifier, Type, dgiot_utils:binary_to_hex(Data), DevAddr]),
%%                                    dgiot_tcp_client:send(ChannelId, ClientId, Data),
%%                                    {noreply, Dclient#dclient{child = ChildState#{<<"identifier">> => Identifier, <<"address">> => Address, <<"originaltype">> => Originaltype, <<"redtype">> => Type}}}
%%                            end;
%%                        _P ->
%%%%                            io:format("~s ~p Payload = ~p.~n", [?FILE, ?LINE, _P]),
%%                            {noreply, Dclient}
%%                    end;
%%                _Other ->
%%                    ?LOG(error, "_Other ~p", [_Other]),
%%                    {noreply, Dclient}
%%            end;
%%        false ->
%%            case binary:split(Topic, <<$/>>, [global, trim]) of
%%                [<<"$dg">>, <<"device">>, ProductId, _DevAddr, <<"profile">>] ->
%%                    %% 设置参数
%%%%                    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
%%%%                    Payloads = Decoder:set_params(ProfilePayload, ProductId, DevAddr),
%%%%                    lists:map(fun(X) ->
%%%%                        dgiot_tcp_client:send(ChannelId, ClientId, X)
%%%%                              end, Payloads),
%%                    {noreply, Dclient};
%%                _ ->
%%                    {noreply, Dclient}
%%            end
%%    end;



