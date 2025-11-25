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

-module(modbus_rtu_builder).
-author("jonhl").

-include("dgiot_modbus.hrl").

-export([
    build_request/2,
    build_requests_recursive/2,
    build_single_request/2,
    build_read_request/3,
    build_write_request/3,
    build_multi_write_request/3,
    calculate_crc_recursive/2
]).

%% @doc 构建请求（递归入口）
build_request(DataSources, SlaveId) ->
    build_requests_recursive(DataSources, #{<<"slaveId">> => SlaveId}).

%% @doc 递归构建请求列表
build_requests_recursive([], Acc) -> Acc;

build_requests_recursive([DataSource | Rest], Acc) ->
    NewAcc = build_single_request(DataSource, Acc),
    build_requests_recursive(Rest, NewAcc).

%% @doc 构建单个请求
build_single_request(#{<<"operatetype">> := OperateType, <<"address">> := Address} = DataSource, Acc) ->
    SlaveId = maps:get(<<"slaveId">>, Acc),
    
    case OperateType of
        OperateType when OperateType =:= <<"readCoils">>; 
                         OperateType =:= <<"readHregs">>; 
                         OperateType =:= <<"readIregs">>; 
                         OperateType =:= <<"readDregs">> ->
            {Start, Num} = parse_address(Address),
            build_read_request(SlaveId, Start, Num);
        OperateType when OperateType =:= <<"writeCoil">>; 
                         OperateType =:= <<"writeHreg">>; 
                         OperateType =:= <<"writeIreg">>; 
                         OperateType =:= <<"writeDreg">> ->
            {Start, _Num} = parse_address(Address),
            build_write_request(SlaveId, Start, DataSource);
        OperateType when OperateType =:= <<"writeHregs">>; 
                         OperateType =:= <<"writeIregs">>; 
                         OperateType =:= <<"writeDregs">> ->
            {Start, _Num} = parse_address(Address),
            build_multi_write_request(SlaveId, Start, DataSource);
        _ -> 
            io:format("~s ~p Unknown operate type: ~p~n", [?FILE, ?LINE, OperateType]),
            Acc
    end.

%% @doc 构建读请求
build_read_request(SlaveId, Start, Num) ->
    case Num of
        Num when Num >= 1, Num =< 125 ->
            <<SlaveId:8, 3:8, Start:16, Num:16>>;
        _ ->
            io:format("~s ~p Invalid read range: Start=~p, Num=~p~n", [?FILE, ?LINE, Start, Num]),
            <<>>
    end.

%% @doc 构建写单个寄存器请求
build_write_request(SlaveId, Start, #{<<"data">> := Data}) ->
    case Data of
        Data when is_binary(Data), byte_size(Data) =:= 2 ->
            <<Value:16>> = Data,
            <<SlaveId:8, 6:8, Start:16, Value:16>>;
        _ ->
            io:format("~s ~p Invalid write data: ~p~n", [?FILE, ?LINE, Data]),
            <<>>
    end.

%% @doc 构建写多个寄存器请求（递归实现）
build_multi_write_request(SlaveId, Start, #{<<"data">> := Data}) when is_binary(Data) ->
    ByteCount = byte_size(Data),
    case ByteCount rem 2 of
        0 ->
            Num = ByteCount div 2,
            <<SlaveId:8, 16:8, Start:16, Num:8, ByteCount:8, Data/binary>>;
        _ ->
            io:format("~s ~p Invalid multi-write data length: ~p~n", [?FILE, ?LINE, ByteCount]),
            <<>>
    end.

%% @doc 解析地址（递归处理地址格式）
parse_address(Address) when is_binary(Address) ->
    parse_address_recursive(Address, 0, 0);

parse_address(Address) when is_integer(Address) ->
    {Address, 1};

parse_address(Address) ->
    io:format("~s ~p Invalid address format: ~p~n", [?FILE, ?LINE, Address]),
    {0, 1}.

%% @doc 递归解析地址格式
parse_address_recursive(<<>>, Start, Num) -> {Start, Num};
parse_address_recursive(<<"+", Rest/binary>>, Start, Num) ->
    parse_address_recursive(Rest, Start, Num);
parse_address_recursive(Binary, _Start, _Num) ->
    case binary:split(Binary, <<"+">>) of
        [StartBin, NumBin] ->
            StartInt = dgiot_utils:to_int(StartBin),
            NumInt = dgiot_utils:to_int(NumBin),
            {StartInt, NumInt};
        [SingleBin] ->
            StartInt = dgiot_utils:to_int(SingleBin),
            {StartInt, 1}
    end.

%% @doc 递归计算CRC校验码
calculate_crc_recursive(Data, CRC) ->
    calculate_crc_recursive(Data, CRC, 0).

calculate_crc_recursive(<<>>, CRC, _Index) -> CRC;
calculate_crc_recursive(<<Byte, Rest/binary>>, CRC, Index) ->
    NewCRC = update_crc(Byte, CRC),
    calculate_crc_recursive(Rest, NewCRC, Index + 1).

%% @doc 更新CRC值
update_crc(Byte, CRC) ->
    CRC1 = CRC bxor Byte,
    update_crc_recursive(8, CRC1).

update_crc_recursive(0, CRC) -> CRC;
update_crc_recursive(Count, CRC) ->
    case CRC band 1 of
        1 -> update_crc_recursive(Count - 1, (CRC bsr 1) bxor 16#A001);
        0 -> update_crc_recursive(Count - 1, CRC bsr 1)
    end.
