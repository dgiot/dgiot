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

%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_plc_parser 模块 - 简化的PLC协议解析层
%%%
%%% 核心功能：生成事务ID、构建Modbus请求、解析Modbus响应
%%% 遵循七层架构原则：协议层只负责协议解析，不包含业务逻辑
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_plc_parser).

%% API
-export([
    generate_transaction_id/0,
    parse_modbus_response/1,
    build_modbus_read_request/3,
    build_modbus_write_request/4,
    validate_modbus_frame/1
]).

-include_lib("dgiot/include/logger.hrl").

%% 宏定义
-define(MODBUS_PROTOCOL_ID, 0).

%% Modbus功能码
-define(FUNCTION_READ_HOLDING_REGISTERS, 16#03).
-define(FUNCTION_WRITE_SINGLE_REGISTER, 16#06).
-define(FUNCTION_WRITE_MULTIPLE_REGISTERS, 16#10).

%% 记录定义
-record(modbus_frame, {
    transaction_id :: integer(),
    protocol_id :: integer(),
    length :: integer(),
    unit_id :: integer(),
    function_code :: integer(),
    data :: binary()
}).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 生成事务ID
%% @spec generate_transaction_id() -> integer()
generate_transaction_id() ->
    erlang:unique_integer([positive]) rem 65535.

%% @doc 解析Modbus TCP响应
%% @spec parse_modbus_response(binary()) -> {ok, #{transaction_id := integer(), function_code := integer(), data := map()}} | {error, term()}
parse_modbus_response(Data) when is_binary(Data) ->
    case validate_modbus_frame(Data) of
        {ok, #modbus_frame{
            transaction_id = TId,
            function_code = FunctionCode,
            data = FrameData
        }} ->
            case parse_response_data(FunctionCode, FrameData) of
                {ok, ParsedData} ->
                    {ok, #{transaction_id => TId, function_code => FunctionCode, data => ParsedData}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} = Error ->
            ?LOG(error, "Modbus响应验证失败: ~p", [Reason]),
            Error
    end.

%% @doc 构建Modbus读取请求
%% @spec build_modbus_read_request(integer(), integer(), integer()) -> binary()
build_modbus_read_request(SlaveId, StartAddress, RegisterCount) ->
    TransactionId = generate_transaction_id(),
    ProtocolId = ?MODBUS_PROTOCOL_ID,
    Length = 6,  % UnitId(1) + FunctionCode(1) + Address(2) + Count(2)
    
    <<
        TransactionId:16,
        ProtocolId:16,
        Length:16,
        SlaveId:8,
        ?FUNCTION_READ_HOLDING_REGISTERS:8,
        StartAddress:16,
        RegisterCount:16
    >>.

%% @doc 构建Modbus写入请求
%% @spec build_modbus_write_request(integer(), integer(), integer() | list(), integer()) -> binary()
build_modbus_write_request(SlaveId, Address, Value, ?FUNCTION_WRITE_SINGLE_REGISTER) when is_integer(Value) ->
    TransactionId = generate_transaction_id(),
    ProtocolId = ?MODBUS_PROTOCOL_ID,
    Length = 6,  % UnitId(1) + FunctionCode(1) + Address(2) + Value(2)
    
    <<
        TransactionId:16,
        ProtocolId:16,
        Length:16,
        SlaveId:8,
        ?FUNCTION_WRITE_SINGLE_REGISTER:8,
        Address:16,
        Value:16
    >>;

build_modbus_write_request(SlaveId, Address, Values, ?FUNCTION_WRITE_MULTIPLE_REGISTERS) when is_list(Values) ->
    RegisterCount = length(Values),
    ByteCount = RegisterCount * 2,
    
    % 构建值列表二进制
    ValuesBinary = << <<V:16>> || V <- Values >>,
    
    TransactionId = generate_transaction_id(),
    ProtocolId = ?MODBUS_PROTOCOL_ID,
    Length = 7 + ByteCount,  % UnitId(1) + FunctionCode(1) + Address(2) + Count(2) + ByteCount(1) + Values
    
    <<
        TransactionId:16,
        ProtocolId:16,
        Length:16,
        SlaveId:8,
        ?FUNCTION_WRITE_MULTIPLE_REGISTERS:8,
        Address:16,
        RegisterCount:16,
        ByteCount:8,
        ValuesBinary/binary
    >>.

%% @doc 验证Modbus TCP帧
%% @spec validate_modbus_frame(binary()) -> {ok, #modbus_frame{}} | {error, term()}
validate_modbus_frame(<<
    TransactionId:16,
    ProtocolId:16,
    Length:16,
    UnitId:8,
    FunctionCode:8,
    Rest/binary
>>) when ProtocolId =:= ?MODBUS_PROTOCOL_ID ->
    % 验证最小长度
    MinLength = 2,  % UnitId + FunctionCode
    
    if
        Length < MinLength -> 
            {error, invalid_packet};
        Length > byte_size(Rest) + 2 ->
            {error, insufficient_data};
        true ->
            % 截取正确长度的数据
            <<Data:(Length-2)/binary, _/binary>> = Rest,
            Frame = #modbus_frame{
                transaction_id = TransactionId,
                protocol_id = ProtocolId,
                length = Length,
                unit_id = UnitId,
                function_code = FunctionCode,
                data = Data
            },
            {ok, Frame}
    end;
validate_modbus_frame(Data) when byte_size(Data) < 8 ->
    {error, insufficient_data};
validate_modbus_frame(_) ->
    {error, invalid_protocol}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% @doc 解析响应数据
parse_response_data(?FUNCTION_READ_HOLDING_REGISTERS, <<ByteCount:8, RegistersData/binary>>) ->
    % 解析读取响应
    if
        ByteCount rem 2 =/= 0 ->
            {error, invalid_byte_count};
        true ->
            WordCount = ByteCount div 2,
            Registers = parse_registers(RegistersData, WordCount, []),
            {ok, #{type => read, byte_count => ByteCount, register_count => WordCount, registers => Registers}}
    end;

parse_response_data(?FUNCTION_WRITE_SINGLE_REGISTER, <<Address:16, Value:16>>) ->
    % 解析写单个寄存器响应
    {ok, #{type => write_single, address => Address, value => Value}};

parse_response_data(?FUNCTION_WRITE_MULTIPLE_REGISTERS, <<Address:16, RegisterCount:16>>) ->
    % 解析写多个寄存器响应
    {ok, #{type => write_multiple, address => Address, register_count => RegisterCount}};

parse_response_data(FunctionCode, Data) ->
    case FunctionCode of
        Code when Code >= 128 ->
            % 异常响应
            <<ExceptionCode:8>> = Data,
            {error, #{exception_code => ExceptionCode, function_code => FunctionCode}};
        _ ->
            {ok, #{function_code => FunctionCode, raw_data => Data}}
    end.

%% @private
%% @doc 解析寄存器数据
parse_registers(<<>>, _WordCount, Acc) ->
    lists:reverse(Acc);
parse_registers(<<Value:16, Rest/binary>>, WordCount, Acc) ->
    parse_registers(Rest, WordCount, [Value | Acc]).

%%%===================================================================
%%% 单元测试（可选）
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_transaction_id_test() ->
    TId = generate_transaction_id(),
    ?assert(is_integer(TId)),
    ?assert(TId >= 0),
    ?assert(TId < 65535).

build_modbus_read_request_test() ->
    Request = build_modbus_read_request(1, 100, 10),
    ?assert(is_binary(Request)),
    ?assertEqual(12, byte_size(Request)).  % 7字节头部 + 5字节数据

build_modbus_write_request_test() ->
    Request = build_modbus_write_request(1, 100, 1234, ?FUNCTION_WRITE_SINGLE_REGISTER),
    ?assert(is_binary(Request)),
    ?assertEqual(12, byte_size(Request)).

validate_modbus_frame_test() ->
    % 创建有效Modbus帧
    ValidFrame = <<1:16, 0:16, 6:16, 1:8, ?FUNCTION_READ_HOLDING_REGISTERS:8, 100:16, 10:16>>,
    {ok, Frame} = validate_modbus_frame(ValidFrame),
    ?assertEqual(1, Frame#modbus_frame.transaction_id),
    ?assertEqual(?FUNCTION_READ_HOLDING_REGISTERS, Frame#modbus_frame.function_code).

-endif.