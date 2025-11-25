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

-module(modbus_rtu_parser).
-author("jonhl").

-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    parse_response/2,
    parse_responses_recursive/3,
    parse_single_response/3,
    validate_crc_recursive/2,
    extract_data_recursive/3
]).

%% @doc 解析响应（递归入口）
parse_response(Data, Request) ->
    parse_responses_recursive(Data, Request, []).

%% @doc 递归解析响应列表
parse_responses_recursive(<<>>, _Request, Acc) -> lists:reverse(Acc);

parse_responses_recursive(Data, Request, Acc) ->
    case parse_single_response(Data, Request, []) of
        {ok, Result, Rest} ->
            parse_responses_recursive(Rest, Request, [Result | Acc]);
        {error, Reason} ->
            ?LOG(error, "Parse response error: ~p", [Reason]),
            lists:reverse(Acc)
    end.

%% @doc 解析单个响应
parse_single_response(<<SlaveId:8, FunctionCode:8, Rest/binary>>, Request, Acc) ->
    case validate_crc_recursive(<<SlaveId:8, FunctionCode:8, Rest/binary>>, 16#FFFF) of
        {ok, <<SlaveId:8, FunctionCode:8, Data/binary>>} ->
            parse_response_by_function(SlaveId, FunctionCode, Data, Request, Acc);
        {error, crc_error} ->
            {error, crc_error}
    end;

parse_single_response(_Data, _Request, _Acc) ->
    {error, invalid_format}.

%% @doc 根据功能码解析响应
parse_response_by_function(SlaveId, 3, Data, _Request, _Acc) ->  % Read Holding Registers
    parse_read_response(SlaveId, 3, Data);

parse_response_by_function(SlaveId, 6, Data, _Request, _Acc) ->  % Write Single Register
    parse_write_response(SlaveId, 6, Data);

parse_response_by_function(SlaveId, 16, Data, _Request, _Acc) -> % Write Multiple Registers
    parse_multi_write_response(SlaveId, 16, Data);

parse_response_by_function(_SlaveId, FunctionCode, Data, _Request, _Acc) ->
    ?LOG(error, "Unsupported function code: ~p, Data: ~p", [FunctionCode, Data]),
    {error, {unsupported_function, FunctionCode}}.

%% @doc 解析读响应
parse_read_response(SlaveId, FunctionCode, <<ByteCount:8, Data/binary>>) ->
    case byte_size(Data) of
        ByteCount ->
            Values = extract_data_recursive(Data, [], 0),
            Result = #{slave_id => SlaveId, function_code => FunctionCode, values => Values},
            {ok, Result, <<>>};
        _ ->
            {error, data_length_mismatch}
    end.

%% @doc 解析写单个寄存器响应
parse_write_response(SlaveId, FunctionCode, <<Address:16, Value:16>>) ->
    Result = #{slave_id => SlaveId, function_code => FunctionCode, address => Address, value => Value},
    {ok, Result, <<>>};

parse_write_response(_SlaveId, _FunctionCode, _Data) ->
    {error, invalid_write_response}.

%% @doc 解析写多个寄存器响应
parse_multi_write_response(SlaveId, FunctionCode, <<Address:16, Quantity:16>>) ->
    Result = #{slave_id => SlaveId, function_code => FunctionCode, address => Address, quantity => Quantity},
    {ok, Result, <<>>};

parse_multi_write_response(_SlaveId, _FunctionCode, _Data) ->
    {error, invalid_multi_write_response}.

%% @doc 递归提取数据
extract_data_recursive(<<>>, Acc, _Index) -> lists:reverse(Acc);

extract_data_recursive(<<Value:16, Rest/binary>>, Acc, Index) ->
    extract_data_recursive(Rest, [Value | Acc], Index + 1).

%% @doc 递归验证CRC校验码
validate_crc_recursive(Data, _CRC) ->
    case byte_size(Data) of
        Size when Size >= 2 ->
            <<Body:Size/binary, CRC1:16>> = Data,
            CalculatedCRC = calculate_crc_recursive(Body, 16#FFFF),
            case CRC1 of
                CalculatedCRC -> {ok, Body};
                _ -> {error, crc_error}
            end;
        _ ->
            {error, insufficient_data}
    end.

%% @doc 递归计算CRC校验码
calculate_crc_recursive(<<>>, CRC) -> CRC;

calculate_crc_recursive(<<Byte, Rest/binary>>, CRC) ->
    NewCRC = update_crc(Byte, CRC),
    calculate_crc_recursive(Rest, NewCRC).

%% @doc 更新CRC值
update_crc(Byte, CRC) ->
    CRC1 = CRC bxor Byte,
    update_crc_recursive(8, CRC1).

%% @doc 递归更新CRC
update_crc_recursive(0, CRC) -> CRC;

update_crc_recursive(Count, CRC) ->
    case CRC band 1 of
        1 -> update_crc_recursive(Count - 1, (CRC bsr 1) bxor 16#A001);
        0 -> update_crc_recursive(Count - 1, CRC bsr 1)
    end.
