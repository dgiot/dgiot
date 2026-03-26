%%--------------------------------------------------------------------
%% @doc Modbus客户端模块
%%--------------------------------------------------------------------
-module(dgiot_modbus_client).
-author("johnliu").

-include_lib("dgiot/include/logger.hrl").

-export([
    encode_command/4,
    parse_response/2,
    parse_request/1,
    calculate_crc16/1,
    build_read_command/3,
    build_write_command/3,
    build_coil_command/3
]).

%% 功能码
-define(FC_READ_HOLDING,    16#03).
-define(FC_WRITE_SINGLE,    16#06).
-define(FC_WRITE_COIL,      16#05).
-define(FC_EXCEPTION_MASK,  16#80).

%%====================================================================
%% 编码函数
%%====================================================================
encode_command(SlaveId, FunctionCode, Address, Data) ->
    Command = <<SlaveId:8, FunctionCode:8, Address:16, Data/binary>>,
    <<Command/binary, (calculate_crc16(Command)):16>>.

build_read_command(SlaveId, StartAddress, Count) ->
    Data = <<StartAddress:16, Count:16>>,
    encode_command(SlaveId, ?FC_READ_HOLDING, StartAddress, Data).

build_write_command(SlaveId, Address, Value) ->
    Data = <<Address:16, Value:16>>,
    encode_command(SlaveId, ?FC_WRITE_SINGLE, Address, Data).

build_coil_command(SlaveId, Address, true) ->
    encode_command(SlaveId, ?FC_WRITE_COIL, Address, <<16#FF00:16>>);
build_coil_command(SlaveId, Address, false) ->
    encode_command(SlaveId, ?FC_WRITE_COIL, Address, <<16#0000:16>>).

%%====================================================================
%% 解析函数
%%====================================================================
parse_response(Response, ExpectedFunc) ->
    case Response of
        <<SlaveId:8, FuncCode:8, Rest/binary>> ->
            parse_by_func_code(SlaveId, FuncCode, Rest, ExpectedFunc);
        _ ->
            {error, invalid_response}
    end.

parse_by_func_code(_SlaveId, FuncCode, Rest, ExpectedFunc) when FuncCode =:= ExpectedFunc ->
    parse_response_data(FuncCode, Rest);
parse_by_func_code(_SlaveId, FuncCode, <<ExceptionCode:8, _Crc:16>>, ExpectedFunc)
    when FuncCode =:= ExpectedFunc + ?FC_EXCEPTION_MASK ->
    {error, {modbus_exception, ExceptionCode}};
parse_by_func_code(_SlaveId, FuncCode, _Rest, ExpectedFunc) ->
    {error, {unexpected_func, ExpectedFunc, FuncCode}}.

parse_response_data(?FC_READ_HOLDING, <<ByteCount:8, Data:ByteCount/binary, _Crc:16>>) ->
    Registers = [ (H bsl 8) bor L || <<H:8, L:8>> <= Data ],
    {ok, Registers};
parse_response_data(?FC_WRITE_SINGLE, <<Address:16, Value:16, _Crc:16>>) ->
    {ok, #{address => Address, value => Value}};
parse_response_data(?FC_WRITE_COIL, <<Address:16, Value:16, _Crc:16>>) ->
    State = case Value of 16#FF00 -> true; _ -> false end,
    {ok, #{address => Address, state => State}};
parse_response_data(_, _) ->
    {error, unsupported}.

%% 解析请求帧
parse_request(<<SlaveId:8, FuncCode:8, StartAddr:16, Count:16, Crc:16>>) ->
    Data = <<SlaveId:8, FuncCode:8, StartAddr:16, Count:16>>,
    case calculate_crc16(Data) of
        Crc -> {ok, #{slave_id => SlaveId, func => FuncCode, 
                      start_addr => StartAddr, count => Count}};
        _ -> {error, crc_mismatch}
    end;
parse_request(_) -> {error, invalid_request}.

%%====================================================================
%% CRC16计算
%%====================================================================
calculate_crc16(Data) -> crc16_loop(Data, 16#FFFF).

crc16_loop(<<>>, Crc) -> Crc;
crc16_loop(<<Byte:8, Rest/binary>>, Crc) ->
    crc16_loop(Rest, crc16_byte(Crc bxor Byte, 8)).

crc16_byte(Crc, 0) -> Crc;
crc16_byte(Crc, N) ->
    Next = case (Crc band 1) =:= 1 of
        true -> (Crc bsr 1) bxor 16#A001;
        false -> Crc bsr 1
    end,
    crc16_byte(Next, N - 1).
