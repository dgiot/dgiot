%%%-------------------------------------------------------------------
%%% @doc
%%% uav_payload_checksum.erl - 载荷协议校验和及CRC计算模块
%%%
%%% 提供多种校验算法：
%%% - 累加和校验（8位）：用于飞控到载荷帧
%%% - CRC-16-CCITT：用于通用帧
%%% - CRC-16-IBM：用于部分协议
%%%
%%% 主要功能：
%%% - calculate_checksum/1: 计算累加和（8位）
%%% - verify_checksum/2: 验证累加和
%%% - calculate_crc16/1: 计算 CRC-16-CCITT（查表法，初始 0xFFFF）
%%% - verify_crc16/3: 验证 CRC16（从指定位置开始）
%%% - calculate_crc16_big/1: 计算大端 CRC-16（用于复合数据帧）
%%% - calculate_crc16_ibm/1: 计算 CRC-16-IBM（多项式 0x8005，初始 0xFFFF）
%%% - validate_crc16_ibm/2: 验证 CRC-16-IBM
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(uav_payload_checksum).
-export([calculate_checksum/1, calculate_crc16/1, 
         verify_checksum/2, verify_crc16/3,
         calculate_crc16_big/1,
         calculate_crc16_ibm/1, calculate_crc16_ibm/2,
         validate_crc16_ibm/2]).


-define(CRC16_TABLE, [
    16#0000, 16#1021, 16#2042, 16#3063, 16#4084, 16#50A5, 16#60C6, 16#70E7,
    16#8108, 16#9129, 16#A14A, 16#B16B, 16#C18C, 16#D1AD, 16#E1CE, 16#F1EF,
    16#1231, 16#0210, 16#3273, 16#2252, 16#52B5, 16#4294, 16#72F7, 16#62D6,
    16#9339, 16#8318, 16#B37B, 16#A35A, 16#D3BD, 16#C39C, 16#F3FF, 16#E3DE,
    16#2462, 16#3443, 16#0420, 16#1401, 16#64E6, 16#74C7, 16#44A4, 16#5485,
    16#A56A, 16#B54B, 16#8528, 16#9509, 16#E5EE, 16#F5CF, 16#C5AC, 16#D58D,
    16#3653, 16#2672, 16#1611, 16#0630, 16#76D7, 16#66F6, 16#5695, 16#46B4,
    16#B75B, 16#A77A, 16#9719, 16#8738, 16#F7DF, 16#E7FE, 16#D79D, 16#C7BC,
    16#48C4, 16#58E5, 16#6886, 16#78A7, 16#0840, 16#1861, 16#2802, 16#3823,
    16#C9CC, 16#D9ED, 16#E98E, 16#F9AF, 16#8948, 16#9969, 16#A90A, 16#B92B,
    16#5AF5, 16#4AD4, 16#7AB7, 16#6A96, 16#1A71, 16#0A50, 16#3A33, 16#2A12,
    16#DBFD, 16#CBDC, 16#FBBF, 16#EB9E, 16#9B79, 16#8B58, 16#BB3B, 16#AB1A,
    16#6CA6, 16#7C87, 16#4CE4, 16#5CC5, 16#2C22, 16#3C03, 16#0C60, 16#1C41,
    16#EDAE, 16#FD8F, 16#CDEC, 16#DDCD, 16#AD2A, 16#BD0B, 16#8D68, 16#9D49,
    16#7E97, 16#6EB6, 16#5ED5, 16#4EF4, 16#3E13, 16#2E32, 16#1E51, 16#0E70,
    16#FF9F, 16#EFBE, 16#DFDD, 16#CFFC, 16#BF1B, 16#AF3A, 16#9F59, 16#8F78,
    16#9188, 16#81A9, 16#B1CA, 16#A1EB, 16#D10C, 16#C12D, 16#F14E, 16#E16F,
    16#1080, 16#00A1, 16#30C2, 16#20E3, 16#5004, 16#4025, 16#7046, 16#6067,
    16#83B9, 16#9398, 16#A3FB, 16#B3DA, 16#C33D, 16#D31C, 16#E37F, 16#F35E,
    16#02B1, 16#1290, 16#22F3, 16#32D2, 16#4235, 16#5214, 16#6277, 16#7256,
    16#B5EA, 16#A5CB, 16#95A8, 16#8589, 16#F56E, 16#E54F, 16#D52C, 16#C50D,
    16#34E2, 16#24C3, 16#14A0, 16#0481, 16#7466, 16#6447, 16#5424, 16#4405,
    16#A7DB, 16#B7FA, 16#8799, 16#97B8, 16#E75F, 16#F77E, 16#C71D, 16#D73C,
    16#26D3, 16#36F2, 16#0691, 16#16B0, 16#6657, 16#7676, 16#4615, 16#5634,
    16#D94C, 16#C96D, 16#F90E, 16#E92F, 16#99C8, 16#89E9, 16#B98A, 16#A9AB,
    16#5844, 16#4865, 16#7806, 16#6827, 16#18C0, 16#08E1, 16#3882, 16#28A3,
    16#CB7D, 16#DB5C, 16#EB3F, 16#FB1E, 16#8BF9, 16#9BD8, 16#ABBB, 16#BB9A,
    16#4A75, 16#5A54, 16#6A37, 16#7A16, 16#0AF1, 16#1AD0, 16#2AB3, 16#3A92,
    16#FD2E, 16#ED0F, 16#DD6C, 16#CD4D, 16#BDAA, 16#AD8B, 16#9DE8, 16#8DC9,
    16#7C26, 16#6C07, 16#5C64, 16#4C45, 16#3CA2, 16#2C83, 16#1CE0, 16#0CC1,
    16#EF1F, 16#FF3E, 16#CF5D, 16#DF7C, 16#AF9B, 16#BFBA, 16#8FD9, 16#9FF8,
    16#6E17, 16#7E36, 16#4E55, 16#5E74, 16#2E93, 16#3EB2, 16#0ED1, 16#1EF0
]).

%% CRC-16-IBM算法参数
-define(CRC16_IBM_POLYNOMIAL, 16#8005).  % 多项式
-define(CRC16_IBM_INITIAL, 16#FFFF).     % 初始值
-define(CRC16_IBM_XOROUT, 16#0000).      % 最终异或值




%% @spec calculate_checksum(Data::binary()) -> Checksum::checksum()
calculate_checksum(Data) when is_binary(Data) ->
    calculate_checksum(Data, 0).

calculate_checksum(<<>>, Sum) ->
    Sum band 255;  % 8
calculate_checksum(<<Byte, Rest/binary>>, Sum) ->
    calculate_checksum(Rest, (Sum + Byte) band 65535).


%% @spec verify_checksum(Data::binary(), Expected::checksum()) -> boolean()
verify_checksum(Data, Expected) ->
    calculate_checksum(Data) =:= Expected.




%% @spec calculate_crc16(Data::binary()) -> CRC16::crc16()
calculate_crc16(Data) when is_binary(Data) ->
    calculate_crc16(Data, 16#FFFF).  % CRC16-CCITT初始值

calculate_crc16(<<>>, CRC) -> CRC;
calculate_crc16(<<Byte, Rest/binary>>, CRC) ->
    Index = ((CRC bsr 8) bxor Byte) band 255,
    NewCRC = ((CRC bsl 8) band 65535) bxor element(Index + 1, list_to_tuple(?CRC16_TABLE)),
    calculate_crc16(Rest, NewCRC).


%% @spec verify_crc16(Data::binary(), Expected::crc16(), Start::integer()) -> boolean()
verify_crc16(Data, Expected, Start) ->
    % 
    DataSize = byte_size(Data),
    case DataSize >= Start of
        true ->
            <<_:Start/binary, CheckData/binary>> = Data,
            calculate_crc16(CheckData) =:= Expected;
        false ->
            false
    end.




%% @spec calculate_crc16_big(Data::binary()) -> CRC16::crc16()
calculate_crc16_big(Data) ->
    % CRC16
    CRC = calculate_crc16(Data),
    <<High:8, Low:8>> = <<CRC:16>>,
    <<Low:8, High:8>>.

%% @spec calculate_crc16_ibm(Data::binary()) -> CRC16::crc16()
calculate_crc16_ibm(Data) when is_binary(Data) ->
    calculate_crc16_ibm(Data, ?CRC16_IBM_INITIAL).

%% @spec calculate_crc16_ibm(Data::binary(), Initial::integer()) -> CRC16::crc16()
calculate_crc16_ibm(Data, Initial) when is_binary(Data), is_integer(Initial) ->
    calculate_crc(Data, Initial, ?CRC16_IBM_POLYNOMIAL).

%% @spec validate_crc16_ibm(Data::binary(), Expected::crc16()) -> boolean()
validate_crc16_ibm(Data, Expected) ->
    calculate_crc16_ibm(Data) =:= Expected.

%% @private
%% @doc 计算CRC值（核心算法）
calculate_crc(<<>>, CRC, _Poly) ->
    CRC bxor ?CRC16_IBM_XOROUT;
calculate_crc(<<Byte, Rest/binary>>, CRC, Poly) ->
    NewCRC = calculate_crc_byte(Byte, CRC, Poly),
    calculate_crc(Rest, NewCRC, Poly).

%% @private
%% @doc 计算单个字节的CRC值
calculate_crc_byte(Byte, CRC, Poly) ->
    % 输入反转
    ByteReversed = reverse_bits(Byte, 8),
    % 与CRC高8位异或
    Temp = ((CRC bsr 8) bxor ByteReversed) band 255,
    % 查表或计算
    Temp2 = Temp bsl 8,
    Temp3 = Temp2 bxor (CRC bsl 8),
    % 多项式计算
    NewCRC = (Temp3 bsl 1) bxor (if (Temp3 band 16#8000) =/= 0 -> Poly; true -> 0 end),
    % 输出反转
    reverse_bits(NewCRC band 65535, 16).

%% @private
%% @doc 反转指定位数的位序
reverse_bits(Value, Bits) ->
    reverse_bits(Value, Bits, 0).

reverse_bits(0, 0, Result) -> Result;
reverse_bits(Value, Bits, Result) ->
    NewResult = (Result bsl 1) bor (Value band 1),
    reverse_bits(Value bsr 1, Bits - 1, NewResult).