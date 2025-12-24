%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(s7_decoder).
-author("johnliu").
-include("dgiot_plc.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    parse_tpkt/2
    , encode/2
    , to_frame/1
    , set_params/3
    , parse_frame/1
    , parse_frame/2
    , encode_data/3
]).

%% 请求连接
%% 0300001611e00000000100c0010ac1020102c2020100
%% 确认连接
%% 0300001902f08032010000040000080000f0000001000101e0
%% 读取CPU
%% 0300002102f080320700000001000800080001120411440100ff09000400110000

%% COTP申请连接
encode(<<"s71200">>, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c0010ac1020102c2020100">>);
encode(<<"s7300">>, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c0010ac1020102c2020100">>);
encode(<<"s7400">>, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c0010ac1020102c2020100">>);
encode(<<"s71500">>, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c0010ac1020102c2020100">>);
encode(<<"s7200Smart">>, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c1021000c2020300c0010a">>);
encode(<<"s7200">>, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c0010ac1020102c2020100">>);
encode(_, request_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001611e00000000100c0010ac1020102c2020100">>);

%% 确定正式连接
encode(<<"s71200">>, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000040000080000f0000001000101e0">>);
encode(<<"s7300">>, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000040000080000f0000001000101e0">>);
encode(<<"s7400">>, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000040000080000f0000001000101e0">>);
encode(<<"s71500">>, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000040000080000f0000001000101e0">>);
encode(<<"s7200Smart">>, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000ccc100080000f0000001000103c0">>);
encode(<<"s7200">>, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000040000080000f0000001000101e0">>);
encode(_, confirm_connection) ->
    dgiot_utils:hex_to_binary(<<"0300001902f08032010000040000080000f0000001000101e0">>);

%% 读取CPU
encode(<<"s71200">>, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s7300">>, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s7400">>, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s71500">>, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s7200Smart">>, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s7200">>, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(_, read_plc_cpu) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);

%% 0300002102f080320700000800000800080001120411440100ff090004001c0000
%% 读取序列号
encode(<<"s71200">>, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s7300">>, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300002102f080320700000001000800080001120411440100ff09000400110000">>);
encode(<<"s7400">>, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300001F02f080320100002681000e00000401120a1002000e000005001ff0">>);
encode(<<"s71500">>, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300002102F080320700000800000800080001120411440100FF090004001C0000">>);
encode(<<"s7200Smart">>, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300001F02f080320100002681000e00000401120a1002000e000005001ff0">>);
encode(<<"s7200">>, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300001F02f080320100002681000e00000401120a1002000e000005001ff0">>);
encode(_, read_plc_sn) ->
    dgiot_utils:hex_to_binary(<<"0300001F02f080320100002681000e00000401120a1002000e000005001ff0">>);

encode(_, _) ->
    dgiot_utils:hex_to_binary(<<"0300001F02f080320100002681000e00000401120a1002000e000005001ff0">>).


%% 0300003102F080320100000001000E00000401120A100100010000830000F1
%% 0300003102F080320100000001000E00000401120A100100010000830000F1
%%               32010000040000080000f0000001000101e0
%%               320100000001000e00000401120a100100010000830000f1
%% 0300001302F080320200000001000000008500
%%               3203000000010002000500000401ff03000100
to_frame(#{
    <<"address">> := Address,
    <<"number">> := Number,
    <<"originaltype">> := Originaltype}) ->
    encode_data(Address, Originaltype, dgiot_utils:to_int(Number));

to_frame(#{
    <<"address">> := Address,
    <<"originaltype">> := Originaltype}) ->
    encode_data(Address, Originaltype, 1).

encode_data(Address, Originaltype, Number) ->
    {Type, Length} = s7_protocol:get_len(Originaltype, Number),
    Data = s7_protocol:analysis_address(Address, Length),
    {Type, s7_protocol:build_read_command(Type, Data)}.

set_params(Payload, _ProductId, _DevAddr) ->
    PayloadLength = length(maps:keys(Payload)),
    Payloads =
        lists:foldl(fun(Index, Acc) ->
            case maps:find(Index, Payload) of
                {ok, #{
                    <<"dataForm">> := #{
                        <<"protocol">> := <<"S7">>,
                        <<"control">> := Setting},
                    <<"dataSource">> := #{
                        <<"address">> := Address,
                        <<"originaltype">> := Originaltype} = _DataSource
                } = Data} ->
                    case maps:find(<<"value">>, Data) of
                        error ->
                            Acc;
                        {ok, Value} when erlang:byte_size(Value) == 0 ->
                            Acc;
                        {ok, Value} ->
                            Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                            Value1 = dgiot_task:string2value(Str1, <<"type">>),
                            {Type, Len} = s7_protocol:get_len(Originaltype, 1),
                            Analysis_Data = s7_protocol:analysis_address(Address, Len),
                            Acc ++ [s7_protocol:build_write_command(Type, Analysis_Data#{<<"originaltype">> => Originaltype, <<"value">> => Value1, <<"length">> => Len})];
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
                    end, [], lists:seq(1, PayloadLength)),
    Payloads.

%%其中，第1~4层会由计算机自己完成（底层驱动程序）；
%%第5层TPKT，应用程数据传输协议，介于TCP和COTP协议之间；这是一个传输服务协议，主要用来在COTP和TCP之间建立桥梁；
%%第6层COTP，COTP 是 OSI 7层协议定义的位于TCP之上的协议。COTP 以“Packet”为基本单位来传输数据，这样接收方会得到与发送方具有相同边界的数据；
%%第7层，S7 communication，这一层和用户数据相关，对PLC数据的读取报文在这里完成；
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 定 义         |    类型                    |    长度         |          描 述                                         |
%------------------------------------------------------------------------------------------------------------------------
%%              |    Unsigned integer        |      1         |     0X03, 版本信息                                      |
%               -------------------------------------------------------------------------------------------------------
%%       TPKT   |    Unsigned integer        |     1          |    Reserved，保留(值为0x00)                             |
%               --------------------------------------------------------------------------------------------------------
%%              |   Unsigned integer         |      2         |    Length，TPKT、COTP、S7三层协议的总长度，                |
%%              |                            |                |    也就是TCP的payload的长度                              |
%-------------------------------------------------------------------------------------------------------------------------
%%              |    Unsigned integer        |      1         |     Length，COTP后续数据的长度（注意：长度不包含length的长度） |
%%              -------------------------------------------------------------------------------------------------------
%%       COTP   |    Unsigned integer        |      1         |     PDU typ，类型有                                    |
%%              |                            |                |    0x1: ED Expedited Data，加急数据                     |
%%              |                            |                |    0x2: EA Expedited Data Acknowledgement，加急数据确认 |
%%              |                            |                |    0x4: UD，用户数据                                    |
%%              |                            |                |    0x5: RJ Reject，拒绝                                |
%%              |                            |                |    0x6: AK Data Acknowledgement，数据确认              |
%%              |                            |                |    0x7: ER TPDU Error，TPDU错误                        |
%%              |                            |                |    0xD: CC Connect Confirm，连接确认                   |
%%              |                            |                |    0xE: CR Connect Request，连接请求                   |
%%              |                            |                |    0xF: DT Data，数据传输                              |
%%              -------------------------------------------------------------------------------------------------------
%%              |    Unsigned integer        |      2         |     Destination reference.                            |
%%              -------------------------------------------------------------------------------------------------------
%%              |    Unsigned integer        |      2         |    Source reference                                   |
%%              -------------------------------------------------------------------------------------------------------
%%              |    Boolean                 |      1         |    opt，其中包括Extended formats、No explicit flow control |
%%              |                            |                |    值都是Boolean类型、                                    |
%%              -------------------------------------------------------------------------------------------------------
%%              |    bytes                   |     length-7   |    Parameter，参数。一般参数包含三部分。                      |
%%              |                            |                |    Parameter code(Unsigned integer, 1 byte)、           |
%%              |                            |                |    Parameter length(Unsigned integer, 1 byte)          |
%%              |                            |                |    Parameter data                                       |
%%------------------------------------------------------------------------------------------------------------------------
%%              |    Unsigned integer        |       1        |    Protocol Id: 0x32 为 协议ID   一般指定为0x32                             |
%               -------------------------------------------------------------------------------------------------------
%%              |    Unsigned integer        |       1        |    ROSCTR PDU类型，一般有                                       |
%%              |                            |                |    0x01 Job 主设备发起请求                               |
%%      S7      |                            |                |    0x02 Ack 确认响应                                    |
%%              |                            |                |    0x03 Ack_data 确认数据响应，一般作为确认0x01的请求       |
%%              |                            |                |    0x07 USERDATA    协议的扩展，参数字段包含请求/响应ID     |
%               -------------------------------------------------------------------------------------------------------
%%              |        bytes              |        2        |   s7comm.header.redid Redundancy Identification (Reserved): 0x0000 冗余数据，通常为0×0000                                 |
%               -------------------------------------------------------------------------------------------------------
%%              |        bytes              |        2        |   Protocol Data Unit Reference: 1  3e 02（十进制为15874）协议数据单元的参考、通过请求事件增加  |
%               -------------------------------------------------------------------------------------------------------
%%              |        bytes              |        2        |   Parameter length: 8 参数的总长度 也就是parameter的长度                     |
%               -------------------------------------------------------------------------------------------------------
%%              |        bytes              |        2        |   Data length: 8 数据的长度、也就是data部分数据的长度 如果无即为0             |
%               -------------------------------------------------------------------------------------------------------
%%              |        bytes              |       data   |   数据包                                                   |
%-------------------------------------------------------------------------------------------------------------------------
%%TPKT
%%0 (Unsigned integer, 1 byte): Version，版本信息。
%%1 (Unsigned integer, 1 byte): Reserved，保留(值为0x00)。
%%2-3 (Unsigned integer, 2 bytes): Length，TPKT、COTP、S7三层协议的总长度，也就是TCP的payload的长度。
%%COTP协议分为两种形态
%%1、COTP连接包（COTP Connection Packet） 例如连接时两次验证分别为请求和响应

%% ISO 8073/X.224 COTP Connection-Oriented Transport Protocol
%% dgiot_utils:hex_to_binary(<<"03 00 00 16 11 D0 00 01 00 12 00 C0 01 0A C1 02 01 02 C2 02 01 00">>).
parse_tpkt(<<16#03, 16#00, Len:16, Data/binary>>, Opts) ->
    case size(Data) =:= (Len - 4) of
        true ->
            parse_frame(Data, [], Opts);
        _ ->
            pass
    end;

parse_tpkt(<<_:1/binary, Rest/binary>>, Opts) ->
    parse_tpkt(Rest, Opts);

parse_tpkt(_, _Opts) ->
    {<<>>, []}.

parse_frame(<<_:21/binary, 16#FF, 16#04, _:16, Data/binary>> = Buff) when size(Buff) >= 21 ->
    Data;

parse_frame(_) ->
    <<>>.

%% Buff = <<3,0,0,26,2,240,128,50,3,0,0,0,1,0,2,0,5,0,0,4,1,255,3,0,1,0>>.
parse_frame(<<_:21/binary, Data:8, _/binary>> = Buff, #{<<"redtype">> := write, <<"identifier">> := Identifier}) when size(Buff) >= 22 ->
    Value =
        case Data of
            16#FF ->
                <<"OK">>;
            _ ->
                <<"ERROR">>
        end,
    {write, <<"write ", Identifier/binary, " ", Value/binary>>};

parse_frame(<<_:21/binary, 16#FF, 16#03, _:16, Value:8, _/binary>> = Buff, #{<<"redtype">> := bit, <<"identifier">> := Identifier}) when size(Buff) >= 21 ->
    {ok, #{Identifier => Value}};

%% Buff = dgiot_utils:hex_to_binary(<<"0300001902F080320300000001000200040000040105000000">>).
parse_frame(Buff, #{<<"redtype">> := block_read, <<"product">> := ProductId}) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            {ok, s7_protocol:parse_block(Buff, Props)};
        _ ->
            {?S7_ERROR_CODE_READ_LENGTH_OVER_PLC_ASSIGN, block_read}
    end;

parse_frame(<<_:21/binary, 16#FF, 16#04, _:16, Data/binary>> = Buff, #{<<"redtype">> := byte, <<"identifier">> := Identifier, <<"originaltype">> := Originaltype}) when size(Buff) >= 21 ->
    case s7_protocol:parse_value(Data, Originaltype, 0) of
        null ->
            {?S7_ERROR_CODE_READ_LENGTH_OVER_PLC_ASSIGN, Identifier};
        Value ->
            {ok, #{Identifier => Value}}
    end;

parse_frame(<<_:21/binary, 16#FF, 16#03, _/binary>>, #{<<"identifier">> := Identifier}) ->
    {?S7_ERROR_CODE_DATA_LENGTH_CHECK_FAILED, Identifier};

parse_frame(<<_:21/binary, 16#05, 16#00, _/binary>> = Buff, #{<<"identifier">> := Identifier}) when size(Buff) >= 21 ->
    {?S7_ERROR_CODE_READ_LENGTH_OVER_PLC_ASSIGN, Identifier};

parse_frame(<<_:21/binary, 16#06, 16#00, _/binary>> = Buff, #{<<"identifier">> := Identifier}) when size(Buff) >= 21 ->
    {?S7_ERROR_CODE_ERROR_0006, Identifier};

parse_frame(<<_:21/binary, 16#0A, 16#00, _/binary>> = Buff, #{<<"identifier">> := Identifier}) when size(Buff) >= 21 ->
    {?S7_ERROR_CODE_ERROR_000A, Identifier};

parse_frame(Buff, #{<<"identifier">> := Identifier} = _Opts) ->
%%    io:format("~s ~p recv Buff = ~p.~n~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
%%    io:format("~s ~p Opts = ~p.~n~n", [?FILE, ?LINE, Opts]),
%%    parse_tpkt(Buff, Opts),
    {?S7_ERROR_CODE_UNKOWN, {dgiot_utils:binary_to_hex(Buff), Identifier}}.

parse_frame(<<>>, Acc, _Opts) ->
    {<<>>, Acc};

%% COTP
%% %% Buff = dgiot_utils:hex_to_binary(<<"0300001611D00001001300C0010AC1020102C2020100">>).
parse_frame(<<COTPLEN:8, COTPDATA:COTPLEN/binary>>, Acc, _Opts) ->
    case COTPDATA of
        <<Pdutype:8, DestReference:16, SourceReference:16, Opt:1/binary, Data/binary>> ->
            <<_:6, Extended_formats:1, No_explicit_flow_control:1>> = Opt,
            Frame = #{
                <<"pdutype">> => Pdutype,
                <<"destreference">> => DestReference,
                <<"sourcereference">> => SourceReference,
                <<"opt">> => Opt,
                <<"extended_formats">> => Extended_formats,
                <<"no_explicit_flow_control">> => No_explicit_flow_control,
                <<"data">> => s7_protocol:decoder_cotp(Data),
                <<"type">> => <<"response_connection">>
            },
            {<<>>, Acc ++ [Frame]};
        _ ->
            {<<>>, Acc}
    end;

%% S7 Communication
%%  03 00 00 1A 02 F0 80 32 03 00
%%  00 00 01 00 02 00 05 00 00 04
%%  01 FF 03 00 01 00
parse_frame(<<COTPLEN:8, COTPDATA:COTPLEN/binary, 16#32, 16#03, Reserved:16, PDURef:16, Paramlen:16, DataLen:16, ErrorClass:8, ErrorCode:8, PData/binary>>, Acc, _Opts) ->
    <<Pdutype:8, LastDunit:1, TPDUNum:7, _/binary>> = COTPDATA,
    Frame = #{
        <<"pdutype">> => Pdutype,
        <<"tpdunum">> => TPDUNum,
        <<"lastdunit">> => LastDunit,
        <<"rosctr">> => 3,
        <<"reserved">> => Reserved,
        <<"pduref">> => PDURef,
        <<"errorclass">> => ErrorClass,
        <<"errorcode">> => ErrorCode,
        <<"paramlen">> => Paramlen,
        <<"datalen">> => DataLen,
        <<"pdata">> => PData
    },
    {<<>>, s7_protocol:parse(Frame, Acc)};

parse_frame(<<COTPLEN:8, COTPDATA:COTPLEN/binary, 16#32, 16#07, Reserved:16, PDURef:16, Paramlen:16, DataLen:16, PData/binary>>, Acc, _Opts) ->
    <<Pdutype:8, LastDunit:1, TPDUNum:7, _/binary>> = COTPDATA,
    Frame = #{
        <<"pdutype">> => Pdutype,
        <<"tpdunum">> => TPDUNum,
        <<"lastdunit">> => LastDunit,
        <<"rosctr">> => 7,
        <<"reserved">> => Reserved,
        <<"pduref">> => PDURef,
        <<"paramlen">> => Paramlen,
        <<"datalen">> => DataLen,
        <<"pdata">> => PData
    },
    {<<>>, s7_protocol:parse(Frame, Acc)};

parse_frame(<<COTPLEN:8, COTPDATA:COTPLEN/binary, ProtocolId:8, ROSCTR:8, Reserved:16, PDURef:16, Paramlen:16, DataLen:16, PData/binary>>, Acc, _Opts) ->
    <<Pdutype:8, LastDunit:1, TPDUNum:7, _/binary>> = COTPDATA,
    Frame = #{
        <<"pdutype">> => Pdutype,
        <<"tpdunum">> => TPDUNum,
        <<"lastdunit">> => LastDunit,
        <<"protocolid">> => ProtocolId,
        <<"rosctr">> => ROSCTR,
        <<"reserved">> => Reserved,
        <<"pduref">> => PDURef,
        <<"paramlen">> => Paramlen,
        <<"datalen">> => DataLen,
        <<"pdata">> => PData
    },
    {<<>>, s7_protocol:parse(Frame, Acc)};

parse_frame(Rest, Acc, _Opts) ->
    {Rest, Acc}.
