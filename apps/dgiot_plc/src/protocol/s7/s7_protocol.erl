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

%% @doc bkv Protocol Processor.
-module(s7_protocol).
-author("johnliu").

-include_lib("dgiot_plc.hrl").
-include_lib("dgiot/include/logger.hrl").

%% 注册协议类型
-protocol_type(#{
    cType => ?S7,
    type => <<"S7">>,
    colum => 10,
    title => #{
        zh => <<"西门子S7协议"/utf8>>
    },
    description => #{
        zh => <<"西门子S7协议"/utf8>>
    }
}).

-params(#{
    <<"address">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"M30.1"/utf8>>,
        title => #{
            zh => <<"数据起始地址"/utf8>>
        },
        description => #{
            zh => <<"数据的起始地址，也就是偏移地址"/utf8>>
        }
    },
    <<"originaltype">> => #{
        order => 3,
        type => string,
        required => true,
        default => #{<<"value">> => <<"bool">>, <<"label">> => <<"bool"/utf8>>},
        enum => [
            #{<<"value">> => <<"bool">>, <<"label">> => <<"布尔(bool)型"/utf8>>},
            #{<<"value">> => <<"byte">>, <<"label">> => <<"字节(byte)"/utf8>>},
            #{<<"value">> => <<"short">>, <<"label">> => <<"短整型有符号(short)"/utf8>>},
            #{<<"value">> => <<"ushort">>, <<"label">> => <<"短整型无符号(ushort)"/utf8>>},
            #{<<"value">> => <<"int32">>, <<"label">> => <<"32位整数有符号(int32)"/utf8>>},
            #{<<"value">> => <<"uint32">>, <<"label">> => <<"32位整数无符号(uint32)"/utf8>>},
            #{<<"value">> => <<"int64">>, <<"label">> => <<"64位整数有符号(int64)"/utf8>>},
            #{<<"value">> => <<"uint64">>, <<"label">> => <<"64位整数无符号(uint64)"/utf8>>},
            #{<<"value">> => <<"float">>, <<"label">> => <<"浮点(float)型"/utf8>>},
            #{<<"value">> => <<"double">>, <<"label">> => <<"双精度浮点(double)型"/utf8>>},
            #{<<"value">> => <<"string">>, <<"label">> => <<"字符串(string)"/utf8>>}
        ],
        title => #{
            zh => <<"数据类型"/utf8>>
        },
        description => #{
            zh => <<"数据类型"/utf8>>
        }
    }
}).

-export([
    parse/2,
    decoder_cotp/1,
    build_read_command/2,
    build_write_command/2,
    get_userzone/3,
    analysis_address/2,
    parse_value/3,
    parse_block/2,
    format_value/2,
    get_len/2,
    get_block_value/3
]).

parse(#{
    <<"rosctr">> := Rosctr,
    <<"paramlen">> := Paramlen,
    <<"datalen">> := DataLen,
    <<"pdata">> := PData} = Frame, Acc) ->
    {Param, Data} = decoder(Rosctr, Paramlen, DataLen, PData),
    Acc ++ [maps:without([<<"pdata">>], Frame#{<<"param">> => Param, <<"data">> => Data})].

decoder(Rosctr, Paramlen, DataLen, PData) when DataLen > 0 ->
    case PData of
        <<Parameter:Paramlen/binary, Data:DataLen/binary, _/binary>> ->
            Param = decoder_param(Rosctr, Parameter),
            Value = decoder_data(Rosctr, Param, Data),
            {Param, Value};
        _ ->
            {#{}, #{}}
    end;

decoder(Rosctr, Paramlen, _, PData) ->
    case PData of
        <<Parameter:Paramlen/binary, _/binary>> ->
            {decoder_param(Rosctr, Parameter), #{}};
        _ ->
            {#{}, #{}}
    end.

decoder_param(3, <<Function:8, ICount:8>>) when Function =:= 16#04 ->
    #{
        <<"function">> => Function,
        <<"ICount">> => ICount
    };

decoder_param(3, <<Function:8, Reserved:8, MACing:16, MACed:16, PDULen:16>>) when Function =:= 16#F0 ->
    #{
        <<"function">> => Function,
        <<"paramreserved">> => Reserved,
        <<"macing">> => MACing,
        <<"maced">> => MACed,
        <<"pdulen">> => PDULen
    };

decoder_param(7, <<Head:24, ParamLen:8, Method:8, TypeResp:4, FunctionGroup:4, Subfunction:8, SequenceNumber:8, DURefNum:8, LastDU:8, ErrorCode:16>>) ->
    #{
        <<"head">> => Head,
        <<"paramlen">> => ParamLen,
        <<"method">> => Method,
        <<"typeresp">> => TypeResp,
        <<"functiongroup">> => FunctionGroup,
        <<"subfunction">> => Subfunction,
        <<"sequencenumber">> => SequenceNumber,
        <<"durefnum">> => DURefNum,
        <<"lastdu">> => LastDU,
        <<"errorcode">> => ErrorCode
    };

decoder_param(_, _) ->
    #{}.

%% .... 0100 = Function group: CPU functions (4)
%% 0000 .... .... .... = Diagnostic type: CPU (0x0)
decoder_data(_Rosctr, #{<<"functiongroup">> := 4}, <<ReCode:8, TranSize:8, Len:16, SzlData:Len/binary>>) ->
    case SzlData of
        <<DiagnosticType:4, SID2:4, Module_Iden:8, SZLIndex:16, SZLPlen:16, SZLPCount:16, SZLDataTree/binary>> ->
            {SZLTree, CpuType} =
                case decoder_szldatatree(Module_Iden, SZLPlen, SZLPCount, SZLDataTree) of
                    [{_, #{<<"mlfb">> := MLFB}} | _] = List ->
                        {List, MLFB};
                    List ->
                        case proplists:get_value(5, List) of
                            #{<<"serialnumber">> := Serialnumber} ->
                                {List, Serialnumber};
                            _ ->
                                {List, <<>>}
                        end
                end,
            #{
                <<"recode">> => ReCode,
                <<"transize">> => TranSize,
                <<"diagnostictype">> => DiagnosticType,
                <<"sid2">> => SID2,
                <<"module_iden">> => Module_Iden,
                <<"szlindex">> => SZLIndex,
                <<"szlplen">> => SZLPlen,
                <<"szlpcount">> => SZLPCount,
                <<"cputype">> => CpuType,
                <<"szltree">> => SZLTree
            };
        _ ->
            #{}
    end;
%% SN
decoder_data(_Rosctr, #{<<"function">> := 4}, <<16#FF, 16#04, 16#00, 16#70, Data/binary>>) ->
    #{
        <<"sn">> => Data
    };

decoder_data(_Rosctr, _Param, _Data) ->
%%    io:format("~s ~p Param = ~p.~n", [?FILE, ?LINE, Param]),
%%    io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Data)]),
    #{}.


%% 11 d0 00 01 001200c0010ac1020102c2020100
decoder_cotp(COTPData) ->
    decoder_cotp(COTPData, 1, []).

decoder_cotp(<<Code:1/binary, 16#00, Rest/binary>>, Num, Acc) ->
    NewAcc = Acc ++ [{
        dgiot_utils:binary_to_hex(Code),
        #{
            <<"anum">> => Num,
            <<"code">> => dgiot_utils:binary_to_hex(Code)
        }}],
    decoder_cotp(Rest, Num + 1, NewAcc);

decoder_cotp(<<Code:1/binary, Len:8, Data:Len/binary, Rest/binary>>, Num, Acc) ->
    NewAcc = Acc ++ [{
        dgiot_utils:binary_to_hex(Code),
        #{
            <<"anum">> => Num,
            <<"code">> => dgiot_utils:binary_to_hex(Code),
            <<"Len">> => Len,
            <<"original_data">> => Data,
            <<"data">> => dgiot_utils:binary_to_hex(Data)
        }}],
    decoder_cotp(Rest, Num + 1, NewAcc);

decoder_cotp(_, _Num, Acc) ->
    Acc.

%% SZL data tree
decoder_szldatatree(Module_Iden, SZLPlen, SZLPCount, SZLTree) ->
    decoder_szldatatree(Module_Iden, SZLPlen, SZLPCount, SZLTree, 0, []).

decoder_szldatatree(_, _, SZLPCount, _, Num, Acc) when SZLPCount == Num ->
    Acc;
decoder_szldatatree(Module_Iden, SZLPlen, SZLPCount, SZLTree, Num, Acc) ->
    <<Tree:SZLPlen/binary, Rest/binary>> = SZLTree,
    NewAcc = decoder_szldata(Module_Iden, Tree, Num, Acc),
    decoder_szldatatree(Module_Iden, SZLPlen, SZLPCount, Rest, Num + 1, NewAcc).

decoder_szldata(17, <<Index:16, MLFB:20/binary, BGTyp:16, Ausbg:16, Ausbe:16>>, Num, Acc) ->
    Acc ++ [{
        Index,
        #{
            <<"anum">> => Num,
            <<"index">> => Index,
            <<"mlfb">> => MLFB,
            <<"bgtyp">> => BGTyp,
            <<"ausbg">> => Ausbg,
            <<"ausbe">> => Ausbe
        }}];

decoder_szldata(28, <<Index:16, Serialnumber:16/binary, _:8/binary, Reserved:8/binary>>, Num, Acc) ->
    Acc ++ [{
        Index,
        #{
            <<"anum">> => Num,
            <<"index">> => Index,
            <<"serialnumber">> => Serialnumber,
            <<"reserved">> => Reserved
        }}];

decoder_szldata(_, _, _, Acc) ->
    Acc.

build_read_command(Type, Data) ->
    case get_userzone(read, Type, Data) of
        not_frame ->
            not_frame;
        Userzone ->
            <<
                16#03, 16#00, 16#00, 16#1F, 16#02, 16#F0, 16#80,
                16#32, 16#01, 16#00, 16#00, 16#00, 16#01, 16#00, 16#0E, 16#00, 16#00,
                16#04, 16#01, 16#12, 16#0A, 16#10, Userzone/binary
            >>
    end.

build_write_command(Type, #{<<"length">> := Val_len} = Data) ->
    Len = 35 + Val_len,
    Write_Len = Val_len + 4,
    Userzone = get_userzone(write, Type, Data),
    <<
        16#03, 16#00, Len:16, 16#02, 16#F0, 16#80,
        16#32, 16#01, 16#00, 16#00, 16#00, 16#01, 16#00, 16#0E,
        Write_Len:16, 16#05, 16#01, 16#12, 16#0A, 16#10,
        Userzone/binary
    >>.

%%     0300002702F080320100000001000E00080501120A1002000400018400032000040020 41500000
%%     0300002702f080320100000001000e00080501120a1002000400018400032000040020 4141999a
%%      <<Value:32/float, _/binary>> = dgiot_utils:hex_to_binary(<<"4141999a">>).
get_userzone(write, byte, #{<<"db_block">> := Db_block, <<"data_code">> := Data_code, <<"address_start">> := Address_start, <<"length">> := Val_len, <<"value">> := Value, <<"originaltype">> := Originaltype} = Data) ->
    Addr = frame_byte_addr(write_byte, Data),
    Len = Val_len * 8,
    NewValue = format_value(Value, Originaltype),
    <<Addr/binary, Db_block:16, Data_code:8, Address_start:24, 16#00, 16#04, Len:16, NewValue/binary>>;

get_userzone(write, bit, #{<<"db_block">> := Db_block, <<"data_code">> := Data_code, <<"address_start">> := Address_start, <<"length">> := Val_len, <<"value">> := Value} = Data) ->
    Frame_Data_code = frame_byte_addr(write_bit, Data),
    Len = Val_len * 8,
    <<16#01, Val_len:16, Db_block:16, Data_code:8, Address_start:24, Frame_Data_code/binary, Val_len:16, Value:Len>>;

get_userzone(Type, byte, #{<<"db_block">> := Db_block, <<"data_code">> := Data_code, <<"address_start">> := Address_start} = Data) ->
    Addr = frame_byte_addr(Type, Data),
    <<Addr/binary, Db_block:16, Data_code:8, Address_start:24>>;

get_userzone(_, bit, #{<<"db_block">> := Db_block, <<"data_code">> := Data_code, <<"address_start">> := Address_start}) ->
    <<16#01, 16#00, 16#01, Db_block:16, Data_code:8, Address_start:24>>;

get_userzone(_, _, _) ->
    not_frame.

frame_byte_addr(write_bit, #{<<"data_code">> := Data_code}) when Data_code == 16#1C ->
    <<16#00, 16#09>>;

frame_byte_addr(write_bit, _) ->
    <<16#00, 16#03>>;

frame_byte_addr(read, #{<<"data_code">> := Data_code, <<"length">> := Length}) when Data_code == 16#1E; Data_code == 16#1F ->
    Len = dgiot_utils:to_int(Length / 2),
    <<Data_code:8, Len:16>>;

frame_byte_addr(_, #{<<"data_code">> := Data_code, <<"length">> := Length}) when Data_code == 16#06; Data_code == 16#07 ->
    Len = dgiot_utils:to_int(Length / 2),
    <<16#04, Len:16>>;

%% 0300001f02f080320100000001000e00000401120a1002 03a4 000d840002d0
%% 0300001f02f080320100000001000e00000401120a1002 01c4 000d840002d0
frame_byte_addr(_, #{<<"length">> := Length}) when Length > 452 ->
    <<16#02, 16#01, 16#C4>>;

frame_byte_addr(_, #{<<"length">> := Length}) ->
    <<16#02, Length:16>>.

calculate_address_started(Address, IsCT) ->
    case binary:split(Address, <<$.>>, [global, trim]) of
        [First, Second] ->
            IntFirst = dgiot_utils:to_int(First),
            IntSecond = dgiot_utils:to_int(Second),
            IntFirst * 8 + IntSecond;
        _ when IsCT ->
            dgiot_utils:to_int(Address);
        _ ->
            Int = dgiot_utils:to_int(Address),
            Int * 8
    end.

analysis_address(<<Add:3/binary, Ress/binary>>, Length) when Add =:= <<"AIX">>; Add =:= <<"AIB">>; Add =:= <<"AIW">>; Add =:= <<"AID">> ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#06,
        <<"address">> => Ress,
        <<"block">> => Add,
        <<"address_start">> => calculate_address_started(Ress, false)
    };
analysis_address(<<"AI", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#06,
        <<"address">> => Ress,
        <<"block">> => <<"AI">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<Add:3/binary, Ress/binary>>, Length) when Add =:= <<"AQX">>; Add =:= <<"AQB">>; Add =:= <<"AQW">>; Add =:= <<"AQD">> ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#07,
        <<"address">> => Ress,
        <<"block">> => Add,
        <<"address_start">> => calculate_address_started(Ress, false)
    };
analysis_address(<<"AQ", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#07,
        <<"address">> => Ress,
        <<"block">> => <<"AQ">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<Add:2/binary, Ress/binary>>, Length) when Add =:= <<"IX">>; Add =:= <<"IB">>; Add =:= <<"IW">>; Add =:= <<"ID">> ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#81,
        <<"address">> => Ress,
        <<"block">> => Add,
        <<"address_start">> => calculate_address_started(Ress, false)
    };
analysis_address(<<"I", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#81,
        <<"address">> => Ress,
        <<"block">> => <<"I">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<Add:2/binary, Ress/binary>>, Length) when Add =:= <<"QX">>; Add =:= <<"QB">>; Add =:= <<"QW">>; Add =:= <<"QD">> ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#82,
        <<"address">> => Ress,
        <<"block">> => Add,
        <<"address_start">> => calculate_address_started(Ress, false)
    };
analysis_address(<<"Q", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#82,
        <<"address">> => Ress,
        <<"block">> => <<"Q">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<Add:2/binary, Ress/binary>>, Length) when Add =:= <<"MX">>; Add =:= <<"MB">>; Add =:= <<"MW">>; Add =:= <<"MD">> ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#83,
        <<"address">> => Ress,
        <<"block">> => Add,
        <<"address_start">> => calculate_address_started(Ress, false)
    };
analysis_address(<<"M", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#83,
        <<"address">> => Ress,
        <<"block">> => <<"M">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<Add:3/binary, Ress/binary>>, _) when Add =:= <<"DBX">>; Add =:= <<"DBB">>; Add =:= <<"DBW">>; Add =:= <<"DBD">> ->
    {true, Ress};
%% s7_protocol:analysis_address(<<"DB1.DBW70">>,1).
analysis_address(<<"DB", Ress/binary>>, Length) ->
    {Db_block, Temp_addr} =
        case binary:split(Ress, <<$.>>, [global, trim]) of
            [First, Second] ->
                NewSecond =
                    case analysis_address(Second, 1) of
                        {true, Second1} ->
                            Second1;
                        _ ->
                            Second
                    end,
                {dgiot_utils:to_int(First), NewSecond};
            [First, Second, Third] ->
                NewSecond =
                    case analysis_address(Second, 1) of
                        {true, Second1} ->
                            Second1;
                        _ ->
                            Second
                    end,
                {dgiot_utils:to_int(First), <<NewSecond/binary, ".", Third/binary>>};
            _ ->
                {0, Ress}
        end,
%%    io:format("~s ~p Temp_addr = ~p.~n", [?FILE, ?LINE, Temp_addr]),
%%    io:format("~s ~p calculate_address_started = ~p.~n", [?FILE, ?LINE, calculate_address_started(Temp_addr, false)]),
    #{
        <<"db_block">> => Db_block,
        <<"length">> => Length,
        <<"data_code">> => 16#84,
        <<"address">> => Temp_addr,
        <<"block_type">> => <<"DB">>,
        <<"address_start">> => calculate_address_started(Temp_addr, false)
    };

analysis_address(<<"T", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#1F,
        <<"address">> => Ress,
        <<"block">> => <<"T">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<"C", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 0,
        <<"length">> => Length,
        <<"data_code">> => 16#1E,
        <<"address">> => Ress,
        <<"block">> => <<"C">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(<<Add:2/binary, Ress/binary>>, Length) when Add =:= <<"VX">>; Add =:= <<"VB">>; Add =:= <<"VW">>; Add =:= <<"VD">> ->
    #{
        <<"db_block">> => 1,
        <<"length">> => Length,
        <<"data_code">> => 16#84,
        <<"address">> => Ress,
        <<"block">> => Add,
        <<"address_start">> => calculate_address_started(Ress, false)
    };
analysis_address(<<"V", Ress/binary>>, Length) ->
    #{
        <<"db_block">> => 1,
        <<"length">> => Length,
        <<"data_code">> => 16#84,
        <<"address">> => Ress,
        <<"block">> => <<"V">>,
        <<"address_start">> => calculate_address_started(Ress, false)
    };

analysis_address(_, _) ->
    #{<<"address">> => <<"0">>}.

format_value(Value, <<"byte">>) ->
    <<Value:8>>;

format_value(Value, <<"short">>) ->
    <<Value:16/signed-big-integer>>;

format_value(Value, <<"ushort">>) ->
    <<Value:16/unsigned-big-integer>>;

format_value(Value, <<"int32">>) ->
    <<Value:32/integer>>;

format_value(Value, <<"uint32">>) ->
    <<Value:32/integer>>;

format_value(Value, <<"int64">>) ->
    <<Value:64/integer>>;

format_value(Value, <<"uint64">>) ->
    <<Value:64/integer>>;

format_value(Value, <<"float">>) ->
    <<Value:32/float>>;

format_value(Value, <<"double">>) ->
    <<Value:64/float>>;

%%
format_value(Value, _) ->
    <<Value/binary>>.

get_block_value(DeviceId, Address, Originaltype) ->
    case dgiot_data:get({DeviceId, Address}) of
        {Block_address, Offset, Len} when Len > 0 ->
            case dgiot_data:get(dgiot_dbque, {DeviceId, Block_address}) of
                {Time, Startaddr, Buff} ->
                    {IntOffset, Bytes} =
                        case binary:split(Offset, <<$.>>, [global, trim]) of
                            [Addr, Num] ->
                                {dgiot_utils:to_int(Addr) - Startaddr, dgiot_utils:to_int(Num) + 1};
                            _ ->
                                {dgiot_utils:to_int(Offset) - Startaddr, 0}
                        end,
                    case catch Buff of
                        <<_:IntOffset/binary, V:Len/binary, _/binary>> ->
                            case parse_value(V, Originaltype, Bytes) of
                                null ->
                                    null;
                                Value ->
                                    {Time, Value}
                            end;
                        _ ->
                            null
                    end;
                _ ->
                    null

            end;
        _ ->
            null
    end.

parse_block(Buff, Props) ->
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"identifier">> := Identifier,
                <<"dataForm">> := #{
                    <<"protocol">> := <<"S7">>},
                <<"dataSource">> := #{
                    <<"address">> := Address,
                    <<"originaltype">> := Originaltype}
            } ->
                #{<<"address">> := Offset} = s7_protocol:analysis_address(Address, 1),
                {IntOffset, Bytes} =
                    case binary:split(Offset, <<$.>>, [global, trim]) of
                        [Addr, Num] ->
                            {dgiot_utils:to_int(Addr), dgiot_utils:to_int(Num) + 1};
                        _ ->
                            {dgiot_utils:to_int(Offset), 0}
                    end,
                {_, IntLen} = s7_protocol:get_len(Originaltype, 1),
%%                {_, IntOffsetLen} = s7_protocol:get_len(Originaltype, IntOffset),
                case IntOffset of
                    0 ->
                        <<V:IntLen/binary, _/binary>> = Buff,
                        case parse_value(V, Originaltype, Bytes) of
                            null ->
                                Acc;
                            Value ->
                                Acc#{Identifier => Value}
                        end;
                    _ ->
%%                        io:format("~s ~p IntOffset = ~p.", [?FILE, ?LINE, IntOffset]),
%%                        io:format("~s ~p IntLen = ~p.~n", [?FILE, ?LINE, IntLen]),
                        case catch Buff of
                            <<_:IntOffset/binary, V:IntLen/binary, _/binary>> ->
                                case parse_value(V, Originaltype, Bytes) of
                                    null ->
                                        Acc;
                                    Value ->
                                        Acc#{Identifier => Value}
                                end;
                            _ ->
                                Acc
                        end
                end;
            _ ->
                Acc
        end
                end, #{}, Props).

parse_value(<<>>, _, _) ->
    null;

parse_value(Buff, <<"byte">>, _) when size(Buff) >= 1 ->
    <<Value:8, _/binary>> = Buff,
    Value;

parse_value(Buff, <<"short">>, _) when size(Buff) >= 2 ->
    <<Value:16/signed-big-integer, _/binary>> = Buff,
    Value;

%% <<Value:16/unsigned-big-integer, _/binary>> = dgiot_utils:hex_to_binary(<<"f0ae">>).
parse_value(Buff, <<"ushort">>, _) when size(Buff) >= 2 ->
    <<Value:16/unsigned-big-integer, _/binary>> = Buff,
    Value;

%% dgiot_utils:hex_to_binary(<<"3f80000000000000">>)
parse_value(Buff, <<"int32">>, _) when size(Buff) >= 4 ->
    <<Value:32/integer, _/binary>> = Buff,
    Value;

parse_value(Buff, <<"uint32">>, _) when size(Buff) >= 4 ->
    <<Value:32/integer, _/binary>> = Buff,
    Value;

parse_value(Buff, <<"int64">>, _) when size(Buff) >= 8 ->
    <<Value:64/integer, _/binary>> = Buff,
    Value;

parse_value(Buff, <<"uint64">>, _) when size(Buff) >= 8 ->
    <<Value:64/integer, _/binary>> = Buff,
    Value;

parse_value(Buff, <<"float">>, _) when size(Buff) >= 4 ->
    case catch Buff of
        <<Value:32/float, _/binary>> ->
            Value;
        _ ->
            null
    end;

parse_value(Buff, <<"double">>, _) when size(Buff) >= 8 ->
    case catch Buff of
        <<Value:64/float, _/binary>> ->
            Value;
        _ ->
            null
    end;

%% <<Value:1/binary,_/binary>> = dgiot_utils:hex_to_binary(<<"f0ae">>).
parse_value(Buff, <<"string">>, _) ->
    <<Value:1/binary, _/binary>> = Buff,
    Value;

parse_value(Buff, <<"bool">>, Bytes) when Bytes > 0 ->
    NewBytes = 8 - Bytes,
    <<_:NewBytes, Value:1, _/bits>> = Buff,
    Value;

parse_value(Buff, _, _) ->
    <<Value:8, _/binary>> = Buff,
    Value.

%% 获取寄存器字节长度
get_len(<<"bool">>, Num) -> {bit, 1 * Num};
get_len(<<"byte">>, Num) -> {byte, 1 * Num};
get_len(<<"short">>, Num) -> {byte, 2 * Num};
get_len(<<"ushort">>, Num) -> {byte, 2 * Num};
get_len(<<"int32">>, Num) -> {byte, 4 * Num};
get_len(<<"uint32">>, Num) -> {byte, 4 * Num};
get_len(<<"int64">>, Num) -> {byte, 8 * Num};
get_len(<<"uint64">>, Num) -> {byte, 8 * Num};
get_len(<<"float">>, Num) -> {byte, 4 * Num};
get_len(<<"double">>, Num) -> {byte, 8 * Num};
get_len(<<"string">>, Num) -> {byte, 1 * Num};
get_len(_, Num) -> {byte, 1 * Num}.




