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

%% @doc dlt645 Protocol Processor.
-module(dlt645_proctol).
-author("johnliu").

-include_lib("dgiot_meter.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    parse_data_to_json/2,
    reverse/1,
    checkRateNumber/1,
    convert_byte_to_float_10/1,
    convert_byte_to_float_8_1h/1,
    convert_byte_to_float_8_1t/1]).


-record(di_data_A6, {di1, di2, di3, di4,di5}).

-record(di_645_data_97, {di1, di2, di3}).

binary_to_hex(Id) ->
    << <<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X,16)>>.

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
  reverse(Rest, <<H/binary, Acc/binary>>).
%% @doc Parse frame
-spec parse_data_to_json(binary(), binary()) ->
  {ok,  binary()} | {error, any()}.

%A1 Multi00  ff
parse_data_to_json(<<16#00, Di2:8, 16#FF, Di4:8>>,  Data) ->
%%  io:format("parse_data_to_json Data ~p ~n", [Data]),
  DataIndex = #di_645data{di1 = 16#00, di2 = Di2, di3 =16#00, di4 = Di4},
  Value = jsx:encode(split_a1_data({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#00, Di2, 16#FF, Di4])),
%%  io:format("00 zz ff zz parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};
%  9xxx   xxxxxx.xx
parse_data_to_json(<<Di1:4,Di2:4,Di3:8>>,  Data)
when(Di1==16#9)->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([<<Di1:4,Di2:4>>,Di3])),
%%  io:format("00 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%  Axxx xx.xxxx
parse_data_to_json(<<Di1:4,Di2:4,Di3:8>>,  Data)
  when(Di1==16#A)->
  Value = convert_byte_to_float_6_4(Data),
  Key = binary_to_hex(list_to_binary([<<Di1:4,Di2:4>>,Di3])),
%%  io:format("00 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A1
parse_data_to_json(<<16#00, Di2:8, Di3:8, Di4:8>>, Data) ->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([16#00, Di2, Di3, Di4])),
%%  io:format("00 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A1 00 zz zz ff
parse_data_to_json(<<16#00, Di2:8, Di3:8,16#FF>>, Data) ->
%%  io:format("parse_data_to_json Data ~p ~n", [Data]),
  DataIndex = #di_645data{di1 = 16#00, di2 = Di2, di3 =Di3, di4 = 16#00},
  Value = jsx:encode(split_a1_data_1({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#00, Di2, Di3, 16#FF])),
%%  io:format("00 zz zz ff parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};

%A2 Multi
parse_data_to_json(<<16#01, Di2:8, 16#FF, Di4:8>>,  Data) ->
  DataIndex = #di_645data{di1 = 16#01, di2 = Di2, di3 =16#00, di4 = Di4},
  Value = jsx:encode(split_a2_data_1({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#01, Di2, 16#FF, Di4])),
%%  io:format("01 .. FF ..  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A2
parse_data_to_json(<<16#01, Di2:1/binary, Di3:1/binary, Di4:1/binary>>,<<Data0:3/binary,Data1:5/binary>>) ->
  Value0 = convert_byte_to_float_6_4(Data0),
  Value1 = convert_time_to_st_YMDHM(Data1),
  ListDataIndex = list_to_binary([16#01, Di2, Di3, Di4,16#00]),
  ListDataIndex1 = list_to_binary([16#01, Di2, Di3, Di4,16#01]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  Value = jsx:encode([{HexDataIndex, Value0},{HexDataIndex1, Value1}]),
  Key = binary_to_hex(list_to_binary([16#01, Di2, Di3, Di4])),
%%  io:format("01   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A2 00 zz zz ff
parse_data_to_json(<<16#01, Di2:8, Di3:8,16#FF>>, Data) ->
  DataIndex = #di_645data{di1 = 16#01, di2 = Di2, di3 =Di3, di4 = 16#00},
  Value = jsx:encode(split_a2_data_2({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#01, Di2, Di3, 16#FF])),
%%  io:format("01 zz zz ff parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};

%A3 Multi 02 01/07 ff xxx.x
parse_data_to_json(<<16#02,Di2:8, 16#FF, Di4:8>>,  Data) when Di2==16#01 orelse Di2==16#07  ->
  DataIndex = #di_645data{di1 = 16#02, di2 = Di2, di3 =16#01, di4 = Di4},
  Value = jsx:encode(split_a3_data_1({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#02, Di2, 16#FF, Di4])),
  %% io:format("02 01/07 ff xxx.x parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A3  02 01/07  xxx.x
parse_data_to_json(<<16#02, Di2:8, Di3:1/binary, Di4:1/binary>>, Data)
  when Di2==16#01 orelse Di2==16#07->
  Value = convert_byte_to_float_4_1(Data),
  Key = binary_to_hex(list_to_binary([16#02, Di2, Di3, Di4])),
%%  io:format("02 01/07 xxx.x   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};
%A3 Multi 02 02 ff xxx.xxx
parse_data_to_json(<<16#02,16#02, 16#FF, Di4:8>>,  Data) ->
  DataIndex = #di_645data{di1 = 16#02, di2 = 16#02, di3 =16#01, di4 = Di4},
  Value = jsx:encode(split_a3_data_2({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#02, 16#02, 16#FF, Di4])),
%%  io:format("02  02 FF xxx.xxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%A3 02 02  xxx.xxx
parse_data_to_json(<<16#02, 16#02, Di3:1/binary, Di4:1/binary>>, Data) ->
  Value = convert_byte_to_float_6_3(Data),
  Key = binary_to_hex(list_to_binary([16#02, 16#02, Di3, Di4])),
%%  io:format("0202  xxx.xxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%A3 Multi 02 03/04/05  ff xx.xxxx
parse_data_to_json(<<16#02,Di2:8, 16#FF, Di4:8>>,  Data) when Di2==16#03 orelse Di2==16#04 orelse Di2==16#05 ->
  DataIndex = #di_645data{di1 = 16#02, di2 = Di2, di3 =16#01, di4 = Di4},
  Value = jsx:encode(split_a3_data_3({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#02, Di2, 16#FF, Di4])),
 %% io:format("02  03/04/05 FF xx.xxxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A3????????02 03/04/05   xx.xxxx
parse_data_to_json(<<16#02,Di2:8, Di3:1/binary, Di4:1/binary>>,Data)
  when Di2==16#03 orelse Di2==16#04 orelse Di2==16#05 ->
  Value =   Value = convert_byte_to_float_6_4(Data),
  Key = binary_to_hex(list_to_binary([16#02, Di2, Di3, Di4])),
 %% io:format("02 03/04/05  xx.xxxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%A3 Multi 02 06 ff x.xxx
parse_data_to_json(<<16#02,16#06, 16#FF, Di4:8>>,  Data)  ->
  DataIndex = #di_645data{di1 = 16#02, di2 = 16#06, di3 =16#01, di4 = Di4},
  Value = jsx:encode(split_a3_data_4({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#02, 16#06, 16#FF, Di4])),
  %% io:format("02  03/04/05 FF  x.xxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A3 02 06x.xxx
parse_data_to_json(<<16#02, 16#06, Di3:1/binary, Di4:1/binary>>, Data) ->
  Value = convert_byte_to_float_4_3(Data),
  Key = binary_to_hex(list_to_binary([16#02, 16#06, Di3, Di4])),
  %% io:format("02 06 x.xxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%A3 Multi 02 08/09 ff xx.xx
parse_data_to_json(<<16#02,Di2:8, 16#FF, Di4:8>>,  Data) when Di2==16#08 orelse Di2==16#09  ->
  DataIndex = #di_645data{di1 = 16#02, di2 = Di2, di3 =16#01, di4 = Di4},
  Value = jsx:encode(split_a3_data_5({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#02, Di2, 16#FF, Di4])),
  %% io:format("02 08/09 ff xx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A3  02 08/09  xx.xx
parse_data_to_json(<<16#02, Di2:8, Di3:1/binary, Di4:1/binary>>,Data)
  when Di2==16#08 orelse Di2==16#09->
  Value = convert_byte_to_float_4_2(Data),
  Key = binary_to_hex(list_to_binary([16#02, Di2, Di3, Di4])),
  %% io:format("02 08/09  xx.xx   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%A3 Multi 02 0A/0B 01-03 ff xx.xx
parse_data_to_json(<<16#02,Di2:8, Di3:8, 16#FF>>,  Data)  when
  (Di2 ==16#0A orelse Di2==16#0B) andalso (Di3==16#01 orelse Di3==16#02 orelse Di3==16#03)->
  DataIndex = #di_645data{di1 = 16#02, di2 = Di2, di3 =Di3, di4 = 16#01},
  Value = jsx:encode(split_a3_data_6({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#02, Di2, Di3, 16#FF])),
  %% io:format("02 0A/0B 01-03 ff xx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A3  02 0A/0B 01-03  xx.xx  xx.xx
parse_data_to_json(<<16#02, Di2:8, Di3:8, Di4:1/binary>>, Data) when
  (Di2 ==16#0A orelse Di2==16#0B) andalso (Di3==16#01 orelse Di3==16#02 orelse Di3==16#03) ->
  Value = convert_byte_to_float_4_2(Data),
  Key = binary_to_hex(list_to_binary([16#02, Di2, Di3, Di4])),
  %% io:format("02 0A/0B 01-03  xx.xx   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};


%A3 02 80 00 01   xxx.xxx
parse_data_to_json(<<16#02,16#80, 16#00, 16#01>>,Data) ->
  Value =  convert_byte_to_float_6_3(Data),
  Key = binary_to_hex(list_to_binary([16#02,16#80, 16#00, 16#01])),
  %% io:format(" 02 80 00 01   xxx.xxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%A3  02 80 00 02/08/09   xx.xx
parse_data_to_json(<<16#02, 16#80, 16#00, Di4:8>>, Data) when
  (Di4 ==16#02 orelse Di4==16#08 orelse Di4==16#09 ) ->
  Value = convert_byte_to_float_4_2(Data),
  Key = binary_to_hex(list_to_binary([16#02, 16#80, 16#00, Di4])),
  %% io:format("02 80 00 02/08/09   xx.xx   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%A3 02 80 00 03/04/05/06   xx.xxxx
parse_data_to_json(<<16#02,16#80, 16#00, Di4:8>>, Data) when
  (Di4 ==16#03 orelse Di4==16#04 orelse Di4==16#05 orelse Di4==16#06 ) ->
  Value = convert_byte_to_float_6_4(Data),
  Key = binary_to_hex(list_to_binary([16#02,16#80, 16#00, Di4])),
  %% io:format("02 80 00 03/04/05/06   xx.xxxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};
%A3  02 80 00 07  xxx.x
parse_data_to_json(<<16#02, 16#80, 16#00, 16#07>>, Data) ->
  Value = convert_byte_to_float_4_1(Data),
  Key = binary_to_hex(list_to_binary([16#02, 16#80, 16#00, 16#07])),
  %% io:format("02 80 00 07  xxx.x   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};
%A3  02 80 00 0A xxxxxxxx
parse_data_to_json(<<16#02, 16#80, 16#00, 16#0A>>, <<Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>) ->
  Value = Da1*10000000 +  Da0*1000000 +  Da3*100000 + Da2*10000 +Da5*1000+Da4*100+Da7*10+Da6,
  Key = binary_to_hex(list_to_binary([16#02, 16#80, 16#00, 16#0A])),
  %% io:format("02 80 00 0A  xxxxxxxx   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};
%A3  02 80 00 0B  xxxx.xxxx
parse_data_to_json(<<16#02, 16#80, 16#00, 16#0B>>, Data) ->
  Value = convert_byte_to_float_8_4(Data),
  Key = binary_to_hex(list_to_binary([16#02, 16#80, 16#00, 16#0B])),
  %% io:format("02 80 00 0B  xxxx.xxxx   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 01-0F 00 00   6*3
parse_data_to_json(<<16#03, Di2:8, 16#00, 16#00>>, Data) when  Di2 =< 16#0F ->
  Value = jsx:encode(split_a4_data_1({#di_data_A6{di1=16#03, di2=Di2, di3=16#00, di4=16#00,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, Di2, 16#00, 16#00])),
  %% io:format("03 01-0F 00 00   6*3   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 01-04 01-03  01-0A   n*n
parse_data_to_json(<<16#03, Di2:8, Di3:8, Di4:8>>, Data) when  (Di2> 0 andalso Di2 =< 16#04) andalso (Di3> 0 andalso Di3 =< 16#03) andalso (Di4> 0 andalso Di4=< 16#0A)->
  Value = jsx:encode(split_a4_data_2({#di_data_A6{di1=16#03, di2=Di2, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, Di2,Di3, Di4])),
  %% io:format("03 01-04 01-03  01-0A   n*n   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 05 00  01-0A   n*n
parse_data_to_json(<<16#03, 16#05, 16#00, Di4:8>>, Data) when  Di4=< 16#0A->
  Value = jsx:encode(split_a4_data_3({#di_data_A6{di1=16#03, di2=16#05, di3=16#00, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#05,16#00, Di4])),
  %% io:format("03 05 00  01-0A   n*3   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 06/11 00  01-0A   n*n
parse_data_to_json(<<16#03, Di2:8, 16#00, Di4:8>>, Data) when  (Di4 > 0 andalso Di4=< 16#0A) andalso  (Di2== 16#06 orelse Di2== 16#11)->
  Value = jsx:encode(split_a4_data_4({#di_data_A6{di1=16#03, di2=16#06, di3=16#00, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, Di2,16#00, Di4])),
  %% io:format("03 06/11 00  01-0A   n*n   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 07-08 00  01-0A   n*n
parse_data_to_json(<<16#03, Di2:8, 16#00, Di4:8>>, Data) when  Di4=< 16#0A andalso (Di2==7 orelse Di2 ==8) ->
  Value = jsx:encode(split_a4_data_5({#di_data_A6{di1=16#03, di2=Di2, di3=16#00, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, Di2,16#00, Di4])),
  %% io:format("03 07-08 00  01-0A   n*n   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 09-0A 00  01-0A   n*n
parse_data_to_json(<<16#03, Di2:8, 16#00, Di4:8>>, Data) when  Di4=< 16#0A andalso (Di2==9 orelse Di2 ==10)->
  Value = jsx:encode(split_a4_data_6({#di_data_A6{di1=16#03, di2=Di2, di3=16#00, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, Di2,16#00, Di4])),
  %% io:format("03 09 00  01-0A   n*n   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 0B-0D 01-03  01-0A   n*n
parse_data_to_json(<<16#03, Di2:8, Di3:8, Di4:8>>, Data) when  Di4=< 16#0A
  andalso (Di3==16#01 orelse Di3 ==16#02 orelse Di3 ==16#03)
  andalso (Di2==16#0B orelse Di2 ==16#0C orelse Di2 ==16#0D)->
  Value = jsx:encode(split_a4_data_2({#di_data_A6{di1=16#03, di2=Di2, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03,Di2,Di3, Di4])),
  %% io:format("03 0B-0D 01-03  01-0A   n*n  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 0E-0F 01-03  01-0A   n*n
parse_data_to_json(<<16#03, Di2:8, Di3:8, Di4:8>>, Data) when  Di4=< 16#0A
  andalso (Di3==16#01 orelse Di3 ==16#02 orelse Di3 ==16#03)
  andalso (Di2==16#0E orelse Di2 ==16#0F)->
  Value = jsx:encode(split_a4_data_5({#di_data_A6{di1=16#03, di2=Di2, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, Di2,Di3, Di4])),
  %% io:format("03 0E-0F 01-03  01-0A   n*n   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 10 00-03  00-0C   n*n
parse_data_to_json(<<16#03, 16#10, Di3:8, Di4:8>>, Data) when  Di4=< 16#0C andalso Di3=< 16#03 ->
  Value = jsx:encode(split_a4_data_7({#di_data_A6{di1=16#03, di2=16#10, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#10,Di3, Di4])),
  %% io:format(" 03 10 00-03  00-0C   n*n parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%% 97 A3
%%A3 B0-B9 10-6F mmddhhmm
parse_data_to_json(<<Di1:8,Di2:8>>, Data) when
(Di1==16#B0 andalso Di2>=16#10 andalso Di2=<16#2F)
orelse (Di1==16#B1 andalso Di2>=16#10 andalso Di2=<16#6F)
orelse (Di1==16#B4 andalso Di2>=16#10 andalso Di2=<16#2F)
orelse (Di1==16#B5 andalso Di2>=16#10 andalso Di2=<16#2F)
orelse (Di1==16#B8 andalso Di2>=16#10 andalso Di2=<16#6F)
orelse (Di1==16#B9 andalso Di2>=16#10 andalso Di2=<16#6F)
orelse (Di1==16#B2 andalso Di2==16#10 andalso Di2==16#11)
orelse (Di1==16#B3 andalso Di2>=16#30 andalso Di2=<16#33)
orelse (Di1==16#B3 andalso Di2>=16#40 andalso Di2=<16#43)->
  %% io:format(" A3 B0-B9 10-6F parse_data_to_json Di1 ~p Di2 ~p ~n", [Di1,Di2]),

 Value= convert_time_to_st_MDHM(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2])),
  %% io:format(" A3 B0-B9 10-6F parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%97 A4 B2 12/13 4*n
%%      B3 10-13
parse_data_to_json(<<Di1:8, Di2:8>>, Data)
  when (Di1==16#B2 andalso Di2==16#12 orelse Di2==16#13) orelse(Di1==16#B3 andalso Di2>=16#10 andalso Di2=<16#13)->
  Value= acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([Di1, Di2])),
  %% io:format("B2 12/13 acc parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%% 97 A4 B2 14 6*n
%%       B3 20-23
parse_data_to_json(<<Di1:8, Di2:8>>, Data)
  when (Di1==16#B2 andalso Di2==16#14) orelse (Di1==16#B3 andalso Di2==16#20 andalso Di2==16#21 andalso Di2==16#22 andalso Di2==16#23)->
  Value= acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([Di1,Di2])),
  %% io:format("04 acc parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%% 97 A4 B6 50-53 X.XXX
parse_data_to_json(<<16#B6, Di2:8>>, Data)
  when Di2==16#50 orelse Di2==16#51 orelse Di2==16#52 orelse Di2==16#53->
  Value = convert_byte_to_float_4_3(Data),
  Key = binary_to_hex(list_to_binary([16#B6,Di2])),
  %% io:format("B6 50-53 x.xxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%% 97 A4 B6 30-33 xx.xxxx
parse_data_to_json(<<16#B6,Di2:8>>,Data)
when Di2 == 16#30 orelse Di2== 16#31 orelse Di2== 16#32 orelse Di2== 16#33->
  Value = convert_byte_to_float_6_4(Data),
  Key = binary_to_hex(list_to_binary([16#B6,Di2])),
  %% io:format("  B6 30-33 xx.xxxx  parse_data_to_json Key ~p Value ~p ~n", [Key,Value]),
  {Key,Value};

 %% 97 A4 B6 34/35/40-43 XX.XX
  parse_data_to_json(<<16#B6,Di2:8>>,Data)
  when (Di2==16#34 orelse Di2==16#35 orelse Di2==16#40 orelse Di2==16#41 orelse Di2==16#42 orelse Di2==16#43) ->
  Value = convert_byte_to_float_4_2(Data),
  Key = binary_to_hex(list_to_binary([16#B6,Di2])),
  %% io:format("B6 34/35/40-43 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%%A4 => 03 12 00 00  3*n
parse_data_to_json(<<16#03,16#12 , 16#00, 16#00>>, Data)->
  Value = jsx:encode(split_a4_data_8({#di_data_A6{di1=16#03, di2=16#12 , di3=16#00, di4=16#00,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#12 , 16#00, 16#00])),
  %% io:format("03 12 00 00  3*n  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 11 00 00  xxxxxx
parse_data_to_json(<<16#03,16#11 , 16#00, 16#00>>, Data)->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#03, 16#11 , 16#00, 16#00])),
  %% io:format("03 11 00 00  3*n  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 12 01-06 01-0A
parse_data_to_json(<<16#03,16#12, Di3:8, Di4:8>>, Data) when  Di3 =< 16#06 andalso Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_9({#di_data_A6{di1=16#03, di2=16#12, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#12, Di3, Di4])),
  %% io:format("03 12 01-06 01-0A    parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 00-0E 00  3
parse_data_to_json(<<16#03, 16#30, Di3:8, 16#00>>, Data)
  when   (Di3 =< 16#0E) ->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, Di3, 16#00])),
  %% io:format("03 30 00-0E 00  3 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 00/03 01-0A
parse_data_to_json(<<16#03,16#30, Di3:8, Di4:8>>, Data) when   (Di3==16#00 orelse Di3==16#03) andalso Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_10({#di_data_A6{di1=16#03, di2=16#30, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, Di3, Di4])),
  %% io:format("03 30 00/03  01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 30 01 01-0A
parse_data_to_json(<<16#03,16#30, 16#01, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_11({#di_data_A6{di1=16#03, di2=16#30, di3=16#01, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#01, Di4])),
  %% io:format("03 30 01 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 02 01-0A
parse_data_to_json(<<16#03,16#30, 16#02, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_12({#di_data_A6{di1=16#03, di2=16#30, di3=16#02, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#02, Di4])),
  %% io:format("03 30 02 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 30 04 01-0A
parse_data_to_json(<<16#03,16#30, 16#04, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_13({#di_data_A6{di1=16#03, di2=16#30, di3=16#04, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#04, Di4])),
  %% io:format("03 30 04 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4 => 03 30 05 01-0A
parse_data_to_json(<<16#03,16#30, 16#05, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_14({#di_data_A6{di1=16#03, di2=16#30, di3=16#05, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#05, Di4])),
  %% io:format("03 30 05 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 06 01-0A
parse_data_to_json(<<16#03,16#30, 16#06, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_15({#di_data_A6{di1=16#03, di2=16#30, di3=16#06, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#06, Di4])),
  %% io:format("03 30 06 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 07 01-0A
parse_data_to_json(<<16#03,16#30, Di3:8, Di4:8>>, Data)
  when  (Di3==16#07 ) andalso Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_16({#di_data_A6{di1=16#03, di2=16#30, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, Di3, Di4])),
  %% io:format("03 30 07 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 09-0B 01-0A
parse_data_to_json(<<16#03,16#30, Di3:8, Di4:8>>, Data)
  when  ( Di3 == 16#09 orelse Di3 == 16#0A orelse Di3 == 16#0B) andalso Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_27({#di_data_A6{di1=16#03, di2=16#30, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, Di3, Di4])),
  %% io:format("03 30 09-0B 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 08 01-0A
parse_data_to_json(<<16#03,16#30, 16#08, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_17({#di_data_A6{di1=16#03, di2=16#30, di3=16#08, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#08, Di4])),
  %% io:format("03 30 08 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 0C 01-0A
parse_data_to_json(<<16#03,16#30, 16#0C, Di4:8>>, Data) when   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_18({#di_data_A6{di1=16#03, di2=16#30, di3=16#0C, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, 16#0C, Di4])),
  %% io:format("03 30 0C 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 30 0D/0E 01-0A
parse_data_to_json(<<16#03,16#30, Di3:8, Di4:8>>, Data) when (Di3 ==16#0D orelse Di3== 16#0E) andalso   Di4=< 16#0A ->
  Value = jsx:encode(split_a4_data_5({#di_data_A6{di1=16#03, di2=16#30, di3=Di3, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#03, 16#30, Di3, Di4])),
  %% io:format("03 30 0D/0E 01-0A   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 32/33 01 01-0A
parse_data_to_json(<<16#03, Di2:8, 16#01, Di4:8>>, Data)
  when (Di2 ==16#32 orelse Di2== 16#33)  andalso   Di4=< 16#0A ->
  Value = convert_time_to_st_YMDHM(Data),
  Key = binary_to_hex(list_to_binary([16#03, Di2, 16#01, Di4])),
  %% io:format(" 03 32/33 01 01-0A  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 32/33 02 01-0A
parse_data_to_json(<<16#03, Di2:8, 16#02, Di4:8>>, Data)
  when (Di2 ==16#32 orelse Di2== 16#33)   andalso   Di4=< 16#0A ->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#03, Di2, 16#02, Di4])),
  %% io:format(" 03 32/33 02 01-0A  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4 => 03 32/33 03-06 01-0A
parse_data_to_json(<<16#03, Di2:8, Di3:8, Di4:8>>, Data)
  when (Di2 ==16#32 orelse Di2== 16#33) andalso Di3 =<16#06  andalso   Di4=< 16#0A ->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([16#03, Di2, Di3, Di4])),
  %% io:format(" 03 32/33 03-06 01-0A  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%%A4 续
%%10   00-03  00 01/02 xxxxxx
parse_data_to_json(<<16#10, Di2:8, 16#00, Di4:8>>, Data)
  when (Di2 =< 16#03) andalso  Di4=< 16#02 ->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#10, Di2, 16#00, Di4])),
  %% io:format(" 10   00-03  00 01/02 xxxxxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10  00  01/02 01 YMDHMS
parse_data_to_json(<<16#10, 16#00, Di3:8,16#01>>, Data)
  when (Di3 == 16#01) orelse  Di3== 16#02 ->
  Value = convert_time_to_st_YMDHMS(Data),
  Key = binary_to_hex(list_to_binary([16#10, 16#00, Di3,16#01])),
  %% io:format(" 10  00  01/02 01  YMDHMS parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%A4续
%%10/11/12/13/18/1A/19   01-03  FF(01-35)  01-0A
parse_data_to_json(<<Di1:8, Di2:8, 16#FF,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03)  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A) andalso ( Di1 == 16#10 orelse  Di1== 16#18 orelse  Di1== 16#1A orelse  Di1== 16#19)->
  %% io:format("  10/18/1A/19   01-03 FF(01-35) 01-0A ",[]),
  Value = jsx:encode(split_a4_data_19({#di_data_A6{di1=Di1, di2=Di2, di3=16#01, di4=Di4},Data})),
  Key = binary_to_hex(list_to_binary([Di1, Di2, 16#FF,Di4])),
  %% io:format("  10/11/12/13/18/1A/19    01-03 FF(01-35) 01-0A  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10/11/12/13/18/1A/19   01-03  01-35  FF (01-0A)
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,16#FF>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03)  andalso (Di3 =< 16#35) andalso ( Di1 == 16#10 orelse  Di1== 16#18 orelse  Di1== 16#1A orelse  Di1== 16#19) ->
  Value = jsx:encode(split_a4_data_20({#di_data_A6{di1=Di1, di2=Di2, di3=Di3, di4=16#01},Data})),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,16#FF])),
  %% io:format("  10/11/12/13/18/1A/19    01-03  01-35  FF (01-0A) parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10/11/12/13/18/1A/19   01-03  01/25 01-0A YMDHMS
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03) andalso ((Di1 >= 16#10 andalso Di1 =< 16#13) orelse (Di1 >= 16#18 andalso Di1 =< 16#1A))
  andalso ( (Di1 == 16#10 andalso (Di3 == 16#01 orelse  Di3== 16#25)) orelse (Di1 == 16#18 andalso (Di3 == 16#01 orelse  Di3== 16#21)))
  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A) ->
  Value = convert_time_to_st_YMDHMS(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("  10/11/12/13/18/1A/19  01-03  01/25 01-0A YMDHMS parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10/11/12/13/18/1A/19   01-03  02-09... 01-0A xxxxxx.xx
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03) andalso ((Di1 >= 16#10 andalso Di1 =< 16#13) orelse (Di1 >= 16#18 andalso Di1 =< 16#1A))
  andalso ((Di3 >= 16#02 andalso  Di3 =< 16#09) orelse (Di3 >= 16#0F andalso  Di3 =< 16#12)
    orelse (Di3 >= 16#18 andalso  Di3 =< 16#1B)
    orelse (Di3 >= 16#21 andalso  Di3 =< 16#35))
  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A)->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("10/11/12/13/18/1A/19   01-03  02-09... 01-0A  xxxxxx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10/11/12/13/18/1A/19   01-03  0A/13/1C... 01-0A xxx.x
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03) andalso ((Di1 >= 16#10 andalso Di1 =< 16#13) orelse (Di1 >= 16#18 andalso Di1 =< 16#1A))
  andalso (Di3 == 16#0A orelse  Di3 == 16#13 orelse Di3 == 16#1C )
  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A)->
  Value = convert_byte_to_float_4_1(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("10/11/12/13/18/1A/19  01-03  0A/13/1C... 01-0A xxx.x parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10/11/12/13/18/1A/19   01-03  0B/14/1D... 01-0A xxx.xxx
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03) andalso ( (Di1 >= 16#10 andalso Di1 =< 16#13) orelse (Di1 >= 16#18 andalso Di1 =< 16#1A))
  andalso (Di3 == 16#0B orelse  Di3 == 16#14 orelse Di3 == 16#1D )
  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A)->
  Value = convert_byte_to_float_6_3(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("10/11/12/13/18/1A/19   01-03  0B/14/1D... 01-0A xxx.xxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%10/11/12/13/18/1A/19   01-03  0C0D/1516/1E1F... 01-0A xx.xxxx
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03) andalso ( (Di1 >= 16#10 andalso Di1 =< 16#13) orelse (Di1 >= 16#18 andalso Di1 =< 16#1A))
  andalso (Di3 == 16#0C orelse  Di3 == 16#15 orelse Di3 == 16#1E orelse Di3 == 16#0D orelse  Di3 == 16#16 orelse Di3 == 16#1F )
  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A)->
  Value = convert_byte_to_float_6_4(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("10/11/12/13/18/1A/19   01-03  0C0D/1516/1E1F... 01-0A xx.xxxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%10/11/12/13/18/1A/19   01-03  0E/17/20... 01-0A x.xxx
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when (Di2 == 16#01 orelse  Di2== 16#02 orelse  Di2== 16#03) andalso ( (Di1 >= 16#10 andalso Di1 =< 16#13) orelse (Di1 >= 16#18 andalso Di1 =< 16#1A))
  andalso (Di3 == 16#0E orelse  Di3 == 16#17 orelse Di3 == 16#20 )
  andalso (Di4 >= 16#01 andalso  Di4 =< 16#0A)->
  Value = convert_byte_to_float_4_3(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("10/11/12/13/18/1A/19   01-03  0E/17/20... 01-0A x.xxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%A4续
%%11-13/19/1A/1c  01-03     zz   FF  xxxxxx
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,16#FF>>, Data)
  when ( Di1 >= 16#11 andalso  Di1 =< 16#13 orelse Di1 == 16#19 orelse Di1 == 16#1A orelse Di1 == 16#1C)
  andalso (Di2>=16#01 andalso Di2=<16#03) andalso (Di3 =/= 16#FF) ->
  Value = jsx:encode(split_a4_data_21({#di_data_A6{di1=Di1, di2=Di2, di3=Di3, di4=16#01},Data})),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,16#FF])),
  %% io:format(" 11-13/19/1A/1c  01-03     zz   FF  xxxxxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%11-13/18-1c  01-03     00   01/02  xxxxxx
%%11-13/19/1A/1c  01-03     zz   01-0a  xxxxxx
%%14-17/1F        00     00   01/02  xxxxxx
%%1D-1E           00     00   01     xxxxxx
%%15 17         00     zz   01-0a     xxxxxx
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,Di4:8>>, Data)
  when ( ( Di1 >= 16#11 andalso  Di1 =< 16#13) orelse  (Di1 >= 16#18 andalso  Di1 =< 16#1C)) andalso (Di2>=16#01 andalso Di2=<16#03) andalso (Di3 == 16#00) andalso (Di4==16#01 orelse Di4==16#02)
  orelse ( Di1 >= 16#11 andalso  Di1 =< 16#13 orelse Di1 == 16#19 orelse Di1 == 16#1A orelse Di1 == 16#1C) andalso (Di2>=16#01 andalso Di2=<16#03) andalso (Di3 =/= 16#FF) andalso (Di4>=16#01 orelse Di4=<16#0A)
  orelse ((Di1 >= 16#14 andalso  Di1 =< 16#17) orelse Di1 == 16#1F) andalso (Di2==16#00) andalso (Di3 ==16#00) andalso (Di4==16#01 orelse Di4==16#02)
  orelse  (Di1 >= 16#1D andalso  Di1 =< 16#1E) andalso (Di2==16#00) andalso (Di3 ==16#00) andalso (Di4==16#01)
  orelse ( Di1 == 16#15 orelse Di1 == 16#17) andalso (Di2==16#00) andalso (Di3 =/= 16#FF) andalso (Di4>=16#01 orelse Di4=<16#0A) ->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("A4.... xxxxxx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%%A4续

%%1B/1c  01-03  FF  01-0A
%%14  00     FF  01-0A
parse_data_to_json(<<Di1:8, Di2:8, 16#FF,Di4:8>>, Data)
  when( (Di1 ==  16#14  andalso Di2 == 16#00) orelse ((Di1 ==  16#1B orelse Di1 == 16#1C ) andalso (Di2 >= 16#01 andalso Di2 =< 16#03)))
  andalso (Di4>=16#01 andalso Di4=<16#0A)->
  %% io:format("  14  00 and  1B/1c    01-03 FF(01-35) 01-0A ",[]),

  Value = jsx:encode(split_a4_data_22({#di_data_A6{di1 = Di1,di2=Di2,di3 = 16#01,di4 = Di4},Data})),
  Key = binary_to_hex(list_to_binary([Di1, Di2, 16#FF,Di4])),
  %% io:format("14  00 and  1B/1c  01-03  FF  01-0A  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%1B/1c  01-03  zz  FF
parse_data_to_json(<<Di1:8, Di2:8, Di3:8,16#FF>>, Data)
  when(((Di1 ==  16#1B orelse Di1 == 16#1C)  andalso (Di2 >= 16#01 andalso Di2 =< 16#03)))->
  Value = jsx:encode(split_a4_data_24({#di_data_A6{di1 = Di1,di2=Di2,di3 = Di3,di4 = 16#01},Data})),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,16#FF])),
  %% io:format("1B/1c  01-03  zz  FF  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%1B /1c 01-03  01/12  01-0A YMDHMS
%%14  00      01/12 01-0A YMDHMS
parse_data_to_json(<<Di1:8,  Di2:8, Di3:8,Di4:8>>, Data)
  when( (Di1 ==  16#14  andalso Di2 == 16#00) orelse ((Di1 ==  16#1B orelse Di1 == 16#1C) andalso (Di2 >= 16#01 andalso Di2 =< 16#03)))
  andalso (Di4>=16#01 andalso Di4=<16#0A) andalso (Di3 == 16#01 orelse  Di3 == 16#12)->
  Value = convert_time_to_st_YMDHMS(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("14  00 /1B/1c  01-03   01/12 01-0A YMDHMS parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%1B/1c  01-03   01-22 01-0A xxxxxx.xx
%%14  00      01-22 01-0A xxxxxx.xx
parse_data_to_json(<<Di1:8,  Di2:8, Di3:8,Di4:8>>, Data)
  when( (Di1 ==  16#14  andalso Di2 == 16#00) orelse ((Di1 ==  16#1B orelse Di1 == 16#1C) andalso (Di2 >= 16#01 andalso Di2 =< 16#03)))
  andalso (Di4>=16#01 andalso Di4=<16#0A) andalso (Di3 =/= 16#01 andalso  Di3 =/= 16#12)->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2, Di3,Di4])),
  %% io:format("14  00  /1B/1c  01-03     01-22 01-0A xxxxxx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};




%%1D/1E/1F   00     FF  01-0A
parse_data_to_json(<<Di1:8,16#00, 16#FF,Di4:8>>, Data)
  when( Di1 ==  16#1D  orelse Di1 == 16#1E orelse Di1 ==  16#1F )  andalso (Di4>=16#01 andalso Di4=<16#0A) ->
  Value = jsx:encode(split_a4_data_25({#di_data_A6{di1 = Di1,di2=16#00,di3 = 16#01,di4 = Di4},Data})),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, 16#FF,Di4])),
  %% io:format("1D/1E/1F   00     FF  01-0A parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};
%%1D/1E/1F  00  zz  FF
parse_data_to_json(<<Di1:8, 16#00, Di3:8,16#FF>>, Data)
  when( Di1 ==  16#1D  orelse Di1 == 16#1E orelse Di1 ==  16#1F )  ->
  Value = jsx:encode(split_a4_data_26({#di_data_A6{di1 = Di1,di2=16#00,di3 = Di3,di4 = 16#01},Data})),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,16#FF])),
  %% io:format("1D/1E/1F  00  zz  FF  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%1D/1E/1F  00    01   01-0A YMDHMS
parse_data_to_json(<<Di1:8,  16#00, Di3:8,Di4:8>>, Data)
  when( ((Di1 ==  16#1D  orelse Di1 == 16#1E) andalso Di3 ==16#01) orelse (Di1 ==  16#1F  andalso (Di3 == 16#01 orelse Di3 == 16#06)))
  andalso (Di4>=16#01 andalso Di4=<16#0A) ->
  Value = convert_time_to_st_YMDHMS(Data),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,Di4])),
  %% io:format("1D/1E/1F  00 01 01-0A YMDHMS parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%1D/1E/  00    02   01-0A c0c1c2c3
parse_data_to_json(<<Di1:8,  16#00, Di3:8,Di4:8>>, Data)
  when    ((Di1 ==  16#1D  orelse Di1 == 16#1E) andalso Di3 ==16#02)
  andalso (Di4>=16#01 andalso Di4=<16#0A) ->
  Value = convert_to_hex(Data,[]),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,Di4])),
  %% io:format("1D/1E/  00    02   01-0A c0c1c2c3 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%1D/1E/1F  00       01-0A xxxxxx.xx
parse_data_to_json(<<Di1:8,  16#00, Di3:8,Di4:8>>, Data)
  when( Di1 ==  16#1D  orelse Di1 == 16#1E orelse Di1 ==  16#1F )
  andalso (Di4>=16#01 andalso Di4=<16#0A) ->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,Di4])),
  %% io:format("1D/1E/1F  00       01-0A xxxxxx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%%
%%16  00  FF  01-0A
parse_data_to_json(<<Di1:8, 16#00, 16#FF,Di4:8>>, Data)
  when Di1 ==  16#16  andalso (Di4>=16#01 andalso Di4=<16#0A)->
  Value = jsx:encode(split_a4_data_23({#di_data_A6{di1 = Di1,di2=16#00,di3 = 16#01,di4 = Di4},Data})),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, 16#FF,Di4])),
  %% io:format("16  00  FF  01-0A  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%16  00    zz  01/13 01-0A YMDHMS
parse_data_to_json(<<Di1:8, 16#00, Di3:8,Di4:8>>, Data)
  when Di1 ==  16#16 andalso (Di4>=16#01 andalso Di4=<16#0A) andalso (Di3 == 16#01 orelse  Di3 == 16#13)->
  Value = convert_time_to_st_YMDHMS(Data),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,Di4])),
  %% io:format("16  00  zz  01/13 01-0A YMDHMS parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%16  00    zz  12 01-0A xxxx.xx
parse_data_to_json(<<Di1:8, 16#00, Di3:8,Di4:8>>, Data)
  when Di1 ==  16#16  andalso (Di4>=16#01 andalso Di4=<16#0A) andalso ( Di3 == 16#12)->
  Value = convert_byte_to_float_6_2(Data),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,Di4])),
  %% io:format("16  00    zz 12 01-0A xxxx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%16  00    zz  01-22 01-0A xxxxxx.xx
parse_data_to_json(<<Di1:8, 16#00, Di3:8,Di4:8>>, Data)
  when Di1 ==  16#16  andalso (Di4>=16#01 andalso Di4=<16#0A) andalso (Di3 =/= 16#01 andalso  Di3 =/= 16#12 andalso  Di3 =/= 16#13)->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([Di1, 16#00, Di3,Di4])),
  %% io:format("16  00    zz  01/12 01-0A xxxxxx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 01 01
parse_data_to_json(<<16#04, 16#00,16#01, 16#01>>, <<Da7:4, Da6:4,Data:3/binary>>) ->
  S= convert_time_to_st_YMD(Data),
  W = Da6+Da7*10,
  DataIndex1 = binary_to_hex(list_to_binary([16#04, 16#00,16#01, 16#01, 16#00])),
  DataIndex2 = binary_to_hex(list_to_binary([16#04, 16#00,16#01, 16#01, 16#01])),
  %% io:format("04 00 01 01 DataIndex1 ~p,DataIndex2 ~p ~n",[DataIndex1,DataIndex2]),
  Value = jsx:encode([{DataIndex1, S},{DataIndex2, W}]),
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#01, 16#01])),
  %% io:format("04 00 01 01  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A5=>  04 00 01 02
parse_data_to_json(<<16#04, 16#00,16#01, 16#02>>, <<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>) ->
  Value= (Da1*10+Da0) *60*60 + (Da3*10+Da2)*60+Da5*10+Da4,
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#01, 16#02])),

  %% io:format("04 00 01 02  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 13 03
parse_data_to_json(<<16#04, 16#00,16#13, 16#03>>, << Da3:4, Da2:4, Da1:4, Da0:4>>) ->
  Value=  (Da1*10+Da0)*60+Da3*10+Da2,
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#13, 16#03])),

  %% io:format("04 00 13 03  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 01 06-09/04001201
parse_data_to_json(<<16#04, 16#00,Di3:8, Di4:8>>,Data)
  when (Di3 == 16#01 andalso (Di4 >= 16#06 andalso Di4 =<16#09))
  orelse (Di3 == 16#12 andalso Di4==16#01)->
  Value =convert_time_to_st_YMDHM(Data),
  Key = binary_to_hex(list_to_binary([16#04, 16#00, 16#01, Di4])),
  %% io:format("04 00 01   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 05 01
parse_data_to_json(<<16#04, 16#00,16#05, 16#01>>,Data) ->
  Value= convert_byte_to_float_4(Data),
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#05, 16#01])),

  %% io:format("04 00 05 01  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>
%%04 00 0B 01-03 DDHH
parse_data_to_json(<<16#04, 16#00,16#0B, Di4:8>>, Data)
  when Di4 =< 16#03 andalso Di4>= 16#01->
  Value= convert_time_to_st_DH(Data),
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#0B, Di4])),
  %% io:format("04 00 0B 01-03 DDHH parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 0D  x.xxx
parse_data_to_json(<<16#04, 14#00,16#0D, Di4:1/binary>>, Data) ->
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#0D, Di4])),
  Value = convert_byte_to_float_4_3(Data),
  %% io:format("Value ~p ~n",[Value]),
  %% io:format("04 00 0D  x.xxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 0F/10 xxxxxx.xx
parse_data_to_json(<<16#04, 14#00,Di3:8, Di4:1/binary>>, Data) when Di3 == 16#0F orelse Di3==16#10->
  Key = binary_to_hex(list_to_binary([16#04, 16#00,Di3, Di4])),
  Value = convert_byte_to_float_8_2(Data),
  %% io:format("04 00 0F/10 xxxxxx.xx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 00 0E xx.xxxx
parse_data_to_json(<<16#04, 14#00,16#0E, Di4:8>>,Data) when Di4 == 16#01 orelse Di4== 16#02->
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#0E, Di4])),
  Value = convert_byte_to_float_6_4(Data),
  %% io:format("  04 00 0E 01/02 xx.xxxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A5=>  04 00 0E xxx.x
parse_data_to_json(<<16#04, 14#00,16#0E, Di4:1/binary>>,Data) when Di4 == 16#03 orelse Di4== 16#04->
  Key = binary_to_hex(list_to_binary([16#04, 16#00,16#0E, Di4])),
  Value = convert_byte_to_float_4_1(Data),
%%   io:format("  04 00 0E  03/04 xxx.x  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};



%A5=> 04 01/02 00  00 mmdd nn
parse_data_to_json(<<16#04, Di2:8,16#00, 16#00>>, Data) when Di2 == 16#01 orelse Di2 == 16#02->
  Value = jsx:encode(split_a5_data_1({#di_data_A6{di1=16#04, di2=Di2, di3=16#00, di4=16#00,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#04, Di2,16#00, 16#00])),
 %%  io:format("04 00 01/02 00 mmdd  nn   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=> 04 01/02 00  01-08 hhmm nn
parse_data_to_json(<<16#04, Di2:8,16#00, Di4:1/binary>>, Data) when Di2 == 16#01 orelse Di2 == 16#02 ->
  Value = jsx:encode(split_a5_data_2({#di_data_A6{di1=16#04, di2=Di2, di3=16#00, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#04, Di2,16#00, Di4])),
 %%  io:format("04 01/02 00 01-08 hhmm nn  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=> 04 03 00  01-08 YYMMDD nn
parse_data_to_json(<<16#04, 16#03,16#00, Di4:1/binary>>, Data)  ->
  Value = jsx:encode(split_a5_data_3({#di_data_A6{di1=16#04, di2=16#03, di3=16#00, di4=Di4,di5=16#00},Data})),
  Key = binary_to_hex(list_to_binary([16#04, 16#03,16#00, Di4])),
 %%  io:format("04 03 00 01-08 mmddnn   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A5=> 04 05 01/02  01-3F nnnn.nnnn
parse_data_to_json(<<16#04,16#05, Di3:1/binary, Di4:1/binary>>, Data)  ->
  Key = binary_to_hex(list_to_binary([16#04, 16#05,Di3, Di4])),
  Value = convert_byte_to_float_8_4(Data),
 %%  io:format(" 04 05 01/02  01-3F nnnn.nnnn   parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A5=> 04 06 01/03  01-3F nnnn.nnnn
parse_data_to_json(<<16#04,16#06, Di3:8, Di4:1/binary>>, Data)
  when Di3 == 16#01 orelse Di3==16#03->
  Key = binary_to_hex(list_to_binary([16#04, 16#06,Di3, Di4])),
  Value = convert_byte_to_float_8_4(Data),
 %%  io:format("04 06 01/03  01-3F nnnn.nnnn  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A5=>  04 06 00/02 xxxxxx.xx
parse_data_to_json(<<16#04, 14#06,Di3:8, Di4:1/binary>>, Data)
  when Di3 == 16#00 orelse Di3==16#02->
  Key = binary_to_hex(list_to_binary([16#04, 14#06,Di3, Di4])),
  Value =convert_byte_to_float_8_2(Data),
 %%  io:format("04 06 00/02 xxxxxx.xx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 09 xxx.x
parse_data_to_json(<<16#04, 14#09,Di3:8, Di4:8>>, Data)
  when ( Di4 == 16#01 andalso( Di3 =< 16#04 orelse Di3==16#0C orelse (Di3 >= 16#07 andalso Di3 =< 16#09)))
  orelse ( Di4 == 16#02 andalso ( Di3  == 16#01 orelse Di3==16#01) )->
  Key = binary_to_hex(list_to_binary([16#04, 14#09,Di3, Di4])),
  Value = convert_byte_to_float_4_1(Data),
%%  io:format("04 09 xxx.x  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A5=>  04 09  xx.xxxx
parse_data_to_json(<<16#04, 14#09,Di3:8, Di4:8>>, Data)
  when ( Di3 == 16#01 andalso Di4==16#03) orelse (Di3 == 16#04 andalso Di4==16#02)
  orelse (Di3 == 16#07 andalso (Di4==16#02 orelse Di4 == 16#03))
  orelse (Di3 == 16#0D andalso (Di4==16#01 orelse Di4 == 16#02))
  orelse (Di3 == 16#09 andalso Di4==16#02)   orelse (Di3 == 16#0A andalso Di4==16#01)   orelse (Di3 == 16#0B andalso Di4==16#01) ->
  Key = binary_to_hex(list_to_binary([16#04, 14#09,Di3, Di4])),
  Value = convert_byte_to_float_6_4(Data),
%%  io:format("04 09  xx.xxxx  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A5=>  04 09  xx.xx
parse_data_to_json(<<16#04, 14#09,Di3:8, Di4:8>>,Data)
  when( Di3 == 16#05 andalso Di4==16#01) orelse (Di3 == 16#06 andalso Di4==16#01)  orelse (Di3 == 16#0F andalso Di4==16#01) ->
  Key = binary_to_hex(list_to_binary([16#04, 14#09,Di3, Di4])),
  Value = convert_byte_to_float_4_2(Data),
%%  io:format("04 09  xx.xx parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%%状态字
%%04   00  05 01-07
%%   00  06 01-03
%%   00  07 01-05
%%   00  08 01
%%   00  11 01
%%   00  09 01-06
%%数据项
%%   04  01 01-FE
%%   04  02 01-FE
%%          0C 01-0A mima

%%          04 0102 0E
parse_data_to_json(<<16#04, Di2:8,Di3:8, Di4:8>>, Data)
  when (Di2 == 16#04 andalso (Di3 == 16#01 orelse Di3 == 16#02 ) andalso (Di4>=16#01 andalso Di4 =< 16#FE))
  orelse (Di3 == 16#0C andalso (Di4 =< 16#0A andalso Di4>=16#01))
  orelse (Di2 == 16#00 andalso (Di3 == 16#05 orelse Di3 == 16#06 orelse Di3 == 16#07 orelse Di3 == 16#09 orelse (Di3 == 16#08 andalso Di4 == 16#01)orelse (Di3 == 16#11 andalso Di4 == 16#01)))
  orelse (Di3 == 16#04 andalso (Di4 == 16#01 orelse Di4==16#02 orelse Di4==16#0E ))->
%%  Value= convert_to_hex(Data,[]),
  Value= convert_to_bits(Data),
  Key = binary_to_hex(list_to_binary([16#04, Di2,Di3, Di4])),
%%  io:format("04 status parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%%ASCII
%%   00  04 01-0E - 0102 09 0A0E
%%   00  05 01-07
%%   00  06 01-03
%%   00  07 01-05
%%   00  08 01
%%   00  11 01
%%   00  09 01-06
% 04 80 00 01-03/32byte
parse_data_to_json(<<16#04, Di2:8,Di3:8, Di4:8>>, Data)
  when (Di2 == 16#00 andalso Di3 == 16#04 andalso ((Di4 >= 16#03 andalso Di4 =< 16#08) orelse(Di4>=16#0B andalso Di4=<16#0D) ))
  orelse (Di2 == 16#80 andalso Di3 == 16#00 andalso (Di4 >= 16#01 andalso Di4 =< 16#03)) ->
  Value= convert_to_ascii(Data,[]),
  Key = binary_to_hex(list_to_binary([16#04, Di2,Di3, Di4])),
%%  io:format("04 ASCII parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};% 04 80 00 01-03/32byte


%A5 =>04 00 01 05
%%          02parse_data_to_json(<<16#04, Di2:8,Di3:8, Di4:8>>, Data)

%%          03 06 07
%%          04 09 0A
%%          05 01-07
%%          0A 02-07
%%
%%          12 03
%%          14 01
parse_data_to_json(<<16#04, Di2:8,Di3:8, Di4:8>>, Data)
  when (Di2 == 16#00 andalso
    ((Di3 == 16#01 andalso Di4 == 16#05) orelse  (Di3 == 16#02) orelse (Di3 == 16#03 andalso (Di4 == 16#06 orelse Di4==16#07))
      orelse (Di3 == 16#12 andalso Di4 == 16#03) orelse  (Di3 == 16#14 andalso Di4 == 16#01 )
      orelse (Di3 == 16#04 andalso (Di4 == 16#09 orelse Di4==16#0A))
      orelse (Di3 == 16#05 andalso (Di4 =< 16#07 orelse Di4>=16#01))
      orelse (Di3 == 16#0A andalso (Di4 =< 16#07 orelse Di4>=16#02))) )->
  Value= acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#04, Di2,Di3, Di4])),
%%  io:format("04 acc parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};




%A5=>  04 xx
parse_data_to_json(<<16#04, Di2:1/binary,Di3:1/binary, Di4:1/binary>>, << Da1:4, Da0:4>>) ->
  Value= (Da1*10+Da0),
  Key = binary_to_hex(list_to_binary([16#04, Di2,Di3, Di4])),

%%  io:format("04  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};


%A6=> multi  05 04 FF 01-FE
parse_data_to_json(<<16#05, 16#04, 16#FF, Di4:1/binary>>, Data) ->
%%   io:format("parse_data_to_json Data ~p ~n", [Data]),
  DataIndex = #di_645data{di1 = 16#05, di2 =  16#04, di3 =16#01, di4 = Di4},
  Value = jsx:encode(split_a1_data({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#05, 16#04, 16#FF, Di4])),
%%  io:format("05 04 FF 01-FE parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};
%A6=> 05 04 00 01-FE
parse_data_to_json(<<16#05, 16#04, 16#00, Di4:1/binary>>, Data) ->
  Value =convert_time_to_st_YMDHM(Data),
  Key = binary_to_hex(list_to_binary([16#05, 16#04, 16#00, Di4])),
%%  io:format("05 04 00 01-FE  Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%A6=> 05 04 01-FE 01-FE
parse_data_to_json(<<16#05, 16#04, Di3:1/binary,  Di4:1/binary>>, Data) ->
  Value = convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([16#05, 16#04, Di3, Di4])),
%%  io:format("05 04 01-FE 01-FE Key ~p Value ~p ~n", [Key, Value]),

  {Key,Value};

%%%%A6 => 05 00 FF 01-0D
%%parse_data_to_json(<<16#05,16#00, 16#FF, Di4:1/binary>>, Data) ->
%%  DataIndex = #di_645data{di1 = 16#05, di2 =  16#00, di3 =16#01, di4 = Di4},
%%  Value = jsx:encode(split_a6_data_1({DataIndex, Data})),
%%  Key = binary_to_hex(list_to_binary([16#05, 16#00, 16#FF, Di4])),
%%  io:format("  05 00 FF 01-0D parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
%%  {Key,Value};

%A6=>  05 00-07 00 01-0D/0C-3C (06-3E)
parse_data_to_json(<<16#05, Di2:1/binary,16#00, Di4:1/binary>>, Data) ->
  Value= convert_time_to_st_YMDHM(Data),
  Key = binary_to_hex(list_to_binary([16#05, Di2, 16#00, Di4])),
%%  io:format("  05 00-07 00 01-0D/0C-3C parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%A6=> 05 00-07 10 01-0D/0C-3C
parse_data_to_json(<<16#05, Di2:1/binary,16#10, Di4:1/binary>>, Data) ->
  DataIndex = #di_data_A6{di1 = 16#05, di2 =  Di2, di3 =16#10, di4 = Di4, di5 = 16#00},
  Value = jsx:encode(split_a6_data_3({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#05, Di2, 16#10, Di4])),
%%  io:format("05 00-07 10 01-0D/0C-3C parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};

%A6=> 05 00-07 01-08 01-0D/0C-3C
parse_data_to_json(<<16#05, Di2:1/binary,Di3:8, Di4:1/binary>>, Data) when Di3 >= 1 andalso Di3 =<8->
  DataIndex = #di_data_A6{di1 = 16#05, di2 =  Di2, di3 =Di3, di4 = Di4, di5 = 16#00},
  Value = jsx:encode(split_a6_data_4({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#05, Di2, Di3, Di4])),
%%  io:format("parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};


%A6=> 05 00-07 09-0A  01-0D/0C-3C
parse_data_to_json(<<16#05, Di2:1/binary,Di3:8, Di4:1/binary>>, Data) when Di3 == 9 orelse Di3 == 10->
%%   io:format("parse_data_to_json Data ~p ~n", [Data]),
  DataIndex = #di_data_A6{di1 = 16#05, di2 =  Di2, di3 =Di3, di4 = Di4, di5 = 16#00},
  Value = jsx:encode(split_a6_data_8({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([16#05, Di2, Di3, Di4])),
%%  io:format("parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};

%%A7 =>06 00-06 00 00 nn
parse_data_to_json(<<16#06, Di2:1/binary,16#00, 16#00>>, Data) ->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#06, Di2,16#00, 16#00])),
  %% io:format("06 00-06 00 00 nn parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};
%%A7 =>06 00-06 00 01 YYMMDDHHMMNN
parse_data_to_json(<<16#06, Di2:1/binary,16#00, 16#01>>, <<Data:5/binary,Data1:8>>) ->
  Value1 = convert_time_to_st_YMDHM(Data),
  Value2= acc(Data1,0,1),
  DataIndex1 = binary_to_hex(list_to_binary([16#06, Di2,16#00, 16#01, 16#00])),
  DataIndex2 = binary_to_hex(list_to_binary([16#06, Di2,16#00, 16#01, 16#01])),
 %%  io:format("06 00-06 00 01 DataIndex1 ~p,DataIndex2 ~p ~n",[DataIndex1,DataIndex2]),
  Value = jsx:encode([{DataIndex1, Value1},{DataIndex2, Value2}]),
  Key = binary_to_hex(list_to_binary([16#06, Di2,16#00, 16#01])),
%%   io:format("06 00-06 00 01 parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};
%%A7 =>06 00-06 00 02  01
parse_data_to_json(<<16#06, Di2:1/binary,16#00, 16#02>>, Data) ->
  Value = acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([16#06, Di2,16#00, 16#02])),
 %%  io:format("06 00-06 00 02  parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};



%97 A5=>  C0 10   YYMMDDWW
parse_data_to_json(<<16#C0, 16#10>>, <<Da7:4, Da6:4,Data:3/binary>>) ->
  S= convert_time_to_st_YMD(Data),
  W = Da6+Da7*10,
  DataIndex1 = binary_to_hex(list_to_binary([16#C0, 16#10, 16#00])),
  DataIndex2 = binary_to_hex(list_to_binary([16#C0, 16#10, 16#01])),
 %%  io:format("C0 10 DataIndex1 ~p,DataIndex2 ~p ~n",[DataIndex1,DataIndex2]),
  Value = jsx:encode([{DataIndex1, S},{DataIndex2, W}]),
  Key = binary_to_hex(list_to_binary([16#C0, 16#10])),
%%   io:format("C0 10 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5=> C0 11
parse_data_to_json(<<16#C0, 16#11>>, <<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>) ->
  Value= (Da1*10+Da0) *60*60 + (Da3*10+Da2)*60+Da5*10+Da4,
  Key = binary_to_hex(list_to_binary([16#C0, 16#11])),

%%   io:format("C0 11 parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5 1byte 状态字、数据项
%C0  20/21/22/32/33/34
%C1  11~16/18
%C2  12 密码
%C3  10~14
%C4  1E
parse_data_to_json(<<Di2:8,Di1:8>>, Data)
  when (Di2 == 16#C0 andalso (Di1 == 16#20 orelse Di1 == 16#21 orelse Di1 == 16#22 orelse Di1 == 16#32 orelse Di1 == 16#33 orelse Di1 == 16#34))
  orelse (Di2 == 16#C1 andalso ((Di1 =< 16#16 andalso Di1>=16#11) orelse Di1 == 16#18))
  orelse (Di2 == 16#C2 andalso Di1 == 16#12)
  orelse (Di2 == 16#C3 andalso (Di1 =< 16#14 andalso Di1 >= 16#10 ))
  orelse (Di2 == 16#C4 andalso Di1 == 16#1E )->
  Value= convert_to_hex(Data,[]),
  Key = binary_to_hex(list_to_binary([Di2,Di1])),
 %%  io:format("97A5 status parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5  C0  30/31
      %C2  11
      %C5  11
      %C8  D0
parse_data_to_json(<<Di1, Di2:8>>, Data)
  when (Di1 == 16#C0 andalso (Di2 == 16#30 orelse Di2 ==16#31))
  orelse (Di1 == 16#C2 andalso Di2 == 16#11)
  orelse (Di1 == 16#C5 andalso Di2 == 16#11)
  orelse (Di1 == 16#C8 andalso Di2 == 16#D0) ->
  Value= acc(Data,0,1),
  Key = binary_to_hex(list_to_binary([Di1, Di2])),
 %%  io:format("97A5 acc parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5=>  C1 17 DDHH
parse_data_to_json(<<16#C1, 16#17>>, Data)->
  Value= convert_time_to_st_DH(Data),
  Key = binary_to_hex(list_to_binary([16#C1, 16#17])),
 %%  io:format("C1 17 DDHH parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5=>  C1 19/1A xxxxxx.x     此处只有7位数，需确定哪位要舍去！！！！
%convert_byte_to_float_8_1h    尾部舍去
%convert_byte_to_float_8_1t    头部舍去
parse_data_to_json(<<16#C1, Di1:8>>, Data) when Di1 == 16#19 orelse Di1==16#1A->
  Key = binary_to_hex(list_to_binary([16#C1, Di1])),
  Value = convert_byte_to_float_8_2(Data),    %小数点后两位
  %% io:format("C1 19/1A xxxxxx.x  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5=> C3 31~3F/41~4F……A1~AF hhmmNN
parse_data_to_json(<<16#C3, Di2:8>>, Data) when Di2 >= 16#31 andalso Di2 =< 16#AF  ->
  Value = convert_time_to_st_hhmmNN(Data),
  Key = binary_to_hex(list_to_binary([16#C3, Di2])),
  %% io:format("C3 31-AF hhmm nn  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5=> C3 20~2F
%       C4 11~1D/1F   MMDDNN
parse_data_to_json(<<Di1:8, Di2:8>>, Data) when (Di1 == 16#C3 andalso Di2 >= 16#20 andalso Di2 =< 16#2F)
orelse (Di1 ==16#C4 andalso ((Di2 >= 16#11 andalso Di2 =< 16#1D) orelse Di2 == 16#1F)) ->
  Value = convert_time_to_st_MMDDNN(Data),
  Key = binary_to_hex(list_to_binary([Di1, Di2])),
 %%  io:format("C3 20-2F/C4 11-1D/1F mmdd nn  parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A5=> C5 10 MMDDHHmm
parse_data_to_json(<<16#C5, 16#10>>, Data) ->
  Value= convert_time_to_st_MDHM(Data),
  Key = binary_to_hex(list_to_binary([16#C5, 16#10])),
 %%  io:format("C5 10 MMDDHHmm parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

%97A6=> D1/D2 10
parse_data_to_json(<<Di1:8, 16#10>>, Data) when Di1 == 16#D1 orelse Di1 == 16#D2 ->
  DataIndex = #di_645_data_97{di1 = Di1, di2 = 16#10 , di3 = 16#00},
  Value = jsx:encode(split_a6_data_99({DataIndex, Data})),
  Key = binary_to_hex(list_to_binary([Di1, 16#10])),
%%   io:format("parse_data_to_json Key ~p Value ~s ~n", [Key, Value]),
  {Key,Value};

%97A6=> D1 20~2B
parse_data_to_json(<<16#D1, Di2:8>>, Data) ->
  Value= convert_byte_to_float_8_2(Data),
  Key = binary_to_hex(list_to_binary([16#D1, Di2])),
 %%  io:format("D1 20-2B parse_data_to_json Key ~p Value ~p ~n", [Key, Value]),
  {Key,Value};

parse_data_to_json(<<_DI3:1/binary, _DI2:1/binary, _DI1:1/binary, _DI0:1/binary>>,  _Data) ->
%%   io:format("no match parse_data_to_json Data ~p ~n", [{_DI3,_DI2,_DI1,_DI0,_Data}]),
%%  io:format("no match parse_data_to_json Data ~p ~n", [{binary_to_hex(_DI3),
%%    binary_to_hex(_DI2),
%%    binary_to_hex(_DI1),
%%    binary_to_hex(_DI0),binary_to_hex(_Data)}]),

  {}.




acc(<<>>,Value,_) -> Value;
acc(<<Da1:4,Da0:4,Rest/binary>>,Value,Mult)->
  NValue = (Da1*10 +Da0)*Mult + Value,
  acc(Rest,NValue,Mult*100).

split_a4_data_1({DataIndex , Data}) -> split_a4_data_1({DataIndex,Data}, []).
split_a4_data_1({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_1({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  Value= acc(Data,0,1),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_1({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_2({DataIndex , Data}) -> split_a4_data_2({DataIndex,Data}, []).
split_a4_data_2({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 ==0 orelse Di5==1 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc)
  when (Di5 >= 2 andalso  Di5=<9) orelse (Di5 >= 15 andalso  Di5=<18) orelse (Di5 >= 24 andalso  Di5=<27) orelse (Di5 >= 33 andalso  Di5=<36)->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:2/binary, Rest/binary>>}, Acc)
  when Di5 ==10 orelse  Di5== 19 orelse  Di5== 28->
  Value= convert_byte_to_float_4_1(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc)
  when Di5 ==11 orelse  Di5== 20 orelse  Di5== 29  ->
  Value= convert_byte_to_float_6_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc)
  when Di5 == 12 orelse  Di5==13 orelse  Di5== 21 orelse  Di5== 22 orelse  Di5== 30 orelse  Di5== 31->
  Value= convert_byte_to_float_6_4(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:2/binary, Rest/binary>>}, Acc)
  when Di5 == 14 orelse  Di5== 23 orelse  Di5== 32->
  Value= convert_byte_to_float_4_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_3({DataIndex , Data}) -> split_a4_data_3({DataIndex,Data}, []).
split_a4_data_3({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_3({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) when (Di5-1) rem 3 == 0->
  Value= convert_byte_to_float_6_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_3({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_3({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_3({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_4({DataIndex , Data}) -> split_a4_data_4({DataIndex,Data}, []).
split_a4_data_4({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_4({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_4({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_5({DataIndex , Data}) -> split_a4_data_5({DataIndex,Data}, []).
split_a4_data_5({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_5({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc)
  when Di5 =< 1->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_5({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_5({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_5({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_6({DataIndex , Data}) -> split_a4_data_6({DataIndex,Data}, []).
split_a4_data_6({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_6({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc)
  when Di5 =< 1->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_6({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_6({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:2/binary, Rest/binary>>}, Acc)
  when Di5 ==2->
  Value= convert_byte_to_float_4_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_6({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_6({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_6({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_7({DataIndex , Data}) -> split_a4_data_7({DataIndex,Data}, []).
split_a4_data_7({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_7({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc)
  when Di5 == 0 orelse Di5 == 3 orelse Di5 == 4->
  Value= acc(Data,0,1),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_7({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_7({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc)
  when Di5 == 6 orelse Di5 == 8->
  Value= convert_time_to_st_MDHM(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_7({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_7({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc)
  when Di5 == 1 orelse Di5 == 2->
  Value= convert_byte_to_float_6_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_7({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_7({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:2/binary, Rest/binary>>}, Acc)
  when Di5 == 5 orelse Di5 == 7->
  Value= convert_byte_to_float_4_1(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_7({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_8({DataIndex , Data}) -> split_a4_data_8({DataIndex,Data}, []).
split_a4_data_8({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_8({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  Value= acc(Data,0,1),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_8({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_9({DataIndex , Data}) -> split_a4_data_9({DataIndex,Data}, []).
split_a4_data_9({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_9({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 =< 16#01->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_9({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_9({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) when Di5 == 16#02->
  Value= convert_byte_to_float_6_4(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_9({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_9({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:5/binary, Rest/binary>>}, Acc) when Di5 == 16#03->
  Value= convert_time_to_st_YMDHM(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_9({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_10({DataIndex , Data}) -> split_a4_data_10({DataIndex,Data}, []).
split_a4_data_10({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_10({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_10({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_10({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_10({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_11({DataIndex , Data}) -> split_a4_data_11({DataIndex,Data}, []).
split_a4_data_11({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_11({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_11({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_11({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_11({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_11({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_11({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_12({DataIndex , Data}) -> split_a4_data_12({DataIndex,Data}, []).
split_a4_data_12({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_12({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_12({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_12({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_12({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_12({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) when Di5 rem 2 == 0->
  Value= convert_byte_to_float_6_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_12({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_12({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:5/binary, Rest/binary>>}, Acc) when Di5 rem 2 == 1->
  Value= convert_time_to_st_YMDHM(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_12({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_13({DataIndex , Data}) -> split_a4_data_13({DataIndex,Data}, []).
split_a4_data_13({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_13({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#01 orelse Di5 == 16#02->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_13({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_13({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#00->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_13({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_14({DataIndex , Data}) -> split_a4_data_14({DataIndex,Data}, []).
split_a4_data_14({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_14({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_14({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_14({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_14({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_14({DataIndex = #di_data_A6{ di5 = Di5}, <<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4, Rest/binary>>}, Acc) ->
  HH = Da1*60+Da0,MM= Da3*10+Da2,NN = Da5*10+Da4,
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5+1]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a4_data_14({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, HH+MM},{HexDataIndex1,NN}] )).

split_a4_data_15({DataIndex , Data}) -> split_a4_data_15({DataIndex,Data}, []).
split_a4_data_15({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_15({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_15({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_15({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01 ->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_15({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_15({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  {Value,NN}=convert_time_to_st_MMDDNN(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5+1]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a4_data_15({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value},{HexDataIndex1,NN}] )).

split_a4_data_16({DataIndex , Data}) -> split_a4_data_16({DataIndex,Data}, []).
split_a4_data_16({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_16({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_16({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_16({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_16({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_16({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:1/binary, Rest/binary>>}, Acc) when Di5 == 16#02->
  Value= acc(Data,0,1),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_16({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_27({DataIndex , Data}) -> split_a4_data_27({DataIndex,Data}, []).
split_a4_data_27({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_27({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_27({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_27({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_27({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_27({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:1/binary, Rest/binary>>}, Acc) when Di5 == 16#02->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_16({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_17({DataIndex , Data}) -> split_a4_data_17({DataIndex,Data}, []).
split_a4_data_17({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_17({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_17({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_17({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_17({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_17({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary,Data0:1/binary, Rest/binary>>}, Acc)->
  Value= convert_time_to_st_YMD(Data),
  NN = acc(Data0,0,1),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5+1]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a4_data_17({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value},{HexDataIndex1,NN}] )).

split_a4_data_18({DataIndex , Data}) -> split_a4_data_18({DataIndex,Data}, []).
split_a4_data_18({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_18({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:6/binary, Rest/binary>>}, Acc) when Di5 == 16#00 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_18({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_18({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) when Di5 == 16#01->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_18({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_18({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:2/binary, Rest/binary>>}, Acc)->
  Value= convert_time_to_st_DH(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_18({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_19({DataIndex , Data}) -> split_a4_data_19({DataIndex,Data}, []).
split_a4_data_19({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_19({DataIndex = #di_data_A6{di1 = Di1, di3 = Di3}, <<Data:6/binary, Rest/binary>>}, Acc)
  when  (Di1 == 16#10 andalso (Di3 == 16#01 orelse  Di3== 16#25)) orelse (Di1 == 16#18 andalso (Di3 == 16#01 orelse  Di3== 16#21)) ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_19({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_19({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:4/binary, Rest/binary>>}, Acc)
  when ((Di3 >= 16#02 andalso  Di3 =< 16#09) orelse (Di3 >= 16#0F andalso  Di3 =< 16#12)
  orelse (Di3 >= 16#18 andalso  Di3 =< 16#1B)
  orelse (Di3 >= 16#21 andalso  Di3 =< 16#35))->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_19({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_19({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:2/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0A orelse  Di3 == 16#13 orelse Di3 == 16#1C )->
  Value= convert_byte_to_float_4_1(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_19({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_19({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:3/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0B orelse  Di3 == 16#14 orelse Di3 == 16#1D )->
  Value= convert_byte_to_float_6_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_19({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_19({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:3/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0C orelse  Di3 == 16#15 orelse Di3 == 16#1E orelse Di3 == 16#0D orelse  Di3 == 16#16 orelse Di3 == 16#1F )->
  Value= convert_byte_to_float_6_4(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_19({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_19({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:2/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0E orelse  Di3 == 16#17 orelse Di3 == 16#20 )->
  Value= convert_byte_to_float_4_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_19({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_20({DataIndex , Data}) -> split_a4_data_20({DataIndex,Data}, []).
split_a4_data_20({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_20({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:6/binary, Rest/binary>>}, Acc) when Di3 == 16#01 orelse  Di3== 16#25 ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_20({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_20({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:4/binary, Rest/binary>>}, Acc)
  when ((Di3 >= 16#02 andalso  Di3 =< 16#09) orelse (Di3 >= 16#0F andalso  Di3 =< 16#12)
  orelse (Di3 >= 16#18 andalso  Di3 =< 16#1B)
  orelse (Di3 >= 16#21 andalso  Di3 =< 16#24)
  orelse (Di3 >= 16#26 andalso  Di3 =< 16#35))->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_20({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_20({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:2/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0A orelse  Di3 == 16#13 orelse Di3 == 16#1C )->
  Value= convert_byte_to_float_4_1(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4+ 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_20({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_20({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:3/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0B orelse  Di3 == 16#14 orelse Di3 == 16#1D )->
  Value= convert_byte_to_float_6_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_20({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_20({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:3/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0C orelse  Di3 == 16#15 orelse Di3 == 16#1E orelse Di3 == 16#0D orelse  Di3 == 16#16 orelse Di3 == 16#1F )->
  Value= convert_byte_to_float_6_4(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4+ 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_20({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_20({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:2/binary, Rest/binary>>}, Acc)
  when (Di3 == 16#0E orelse  Di3 == 16#17 orelse Di3 == 16#20 )->
  Value= convert_byte_to_float_4_3(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_20({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_21({DataIndex , Data}) -> split_a4_data_21({DataIndex,Data}, []).
split_a4_data_21({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_21({DataIndex = #di_data_A6{ di4 = Di4}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  Value= acc(Data,0,1),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_21({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_22({DataIndex , Data}) -> split_a4_data_22({DataIndex,Data}, []).
split_a4_data_22({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_22({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:6/binary, Rest/binary>>}, Acc) when Di3 == 16#01 orelse  Di3 == 16#12->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_22({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_22({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_22({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_24({DataIndex , Data}) -> split_a4_data_24({DataIndex,Data}, []).
split_a4_data_24({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_24({DataIndex = #di_data_A6{ di3 = Di3,di4 = Di4}, <<Data:6/binary, Rest/binary>>}, Acc) when Di3 == 16#01 orelse  Di3 == 16#12->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_24({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_24({DataIndex = #di_data_A6{ di4 = Di4}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_24({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_23({DataIndex , Data}) -> split_a4_data_23({DataIndex,Data}, []).
split_a4_data_23({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_23({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:6/binary, Rest/binary>>}, Acc) when Di3 == 16#01 orelse  Di3 == 16#13->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_23({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_23({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:3/binary, Rest/binary>>}, Acc) when Di3 == 16#012->
  Value= convert_byte_to_float_6_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_23({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_23({DataIndex = #di_data_A6{ di3 = Di3}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_23({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a4_data_25({DataIndex , Data}) -> split_a4_data_25({DataIndex,Data}, []).
split_a4_data_25({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_25({DataIndex = #di_data_A6{di1=Di1, di3 = Di3}, <<Data:6/binary, Rest/binary>>}, Acc)
  when( ((Di1 ==  16#1D  orelse Di1 == 16#1E) andalso Di3 ==16#01) orelse (Di1 ==  16#1F  andalso (Di3 == 16#01 orelse Di3 == 16#06))) ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_25({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_25({DataIndex = #di_data_A6{ di1=Di1,di3 = Di3}, <<Data:4/binary, Rest/binary>>}, Acc)
  when    ((Di1 ==  16#1D  orelse Di1 == 16#1E) andalso Di3 ==16#02)  ->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_25({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_25({DataIndex = #di_data_A6{di3 = Di3}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_25({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a4_data_26({DataIndex , Data}) -> split_a4_data_26({DataIndex,Data}, []).
split_a4_data_26({_DataIndex, <<>>}, Acc) -> Acc;
split_a4_data_26({DataIndex = #di_data_A6{ di1=Di1,di3 = Di3,di4 = Di4}, <<Data:6/binary, Rest/binary>>}, Acc)
  when( ((Di1 ==  16#1D  orelse Di1 == 16#1E) andalso Di3 ==16#01) orelse (Di1 ==  16#1F  andalso (Di3 == 16#01 orelse Di3 == 16#06))) ->
  Value= convert_time_to_st_YMDHMS(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_26({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_26({DataIndex = #di_data_A6{ di1=Di1,di3=Di3,di4 = Di4}, <<Data:4/binary, Rest/binary>>}, Acc)
  when    ((Di1 ==  16#1D  orelse Di1 == 16#1E) andalso Di3 ==16#02)  ->
  Value= convert_to_hex(Data,[]),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_26({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] ));
split_a4_data_26({DataIndex = #di_data_A6{ di4 = Di4}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value= convert_byte_to_float_8_2(Data),
  NextDataIndex = DataIndex#di_data_A6{di4 = Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a4_data_26({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a5_data_1({DataIndex , Data}) -> split_a5_data_1({DataIndex,Data}, []).
split_a5_data_1({_DataIndex, <<>>}, Acc) -> Acc;
split_a5_data_1({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  {Value,NN}=convert_time_to_st_MMDDNN(Data),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a5_data_1({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value},{HexDataIndex1,NN}] )).

%split_a5_data_98({DataIndex , Data}) -> split_a5_data_98({DataIndex,Data}, []).
%split_a5_data_98({_DataIndex, <<>>}, Acc) -> Acc;
%split_a5_data_98({DataIndex = #di_645_data_97{ di3 = Di3}, <<Data:3/binary, Rest/binary>>}, Acc) ->
%  {Value,NN}=convert_time_to_st_MMDDNN(Data),
%  NextDataIndex = DataIndex#di_645_data_97{di3 = Di3 + 2},
%  ListDataIndex = list_to_binary([DataIndex#di_645_data_97.di1,DataIndex#di_645_data_97.di2,Di3]),
%  HexDataIndex = binary_to_hex(ListDataIndex),
%  ListDataIndex1 = list_to_binary([DataIndex#di_645_data_97.di1,DataIndex#di_645_data_97.di2,Di3]),
%  HexDataIndex1 = binary_to_hex(ListDataIndex1),
%  split_a5_data_98({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value},{HexDataIndex1,NN}] )).


split_a5_data_2({DataIndex , Data}) -> split_a5_data_2({DataIndex,Data}, []).
split_a5_data_2({_DataIndex, <<>>}, Acc) -> Acc;
split_a5_data_2({DataIndex = #di_data_A6{ di5 = Di5}, <<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4, Rest/binary>>}, Acc) ->
  HH = (Da1*10+Da0)*60,MM= Da3*10+Da2,NN = Da5*10+Da4,
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a5_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, HH+MM},{HexDataIndex1,NN}] )).


split_a5_data_3({DataIndex , Data}) -> split_a5_data_3({DataIndex,Data}, []).
split_a5_data_3({_DataIndex, <<>>}, Acc) -> Acc;
split_a5_data_3({DataIndex = #di_data_A6{ di5 = Di5}, <<Da7:4,Da6:4,Data:3/binary, Rest/binary>>}, Acc) ->
  Value= convert_time_to_st_YMD(Data),
  NN = Da7*10+Da6,
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a5_data_3({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value},{HexDataIndex1,NN}] )).

%split_a5_data_97({DataIndex , Data}) -> split_a5_data_97({DataIndex,Data}, []).
%split_a5_data_97({_DataIndex, <<>>}, Acc) -> Acc;
%split_a5_data_97({DataIndex = #di_645_data_97{ di3 = Di3}, <<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4, Rest/binary>>}, Acc) ->
%  HH = (Da1*10+Da0)*60,MM= Da3*10+Da2,NN = Da5*10+Da4,
%  NextDataIndex = DataIndex#di_645_data_97{di3 = Di3 + 2},
%  ListDataIndex = list_to_binary([DataIndex#di_645_data_97.di1,DataIndex#di_645_data_97.di2,Di3]),
%  HexDataIndex = binary_to_hex(ListDataIndex),
%  ListDataIndex1 = list_to_binary([DataIndex#di_645_data_97.di1,DataIndex#di_645_data_97.di2,Di3]),
%  HexDataIndex1 = binary_to_hex(ListDataIndex1),
%  split_a5_data_97({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, HH+MM},{HexDataIndex1,NN}] )).


split_a3_data_1({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a3_data_1({DataIndex,Data}, []).
split_a3_data_1({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a3_data_1({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = Di3, di4 = _Di4}, <<Data:2/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_4_1(Data),
%%  io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di3 =  Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a3_data_1({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a3_data_2({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a3_data_2({DataIndex,Data}, []).
split_a3_data_2({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a3_data_2({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = Di3, di4 = _Di4}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_6_3(Data),
 %%  io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di3 =  Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a3_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a3_data_3({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a3_data_3({DataIndex,Data}, []).
split_a3_data_3({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a3_data_3({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = Di3, di4 = _Di4}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_6_4(Data),
 %%  io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di3 =  Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a3_data_3({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a3_data_4({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a3_data_4({DataIndex,Data}, []).
split_a3_data_4({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a3_data_4({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = Di3, di4 = _Di4}, <<Data:2/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_4_3(Data),
%%   io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di3 =  Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a3_data_4({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a3_data_5({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a3_data_5({DataIndex,Data}, []).
split_a3_data_5({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a3_data_5({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = Di3, di4 = _Di4}, <<Data:2/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_4_2(Data),
 %%  io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di3 =  Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a3_data_5({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a3_data_6({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a3_data_6({DataIndex,Data}, []).
split_a3_data_6({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a3_data_6({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = Di4}, <<Data:2/binary, Rest/binary>>}, Acc) ->
  Value =convert_byte_to_float_4_2(Data),
  %% io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di4 =  Di4 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a3_data_6({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a1_data({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a1_data({DataIndex,Data}, []).
split_a1_data({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a1_data({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = Di3, di4 = _Di4}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_8_2(Data),
 %%  io:format("Value ~p ~n",[Value]),
  NextDataIndex = DataIndex#di_645data{di3 = Di3 + 1},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a1_data({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a1_data_1({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a1_data_1({DataIndex,Data}, []).
split_a1_data_1({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a1_data_1({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = Di4}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_8_2(Data),
 %%  io:format("Value ~p ~n",[Value]),
  NewDi4 = Di4 + 1,
  NextDataIndex = DataIndex#di_645data{di4 = NewDi4},
  ListDataIndex = list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a1_data_1({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a2_data_1({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a2_data_1({DataIndex,Data}, []).
split_a2_data_1({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a2_data_1({DataIndex = #di_645data{di1 = Di1, di2 = Di2, di3 = Di3, di4 = Di4}, <<Data0:3/binary,Data1:5/binary ,Rest/binary>>}, Acc) ->
  Value0 = convert_byte_to_float_6_4(Data0),
  Value1 = convert_time_to_st_YMDHM(Data1),
  ListDataIndex0 = list_to_binary([Di1, Di2, Di3, Di4,16#00]),
  ListDataIndex1 = list_to_binary([Di1, Di2, Di3, Di4,16#01]),
  HexDataIndex0 = binary_to_hex(ListDataIndex0),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  Value = jsx:encode([{HexDataIndex0, Value0},{HexDataIndex1, Value1}]),
  NewDi3 = Di3 + 1,
  NextDataIndex = DataIndex#di_645data{di3 = NewDi3},
  ListDataIndex= list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a2_data_1({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a2_data_2({DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, Data}) -> split_a2_data_2({DataIndex,Data}, []).
split_a2_data_2({_DataIndex = #di_645data{di1 = _Di1, di2 = _Di2, di3 = _Di3, di4 = _Di4}, <<>>}, Acc) -> Acc;
split_a2_data_2({DataIndex = #di_645data{di1 = Di1, di2 = Di2, di3 = Di3, di4 = Di4}, <<Data0:3/binary,Data1:5/binary ,Rest/binary>>}, Acc) ->
  Value0 = convert_byte_to_float_6_4(Data0),
  Value1 = convert_time_to_st_YMDHM(Data1),
  ListDataIndex0 = list_to_binary([Di1, Di2, Di3, Di4,16#00]),
  ListDataIndex1 = list_to_binary([Di1, Di2, Di3, Di4,16#01]),
  HexDataIndex0 = binary_to_hex(ListDataIndex0),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  Value = jsx:encode([{HexDataIndex0, Value0},{HexDataIndex1, Value1}]),
  NewDi4 = Di4 + 1,
  NextDataIndex = DataIndex#di_645data{di4 = NewDi4},
  ListDataIndex= list_to_binary([DataIndex#di_645data.di1,DataIndex#di_645data.di2,DataIndex#di_645data.di3,DataIndex#di_645data.di4]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a2_data_2({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a6_data_3({DataIndex , Data}) -> split_a6_data_3({DataIndex,Data}, []).
split_a6_data_3({_DataIndex, <<>>}, Acc) -> Acc;
split_a6_data_3({DataIndex = #di_data_A6{ di5 = Di5}, <<Data:3/binary, Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_6_4(Data),
  io:format("Value ~p ~n",[Value]),
  NewDi5 = Di5 + 1,
  NextDataIndex = DataIndex#di_data_A6{di5 = NewDi5},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,DataIndex#di_data_A6.di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a6_data_3({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a6_data_4({DataIndex, Data}) -> split_a6_data_4({DataIndex,Data}, []).
split_a6_data_4({_DataIndex, <<>>}, Acc) -> Acc;
split_a6_data_4({DataIndex= #di_data_A6{ di5 = Di5}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value =  convert_byte_to_float_8_2(Data),
 %%  io:format("Value ~p ~n",[Value]),
  NewDi5 = Di5 + 1,
  NextDataIndex = DataIndex#di_data_A6{di5 = NewDi5},
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,DataIndex#di_data_A6.di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a6_data_4({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).

split_a6_data_99({DataIndex, Data}) -> split_a6_data_99({DataIndex,Data}, []).
split_a6_data_99({_DataIndex, <<>>}, Acc) -> Acc;
split_a6_data_99({DataIndex= #di_645_data_97{ di3 = Di3}, <<Data:4/binary, Rest/binary>>}, Acc) ->
  Value =  convert_byte_to_float_8_2(Data),
  %% io:format("Value ~p ~n",[Value]),
  NewDi3 = Di3 + 1,
  NextDataIndex = DataIndex#di_645_data_97{di3 = NewDi3},
  ListDataIndex = list_to_binary([DataIndex#di_645_data_97.di1,DataIndex#di_645_data_97.di2,DataIndex#di_645_data_97.di3]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  split_a6_data_99({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value}] )).


split_a6_data_8({DataIndex, Data}) -> split_a6_data_8({DataIndex,Data}, []).
split_a6_data_8({_DataIndex , <<>>}, Acc) -> Acc;
split_a6_data_8({DataIndex = #di_data_A6{ di5 = Di5}, <<Data0:3/binary, Data:5/binary,Rest/binary>>}, Acc) ->
  Value = convert_byte_to_float_6_4(Data0),
 %%  io:format(" split_a6_data_8 Value ~p ~n",[Value]),
  ListDataIndex = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5]),
  HexDataIndex = binary_to_hex(ListDataIndex),
  Value1 =convert_time_to_st_YMDHM(Data),
 %%  io:format("split_a6_data_8 Value ~p ~n",[Value1]),
  NextDataIndex = DataIndex#di_data_A6{di5 = Di5 + 2},
  ListDataIndex1 = list_to_binary([DataIndex#di_data_A6.di1,DataIndex#di_data_A6.di2,DataIndex#di_data_A6.di3,DataIndex#di_data_A6.di4,Di5 + 1]),
  HexDataIndex1 = binary_to_hex(ListDataIndex1),
  split_a6_data_8({NextDataIndex, Rest}, lists:merge(Acc, [{HexDataIndex, Value},{HexDataIndex1,Value1}] )).


convert_byte_to_float_8_4(<<16#FF,16#FF,16#FF,16#FF>>)->
  ff;
convert_byte_to_float_8_4(<<Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue = Da1*1000 +  Da0*100 +  Da3*10 + Da2+Da5/10+Da4/100+Da7/1000+Da6/10000,
  list_to_float(io_lib:format("~.4f",[FloatValue])).

convert_byte_to_float_8_2(<<16#FF,16#FF,16#FF,16#FF>>)->
  ff;
convert_byte_to_float_8_2(<<Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue = Da1*100000 +  Da0*10000 +  Da3*1000 + Da2*100 +  Da5*10 +  Da4 + Da7/10 + Da6/100,
  list_to_float(io_lib:format("~.2f",[FloatValue])).

convert_byte_to_float_10(<<16#FF,16#FF,16#FF,16#FF,16#FF>>)->
    ff;
convert_byte_to_float_10(<<Da9:4, Da8:4, Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue = Da1*100000 +  Da0*10000 +  Da3*1000 + Da2*100 +  Da5*10 +  Da4 + Da7/10 + Da6/100 + Da9/1000 + Da8/10000,
  list_to_float(io_lib:format("~.4f",[FloatValue])).

convert_byte_to_float_8_1h(<<Da7:4, _Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue = Da1*100000 +  Da0*10000 +  Da3*1000 + Da2*100 +  Da5*10 +  Da4 + Da7/10,
  list_to_float((io_lib:format("~.1f",[FloatValue]))).

convert_byte_to_float_8_1t(<<Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, _Da1:4, Da0:4>>)->
  FloatValue = Da0*100000 +  Da3*10000 +  Da2*1000 + Da5*100 +  Da4*10 +  Da7 + Da6/10,
  list_to_float((io_lib:format("~.1f",[FloatValue]))).

convert_byte_to_float_4(<<16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_4(<< Da3:4, Da2:4, Da1:4, Da0:4>>)->
  Da1*1000 +  Da0*100 +  Da3*10 + Da2 .

convert_byte_to_float_4_3(<<16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_4_3(<< Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue = Da1 +  Da0/10 +  Da3/100 + Da2/1000,
  list_to_float((io_lib:format("~.3f",[FloatValue]))).

convert_byte_to_float_4_2(<<16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_4_2(<< Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue =  Da1*10 +  Da0 +  Da3/10 + Da2/100 ,
  list_to_float((io_lib:format("~.2f",[FloatValue]))).

convert_byte_to_float_4_1(<<16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_4_1(<< Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue =  Da1*100 +  Da0*10 +  Da3 + Da2/10,
  list_to_float((io_lib:format("~.1f",[FloatValue]))).

convert_byte_to_float_6_2(<<16#FF,16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_6_2(<<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue =  Da1*1000 +  Da0*100 +  Da3*10 + Da2 + Da5/10 + Da4/100,
  list_to_float((io_lib:format("~.2f",[FloatValue]))).

convert_byte_to_float_6_3(<<16#FF,16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_6_3(<<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue =  Da1*100 +  Da0*10 +  Da3 + Da2/10 + Da5/100 + Da4/1000,
  list_to_float((io_lib:format("~.3f",[FloatValue]))).

convert_byte_to_float_6_4(<<16#FF,16#FF,16#FF>>)->
  0.0;
convert_byte_to_float_6_4(<<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  FloatValue =  Da1*10 +  Da0  + Da3/10 + Da2/100+Da5/1000 + Da4/10000,
  list_to_float((io_lib:format("~.4f",[FloatValue]))).

convert_time_to_st_YMDHMS(<<Da11:4,Da10:4,Da9:4,Da8:4,Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  case Da11+Da10+ Da9+Da8+Da7+Da6+Da5+Da4+Da3+Da2+Da1+Da0 of
     0->
         0;
     _->
        YY = Da1*10+Da0+2000,MM = Da3*10+Da2,DD= Da5*10+Da4,HH=Da7*10+Da6,Mm= Da9*10+Da8,SS=Da11*10+Da10,
        DateTime ={{YY,MM,DD},{HH,Mm,SS}},
       %%  io:format("parse_data_to_json Time ~p ~n", [DateTime]),
        calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
 end.
convert_time_to_st_YMDHM(<<Da9:4,Da8:4,Da7:4, Da6:4, Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
  case Da9+Da8+Da7+Da6+Da5+Da4+Da3+Da2+Da1+Da0 of
     0->
         0;
     _->
        YY = Da1*10+Da0+2000,MM = Da3*10+Da2,DD= Da5*10+Da4,HH=Da7*10+Da6,Mm= Da9*10+Da8,
        DateTime ={{YY,MM,DD},{HH,Mm,0}},
   %%      io:format("parse_data_to_json Time ~p ~n", [DateTime]),
        calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
 end.
convert_time_to_st_YMD(<< Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
     case Da0+Da1+Da2+Da3+Da4+Da5 of
       0-> 0;
       _ ->
         YY= Da1*10+Da0+2000,MM = Da3*10+Da2,DD= Da5*10+Da4,
         DateTime ={{YY,MM,DD},{0,0,0}},
         calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
     end.


convert_time_to_st_MDHM(<< Da7:4, Da6:4,Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>)->
   case Da0+Da1+Da2+Da3+Da4+Da5+Da6+Da7 of
     0-> 0;
     _ ->
       {{Year, _, _}, _} =  dgiot_datetime:local_time(),
       MM = Da1*10+Da0,DD= Da3*10+Da2,HH= Da5*10+Da4,Mm= Da7*10+Da6,
       DateTime ={{Year,MM,DD},{HH,Mm,0}},
       calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
   end.
convert_time_to_st_DH(<<  Da3:4, Da2:4, Da1:4, Da0:4>>)->
   case Da0+Da1+Da2+Da3 of
     0-> 0;
     _ ->
       {{Year, Month, _}, _} = dgiot_datetime:local_time(),
       DD= Da1*10+Da0,HH= Da3*10+Da2,
       DateTime ={{Year,Month,DD},{HH,0,0}},
       calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
   end.

convert_time_to_st_MMDDNN(<<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>) ->
  Value= case Da0+Da1+Da2+Da3+Da4+Da5 of
           0-> 0;
           _ ->
             {{Year, _, _}, _} = dgiot_datetime:local_time(),
             MM = Da1*10+Da0,DD= Da3*10+Da2,
             DateTime ={{Year,MM,DD},{0,0,0}},
             calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
         end,
  NN = Da5*10+Da4,
  {Value ,NN}.

convert_time_to_st_hhmmNN(<<Da5:4, Da4:4, Da3:4, Da2:4, Da1:4, Da0:4>>) ->
  Value= case Da0+Da1+Da2+Da3+Da4+Da5 of
           0 -> 0;
           _ ->
             {{Year, Month, Date}, _} = dgiot_datetime:local_time(),
             HH = Da1*10+Da0, MM= Da3*10+Da2,
             DateTime ={{Year, Month, Date}, {HH,MM,0}},
             calendar:datetime_to_gregorian_seconds(DateTime)-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
          end,
  NN = Da5*10+Da4,
  {Value ,NN}.

convert_to_ascii(<<>>,Acc) -> list_to_binary(Acc);
convert_to_ascii(<<Data:8,Rest/binary>>,Acc) ->
  case Data == 0 of
    false ->
      L =  io_lib:format("~c",[Data]),
      convert_to_ascii(Rest,[L|Acc]);
    true ->
      convert_to_ascii(Rest,Acc)
  end.
convert_to_hex(<<>>,Acc) ->
 %%  io:format(" convert_to_hex Acc~p ~n", [Acc]),

  list_to_binary(Acc);
convert_to_hex(<<Data1:4,Data0:4,Rest/binary>>,Acc) ->
  convert_to_hex(Rest,[integer_to_list(Data1,16) ++  integer_to_list(Data0,16)|Acc]).


convert_to_bits(Data) ->
    <<Da15:1,Da14:1,Da13:1,Da12:1,Da11:1,Da10:1,Da9:1,Da8:1,Da7:1, Da6:1, Da5:1, Da4:1, Da3:1, Da2:1, Da1:1, Da0:1 >> = reverse(Data),
 Value = lists:concat([integer_to_list(D,16)||D<-[Da15 ,Da14 ,Da13 ,Da12 ,Da11 ,Da10 ,Da9 ,Da8 ,Da7 , Da6 , Da5 , Da4 , Da3 , Da2 , Da1 , Da0]]),
  list_to_binary(Value).

-spec checkRateNumber(binary()) ->
  {true,binary(),  binary(),integer(),integer()} | {false,binary(),  binary(),integer(),integer()} |{null,null,null,null,null}.
%%有费率数不可直接读，需转换{true,电表标识符,主站标识符,费率位数,字节数}
checkRateNumber(<<Di4:8, Di3:8,16#06,16#05>>) when Di3 >= 16#01 andalso Di3 =<16#0A->
  Key = binary_to_hex(list_to_binary([16#01, Di3,16#06,16#05])),
  Key1 = binary_to_hex(list_to_binary([Di4, Di3,16#06,16#05])),
  ByteNum = case  Di4 >= 16#09 of
              true -> 8;
              false -> 4
            end,
  {true,Key,Key1,Di4,ByteNum};
%%有费率数可直接读 00
checkRateNumber(<<16#00, Di3:8, Di2:8, 16#00>>) when Di2=<16#0A->
  Key = binary_to_hex(list_to_binary([16#00, Di3, Di2,  16#00])),
  {false,Key,Key,Di3,4};
%% 01
checkRateNumber(<<16#00, Di3:8, Di2:8, 16#01>>) when  Di2=<16#04->
  Key = binary_to_hex(list_to_binary([16#00, Di3, Di2,  16#01])),
  {false,Key,Key,Di3,8};
%%无费率数可直接读
checkRateNumber(<<_Di4:8, _Di3:8, _Di2:8, _Di1:8>>) ->
  {null,null,null,null,null}.
