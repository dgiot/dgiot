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

%%%===================================================================
%%% @doc DG-IoT 系统级公共实用函数网关
%%%
%%% 本模块提供系统级的公共实用函数，按功能分类组织，便于维护和使用。
%%%
%%% 功能分类：
%%% 1. 类型转换函数 (Type Conversion)
%%% 2. 字符串处理函数 (String Manipulation)
%%% 3. 二进制处理函数 (Binary Manipulation)
%%% 4. 网络函数 (Network Functions)
%%% 5. 加密和哈希函数 (Encryption & Hash)
%%% 6. 数学和统计函数 (Math & Statistics)
%%% 7. 文件操作函数 (File Operations)
%%% 8. 进程和消息函数 (Process & Message)
%%% 9. 数据验证函数 (Data Validation)
%%% 10. 工具函数 (Utility Functions)
%%%===================================================================

-module(dgiot_utils).
-author("johnliu").
-include("dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 类型转换函数 (Type Conversion)
%%%===================================================================
-export([
    to_binary/1,     %% 转换为二进制
    to_atom/1,       %% 转换为原子
    to_int/1,        %% 转换为整数
    to_list/1,       %% 转换为列表
    to_bool/1,       %% 转换为布尔值
    to_float/1,      %% 转换为浮点数
    to_float/2,      %% 转换为浮点数（指定精度）
    to_map/1,        %% 转换为映射
    list_to_map/1,   %% 列表转换为映射
    to_term/1,       %% 字符串转换为Erlang项式
    to_utf8/2        %% 转换为UTF-8编码
]).

%%%===================================================================
%%% 字符串处理函数 (String Manipulation)
%%%===================================================================
-export([
    binary_to_hex/1,     %% 二进制转十六进制字符串
    hex_to_binary/1,     %% 十六进制字符串转二进制
    hexstr2bin/1,        %% 十六进制字符串转二进制（别名）
    bin2hexstr_A_F/1,    %% 二进制转十六进制字符串（大写）
    bin2hexstr_a_f/1,    %% 二进制转十六进制字符串（小写）
    to_hex/1,           %% 转换为十六进制字符串
    trim_string/1,      %% 去除字符串空白字符
    tokens/2,           %% 字符串分割
    join/2,             %% 列表连接为字符串
    join/3,             %% 列表连接为字符串（带修剪）
    join/4,             %% 列表连接为字符串（带转换函数）
    format/2,           %% 格式化字符串
    squotes_wrapped/1,  %% 添加单引号包装
    reverse/1           %% 反转字符串/二进制
]).

%%%===================================================================
%%% 二进制处理函数 (Binary Manipulation)
%%%===================================================================
-export([
    bits_to_binary/1,          %% 位列表转二进制
    binary_to_bits/1,          %% 二进制转位列表
    get_binary_bits_n/2,       %% 获取二进制指定位
    set_binary_bits_n/3,       %% 设置二进制指定位
    binary_bits_sum/1,         %% 计算二进制位总和
    binary_bits_zero_list/1,   %% 获取二进制零位列表
    binary_bits_nozero_list/1, %% 获取二进制非零位列表
    binary_bits_zip/1,         %% 压缩二进制位
    zip_bin/1,                 %% 压缩二进制（去除前导零）
    is_in_binary/2,            %% 检查二进制是否包含子串
    binary_start_with/2,       %% 检查二进制是否以指定前缀开头
    is_hex_string/1            %% 检查是否为十六进制字符串
]).

%%%===================================================================
%%% 网络函数 (Network Functions)
%%%===================================================================
-export([
    get_hostname/0,       %% 获取主机名
    get_ip/1,             %% 获取IP地址
    get_port/1,           %% 获取端口号
    get_natip/0,          %% 获取NAT IP地址
    get_wlanip/0,         %% 获取WLAN IP地址
    get_computerconfig/0, %% 获取计算机配置
    get_macs/0,           %% 获取MAC地址列表
    get_ipbymac/1,        %% 根据MAC地址获取IP
    get_ipbymac/2,        %% 根据MAC地址和网络获取IP
    get_macbyip/1,        %% 根据IP地址获取MAC
    get_ifaddrs/0,        %% 获取网络接口地址
    get_ifaddr/1,         %% 获取指定网络接口地址
    get_ifip/1,           %% 获取指定网络接口IP
    ping_all/0,           %% ping所有网络
    ping_all/1,           %% ping指定网络
    get_ipv4/1,           %% 获取IPv4地址
    get_ipv6/1,           %% 获取IPv6地址
    resolve/1,            %% 解析主机名
    get_url_path/1,       %% 获取URL路径
    get_ports/0,          %% 获取端口列表
    check_port/1          %% 检查端口是否可用
]).

%%%===================================================================
%%% 加密和哈希函数 (Encryption & Hash)
%%%===================================================================
-export([
    to_md5/1,            %% 计算MD5哈希
    hash/1,              %% 计算哈希值
    hash/2,              %% 计算两个值的哈希
    hash/3,              %% 计算三个值的哈希
    hash/4,              %% 计算四个值的哈希
    hash_2/1,            %% 计算哈希值（版本2）
    hash_2/2,            %% 计算两个值的哈希（版本2）
    hash_2/3,            %% 计算三个值的哈希（版本2）
    hash_2/4,            %% 计算四个值的哈希（版本2）
    modbus_crc16/1,      %% Modbus CRC16校验
    crc16/1,             %% CRC16校验
    crc16_ccitt/1,       %% CRC16-CCITT校验
    crc16_h/1,           %% CRC16校验（高低字节交换）
    xor_sum/1,           %% 异或校验和
    get_parity/1,        %% 计算奇偶校验
    add_33h/1,           %% 加0x33操作
    sub_33h/1,           %% 减0x33操作
    add_value/1,         %% 加值操作
    sub_value/1          %% 减值操作
]).

%%%===================================================================
%%% 数学和统计函数 (Math & Statistics)
%%%===================================================================
-export([
    round/2,            %% 四舍五入（指定精度）
    rate/2,             %% 计算比率
    variance/2,         %% 计算方差
    find_median/1,      %% 查找中位数
    is_number/1         %% 检查是否为数字
]).

%%%===================================================================
%%% 文件操作函数 (File Operations)
%%%===================================================================
-export([
    read/3,             %% 读取文件
    get_file/3,         %% 获取文件
    get_JsonFile/2,     %% 获取JSON文件
    post_file/2,        %% 上传文件
    get_mock/2,         %% 获取模拟数据文件
    write_mock/3,       %% 写入模拟数据文件
    gzip/1,             %% GZIP压缩
    gzip_no_header/1    %% GZIP压缩（无头部）
]).

%%%===================================================================
%%% 进程和消息函数 (Process & Message)
%%%===================================================================
-export([
    send_fsm/3,         %% 发送FSM消息
    send_msg/2,         %% 发送消息
    wait_request/2,     %% 等待请求
    is_alive/1,         %% 检查进程是否存活
    new_counter/2,      %% 创建计数器
    update_counter/1    %% 更新计数器
]).

%%%===================================================================
%%% 数据验证函数 (Data Validation)
%%%===================================================================
-export([
    is_like/2,              %% 模糊匹配
    is_phone/1,             %% 检查是否为手机号
    is_email/1,             %% 检查是否为邮箱
    check_value_is_null/1,  %% 检查值是否为空
    check_val/3             %% 检查值并抛出错误
]).

%%%===================================================================
%%% 工具函数 (Utility Functions)
%%%===================================================================
-export([
    get_env/1,              %% 获取环境变量
    get_env/2,              %% 获取环境变量（带默认值）
    get_env/3,              %% 获取应用环境变量
    append/2,               %% 追加数据
    append/3,               %% 追加键值对
    get_val/2,              %% 获取值
    get_val/3,              %% 获取值（带默认值）
    open_dets/1,            %% 打开DETS表
    open_dets/2,            %% 打开DETS表（带选项）
    merge_maps/1,           %% 合并映射
    make_error_response/3,  %% 创建错误响应
    new_pns/1,              %% 创建PN列表
    new_pns/3,              %% 创建PN列表（内部使用）
    guid/0,                 %% 生成GUID
    parse_objectid/0,       %% 解析对象ID
    shuffle/1,              %% 随机打乱列表
    split_zetag/1,          %% 分割Zetag
    unique_1/1,             %% 去重（方法1）
    unique_2/1,             %% 去重（方法2）
    string2value/1,         %% 字符串转值
    random/0                %% 生成随机值
]).

-define(TIMEZONE, + 8).

send_fsm(Key, Msg, TimeOut) ->
    erlang:send_after(TimeOut, self(), {fsm, Key, Msg}).

send_msg(PidOrName, Msg) ->
    case is_alive(PidOrName) of
        false ->
            {error, not_alive};
        Pid ->
            case Msg of
                {call, Message} ->
                    gen_server:call(Pid, Message, 5000);
                {cast, Message} ->
                    gen_server:cast(Pid, Message);
                _ ->
                    Pid ! Msg
            end
    end.

binary_to_hex(Id) ->
    <<<<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X, 16)>>.

hex_to_binary(Id) ->
    NewId = trim_string(Id),
    <<<<Z>> || <<X:8, Y:8>> <= NewId, Z <- [binary_to_integer(<<X, Y>>, 16)]>>.


-spec(bin2hexstr_A_F(binary()) -> binary()).
bin2hexstr_A_F(B) when is_binary(B) ->
    <<<<(int2hexchar(H, upper)), (int2hexchar(L, upper))>> || <<H:4, L:4>> <= B>>.

-spec(bin2hexstr_a_f(binary()) -> binary()).
bin2hexstr_a_f(B) when is_binary(B) ->
    <<<<(int2hexchar(H, lower)), (int2hexchar(L, lower))>> || <<H:4, L:4>> <= B>>.

int2hexchar(I, _) when I >= 0 andalso I < 10 -> I + $0;
int2hexchar(I, upper) -> I - 10 + $A;
int2hexchar(I, lower) -> I - 10 + $a.

-spec(hexstr2bin(binary()) -> binary()).
hexstr2bin(B) when is_binary(B) ->
    <<<<(hexchar2int(H) * 16 + hexchar2int(L))>> || <<H:8, L:8>> <= B>>.

hexchar2int(I) when I >= $0 andalso I =< $9 -> I - $0;
hexchar2int(I) when I >= $A andalso I =< $F -> I - $A + 10;
hexchar2int(I) when I >= $a andalso I =< $f -> I - $a + 10.


to_md5(V) when is_binary(V); is_list(V) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(V))]));
to_md5(V) ->
    to_md5(to_binary(V)).

to_hex(V) ->
    binary_to_hex(to_binary(V)).

to_binary(V) when is_atom(V) -> 
    try
        atom_to_binary(V, utf8)
    catch
        _:_ -> atom_to_list(V)
    end;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_pid(V) -> to_binary(pid_to_list(V));
to_binary(V) when is_map(V) -> dgiot_json:encode(V);
to_binary(V) when is_float(V) -> to_binary(io_lib:format("~p", [V]));
to_binary(V) when is_binary(V) -> V.

to_atom(V) when is_binary(V) -> binary_to_atom(V, utf8);
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_atom(V) -> V;
to_atom(V) -> to_atom(io_lib:format("~p", [V])).

to_int([V]) -> to_int(V);
to_int(V) when V == null; V == <<"Undefined">>; V == undefined; V == <<>>; V == "" -> 0;
to_int(V) when is_float(V) -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V) -> list_to_integer(V);
to_int(V) when is_binary(V) -> binary_to_integer(V);
to_int(true) -> 1;
to_int(false) -> 0;
to_int(_V) -> throw({error, <<"ValueError">>}).

to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V) -> V;
to_list(V) -> io_lib:format("~p", [V]).


to_bool(<<"false">>) -> false;
to_bool("false") -> false;
to_bool(V) when is_integer(V) and V =< 0 -> false;
to_bool(<<"0">>) -> false;
to_bool(_V) -> true.

to_float(V, Degree) ->
    New = erlang:float_to_binary(to_float(V), [{decimals, Degree}]),
    to_float(New).

to_float(V) when is_float(V) -> V;
to_float(V) when V == ""; V == <<>>; V == null; V == undefined -> 0.0;
to_float(V) when is_integer(V) -> V / 1;
to_float(V) when is_list(V) -> to_float(to_binary(V));
to_float(V) when is_binary(V) ->
    case catch binary_to_float(V) of
        {'EXIT', _} ->
            to_float(to_int(V));
        N ->
            N
    end.

to_utf8(Binary, Type) ->
    utf8(Binary, <<>>, <<>>, Type).
utf8(<<>>, Block, Result, Type) ->
    Code = iconverl:get_utf8(Block, Type),
    <<Result/binary, Code/binary>>;
utf8(<<I:8, Rest/binary>>, Block, Result, Type) when I < 128 andalso I > 0 ->
    Code = iconverl:get_utf8(Block, Type),
    Ascii = <<I:8>>,
    utf8(Rest, <<>>, <<Result/binary, Code/binary, Ascii/binary>>, Type);
utf8(<<I:8, Rest/binary>>, Block, Result, Type) ->
    utf8(Rest, <<Block/binary, I:8>>, Result, Type).

to_map(Map) when is_map(Map) ->
    Map;

to_map(List) when is_list(List) ->
    list_to_map(List);

to_map(Data) when is_binary(Data) ->
    case jsx:is_json(Data) of
        true ->
            jsx:decode(Data, [{labels, binary}, return_maps]);
        _ ->
            Data
    end.

list_to_map(List) -> list_to_map(List, #{}).
list_to_map([], Map) -> Map;
list_to_map([{}], Map) -> Map;
list_to_map([{Key, Value} | Other], Map) ->
    case is_list(Value) of
        true ->
            list_to_map(Other, Map#{to_binary(Key) => list_to_map(Value, #{})});
        false ->
            list_to_map(Other, Map#{to_binary(Key) => Value})
    end;
list_to_map(Arr, _Map) ->
    Arr.


tokens(S, []) ->
    [S];
tokens(S, [P | Other]) ->
    case string:tokens(S, P) of
        [S] ->
            tokens(S, Other);
        Res ->
            Res
    end.

% 将字符串转成erlang格式
to_term(Bin) when is_binary(Bin) ->
    to_term(binary_to_list(Bin));
to_term(Str) when is_list(Str) ->
    New = case lists:nth(length(Str), Str) == $. of
              true -> Str;
              false -> Str ++ "."
          end,
    case erl_scan:string(New) of
        {ok, Scan, _} ->
            case erl_parse:parse_exprs(Scan) of
                {ok, P} ->
                    case erl_eval:exprs(P, []) of
                        {value, Value, []} -> {ok, Value};
                        Reason -> {error, Reason}
                    end;
                Reason ->
                    {error, Reason}
            end;
        Reason ->
            {error, Reason}
    end.

is_like(_, []) -> false;
is_like(Keywords, [Value | Other]) when Value == null; Value == undefined; Value == <<>> ->
    is_like(Keywords, Other);
is_like(Keywords, [Value | Other]) ->
    Re = <<".*?", Keywords/binary, ".*?">>,
    case re:run(Value, Re) of
        {match, _} -> true;
        _ -> is_like(Keywords, Other)
    end.

is_alive(Pid) when is_pid(Pid) ->
    is_process_alive(Pid) andalso Pid;
is_alive(Name) when is_binary(Name) orelse is_list(Name) ->
    is_alive(to_atom(Name));
is_alive(Name) when is_atom(Name) ->
    Pid = case whereis(Name) of
              undefined -> global:whereis_name(Name);
              Pid1 -> Pid1
          end,
    Pid =/= undefined andalso Pid.

join(Sep, L) -> join(Sep, L, false).
join(Sep, L, Trip) -> join(Sep, L, Trip, fun to_binary/1).
join(_Sep, [], _, _) -> [];
join(Sep, [<<>> | T], true, F) -> join(Sep, T, true, F);
join(Sep, [H | T], Trip, F) -> [F(H) | join_prepend(Sep, T, Trip, F)].
join_prepend(_Sep, [], _, _) -> [];
join_prepend(Sep, [<<>> | T], true, F) -> join_prepend(Sep, T, true, F);
join_prepend(Sep, [H | T], Trip, F) -> [Sep, F(H) | join_prepend(Sep, T, Trip, F)].

append(New, Map) when is_map(New) andalso is_map(Map) ->
    maps:merge(Map, New);
append(New, Opts) when is_list(New) andalso is_list(Opts) ->
    Opts ++ New.

append(Key, Value, Map) when is_map(Map) ->
    Map#{Key => Value};
append(Key, Value, Opts) when is_list(Opts) ->
    [{Key, Value} | proplists:delete(Key, Opts)];

append(Key, Value, {Key1, Opts}) ->
    {Key1, append(Key, Value, Opts)};
append(Key, Value, Other) ->
    #{Key => Value, <<"data">> => Other}.


get_val(Key, MapOpt, Default) ->
    case get_val(Key, MapOpt) of
        null -> Default;
        undefined -> Default;
        Value -> Value
    end.

get_val(Key, Map) when is_map(Map) ->
    maps:get(Key, Map, undefined);
get_val(Key, Opts) when is_list(Opts) ->
    proplists:get_value(Key, Opts);
get_val(_, _) -> undefined.


check_value_is_null(Value) when Value == ""; Value == undefined; Value == <<>> ->
    null;
check_value_is_null(Value) ->
    Value.

%% 检查值，没有则抛错
check_val(Key, Params, Err) ->
    Value = get_val(Key, Params, undefined),
    case check_value_is_null(Value) of
        null -> throw({error, Err});
        Value -> Value
    end.


open_dets(Name) ->
    open_dets(Name, []).
open_dets(Name, Opts) ->
    Path = lists:concat(["data/", Name, ".dets"]),
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case dets:open_file(Name, [{file, Path} | Opts]) of
                {ok, Name} ->
                    {ok, Path};
                {error, Why} ->
                    {error, Why}
            end
    end.

%%--------------------------------------------------------------------
%% Btis Query and Op
%%--------------------------------------------------------------------
bits_to_binary(BitList) when is_list(BitList) ->
    <<<<X:1>> || X <- BitList>>.
binary_to_bits(Binary) when is_binary(Binary) ->
    [X || <<X:1>> <= Binary].
get_binary_bits_n(Binary, N) when is_binary(Binary) ->
    Pos = N - 1,
    <<_:Pos, Value:1, _/bits>> = Binary,
    Value.
set_binary_bits_n(Binary, N, Value) when is_binary(Binary) ->
    Pos = N - 1,
    <<Head:Pos, _:1, Tail/bits>> = Binary,
    <<Head:Pos, Value:1, Tail/bits>>.
binary_bits_sum(Binary) when is_binary(Binary) ->
    lists:sum([X || <<X:1>> <= Binary]).
binary_bits_zero_list(Binary) when is_binary(Binary) ->
    List = [X || <<X:1>> <= Binary],
    {ZeroList, _} = lists:foldl(
        fun(X, {Acc, Pos}) ->
            case X of
                1 -> {Acc, Pos + 1};
                0 -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
            end
        end, {[], 0}, List),
    ZeroList.

binary_bits_nozero_list(Binary) when is_binary(Binary) ->
    List = [X || <<X:1>> <= Binary],
    {NoZeroList, _} = lists:foldl(
        fun(X, {Acc, Pos}) ->
            case X of
                0 -> {Acc, Pos + 1};
                _ -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
            end
        end, {[], 0}, List),
    NoZeroList.

binary_bits_zip(Binary) when is_binary(Binary) ->
    ReverBin = list_to_binary(lists:reverse(binary_to_list(Binary))),
    list_to_binary(lists:reverse(binary_to_list(zip_bin(ReverBin)))).

zip_bin(<<0:8, Binary/binary>>) ->
    zip_bin(Binary);
zip_bin(Binary) ->
    Binary.

is_in_binary(<<>>, _Binary) ->
    true;
is_in_binary(Partten, Binary) ->
    case binary:match(Binary, Partten) of
        nomatch -> false;
        _ -> true
    end.

binary_start_with(_Binary, <<>>) ->
    true;
binary_start_with(Binary, Partten) ->
    case binary:match(Binary, Partten) of
        {0, _} -> true;
        _ -> false
    end.


reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

% 定义函数
xor_sum(B) ->
    xor_sum(1, [X || <<X:8>> <= B], 0).

% 定义递归函数
xor_sum(I, List, Acc) when I =< length(List) ->
    xor_sum(I + 1, List, Acc bxor lists:nth(I, List));
xor_sum(_, _, Acc) ->
    Acc.

get_parity(Data) when is_binary(Data) ->
    get_parity(binary_to_list(Data));
get_parity(Data) when is_list(Data) ->
    lists:foldl(
        fun(X, Sum) ->
            ((X rem 256) + Sum) rem 256
        end, 0, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec crc16_ccitt(Data::binary()) -> Result::integer()
%% @doc Caclulate crc.
%%  Name  : CRC-16 CCITT-FALSE
%%  Poly  : 0x8408
%%  Init  : 0xFFFF
%%  Revert: false
%%  XorOut: 0x0000
%%  Check : 0x6F91 ("123456789")
%%  MaxLen: 4095 bytes (32767 bits) - detection single, double, triple, and all odd errors
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec crc16_ccitt(Data :: binary()) -> integer().
crc16_ccitt(Data) when is_binary(Data) ->
    crc16_ccitt(binary_to_list(Data), 16#ffff).

crc16_ccitt([], Crc) ->
    <<A:8, B:8>> = <<Crc:16>>,
    <<B:8, A:8>>;
crc16_ccitt([Head | Tail], Crc) ->
    NewCrc = Crc bxor ((Head bsl 8) band 16#ffff),
    crc16_ccitt(Tail, crc16_ccitt_loop(8, NewCrc)).

crc16_ccitt_loop(Count, Crc) when Count == 0 ->
    Crc;
crc16_ccitt_loop(Count, Crc) ->
    NewCrc = ((Crc bsl 1) band 16#ffff),
    case (Crc band 16#8000) of
        0 ->
            crc16_ccitt_loop(Count - 1, NewCrc);
        _ ->
            crc16_ccitt_loop(Count - 1, NewCrc bxor 16#1021)
    end.

%%    CRC-16/MODBUS 算法：
%%    在CRC计算时只用8个数据位，起始位及停止位，如有奇偶校验位也包括奇偶校验位，都不参与CRC计算。
%%    CRC计算方法是：
%%    1、 加载一值为0XFFFF的16位寄存器，此寄存器为CRC寄存器。
%%    2、 把第一个8位二进制数据（即通讯信息帧的第一个字节）与16位的CRC寄存器的相异或，异或的结果仍存放于该CRC寄存器中。
%%    3、 把CRC寄存器的内容右移一位，用0填补最高位，并检测移出位是0还是1。
%%    4、 如果移出位为零，则重复第三步（再次右移一位）；如果移出位为1，CRC寄存器与0XA001进行异或。
%%    5、 重复步骤3和4，直到右移8次，这样整个8位数据全部进行了处理。
%%    6、 重复步骤2和5，进行通讯信息帧下一个字节的处理。
%%    7、 将该通讯信息帧所有字节按上述步骤计算完成后，得到的16位CRC寄存器的高、低字节进行交换
%%    8、 最后得到的CRC寄存器内容即为：CRC校验码。
crc16(Buff) -> crc16(Buff, 16#FFFF).
crc16(<<>>, Crc) ->
    <<A:8, B:8>> = <<Crc:16>>,
    <<B:8, A:8>>;
crc16(<<B:8, Other/binary>>, Crc) ->
    NewCrc =
        lists:foldl(
            fun(_, Acc) ->
                Odd = Acc band 16#0001,
                New = Acc bsr 1,
                case Odd of
                    1 ->
                        New bxor 16#A001;
                    0 ->
                        New
                end
            end, Crc bxor B, lists:seq(1, 8)),
    crc16(Other, NewCrc).

crc16_h(Buff) ->
    <<A:8, B:8>> = crc16(Buff),
    <<B:8, A:8>>.

add_33h(Data) when is_binary(Data) ->
    add_33h(binary_to_list(Data));

add_33h(Data) when is_list(Data) ->
    lists:map(
        fun(Y) ->
            add_value(Y)
        end, Data).

sub_33h(Data) when is_binary(Data) ->
    sub_33h(binary_to_list(Data));

sub_33h(Data) when is_list(Data) ->
    lists:map(
        fun(Y) ->
            sub_value(Y)
        end, Data).

sub_value(Value) when Value < 16#33 ->
    16#FF + Value - 16#33 + 1;
sub_value(Value) ->
    Value - 16#33.

add_value(Value) when Value < 16#CC ->
    Value + 16#33;
add_value(Value) ->
    Value + 16#33 - 16#FF - 1.

-spec hash(any()) -> list().
hash(Value) ->
    to_list(to_hex(crypto:hash(md5, to_list(Value)))).
-spec hash(any(), any()) -> list().
hash(Value1, Value2) ->
    hash(lists:concat([to_list(Value1), to_list(Value2)])).
-spec hash(any(), any(), any()) -> list().
hash(Value1, Value2, Value3) ->
    hash(lists:concat([to_list(Value1), to_list(Value2), to_list(Value3)])).
-spec hash(any(), any(), any(), any()) -> list().
hash(Value1, Value2, Value3, Value4) ->
    hash(lists:concat([to_list(Value1), to_list(Value2), to_list(Value3), to_list(Value4)])).

hash_2(Value) ->
    to_list(Value).
hash_2(Value1, Value2) ->
    lists:concat([to_list(Value1), to_list(Value2)]).
hash_2(Value1, Value2, Value3) ->
    lists:concat([to_list(Value1), to_list(Value2), to_list(Value3)]).
hash_2(Value1, Value2, Value3, Value4) ->
    lists:concat([to_list(Value1), to_list(Value2), to_list(Value3), to_list(Value4)]).

%% single quotes wrapped binary() | list()
-spec squotes_wrapped(binary() | list()) -> list().
squotes_wrapped(Value) ->
    lists:concat(["'", to_list(Value), "'"]).

-spec round(float(), integer()) -> float().
round(Num, Len) ->
    NewLen = min(308, Len),
    N = math:pow(10, NewLen),
    round(Num * N) / N.

read(Path, Fun, Acc) ->
    case file:open(Path, [read]) of
        {ok, IoDevice} ->
            R = read_line(IoDevice, Fun, Acc),
            file:close(IoDevice),
            R;
        {error, Reason} ->
            {error, Reason}
    end.
read_line(IoDevice, Fun, Acc) ->
    case file:read_line(IoDevice) of
        {ok, Row} ->
            read_line(IoDevice, Fun, Acc ++ [Fun(Row)]);
        eof ->
            Acc;
        {error, _Reason} ->
            Acc
    end.

rate(_Success, 0) ->
    0.0;
rate(Success, All) ->
    min(100.0, round((to_int(Success) / to_int(All) * 100), 2)).


merge_maps(MapList) ->
    lists:foldl(fun(Map, Acc) -> maps:merge(Acc, Map) end, #{}, MapList).

make_error_response(HttpStatusCode, ErrorCode, ErrorMsg) when 400 =< HttpStatusCode andalso HttpStatusCode =< 418 ->
    erlang:throw({HttpStatusCode, ErrorCode, ErrorMsg}).

new_pns(Pns) ->
    NewPns = gb_sets:from_list(lists:map(fun(Pn) -> dgiot_utils:to_int(Pn) end, Pns)),
    new_pns(NewPns, [], 1).

new_pns(_Pns, List, 2049) ->
    dgiot_utils:bits_to_binary(lists:reverse(List));

new_pns(Pns, List, N) ->
    NewList =
        case gb_sets:is_member(N, Pns) of
            true ->
                [1 | List];
            false ->
                [0 | List]
        end,
    new_pns(Pns, NewList, N + 1).

get_env(Key) ->
    case get_env(Key, not_find) of
        not_find -> throw({error, not_find});
        Value -> Value
    end.

get_env(Key, Default) ->
    get_env(?MODULE, Key, Default).

get_env(App, Key, Default) ->
    application:get_env(App, Key, Default).

format(Format, Args) ->
    re:replace(lists:flatten(io_lib:format(Format, Args)), "\"|\n|\s+", " ", [global, {return, binary}]).

guid() ->
<<<<<<< HEAD
    <<<<Y>> || <<X:4>> <= emqx_guid:gen(), Y <- integer_to_list(X, 16)>>.
=======
    <<<<Y>> || <<X:4>> <= dgiot_guid:gen(), Y <- integer_to_list(X, 16)>>.
>>>>>>> origin/dgaiot-plugins

new_counter(Name, Max) ->
    dgiot_data:insert({{Name, counter}, max}, Max),
    dgiot_data:insert({Name, counter}, 0).

update_counter(Name) ->
    {ok, Max} = dgiot_data:lookup({{Name, counter}, max}),
    dgiot_data:update_counter({Name, counter}, {2, 1, Max, 1}).


parse_objectid() ->
    Alphas = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890",
    [lists:nth(round(rand:uniform() * 61 + 1), Alphas) || _ <- lists:seq(1, 10)].


shuffle(List) ->
    lists:sort(fun(_, _) -> rand:uniform() < rand:uniform() end, List).

split_zetag(Tag) when size(Tag) =:= 8 ->
    {Rt, _} = erlang:split_binary(Tag, 4),
    Rt;

split_zetag(_) ->
    throw({error, <<"bad zetag">>}).

%% @doc 通过遍历去重
-spec unique_1(List) -> Return when
    List :: list(),
    Return :: list().
unique_1(List) ->
    unique_1(List, []).

unique_1([], ResultList) -> ResultList;
unique_1([H | L], ResultList) ->
    case lists:member(H, ResultList) of
        true -> unique_1(L, ResultList);
        false -> unique_1(L, [H | ResultList])
    end.

%% @doc 利用set结构去重
-spec unique_2(List) -> Return when
    List :: list(),
    Return :: list().
unique_2(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.


wait_request(Time, _) when Time =< 0 ->
    false;
wait_request(Time, Fun) ->
    Sec = min(Time, 1000),
    receive
    after Sec ->
        case Fun() of
            false ->
                wait_request(Time - Sec, Fun);
            Result ->
                Result
        end
    end.

%%
%% @description: 读取json文件并返回
%%
get_JsonFile(Mod, FileName) ->
    {file, Here} = code:is_loaded(Mod),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/json/", dgiot_utils:to_list(FileName)]),
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
%%            针对获取不到的文件做处理
            <<"{}">>;
        {ok, Bin} ->
            case jsx:is_json(dgiot_utils:to_binary(Bin)) of
                true ->
                    dgiot_json:decode(Bin);
                false ->
                    Bin
            end
    end.

get_file(Root, Files, App) ->
    FileName = App ++ "_" ++ dgiot_utils:to_list(dgiot_datetime:now_secs()) ++ ".zip",
    file:make_dir(Root ++ App),
    ZipRoot = Root ++ "/" ++ App ++ "_" ++ dgiot_utils:to_list(dgiot_datetime:now_secs()),
    file:make_dir(ZipRoot),
    ZipFile = Root ++ "/" ++ App ++ "/" ++ FileName,
    lists:map(fun({FileApp, FileName1}) ->
        FileAppList = unicode:characters_to_list(FileApp),
        FileNameList = unicode:characters_to_list(FileName1),
        ZipApp = ZipRoot ++ "/" ++ FileAppList,
        file:make_dir(ZipApp),
        SrcFile = Root ++ "/" ++ FileAppList ++ "/" ++ FileNameList,
        DestFile = ZipApp ++ "/" ++ FileNameList,
        file:copy(SrcFile, DestFile)
              end, Files),
    Cmd = case os:type() of
              {win32, _} -> "chcp 65001 && dgiot_zip zippath " ++ ZipFile ++ " " ++ ZipRoot;
              _ -> "zip  " ++ ZipFile ++ " " ++ ZipRoot
          end,
    ?LOG(info, "Cmd ~p", [Cmd]),
    spawn(fun() ->
        os:cmd(Cmd)
          end),
    App ++ "/" ++ FileName.

post_file(Root, FileName) ->
    ZipFile = Root ++ "/" ++ FileName,
    ?LOG(info, "ZipFile ~p", [ZipFile]),
    case filelib:is_file(ZipFile) of
        true ->
            Cmd = case os:type() of
                      {win32, _} -> "chcp 65001 && dgiot_zip unzip " ++ ZipFile ++ " " ++ Root;
                      _ -> "unzip -O CP936 " ++ ZipFile ++ " -d " ++ Root
                  end,
            ?LOG(info, "Cmd ~p", [Cmd]),
            {ok, os:cmd(Cmd)};
        false ->
            ?LOG(info, "~p not exist", [ZipFile]),
            {error, ZipFile ++ "not exirt"}
    end.


get_macs() ->
    case inet:getifaddrs() of
        {ok, Iflist} ->
            Macs = lists:foldl(fun({_K, V}, [A, B, C, D, E, F] = Acc) ->
                case proplists:get_value(hwaddr, V) of
                    [A1, B1, C1, D1, E1, F1] ->
                        [A + A1, B + B1, C + C1, D + D1, E + E1, F + F1];
                    _ ->
                        Acc
                end
                               end, [0, 0, 0, 0, 0, 0], Iflist),
            dgiot_utils:to_md5(dgiot_utils:to_binary(lists:concat(Macs)));
        _ ->
            dgiot_utils:to_md5(dgiot_utils:random())
    end.

%随机生成16位Key值
random() ->
    to_md5(io_lib:format("~p", [erlang:make_ref()])).

get_hostname() ->
    dgiot_network:get_hostname().

get_natip() ->
    dgiot_network:get_natip().

get_wlanip() ->
    dgiot_network:get_wlanip().

get_computerconfig() ->
    dgiot_network:get_computerconfig().

get_ipbymac(Mac, Network) ->
    dgiot_network:get_ipbymac(Mac, Network).

get_ipbymac(Mac) ->
    dgiot_network:get_ipbymac(Mac).

get_macbyip(Ip) ->
    dgiot_network:get_macbyip(Ip).

get_ip(A) ->
    dgiot_network:get_ip(A).

get_port(Socket) ->
    dgiot_network:get_port(Socket).

ping_all() ->
    dgiot_network:ping_all().
ping_all(Network) ->
    dgiot_network:ping_all(Network).

get_ifaddrs() ->
    dgiot_network:get_ifaddrs().

get_ifaddr(Inetsname) ->
    dgiot_network:get_ifaddr(Inetsname).

get_ifip(Inetsname) ->
    dgiot_network:get_ifip(Inetsname).

get_ipv4(Hostent) ->
    dgiot_network:get_ipv4(Hostent).

get_ipv6(Hostent) ->
    dgiot_network:get_ipv6(Hostent).

resolve(Host) ->
    dgiot_network:resolve(Host).

get_url_path(Url) when is_list(Url) ->
    dgiot_network:get_url_path(Url).

get_ports() ->
    dgiot_network:get_ports().

check_port(Port) ->
    dgiot_network:check_port(Port).

trim_string(Str) when is_binary(Str) ->
    trim_string(Str, binary);
trim_string(Str) when is_list(Str) ->
    trim_string(Str, list).

trim_string(Str, Ret) ->
    Str1 = re:replace(Str, "\\s+", "", [global, {return, Ret}]),
    re:replace(Str1, "^[\s\x{3000}]+|[\s\x{3000}]+$", "", [global, {return, Ret}, unicode]).


%% @private
%% Reproducible gzip by not setting mtime and OS
%%
%% From https://tools.ietf.org/html/rfc1952
%%
%% +---+---+---+---+---+---+---+---+---+---+
%% |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
%% +---+---+---+---+---+---+---+---+---+---+
%%
%% +=======================+
%% |...compressed blocks...| (more-->)
%% +=======================+
%%
%% +---+---+---+---+---+---+---+---+
%% |     CRC32     |     ISIZE     |
%% +---+---+---+---+---+---+---+---+
gzip(Uncompressed) ->
    Compressed = gzip_no_header(Uncompressed),
    Header = <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0>>,
    Crc = erlang:crc32(Uncompressed),
    Size = byte_size(Uncompressed),
    Trailer = <<Crc:32/little, Size:32/little>>,
    iolist_to_binary([Header, Compressed, Trailer]).

%% @private
gzip_no_header(Uncompressed) ->
    Zstream = zlib:open(),

    try
        zlib:deflateInit(Zstream, default, deflated, -15, 8, default),
        Compressed = zlib:deflate(Zstream, Uncompressed, finish),
        zlib:deflateEnd(Zstream),
        iolist_to_binary(Compressed)
    after
        zlib:close(Zstream)
    end.

is_phone(Phone) ->
    case re:run(Phone, "(^1(3[0-9]|4[01456879]|5[0-35-9]|6[2567]|7[0-8]|8[0-9]|9[0-35-9])\\d{8}$)") of
        {match, _} ->
            true;
        _ ->
            false
    end.

is_email(Email) ->
    case re:run(Email, "(^[a-zA-Z0-9_.-]+@[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*\.(com|cn|net)$)") of
        {match, _} ->
            true;
        _ ->
            false
    end.

%%
%% @description: 读取json文件并返回
%%
get_mock(Module, FileName) ->
    {file, Here} = code:is_loaded(Module),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/mock/"]),
    Name = dgiot_utils:to_list(FileName),
    NewName =
        case filename:extension(Name) of
            [] ->
                Name ++ ".json";
            _ ->
                Name
        end,
    Path = Dir ++ NewName,
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
            #{};
        {ok, Bin} ->
            case jsx:is_json(dgiot_utils:to_binary(Bin)) of
                true ->
                    dgiot_json:decode(Bin);
                false ->
                    Bin
            end
    end.

%%
%% @description: 写入json
%%
write_mock(Module, FileName, Json) ->
    {file, Here} = code:is_loaded(Module),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/mock/"]),
    Name = dgiot_utils:to_list(FileName),
    NewName =
        case filename:extension(Name) of
            [] ->
                Name ++ ".json";
            _ ->
                Name
        end,
    Path = Dir ++ NewName,
    file:write_file(Path, dgiot_json:encode(Json)).


is_number(<<"0">>) ->
    true;
is_number(<<"1">>) ->
    true;
is_number(<<"2">>) ->
    true;
is_number(<<"3">>) ->
    true;
is_number(<<"4">>) ->
    true;
is_number(<<"5">>) ->
    true;
is_number(<<"6">>) ->
    true;
is_number(<<"7">>) ->
    true;
is_number(<<"8">>) ->
    true;
is_number(<<"9">>) ->
    true;
is_number(_) ->
    false.

%% 方差计算
variance(Avg, Values) when length(Values) > 0 ->
    Sum =
        lists:foldl(fun(V, Acc) ->
            Acc + ((V - Avg) * (V - Avg))
                    end, 0, Values),
    Sum / length(Values);

variance(_, _) ->
    0.

%%  中位数
find_median(Values) ->

    (lists:max(Values) + lists:min(Values)) / 2.

%% 这个函数采用递归方式实现，处理二进制数据时每次处理一个字节。
%% 在递归过程中，每个字节都会被异或到累计的CRC高字节中，
%% 然后使用一个预定义的CRC表查找新的CRC高字节和CRC低字节。
%% 函数最终返回计算出的CRC校验值。

modbus_crc16(PData) ->
    modbus_crc16(PData, 16#FF, 16#FF).

modbus_crc16(<<>>, ByCRCHi, ByCRCLo) ->
    <<ByCRCHi:8, ByCRCLo:8>>;
modbus_crc16(<<B:8, Other/binary>>, ByCRCHi, ByCRCLo) ->
    ByIdx = ByCRCHi bxor B,
    NewByCRCHi = ByCRCLo bxor lists:nth(ByIdx + 1, gabyCRCHi()),
    NewByCRCLo = lists:nth(ByIdx + 1, gabyCRCLo()),
    modbus_crc16(Other, NewByCRCHi, NewByCRCLo).

gabyCRCHi() ->
    [
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41,
        16#00, 16#c1, 16#81, 16#40, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0,
        16#80, 16#41, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1, 16#81, 16#40,
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1,
        16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0, 16#80, 16#41,
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1,
        16#81, 16#40, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41,
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#00, 16#c1, 16#81, 16#40,
        16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1,
        16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1, 16#81, 16#40,
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#00, 16#c1, 16#81, 16#40,
        16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0,
        16#80, 16#41, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1, 16#81, 16#40,
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41,
        16#00, 16#c1, 16#81, 16#40, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41,
        16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40, 16#00, 16#c1, 16#81, 16#40,
        16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0, 16#80, 16#41, 16#00, 16#c1,
        16#81, 16#40, 16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41,
        16#00, 16#c1, 16#81, 16#40, 16#01, 16#c0, 16#80, 16#41, 16#01, 16#c0,
        16#80, 16#41, 16#00, 16#c1, 16#81, 16#40].

gabyCRCLo() ->
    [
        16#00, 16#c0, 16#c1, 16#01, 16#c3, 16#03, 16#02, 16#c2, 16#c6, 16#06,
        16#07, 16#c7, 16#05, 16#c5, 16#c4, 16#04, 16#cc, 16#0c, 16#0d, 16#cd,
        16#0f, 16#cf, 16#ce, 16#0e, 16#0a, 16#ca, 16#cb, 16#0b, 16#c9, 16#09,
        16#08, 16#c8, 16#d8, 16#18, 16#19, 16#d9, 16#1b, 16#db, 16#da, 16#1a,
        16#1e, 16#de, 16#df, 16#1f, 16#dd, 16#1d, 16#1c, 16#dc, 16#14, 16#d4,
        16#d5, 16#15, 16#d7, 16#17, 16#16, 16#d6, 16#d2, 16#12, 16#13, 16#d3,
        16#11, 16#d1, 16#d0, 16#10, 16#f0, 16#30, 16#31, 16#f1, 16#33, 16#f3,
        16#f2, 16#32, 16#36, 16#f6, 16#f7, 16#37, 16#f5, 16#35, 16#34, 16#f4,
        16#3c, 16#fc, 16#fd, 16#3d, 16#ff, 16#3f, 16#3e, 16#fe, 16#fa, 16#3a,
        16#3b, 16#fb, 16#39, 16#f9, 16#f8, 16#38, 16#28, 16#e8, 16#e9, 16#29,
        16#eb, 16#2b, 16#2a, 16#ea, 16#ee, 16#2e, 16#2f, 16#ef, 16#2d, 16#ed,
        16#ec, 16#2c, 16#e4, 16#24, 16#25, 16#e5, 16#27, 16#e7, 16#e6, 16#26,
        16#22, 16#e2, 16#e3, 16#23, 16#e1, 16#21, 16#20, 16#e0, 16#a0, 16#60,
        16#61, 16#a1, 16#63, 16#a3, 16#a2, 16#62, 16#66, 16#a6, 16#a7, 16#67,
        16#a5, 16#65, 16#64, 16#a4, 16#6c, 16#ac, 16#ad, 16#6d, 16#af, 16#6f,
        16#6e, 16#ae, 16#aa, 16#6a, 16#6b, 16#ab, 16#69, 16#a9, 16#a8, 16#68,
        16#78, 16#b8, 16#b9, 16#79, 16#bb, 16#7b, 16#7a, 16#ba, 16#be, 16#7e,
        16#7f, 16#bf, 16#7d, 16#bd, 16#bc, 16#7c, 16#b4, 16#74, 16#75, 16#b5,
        16#77, 16#b7, 16#b6, 16#76, 16#72, 16#b2, 16#b3, 16#73, 16#b1, 16#71,
        16#70, 16#b0, 16#50, 16#90, 16#91, 16#51, 16#93, 16#53, 16#52, 16#92,
        16#96, 16#56, 16#57, 16#97, 16#55, 16#95, 16#94, 16#54, 16#9c, 16#5c,
        16#5d, 16#9d, 16#5f, 16#9f, 16#9e, 16#5e, 16#5a, 16#9a, 16#9b, 16#5b,
        16#99, 16#59, 16#58, 16#98, 16#88, 16#48, 16#49, 16#89, 16#4b, 16#8b,
        16#8a, 16#4a, 16#4e, 16#8e, 16#8f, 16#4f, 16#8d, 16#4d, 16#4c, 16#8c,
        16#44, 16#84, 16#85, 16#45, 16#87, 16#47, 16#46, 16#86, 16#82, 16#42,
        16#43, 16#83, 16#41, 16#81, 16#80, 16#40].

%% @doc 检查二进制字符串是否是有效的十六进制字符串
%% 十六进制字符串只包含 0-9, a-f, A-F 字符
is_hex_string(Binary) when is_binary(Binary) ->
    is_hex_string(binary_to_list(Binary));
is_hex_string([]) ->
    true;
is_hex_string([Char | Rest]) ->
    case Char of
        $0 -> is_hex_string(Rest);
        $1 -> is_hex_string(Rest);
        $2 -> is_hex_string(Rest);
        $3 -> is_hex_string(Rest);
        $4 -> is_hex_string(Rest);
        $5 -> is_hex_string(Rest);
        $6 -> is_hex_string(Rest);
        $7 -> is_hex_string(Rest);
        $8 -> is_hex_string(Rest);
        $9 -> is_hex_string(Rest);
        $a -> is_hex_string(Rest);
        $b -> is_hex_string(Rest);
        $c -> is_hex_string(Rest);
        $d -> is_hex_string(Rest);
        $e -> is_hex_string(Rest);
        $f -> is_hex_string(Rest);
        $A -> is_hex_string(Rest);
        $B -> is_hex_string(Rest);
        $C -> is_hex_string(Rest);
        $D -> is_hex_string(Rest);
        $E -> is_hex_string(Rest);
        $F -> is_hex_string(Rest);
        _ -> false
    end.
