%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_string_utils 模块 - 字符串处理函数
%%%
%%% 提供字符串和二进制之间的转换、格式化、分割、连接等处理函数。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_string_utils).

%% API
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

-include("dgiot.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 二进制转十六进制字符串
%% @spec binary_to_hex(binary()) -> binary()
binary_to_hex(Id) ->
    <<<<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X, 16)>>.

%% @doc 十六进制字符串转二进制
%% @spec hex_to_binary(binary()) -> binary()
hex_to_binary(Id) ->
    NewId = trim_string(Id),
    <<<<Z>> || <<X:8, Y:8>> <= NewId, Z <- [binary_to_integer(<<X, Y>>, 16)]>>.

%% @doc 二进制转十六进制字符串（大写）
%% @spec bin2hexstr_A_F(binary()) -> binary()
bin2hexstr_A_F(B) when is_binary(B) ->
    <<<<(int2hexchar(H, upper)), (int2hexchar(L, upper))>> || <<H:4, L:4>> <= B>>.

%% @doc 二进制转十六进制字符串（小写）
%% @spec bin2hexstr_a_f(binary()) -> binary()
bin2hexstr_a_f(B) when is_binary(B) ->
    <<<<(int2hexchar(H, lower)), (int2hexchar(L, lower))>> || <<H:4, L:4>> <= B>>.

%% @private
%% @doc 整数转十六进制字符
int2hexchar(I, _) when I >= 0 andalso I < 10 -> I + $0;
int2hexchar(I, upper) -> I - 10 + $A;
int2hexchar(I, lower) -> I - 10 + $a.

%% @doc 十六进制字符串转二进制（别名）
%% @spec hexstr2bin(binary()) -> binary()
hexstr2bin(B) when is_binary(B) ->
    <<<<(hexchar2int(H) * 16 + hexchar2int(L))>> || <<H:8, L:8>> <= B>>.

%% @private
%% @doc 十六进制字符转整数
hexchar2int(I) when I >= $0 andalso I =< $9 -> I - $0;
hexchar2int(I) when I >= $A andalso I =< $F -> I - $A + 10;
hexchar2int(I) when I >= $a andalso I =< $f -> I - $a + 10.

%% @doc 转换为十六进制字符串
%% @spec to_hex(any()) -> binary()
to_hex(V) ->
    binary_to_hex(dgiot_type_utils:to_binary(V)).

%% @doc 去除字符串空白字符
%% @spec trim_string(binary() | list()) -> binary() | list()
trim_string(Str) when is_binary(Str) ->
    trim_string(Str, binary);
trim_string(Str) when is_list(Str) ->
    trim_string(Str, list).

%% @private
%% @doc 去除字符串空白字符（内部实现）
trim_string(Str, Ret) ->
    Str1 = re:replace(Str, "\\s+", "", [global, {return, Ret}]),
    re:replace(Str1, "^[\s\x{3000}]+|[\s\x{3000}]+$", "", [global, {return, Ret}, unicode]).

%% @doc 字符串分割
%% @spec tokens(string(), [char()]) -> [string()]
tokens(S, []) ->
    [S];
tokens(S, [P | Other]) ->
    case string:tokens(S, P) of
        [S] ->
            tokens(S, Other);
        Res ->
            Res
    end.

%% @doc 列表连接为字符串
%% @spec join(Sep, List) -> string()
join(Sep, L) -> join(Sep, L, false).

%% @doc 列表连接为字符串（带修剪）
%% @spec join(Sep, List, Trip) -> string()
join(Sep, L, Trip) -> join(Sep, L, Trip, fun dgiot_type_utils:to_binary/1).

%% @doc 列表连接为字符串（带转换函数）
%% @spec join(Sep, List, Trip, Fun) -> string()
join(_Sep, [], _, _) -> [];
join(Sep, [<<>> | T], true, F) -> join(Sep, T, true, F);
join(Sep, [H | T], Trip, F) -> [F(H) | join_prepend(Sep, T, Trip, F)].

%% @private
%% @doc 列表连接为字符串（内部实现）
join_prepend(_Sep, [], _, _) -> [];
join_prepend(Sep, [<<>> | T], true, F) -> join_prepend(Sep, T, true, F);
join_prepend(Sep, [H | T], Trip, F) -> [Sep, F(H) | join_prepend(Sep, T, Trip, F)].

%% @doc 格式化字符串
%% @spec format(Format, Args) -> binary()
format(Format, Args) ->
    re:replace(lists:flatten(io_lib:format(Format, Args)), "\"|\n|\s+", " ", [global, {return, binary}]).

%% @doc 添加单引号包装
%% @spec squotes_wrapped(binary() | list()) -> list()
squotes_wrapped(Value) ->
    lists:concat(["'", dgiot_type_utils:to_list(Value), "'"]).

%% @doc 反转字符串/二进制
%% @spec reverse(binary() | list()) -> binary() | list()
reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

%%%===================================================================
%%% 内部函数
%%%===================================================================
