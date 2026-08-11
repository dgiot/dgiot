%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_type_utils 模块 - 类型转换函数
%%%
%%% 提供各种类型之间的转换函数，包括二进制、原子、整数、列表、布尔值、
%%% 浮点数、映射等类型之间的相互转换。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_type_utils).

%% API
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

-include("dgiot.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 转换为二进制
%% @spec to_binary(any()) -> binary()
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_pid(V) -> to_binary(pid_to_list(V));
to_binary(V) when is_map(V) -> dgiot_json:encode(V);
to_binary(V) when is_float(V) -> to_binary(io_lib:format("~p", [V]));
to_binary(V) when is_binary(V) -> V.

%% @doc 转换为原子
%% @spec to_atom(any()) -> atom()
to_atom(V) when is_binary(V) -> binary_to_atom(V, utf8);
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_atom(V) -> V;
to_atom(V) -> to_atom(io_lib:format("~p", [V])).

%% @doc 转换为整数
%% @spec to_int(any()) -> integer()
to_int([V]) -> to_int(V);
to_int(V) when V == null; V == <<"Undefined">>; V == undefined; V == <<>>; V == "" -> 0;
to_int(V) when is_float(V) -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V) -> list_to_integer(V);
to_int(V) when is_binary(V) -> binary_to_integer(V);
to_int(true) -> 1;
to_int(false) -> 0;
to_int(_V) -> throw({error, <<"ValueError">>}).

%% @doc 转换为列表
%% @spec to_list(any()) -> list()
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V) -> V;
to_list(V) -> io_lib:format("~p", [V]).

%% @doc 转换为布尔值
%% @spec to_bool(any()) -> boolean()
to_bool(<<"false">>) -> false;
to_bool("false") -> false;
to_bool(V) when is_integer(V) and V =< 0 -> false;
to_bool(<<"0">>) -> false;
to_bool(_V) -> true.

%% @doc 转换为浮点数
%% @spec to_float(any()) -> float()
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

%% @doc 转换为浮点数（指定精度）
%% @spec to_float(any(), integer()) -> float()
to_float(V, Degree) ->
    New = erlang:float_to_binary(to_float(V), [{decimals, Degree}]),
    to_float(New).

%% @doc 转换为映射
%% @spec to_map(any()) -> map()
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

%% @doc 列表转换为映射
%% @spec list_to_map(list()) -> map()
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

%% @doc 字符串转换为Erlang项式
%% @spec to_term(binary() | list()) -> {ok, term()} | {error, term()}
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

%% @doc 转换为UTF-8编码
%% @spec to_utf8(binary(), binary()) -> binary()
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

%%%===================================================================
%%% 内部函数
%%%===================================================================
