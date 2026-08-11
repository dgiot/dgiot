%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_safe_format 模块 - 安全打印函数
%%%
%%% 提供安全格式化字符串函数，解决中文打印编码问题。
%%% 使用二进制格式配合/utf8标志，确保非ASCII字符正确显示。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_safe_format).

%% API
-export([
    safe_format/2,          %% 安全格式化字符串（解决中文打印问题）
<<<<<<< HEAD
    safe_format/4,           %% 安全格式化字符串（带文件行号）
    ensure_utf8_binary/1   %% 确保值为UTF-8编码的二进制
=======
    safe_format/4           %% 安全格式化字符串（带文件行号）
>>>>>>> origin/dgaiot-plugins
]).

-include("dgiot.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 安全格式化字符串，解决中文打印问题
%% 使用二进制格式配合/utf8标志，确保非ASCII字符正确显示
%% @spec safe_format(Format, Args) -> ok
%% @param Format 格式化字符串，可以包含中文
%% @param Args 格式化参数列表
safe_format(Format, Args) ->
<<<<<<< HEAD
    % 将格式字符串转换为列表，确保UTF-8编码
    ListFormat = ensure_utf8_list(Format),
    % 将参数转换为UTF-8列表格式
    ListArgs = lists:map(fun ensure_utf8_list/1, Args),
    % 使用io:format打印，使用~ts格式说明符确保UTF-8正确显示
    io:format(ListFormat, ListArgs).
=======
    % 将格式字符串转换为二进制，确保UTF-8编码
    BinaryFormat = ensure_utf8_binary(Format),
    % 将参数转换为二进制格式
    BinaryArgs = lists:map(fun ensure_utf8_binary/1, Args),
    % 使用io:format打印
    io:format(BinaryFormat, BinaryArgs).
>>>>>>> origin/dgaiot-plugins

%% @doc 安全格式化字符串，带文件行号
%% 自动添加文件路径和行号信息，便于调试
%% @spec safe_format(File, Line, Format, Args) -> ok
%% @param File 文件名
%% @param Line 行号
%% @param Format 格式化字符串，可以包含中文
%% @param Args 格式化参数列表
safe_format(File, Line, Format, Args) ->
    % 构建带文件行号的格式字符串
<<<<<<< HEAD
    FullFormat = "~ts ~p " ++ Format,
    % 将文件路径转换为UTF-8列表
    ListFile = ensure_utf8_list(File),
    % 构建完整的参数列表
    FullArgs = [ListFile, Line | Args],
=======
    FullFormat = "~s ~p " ++ Format,
    % 将文件路径转换为二进制
    BinaryFile = ensure_utf8_binary(File),
    % 构建完整的参数列表
    FullArgs = [BinaryFile, Line | Args],
>>>>>>> origin/dgaiot-plugins
    % 调用safe_format/2
    safe_format(FullFormat, FullArgs).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
<<<<<<< HEAD
%% @doc 确保值为UTF-8编码的列表
%% 如果是二进制，转换为UTF-8列表
%% 如果是列表，确保是有效的UTF-8
%% 如果是其他类型，转换为字符串再处理
ensure_utf8_list(Value) when is_binary(Value) ->
    % 将二进制转换为UTF-8列表
    case unicode:characters_to_list(Value, utf8) of
        {error, _, _} ->
            % 如果不是有效的UTF-8，使用默认编码
            "Invalid UTF-8 data";
        {incomplete, _, _} ->
            % 不完整的UTF-8数据
            "Incomplete UTF-8 data";
        List ->
            % 已经是有效的UTF-8列表
            List
    end;
ensure_utf8_list(Value) when is_list(Value) ->
    % 检查列表是否是有效的UTF-8
    case unicode:characters_to_binary(Value, utf8, utf8) of
        {error, _, _} ->
            % 转换失败，使用默认编码
            "Invalid UTF-8 string";
        {incomplete, _, _} ->
            % 不完整的UTF-8数据
            "Incomplete UTF-8 string";
        _ ->
            % 已经是有效的UTF-8列表
            Value
    end;
ensure_utf8_list(Value) when is_atom(Value) ->
    % 原子转换为列表
    ensure_utf8_list(atom_to_list(Value));
ensure_utf8_list(Value) when is_integer(Value) ->
    % 整数转换为列表
    integer_to_list(Value);
ensure_utf8_list(Value) when is_float(Value) ->
    % 浮点数转换为列表
    float_to_list(Value, [{decimals, 6}, compact]);
ensure_utf8_list(Value) when is_map(Value) ->
    % 映射转换为JSON字符串
    case dgiot_json:encode(Value) of
        {ok, Json} -> ensure_utf8_list(Json);
        _ -> "{}"
    end;
ensure_utf8_list(Value) ->
    % 其他类型转换为字符串
    lists:flatten(io_lib:format("~p", [Value])).

%% @private
=======
>>>>>>> origin/dgaiot-plugins
%% @doc 确保值为UTF-8编码的二进制
%% 如果是字符串，转换为二进制并添加/utf8标志
%% 如果是二进制，直接返回
%% 如果是其他类型，转换为字符串再处理
ensure_utf8_binary(Value) when is_binary(Value) ->
    % 检查是否已经是UTF-8二进制
    case unicode:characters_to_binary(Value, utf8, utf8) of
        {error, _, _} ->
            % 如果不是有效的UTF-8，使用默认编码
            <<"Invalid UTF-8 data"/utf8>>;
        {incomplete, _, _} ->
            % 不完整的UTF-8数据
            <<"Incomplete UTF-8 data"/utf8>>;
        Binary ->
            % 已经是有效的UTF-8二进制
            Binary
    end;
ensure_utf8_binary(Value) when is_list(Value) ->
    % 尝试将列表转换为UTF-8二进制
    case unicode:characters_to_binary(Value, utf8, utf8) of
        {error, _, _} ->
            % 转换失败，使用默认编码
            <<"Invalid UTF-8 string"/utf8>>;
        {incomplete, _, _} ->
            % 不完整的UTF-8数据
            <<"Incomplete UTF-8 string"/utf8>>;
        Binary ->
            % 转换成功
            Binary
    end;
ensure_utf8_binary(Value) when is_atom(Value) ->
    % 原子转换为二进制
    ensure_utf8_binary(atom_to_binary(Value, utf8));
ensure_utf8_binary(Value) when is_integer(Value) ->
    % 整数转换为二进制
    ensure_utf8_binary(integer_to_binary(Value));
ensure_utf8_binary(Value) when is_float(Value) ->
    % 浮点数转换为二进制
    ensure_utf8_binary(float_to_binary(Value, [{decimals, 6}, compact]));
ensure_utf8_binary(Value) when is_map(Value) ->
    % 映射转换为JSON字符串
    case dgiot_json:encode(Value) of
        {ok, Json} -> ensure_utf8_binary(Json);
        _ -> <<"{}"/utf8>>
    end;
ensure_utf8_binary(Value) ->
    % 其他类型转换为字符串
    ensure_utf8_binary(io_lib:format("~p", [Value])).
