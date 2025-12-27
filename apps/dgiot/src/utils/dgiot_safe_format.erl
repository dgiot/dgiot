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
    safe_format/4           %% 安全格式化字符串（带文件行号）
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
    % 将格式字符串转换为二进制，确保UTF-8编码
    BinaryFormat = ensure_utf8_binary(Format),
    % 将参数转换为二进制格式
    BinaryArgs = lists:map(fun ensure_utf8_binary/1, Args),
    % 使用io:format打印
    io:format(BinaryFormat, BinaryArgs).

%% @doc 安全格式化字符串，带文件行号
%% 自动添加文件路径和行号信息，便于调试
%% @spec safe_format(File, Line, Format, Args) -> ok
%% @param File 文件名
%% @param Line 行号
%% @param Format 格式化字符串，可以包含中文
%% @param Args 格式化参数列表
safe_format(File, Line, Format, Args) ->
    % 构建带文件行号的格式字符串
    FullFormat = "~s ~p " ++ Format,
    % 将文件路径转换为二进制
    BinaryFile = ensure_utf8_binary(File),
    % 构建完整的参数列表
    FullArgs = [BinaryFile, Line | Args],
    % 调用safe_format/2
    safe_format(FullFormat, FullArgs).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
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
