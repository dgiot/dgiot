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

-module(dgiot_csv).
-author("johnliu").
-include("dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    read_from_csv/2,
    save_csv_ets/2,
    read_csv/3,
    save_csv_ets/1,
    read_and_modify_csv/3,
    read_and_modify_csv/4,
    read_and_modify_csv/5
]).

read_from_csv(Path, Fun) ->
    case file:open(Path, [read]) of
        {ok, IoDevice} ->
            R = read_csv(IoDevice, Fun, ","),
            file:close(IoDevice),
            R;
        {error, Reason} ->
            {error, Reason}
    end.

read_csv(IoDevice, Fun, Delimiter) ->
    case file:read_line(IoDevice) of
        {ok, Row} ->
            Cols = [list_to_binary(Col) || Col <- string:tokens(lists:sublist(Row, 1, length(Row) - 1), Delimiter)],
            Fun(Cols),
            read_csv(IoDevice, Fun, Delimiter);
        eof ->
            {ok, read_complete};
        {error, Reason} ->
            ?LOG(error, "~p", [Reason])
    end.

%% @doc 读取并修改CSV文件（使用默认分隔符逗号）
%% InputPath: 输入文件路径
%% OutputPath: 输出文件路径
%% ModifyRule: 修改规则函数，格式为 fun(RowIndex, ColumnList) -> ModifiedColumnList end
%% RowIndex: 行号（从1开始）
%% ColumnList: 列值列表（二进制列表）
read_and_modify_csv(InputPath, OutputPath, ModifyRule) ->
    read_and_modify_csv(InputPath, OutputPath, ModifyRule, ",").

%% @doc 读取并修改CSV文件
%% InputPath: 输入文件路径
%% OutputPath: 输出文件路径
%% ModifyRule: 修改规则函数，格式为 fun(RowIndex, ColumnList) -> ModifiedColumnList end
%% RowIndex: 行号（从1开始）
%% ColumnList: 列值列表（二进制列表）
%% Delimiter: 分隔符
read_and_modify_csv(InputPath, OutputPath, ModifyRule, Delimiter) ->
    read_and_modify_csv(InputPath, OutputPath, ModifyRule, Delimiter, []).

%% @doc 读取并修改CSV文件（带选项）
%% Options: 选项列表，支持以下选项：
%%   {header, true|false} - 是否有标题行（默认false）
%%   {encoding, utf8|latin1} - 文件编码（默认utf8）
read_and_modify_csv(InputPath, OutputPath, ModifyRule, Delimiter, Options) ->
    Header = proplists:get_value(header, Options, false),
    Encoding = proplists:get_value(encoding, Options, utf8),
    case file:open(InputPath, [read, binary]) of
        {ok, InputDevice} ->
            case file:open(OutputPath, [write, binary]) of
                {ok, OutputDevice} ->
                    try
                        Result = process_csv_lines(InputDevice, OutputDevice, ModifyRule, Delimiter, Encoding, Header, 1),
                        file:close(InputDevice),
                        file:close(OutputDevice),
                        Result
                    catch
                        _:Error ->
                            file:close(InputDevice),
                            file:close(OutputDevice),
                            {error, Error}
                    end;
                {error, Reason} ->
                    file:close(InputDevice),
                    {error, {open_output_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {open_input_failed, Reason}}
    end.

%% 处理CSV文件的每一行
process_csv_lines(InputDevice, OutputDevice, ModifyRule, Delimiter, Encoding, Header, RowIndex) ->
    case file:read_line(InputDevice) of
        {ok, Line} ->
            % 移除行尾的换行符
            CleanLine = binary:replace(Line, <<"\n">>, <<>>),
            CleanLine2 = binary:replace(CleanLine, <<"\r">>, <<>>),
            
            % 处理标题行（如果需要）
            case Header andalso RowIndex == 1 of
                true ->
                    % 标题行直接写入，不修改
                    ok = file:write(OutputDevice, [Line]),
                    process_csv_lines(InputDevice, OutputDevice, ModifyRule, Delimiter, Encoding, Header, RowIndex + 1);
                false ->
                    % 解析CSV行
                    Cols = parse_csv_line(CleanLine2, Delimiter, Encoding),
                    
                    % 应用修改规则
                    ModifiedCols = try
                        ModifyRule(RowIndex, Cols)
                    catch
                        _:_ -> Cols  % 如果修改函数出错，保持原样
                    end,
                    
                    % 重新编码并写入
                    OutputLine = encode_csv_line(ModifiedCols, Delimiter),
                    ok = file:write(OutputDevice, [OutputLine, <<"\n">>]),
                    
                    process_csv_lines(InputDevice, OutputDevice, ModifyRule, Delimiter, Encoding, Header, RowIndex + 1)
            end;
        eof ->
            {ok, {rows_processed, RowIndex - 1}};
        {error, Reason} ->
            {error, {read_line_failed, Reason}}
    end.

%% 解析CSV行（简化版本，处理基本场景）
parse_csv_line(Line, Delimiter, Encoding) ->
    case Encoding of
        utf8 ->
            parse_csv_line_utf8(Line, Delimiter);
        latin1 ->
            parse_csv_line_latin1(Line, Delimiter)
    end.

parse_csv_line_utf8(Line, Delimiter) ->
    case Delimiter of
        "," -> binary:split(Line, <<",">>, [global, trim_all]);
        ";" -> binary:split(Line, <<";">>, [global, trim_all]);
        "\t" -> binary:split(Line, <<"\t">>, [global, trim_all]);
        "|" -> binary:split(Line, <<"|">>, [global, trim_all]);
        _ -> 
            DelimiterBin = list_to_binary(Delimiter),
            binary:split(Line, DelimiterBin, [global, trim_all])
    end.

parse_csv_line_latin1(Line, Delimiter) ->
    LineUtf8 = unicode:characters_to_binary(Line, latin1, utf8),
    Cols = parse_csv_line_utf8(LineUtf8, Delimiter),
    [unicode:characters_to_binary(Col, utf8, latin1) || Col <- Cols].

%% 编码CSV行
encode_csv_line(Cols, Delimiter) ->
    DelimiterBin = list_to_binary(Delimiter),
    encode_csv_line_helper(Cols, DelimiterBin, []).

encode_csv_line_helper([Col], _DelimiterBin, Acc) ->
    iolist_to_binary(lists:reverse([Col | Acc]));
encode_csv_line_helper([Col | Rest], DelimiterBin, Acc) ->
    encode_csv_line_helper(Rest, DelimiterBin, [DelimiterBin, Col | Acc]).

save_csv_ets(Module, FilePath) ->
    Url = "http://127.0.0.1:1250" ++ dgiot_utils:to_list(FilePath),
    <<FileName:10/binary, _/binary>> = dgiot_utils:to_md5(FilePath),
    {file, Here} = code:is_loaded(Module),
    DownloadPath = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/csv/"]) ++ dgiot_utils:to_list(FileName) ++ ".csv",
    os:cmd("rm -rf " ++ DownloadPath),
    case dgiot_httpc:download(Url, DownloadPath) of
        {ok, saved_to_file} ->
            AtomName = dgiot_utils:to_atom(FileName),
            dgiot_data:delete(AtomName),
            dgiot_data:init(AtomName),
            put(count, -1),
            Fun = fun(X) ->
                Count = get(count),
                case Count > 0 of
                    true ->
                        dgiot_data:insert(AtomName, Count, X ++ [0]);
                    _ ->
                        pass
                end,
                put(count, Count + 1)
                  end,
            read_from_csv(DownloadPath, Fun),
            FileName;
        _ ->
            not_exist
    end.


save_csv_ets(#{<<"fullpath">> := Fullpath}) ->
    <<FileName:10/binary, _/binary>> = dgiot_utils:to_md5(Fullpath),
    AtomName = dgiot_utils:to_atom(FileName),
    dgiot_data:delete(AtomName),
    dgiot_data:init(AtomName),
    put(count, -1),
    Fun = fun(X) ->
        Count = get(count),
        case Count > 0 of
            true ->
                dgiot_data:insert(AtomName, Count, X ++ [0]);
            _ ->
                pass
        end,
        put(count, Count + 1)
          end,
    read_from_csv(Fullpath, Fun),
    AtomName.
