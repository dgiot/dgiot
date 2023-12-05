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
     read_from_csv/2
    , save_csv_ets/2
    , read_csv/3
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
            FileName
    end.
