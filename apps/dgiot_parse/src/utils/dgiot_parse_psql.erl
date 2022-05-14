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

-module(dgiot_parse_psql).
-author("johnliu").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    export_tables/0,
    import_tables/0,
    export_table/1,
    import_table/2
]).

%%数据表导出
%%postgres=# \c parse;
%%You are now connected to database "parse" as user "postgres".
%%parse=# \dt
%%List of relations
%%Schema |             Name             | Type  |  Owner
%%--------+------------------------------+-------+----------
%%public | App                          | table | postgres
%%public | Article                      | table | postgres
%%public | Category                     | table | postgres
%%public | Channel                      | table | postgres
%%public | Crond                        | table | postgres
%%public | Device                       | table | postgres
%%public | Devicelog                    | table | postgres
%%public | Dict                         | table | postgres
%%public | Evidence                     | table | postgres
%%public | Instruct                     | table | postgres
%%public | License                      | table | postgres
%%public | Log                          | table | postgres
%%public | LogLevel                     | table | postgres
%%public | Menu                         | table | postgres
%%public | MetaData                     | table | postgres
%%public | Notification                 | table | postgres
%%public | Permission                   | table | postgres
%%public | Product                      | table | postgres
%%public | ProductTemplet               | table | postgres
%%public | Project                      | table | postgres
%%public | Timescale                    | table | postgres
%%public | View                         | table | postgres
%%public | _Audience                    | table | postgres
%%public | _GlobalConfig                | table | postgres
%%public | _GraphQLConfig               | table | postgres
%%public | _Hooks                       | table | postgres
%%public | _Installation                | table | postgres
%%public | _JobSchedule                 | table | postgres
%%public | _JobStatus                   | table | postgres
%%public | _Join:app:Project            | table | postgres
%%public | _Join:children:Product       | table | postgres
%%public | _Join:deletedBy:Notification | table | postgres
%%public | _Join:menus:_Role            | table | postgres
%%public | _Join:product:Channel        | table | postgres
%%public | _Join:product:Project        | table | postgres
%%public | _Join:readBy:Notification    | table | postgres
%%public | _Join:role:_User             | table | postgres
%%public | _Join:roles:_Role            | table | postgres
%%public | _Join:roles:_User            | table | postgres
%%public | _Join:rules:_Role            | table | postgres
%%public | _Join:users:_Role            | table | postgres
%%public | _Join:users:_Session         | table | postgres
%%public | _PushStatus                  | table | postgres
%%public | _Role                        | table | postgres
%%public | _SCHEMA                      | table | postgres
%%public | _Session                     | table | postgres
%%public | _User                        | table | postgres
export_tables() ->
    case dgiot_parse:get_schemas() of
        {ok, #{<<"results">> := Results}} ->
            lists:map(fun
                           (#{<<"className">> := <<"_Role">>}) ->
                               pass;
                           (#{<<"className">> := <<"_User">>}) ->
                               pass;
                           (#{<<"className">> := <<"Product">>}) ->
                               pass;
                           (#{<<"className">> := <<"Device">>}) ->
                               pass;
                           (#{<<"className">> := Table}) ->
                               export_table(Table)
                       end,
                Results);
        _ ->
            pass
    end.

export_table(Table) ->
    Header = "sudo -u postgres /usr/local/pgsql/12/bin/pg_dump -h localhost -U postgres -d parse -t public.\"",
    TableName = dgiot_utils:to_list(Table),
    Tail = """\"  --inserts > ",
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join(["\"" ++ Dir, "/priv/sql/", TableName ++ ".sql\""]),
    Sql = Header ++ TableName ++ Tail ++ Path,
    io:format("~s ~p ~s ~n",[?FILE, ?LINE, Sql]),
    os:cmd(Header ++ TableName ++ Tail ++ Path).

import_table(Table, Path) ->
    Header = "sudo -u postgres /usr/local/pgsql/12/bin/psql -h localhost -U postgres  -d parse ",
    TableName = dgiot_utils:to_list(Table),
    %%  psql -h localhost -U postgres  -d parse -c "Drop table public.\"Menu\";";
    os:cmd(Header ++ "-c \"Drop table public.\"" ++ TableName ++ "\";"),
    %%  psql -h localhost -U postgres  -d parse  -f ./Menu.sql
    os:cmd(Header ++ "-f \"" ++ dgiot_utils:to_list(Path) ++ "\";").

%%{"menus":{"__op":"AddRelation","objects":[{"__type":"Pointer","className":"Menu","objectId":"4fe88ddb1a"}]},"_method":"PUT","_ApplicationId":"b068267541e3e578fff1ea84ed0f4cee","_ClientVersion":"js2.8.0","_MasterKey":"58ef0697b520b56757402b7fa06cd7fe","_InstallationId":"93d02d8d-cc00-1095-5e96-db328f522865"}

import_tables() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/sql/"]),
    F =
        fun(F, {I, Acc}) ->
            io:format("~s ~p ~s ~n",[?FILE, ?LINE,I]),
            Table = dgiot_utils:to_binary(re:replace(filename:basename(F), <<".sql">>, <<>>, [{return, binary}])),
            import_table(Table, I),
            Acc ++ [I]
        end,
    {_Count, Files} = filelib:fold_files(Path, dgiot_utils:to_list(<<".sql">>), true, F, {0, []}),
    Files.