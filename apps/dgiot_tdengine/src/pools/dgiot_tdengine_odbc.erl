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

-module(dgiot_tdengine_odbc).
-author("jonhl").
-export([start/2, start/1, stop/1]).
-include_lib("dgiot/include/logger.hrl").

start(Ip, Port) ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN={TAOS_DSN}; UID=root;PWD=taosdata; HOST=" ++ dgiot_utils:to_list(Ip) ++ ":" ++ dgiot_utils:to_list(Port), []),
    start(Ref).

start(Ref) ->
    odbc:sql_query(Ref, "CREATE TABLE EMPLOYEE
						(NR integer, FIRSTNAME char varying(20),
						LASTNAME char varying(20), GENDER char(1),
						PRIMARY KEY(NR))"),

    odbc:sql_query(Ref, "INSERT INTO EMPLOYEE VALUES(1, 'Jane', 'Doe', 'F')"),
    {ok, R1} = odbc:describe_table(Ref, "EMPLOYEE"),
    io:format("Table description ~p~n", [R1]),

    odbc:param_query(Ref, "INSERT INTO EMPLOYEE (NR, FIRSTNAME, "
    "LASTNAME, GENDER) VALUES(?, ?, ?, ?)",
        [{sql_integer, [2, 3, 4, 5, 6, 7, 8]},
            {{sql_varchar, 20},
                ["John", "Monica", "Ross", "Rachel", "Piper", "Prue", "Louise"]},
            {{sql_varchar, 20},
                ["Doe", "Geller", "Geller", "Green", "Halliwell", "Halliwell", "Lane"]},
            {{sql_char, 1},
                ["M", "F", "M", "F", "F", "F", "F"]}]),
    {selected, C2, R2} = odbc:sql_query(Ref, "SELECT * FROM EMPLOYEE"),
    io:format("SELECT on Employee ~p~n", [{selected, C2, R2}]),

    {selected, C3, R3} = odbc:param_query(Ref, "SELECT * FROM EMPLOYEE WHERE GENDER=?",
        [{{sql_char, 1}, ["M"]}]),
    io:format("SELECT on Employee ~p~n", [{selected, C3, R3}]),

    odbc:sql_query(Ref, "DROP TABLE EMPLOYEE").


stop(Ref) ->
    odbc:disconnect(Ref),
    odbc:stop().
