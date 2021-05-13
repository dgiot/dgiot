%% -----------------------------------------------------------------------------
%%
%% sqlparse_fold.hrl: SQL - unparsing utilities.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-ifndef(SQLPARSE_FOLD_HRL).
-define(SQLPARSE_FOLD_HRL, true).

-include("sqlparse.hrl").

-define(CHAR_NEWLINE, case os:type() of
                          {unix, _} -> "\n";
                          _ -> "\r\n"
                      end).
-define(CHAR_TAB, "\t").

-define(CUSTOM_INIT(FunState, Ctx, PTree, FoldState),
    ?D("Start~n FunState: ~p~n CtxIn: ~p~n PTree: ~p~n FoldState: ~p~n",
        [FunState, Ctx, PTree, FoldState])).
-define(CUSTOM_RESULT(RT),
    ?D("~n CtxOut: ~p~n", [RT]),
    RT).

-define(DATA_TYPES, [
    "bfile",
    "binary_double",
    "binary_float",
    "blob",
    "clob",
    "char",
    "date",
    "integer",
    "long",
    "nchar",
    "nclob",
    "number",
    "nvarchar2",
    "raw",
    "rowid",
    "timestamp",
    "urowid",
    "varchar2"
]).

-define(FOLD_INIT(FunState, Ctx, PTree),
    ?D("Start~n FunState: ~p~n CtxIn: ~p~n PTree: ~p~n",
        [FunState, Ctx, PTree])).
-define(FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    (fun() ->
        _FunState = set_state_stmnt(FunStateIn, Rule),
        ?D("Start~n FunState: ~p~n CtxIn: ~p~n PTree: ~p~n",
            [_FunState, Ctx, PTree]),
        _FunState
     end)()).
-define(FOLD_RESULT(Ctx),
    ?D("~n CtxOut: ~p~n", [Ctx]),
    Ctx).

-define(OBJECT_PRIVILEGES, [
    "all",
    "all privileges",
    "alter",
    "delete",
    "execute",
    "index",
    "insert",
    "references",
    "select",
    "update"
]).

-define(SYSTEM_PRIVILEGES, [
    "admin",
    "alter any index",
    "alter any materialized view",
    "alter any table",
    "alter any view",
    "create any index",
    "create any materialized view",
    "create any table",
    "create any view",
    "create materialized view",
    "create table",
    "create view",
    "delete any table",
    "drop any index",
    "drop any materialized view",
    "drop any table",
    "drop any view",
    "insert any table",
    "select any table",
    "update any table"
]).

-define(TABLE_OPTIONS, [
    "bag",
    "cluster",
    "local",
    "ordered_set",
    "schema",
    "set"
]).

-record(fstate, {
    indent_lvl = 0,
    stmnts = []
}).

-endif.
