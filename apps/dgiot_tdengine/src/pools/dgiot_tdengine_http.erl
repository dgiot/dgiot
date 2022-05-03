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

-module(dgiot_tdengine_http).
-author("kenneth").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(HTTPOption, [{timeout, 60000}, {connect_timeout, 60000}]).
-define(REQUESTOption, [{body_format, binary}]).
%% API
-export([start/0, request/4, do_request/3]).
-export([test_showdb/0, test_createdb/0, test_alter/0, show_database/3]).

start() ->
    dgiot_utils:new_counter({tdengine, index}, ?MAX_PROFILE),
    lists:foreach(
        fun(X) ->
            Profile = list_to_atom(lists:concat([http, X])),
            case whereis(list_to_atom(lists:concat([httpc, Profile]))) of
                undefined ->
                    httpc_profile_sup:start_child([{profile, Profile}]);
                _ ->
                    ok
            end
        end,
        lists:seq(1, ?MAX_PROFILE)).

%% Base64
request(Url, UserName, Password, Sql) ->
    Authorization = lists:concat(["Basic ", binary_to_list(base64:encode(<<UserName/binary, ":", Password/binary>>))]),
    do_request(Url, Authorization, Sql).

%% 如果自定义 Authorization: Taosd <TOKEN>
do_request(Url, Authorization, Sql) when is_binary(Url) ->
    do_request(binary_to_list(Url), Authorization, Sql);

do_request(Url, Authorization, Sql) when is_binary(Authorization) ->
    do_request(Url, binary_to_list(Authorization), Sql);

do_request(Url, Authorization, Sql) when is_binary(Sql) ->
    do_request(Url, Authorization, binary_to_list(Sql));

do_request(Url, Authorization, Sql) ->
    Header = [{"Authorization", Authorization}],
    {Sql1, Formatter} =
        case re:run(Sql, <<"tbname\s+as\s+([^\s,]+)">>, [{capture, all_but_first, binary}]) of
            {match, [As]} ->
                {re:replace(Sql, <<"tbname\s+as\s+[^\s,]+">>, <<"tbname">>, [{return, binary}]), #{<<"tbname">> => As}};
            _ ->
                {Sql, #{}}
        end,
    Request = {Url, Header, "text/sql", Sql1},
    Index = dgiot_utils:update_counter({tdengine, index}),
    Profile = list_to_atom(lists:concat([http, Index])),
    timer:sleep(1),
    case catch httpc:request(post, Request, ?HTTPOption, ?REQUESTOption, Profile) of
        {ok, {{_HTTPVersion, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            format_body(StatusCode, Body, Formatter);
        {error, {failed_connect, _}} ->
            dgiot_metrics:inc(dgiot_tdengine,<<"tdengine_save_failed">>,1),
            {error, disconnect};
        {'EXIT', {noproc, {gen_server, call, [_ | _]}}} ->
            {error, <<"httpc_not_start">>};
        {Err, Reason} when Err == error; Err == 'EXIT' ->
            {error, Reason}
    end.


%% head: 表的定义，如果不返回结果集，仅有一列“affected_rows”
%% 返回受影响行数
format_body(_StatusCode, #{
    <<"status">> := <<"succ">>,
    <<"head">> := [<<"affected_rows">>],
    <<"data">> := [[AffectedRows]]
}, _Formatter) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine_save_success">>, 1),
    {ok, #{<<"affected_rows">> => AffectedRows}};

format_body(_StatusCode, #{
    <<"head">> := [<<"count(*)">>],
    <<"data">> := [[Count]]
}, _Formatter) ->
    {ok, #{<<"count">> => Count}};

%% 返回结果集
format_body(_StatusCode, #{
    <<"status">> := <<"succ">>,
    <<"head">> := Fields,
    <<"data">> := Rows
}, Formatter) ->
    Result = [format_head(Fields, Row, #{}, Formatter) || Row <- Rows],
    {ok, #{<<"results">> => Result}};

format_body(_StatusCode, #{<<"status">> := <<"error">>, <<"code">> := Code, <<"desc">> := Reason}, _Formatter) ->
    {error, #{<<"code">> => Code, <<"desc">> => Reason}};
format_body(StatusCode, Body, Formatter) when is_binary(Body) ->
    format_body(StatusCode, jsx:decode(Body, [{labels, binary}, return_maps]), Formatter);
format_body(_StatusCode, Body, _Formatter) ->
    {ok, Body}.


format_head([], [], Acc, _Formatter) -> Acc;
format_head([<<"struct_", H/binary>> | Headers], [V | Values], Acc, Formatter) ->
    [P, S] = re:split(H, <<"_">>),
    Parent = maps:get(P, Acc, #{}),
    format_head(Headers, Values, Acc#{P => Parent#{S => V}}, Formatter);

format_head([H | Headers], [V1 | Values], Acc, Formatter) when H == <<"tbname">>; H == <<"TBNAME">> ->
    case maps:get(<<"tbname">>, Formatter, undefined) of
        undefined ->
            format_head(Headers, Values, Acc#{H => re:replace(V1, ?PRE, <<>>, [{return, binary}])}, Formatter);
        NewH ->
            format_head(Headers, Values, Acc#{NewH => re:replace(V1, ?PRE, <<>>, [{return, binary}])}, Formatter)
    end;

format_head([H0 | Headers], [V | Values], Acc, Formatter) ->
    case re:run(H0, <<"last_row\\(([^\\)]+)\\)">>, [{capture, all_but_first, binary}]) of
        {match, [H]} ->
            format_head([H | Headers], [V | Values], Acc, Formatter);
        _ ->
            format_head(Headers, Values, Acc#{H0 => V}, Formatter)
    end.


show_database(Url, UserName, Password) ->
    request(Url, UserName, Password, <<"SHOW DATABASES;">>).

test_showdb() ->
    lists:foreach(
        fun(Sql) ->
            R = request(<<"http://132.232.121.164:6020/rest/sql">>, <<"root">>, <<"taosdata">>, Sql),
            io:format("~p~n", [R])
        end, [
            <<"select * from dgiot.device;">>,
            <<"use dgiot2;">>,
            <<"show databases;">>,
            <<"delete from dgiot.device where devaddr = '1111'">>
        ]).

test_createdb() ->
    request(<<"http://127.0.0.1:6041/rest/sql">>, <<"root">>, <<"taosdata">>, <<"create database test_alter;">>),
    TAGS1 =
        lists:foldl(fun(X, Acc) ->
            Binx = dgiot_utils:to_binary(X),
            case Acc of
                <<>> ->
                    <<"tag", Binx/binary, " NCHAR(10)">>;
                _ ->
                    <<Acc/binary, ", tag", Binx/binary, " NCHAR(10)">>
            end
                    end, <<>>, lists:seq(1, 128)),
    request(<<"http://127.0.0.1:6041/rest/sql/test_alter">>, <<"root">>, <<"taosdata">>,
        <<"CREATE STABLE test_alter(ts timestamp, speed int) TAGS (", TAGS1/binary, ");">>).

test_alter() ->
    lists:map(fun(X) ->
        Binx = dgiot_utils:to_binary(X),
        Sql = <<"ALTER STABLE test_alter ADD COLUMN field", Binx/binary, " int;">>,
        io:format("Sql ~p~n", [Sql]),
        request(<<"http://127.0.0.1:6041/rest/sql/test_alter">>, <<"root">>, <<"taosdata">>, Sql),
        timer:sleep(100)
              end, lists:seq(1, 4096)).