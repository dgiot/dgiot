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

-module(dgiot_tdengine_pool).
-author("johnliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([start/3, run_sql/3, login/1]).

start(<<"WS">>, Ip, Port) ->
    {<<"WS">>, {binary_to_list(Ip), Port}};
start(_Type, Ip, Port) ->
    dgiot_tdengine_http:start(),
    {<<"HTTP">>, list_to_binary(lists:concat(["http://", binary_to_list(Ip), ":", Port, "/rest/sql"]))}.

login(#{<<"driver">> := <<"WS">>, <<"url">> := {Ip, Port}}) ->
    dgiot_tdengine_ws:login(Ip, Port).

%% WebSocket
run_sql(#{<<"driver">> := <<"WS">>, <<"ws_pid">> := ConnPid, <<"ws_ref">> := StreamRef} = _Context, _Action, Sql) when byte_size(Sql) > 0 ->
    Body = #{<<"action">> => <<"version">>},
    Frame = {text, jiffy:encode(Body)},
    gun:ws_send(ConnPid, StreamRef, Frame),
    {ws, Ack} = gun:await(ConnPid, StreamRef),
    case Ack of
        {} ->
            ok;
        _ ->
            ok
    end;

%% Action 用来区分数据库操作语句类型(DQL、DML、DDL、DCL)
run_sql(#{<<"driver">> := <<"HTTP">>, <<"url">> := Url, <<"username">> := UserName, <<"password">> := Password} = Context, _Action, Sql) when byte_size(Sql) > 0 ->
    ?LOG(debug, " ~p, ~p, ~p, (~ts)", [Url, UserName, Password, unicode:characters_to_list(Sql)]),
    case dgiot_tdengine_http:request(Url, UserName, Password, Sql) of
        {ok, Result} ->
            case maps:get(<<"channel">>, Context, <<"">>) of
                <<"">> ->
                    ?LOG(debug, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), Result]);
                ChannelId ->
                    dgiot_bridge:send_log(ChannelId, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), jsx:encode(Result)])
            end,
            {ok, Result};
        {error, #{<<"code">> := 896} = Reason} ->
            {ok, Reason#{<<"affected_rows">> => 0}};
        {error, Reason} ->
%%            ?LOG(error, "Execute Fail ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), Reason]),
            {error, Reason}
    end;

run_sql(_Context, _Action, _Sql) ->
    ok.


