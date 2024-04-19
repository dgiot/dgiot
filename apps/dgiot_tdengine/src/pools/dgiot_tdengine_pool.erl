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
-export([insert_sql/3, run_sql/3, login/2]).

%%  ChannelId = <<"24b9b4bc50">>.
%%  Ip = "127.0.0.1".
%%  Port = 6041.
%%  UserName = <<"root">>.
%%  Password = <<"taosdata">>.
login(ChannelId, #{<<"username">> := UserName,
    <<"password">> := Password, <<"ip">> := Ip, <<"port">> := Port}) ->
    dgiot_tdengine_ws:login(ChannelId, Ip, Port, UserName, Password).

%% WebSocket
insert_sql(#{<<"driver">> := <<"WS">>, <<"ws_pid">> := ConnPid, <<"ws_ref">> := StreamRef} = _Context, _Action, Sql) when byte_size(Sql) > 0 ->
%%    io:format("~s ~p insert_sql = ~p.~n", [?FILE, ?LINE, Sql]),
    Req_id =
        case get(req_id) of
            undefined ->
                0;
            Id ->
                Id
        end,
    put(req_id, Req_id + 1),
    Body = #{
        <<"action">> => <<"query">>,
        <<"args">> => #{<<"req_id">> => Req_id, <<"sql">> => Sql}
    },
    Frame = {text, dgiot_json:encode(Body)},
    gun:ws_send(ConnPid, StreamRef, Frame),
    case gun:await(ConnPid, StreamRef) of
        {ws, {text, Data}} ->
            case catch dgiot_json:decode(Data, [return_maps]) of
                #{<<"code">> := _} = R ->
%%                    case maps:get(<<"channel">>, Context, <<"">>) of
%%                        <<"">> ->
%%%%                            ?LOG(debug, "Execute (~ts) ", [unicode:characters_to_list(Sql)]);
%%                            pass;
%%                        ChannelId ->
%%                            dgiot_bridge:send_log(ChannelId, "Execute (~ts) ~p", [unicode:characters_to_list(Sql), dgiot_json:encode(R)])
%%                    end,
                    {ok, R};
                _ ->
                    {error, Data}
            end;
        Error ->
            {error, Error}
    end;

insert_sql(#{<<"driver">> := <<"HTTP">>} = Context, _Action, Sql) when byte_size(Sql) > 0 ->
    run_sql(Context, _Action, Sql);

insert_sql(_Context, _Action, _Sql) ->
    ok.

%% Action 用来区分数据库操作语句类型(DQL、DML、DDL、DCL)
run_sql(#{<<"url">> := Url, <<"username">> := UserName, <<"password">> := Password} = _Context, _Action, Sql) when byte_size(Sql) > 0 ->
%%    io:format("Url = ~p, UserName = ~p, Password = ~p.~n", [Url, UserName, Password]),
%%    io:format("~s ~p Sql = ~p.~n", [?FILE, ?LINE, Sql]),
    case dgiot_tdengine_http:request(Url, UserName, Password, Sql) of
        {ok, #{<<"code">> := 0, <<"column_meta">> := Column_meta, <<"data">> := Data} = Result} ->
            NewData = get_data(Column_meta, Data),
%%            case maps:get(<<"channel">>, Context, <<"">>) of
%%                <<"">> ->
%%%%                    ?LOG(debug, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), NewData]);
%%                    pass;
%%                ChannelId ->
%%                    dgiot_bridge:send_log(ChannelId, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), dgiot_json:encode(NewData)])
%%            end,
            {ok, Result#{<<"results">> => NewData}};
        {ok, #{<<"code">> := 9826} = Reason} ->
            {error, Reason};
        {ok, Result} ->
%%            case maps:get(<<"channel">>, Context, <<"">>) of
%%                <<"">> ->
%%%%                    ?LOG(debug, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), Result]);
%%                    pass;
%%                ChannelId ->
%%                    dgiot_bridge:send_log(ChannelId, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), dgiot_json:encode(Result)])
%%            end,
            {ok, Result};
        {error, #{<<"code">> := 896} = Reason} ->
            {ok, Reason#{<<"affected_rows">> => 0}};
        {error, Reason} ->
            {error, Reason}
    end;

run_sql(_Context, _Action, _Sql) ->
    ok.


%%    {ok, #{<<"code">> => 0,
%%    <<"column_meta">> => [
%%                   [<<"createdat">>, <<"TIMESTAMP">>, 8],
%%                   [<<"energy">>, <<"FLOAT">>, 4]
%%              ],
%%    <<"data">> => [
%%                   [<<"2023-01-06T11:41:17.901Z">>, 6729]
%%               ],
%%    <<"rows">> => 1}
%%    },
%%
%%    {ok, #{<<"results">> =>
%%    [#{<<"createdat">> => <<"2022-11-10 18:30:04.578">>,
%%    <<"online">> => 1}]}},

get_data(Column_meta, Data) ->
    get_data(Column_meta, Data, []).

get_data(_, [], Acc) ->
    Acc;

get_data(Column_meta, [Data | Rest], Acc) ->
    Column = get_column(Column_meta, Data),
    get_data(Column_meta, Rest, Acc ++ [Column]).

get_column(Column_meta, Data) ->
    get_column(Column_meta, Data, #{}).

get_column([], [], Acc) ->
    Acc;

get_column([Column_meta | Cest], [Value | Rest], Acc) ->
    [Field | _] = Column_meta,
    get_column(Cest, Rest, Acc#{Field => Value}).





















