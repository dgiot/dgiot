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

-module(dgiot_tdengine_select).
-author("jonliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([select/2, format_batch/1, format_db/1, format_value/1, format_limit/1]).

select(TableName, Query) ->
    Order = format_order(Query),
    Limit = format_limit(Query),
    Offset = format_offset(Query),
    Where = format_where(Query),
    Interval = format_interval(Query),
    Fill = format_fill(Query),
    From = format_from(Query),
    Group = format_group(Query),
    Tail = list_to_binary(dgiot_utils:join(" ", [TableName, From, Where, Interval, Fill, Group, Order, Limit, Offset], true)),
    Keys = format_keys(Query),
    DB = maps:get(<<"db">>, Query),
    <<"SELECT ", Keys/binary, " FROM ", DB/binary, Tail/binary>>.

format_value(V) when is_binary(V) -> <<"'", V/binary, "'">>;
format_value(V) -> dgiot_utils:to_binary(V).

format_db(<<>>) -> <<>>;
format_db(DB) -> <<DB/binary, ".">>.

format_using(_, <<>>) -> <<>>;
format_using(DB, Table) ->
    <<" using ", DB/binary, Table/binary>>.

format_tags(<<>>) -> <<>>;
format_tags(Tags) -> <<" TAGS (", Tags/binary, ")">>.

format_batch(#{<<"db">> := DB, <<"tableName">> := TableName, <<"fields">> := _Fields0, <<"values">> := Values0} = Batch) ->
    Using = maps:get(<<"using">>, Batch, <<>>),
    Using1 = format_using(DB, Using),
    Tags = maps:get(<<"tags">>, Batch, []),
    TagFields = format_tags(list_to_binary(dgiot_utils:join(",", Tags, false, fun format_value/1))),
    <<DB/binary, TableName/binary, Using1/binary, TagFields/binary, " VALUES ", Values0/binary>>;
format_batch(#{<<"db">> := DB, <<"tableName">> := TableName, <<"values">> := Values0} = Batch) ->
    Using = maps:get(<<"using">>, Batch, <<>>),
    Using1 = format_using(DB, Using),
    Tags = maps:get(<<"tags">>, Batch, []),
    TagFields = format_tags(list_to_binary(dgiot_utils:join(",", Tags, false, fun format_value/1))),
    <<DB/binary,TableName/binary, Using1/binary, TagFields/binary, " VALUES ", Values0/binary>>.

format_order([], Acc) -> Acc;
format_order([<<"-", Field/binary>> | Other], Acc) ->
    format_order(Other, Acc ++ [<<Field/binary, " DESC">>]);
format_order([<<"+", Field/binary>> | Other], Acc) ->
    format_order([Field | Other], Acc);
format_order([Field | Other], Acc) ->
    format_order(Other, Acc ++ [<<Field/binary, " ASC">>]).
format_order(#{<<"order">> := Order}) when Order =/= <<>>, Order =/= undefined ->
    Order1 = list_to_binary(dgiot_utils:join(",", format_order(re:split(Order, <<",">>), []))),
    <<"ORDER BY ", Order1/binary>>;
format_order(_) ->
    <<>>.

format_limit(#{<<"limit">> := Limit}) when is_integer(Limit), Limit =/= undefined ->
    L = integer_to_binary(Limit),
    <<"LIMIT ", L/binary>>;
format_limit(_) ->
    <<"LIMIT 5000">>.

format_offset(#{<<"skip">> := Skip}) when Skip =/= undefined, is_integer(Skip) ->
    S = integer_to_binary(Skip),
    <<"OFFSET ", S/binary>>;
format_offset(_) ->
    <<>>.

format_keys(#{<<"keys">> := Keys}) when Keys =/= undefined, Keys =/= <<>>, is_list(Keys) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            <<>> -> X;
            _ -> <<Acc/binary, ",", X/binary>>
        end
                end, <<>>, Keys);
format_keys(#{<<"keys">> := Keys}) when Keys =/= undefined, Keys =/= <<>> ->
    Keys;
format_keys(_) ->
    <<"*">>.

format_interval(#{<<"interval">> := Interval}) when Interval =/= undefined, is_integer(Interval) ->
    I = integer_to_binary(Interval),
    <<"INTERVAL(", I/binary, ")">>;
format_interval(_) ->
    <<>>.

format_fill(#{<<"fill">> := Value}) when Value =/= <<>>, Value =/= undefined ->
    <<"FILL(", Value/binary, ")">>;
format_fill(_) ->
    <<>>.

format_from(#{<<"from">> := From}) when From =/= <<>>, From =/= undefined ->
    From;
format_from(_) ->
    <<>>.

format_group(#{<<"group">> := Group}) when Group =/= <<>>, Group =/= undefined ->
    <<"GROUP BY ", Group/binary>>;
format_group(_) ->
    <<>>.

format_where([], Acc) ->
    Acc;
format_where([{Field, #{<<"$gt">> := Value}} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " > ", V/binary>> | Acc]);
format_where([{Field, #{<<"$gte">> := Value}} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " >= ", V/binary>> | Acc]);
format_where([{Field, #{<<"$lt">> := Value}} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " < ", V/binary>> | Acc]);
format_where([{Field, #{<<"$lte">> := Value}} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " <= ", V/binary>> | Acc]);
format_where([{Field, #{<<"$ne">> := Value}} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " <> ", V/binary>> | Acc]);
format_where([{Field, #{<<"$regex">> := Value}} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " LIKE '", V/binary, "'">> | Acc]);
format_where([{Field, Value} | Other], Acc) ->
    V = dgiot_utils:to_binary(Value),
    format_where(Other, [<<Field/binary, " = '", V/binary, "'">> | Acc]).
format_where(#{<<"where">> := Where0}) when Where0 =/= undefined, Where0 =/= <<>> ->
    Where =
        case jsx:is_json(Where0) of
            true -> jsx:decode(Where0, [return_maps]);
            fasle -> Where0
        end,
    Where1 =
        case is_list(Where) of
            true ->
                Where3 =
                    lists:foldl(fun(X, Acc) ->
                        Acc ++ maps:to_list(X)
                                end, [], Where),
                format_where(Where3, []);
            false when is_map(Where) ->
                format_where(maps:to_list(Where), [])
        end,
    case list_to_binary(lists:join(" AND ", lists:reverse(Where1))) of
        <<>> ->
            <<>>;
        Where2 ->
            <<"WHERE ", Where2/binary>>
    end;
format_where(_) ->
    <<>>.
