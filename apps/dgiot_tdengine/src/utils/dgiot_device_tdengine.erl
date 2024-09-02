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

-module(dgiot_device_tdengine).
-author("jonhliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_device/include/dgiot_device.hrl").


-export([get_device/3, get_device/4, get_device/5]).
-export([get_history_data/4, get_realtime_data/4, get_gps_track/4]).
-export([get_history_data2/7]).

%% #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1} 查询td最新的一条device
get_device(ProductId, DevAddr, Query) ->
    case dgiot_data:get({ProductId, ?TYPE}) of
        not_find ->
            {error, <<"not find channel">>};
        ChannelId ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            TableName = ?Table(DeviceId),
%%            DB = dgiot_tdengine:get_database(ChannelId, ProductId),
            case get_realtime_data(ChannelId, ProductId, TableName, Query) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_device(Channel, ProductId, DevAddr, Query) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    TableName = ?Table(DeviceId),
    DB = dgiot_tdengine:get_database(Channel, ProductId),
    case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => DB}) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

get_device(Channel, ProductId, DeviceId, _DevAddr, #{<<"keys">> := <<"last_row(*)">>, <<"limit">> := 1} = Query) ->
    case dgiot_data:get({td, ProductId, DeviceId}) of
        not_find ->
            TableName = ?Table(DeviceId),
            DB = dgiot_tdengine:get_database(Channel, ProductId),
            case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => DB}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end;
        Value -> {ok, #{<<"results">> => [Value]}}
    end;

get_device(Channel, ProductId, DeviceId, _DevAddr, Query) ->
    TableName = ?Table(DeviceId),
    DB = dgiot_tdengine:get_database(Channel, ProductId),
    case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => DB}) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

get_tablename(DB, [FirstDevId | _] = DeviceIds) ->
    format_join(DB, ?Table(FirstDevId), DeviceIds, <<>>, <<>>);

get_tablename(DB, DeviceId) ->
    TableName = ?Table(DeviceId),
    {<<DB/binary, TableName/binary>>, <<"1=1">>, TableName}.

format_join(_DB, FirstTableName, [], Tcc, Wcc) ->
    {Tcc, Wcc, FirstTableName};
format_join(DB, FirstTableName, [DeviceId | DeviceIds], Tcc, Wcc) ->
    TableName = ?Table(DeviceId),
    {NewTcc, NewWcc} =
        case Tcc of
            <<>> ->
                {<<DB/binary, TableName/binary>>, <<FirstTableName/binary, ".createdat = ", FirstTableName/binary, ".createdat">>};
            _ ->
                {<<Tcc/binary, ", ", DB/binary, TableName/binary>>, <<Wcc/binary, " AND ", FirstTableName/binary, ".createdat = ", TableName/binary, ".createdat">>}
        end,
    format_join(DB, TableName, DeviceIds, NewTcc, NewWcc).

%% SELECT max(day_electricity) '时间' ,max(charge_current) '日期' FROM _2d26a94cf8._c5e1093e30 WHERE createdat >= now - 1h INTERVAL(1h) limit 10;
%% SELECT spread(cumulativescale) FROM _797197ad06 WHERE  createdat >= now - 1Y INTERVAL(1h);
%% SELECT last(cumulativescale) FROM _797197ad06 WHERE createdat >= now - 1Y INTERVAL(1h);
get_history_data(Channel, ProductId, DeviceId, Query) ->
    dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            DB = dgiot_tdengine:get_database(Channel, ProductId),
            {TableName, DefWhere, FirstTableName} = get_tablename(DB, DeviceId),
            Function = maps:get(<<"function">>, Query, <<>>),
            Keys = maps:get(<<"keys">>, Query, <<"*">>),
            Limit = dgiot_tdengine_select:format_limit(Query),
            Starttime = dgiot_utils:to_binary(maps:get(<<"starttime">>, Query, dgiot_datetime:now_ms() - 25920000000)),
            Endtime = dgiot_utils:to_binary(maps:get(<<"endtime">>, Query, dgiot_datetime:now_ms())),
            {Names, Newkeys} = dgiot_product_tdengine:get_keys(ProductId, FirstTableName, Function, Keys),
            Tail =
                case maps:get(<<"interval">>, Query, <<>>) of
                    <<>> ->
                        <<" where ", DefWhere/binary, " AND ", FirstTableName/binary, ".createdat >= ", Starttime/binary, " AND ", FirstTableName/binary, ".createdat <= ", Endtime/binary, " ", Limit/binary, ";">>;
                    Interval ->
                        <<" where ", DefWhere/binary, " AND ", FirstTableName/binary, ".createdat >= ", Starttime/binary, " AND ", FirstTableName/binary, ".createdat <= ", Endtime/binary, " INTERVAL(", Interval/binary, ") ", Limit/binary, ";">>
                end,
            Sql = <<"SELECT ", Newkeys/binary, " FROM ", TableName/binary, Tail/binary>>,
%%            io:format("~s ~p Sql = ~p.~n", [?FILE, ?LINE, Sql]),
            {Names, dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)}
        end).

%% SELECT last(*) FROM _2d26a94cf8._c5e1093e30;
get_realtime_data(Channel, ProductId, DeviceId, _Query) ->
    dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            DB = dgiot_tdengine:get_database(Channel, ProductId),
            {TableName, DefWhere, FirstTableName} = get_tablename(DB, DeviceId),
            Sql = <<"SELECT * FROM ", TableName/binary, " where ", DefWhere/binary, " order by ", FirstTableName/binary, ".createdat desc limit 1;">>,
%%                    io:format("~p", [Sql]),
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end).

get_history_data2(Order, Channel, DeviceId, Interval, ProductId, StartTime, _EndTime) ->
%%    io:format("~s ~p Order= ~p, Channel= ~p, TableName= ~p, TableName= ~p,~n Interval= ~p, ProductId= ~p, ~n StartTime= ~p, EndTime =~p. ~n",[?FILE,?LINE,Order, Channel, TableName, TableName, Interval, ProductId, StartTime, _EndTime]),
    dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            DB = dgiot_tdengine:get_database(Channel, ProductId),
            TableName = ?Table(DeviceId),
            BinStartTime = dgiot_utils:to_binary(StartTime),
            Tail = <<" where createdat >= ", BinStartTime/binary, " INTERVAL(", Interval/binary, ") ", ";">>,
            Sql = <<"SELECT ", Order/binary, " FROM ", DB/binary, TableName/binary, Tail/binary>>,
            ?LOG(error, "Sql ~s", [Sql]),
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end).

get_gps_track(Channel, ProductId, DeviceId, Args) ->
    Query = Args#{<<"keys">> => <<"latitude,longitude">>},
    {_Names, Results} =
        case dgiot_device_tdengine:get_history_data(Channel, ProductId, DeviceId, Query) of
            {TdNames, {ok, #{<<"results">> := TdResults}}} ->
%%                io:format("~s ~p TdResults = ~p.~n", [?FILE, ?LINE, TdResults]),
                NewTdResults =
                    lists:foldl(
                        fun
                            (#{<<"latitude">> := null}, Acc1) ->
                                Acc1;
                            (#{<<"longitude">> := null}, Acc2) ->
                                Acc2;
                            (#{<<"latitude">> := Latitude, <<"longitude">> := Longitude} = X, Acc) ->
                                Maptype = dgiot_utils:to_binary(application:get_env(dgiot_device, map_type, "baidu")),
                                #{<<"longitude">> := Mglng, <<"latitude">> := Mglat} =
                                    dgiot_gps:fromwgs84(#{<<"longitude">> => Longitude, <<"latitude">> => Latitude}, Maptype),
                                Acc ++ [X#{<<"lat">> => Mglat, <<"lng">> => Mglng}]
                        end, [], TdResults),
                {TdNames, NewTdResults};
            _ ->
                {[], []}
        end,
    {ok, #{<<"results">> => Results}}.
