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
-include("dgiot_device.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([get_device/3, get_device/4, get_device/5]).
-export([get_history_data/3, get_realtime_data/3]).

%% #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1} 查询td最新的一条device
get_device(ProductId, DevAddr, Query) ->
    case dgiot_data:get({ProductId, ?TYPE}) of
        not_find ->
            {error, <<"not find channel">>};
        ChannelId ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            TableName = ?Table(DeviceId),
            case dgiot_tdengine:query_object(ChannelId, TableName, Query#{<<"db">> => ProductId}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_device(Channel, ProductId, DevAddr, Query) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    TableName = ?Table(DeviceId),
    case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => ProductId}) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

get_device(Channel, ProductId, DeviceId, _DevAddr, #{<<"keys">> := <<"last_row(*)">>, <<"limit">> := 1} = Query) ->
    case dgiot_data:get({td, ProductId, DeviceId}) of
        not_find ->
            case dgiot_data:get({tdengine_os, Channel}) of
                <<"windows">> ->
                    case dgiot_parse:query_object(<<"Timescale">>, Query#{
                        <<"where">> => #{
                            <<"product">> => ProductId,
                            <<"device">> => DeviceId
                        },
                        <<"keys">> => [<<"values">>],
                        <<"order">> => <<"-createdAt">>,
                        <<"limit">> => 1
                    }) of
                        {ok, #{<<"results">> := [Data | _] = Result}} when length(Result) > 0 ->
                            {ok, #{<<"results">> => [maps:get(<<"values">>, Data)]}};
                        {ok, #{<<"results">> := Result}} ->
                            {error, Result};
                        Error ->
                            Error
                    end;
                _ ->
                    TableName = ?Table(DeviceId),
                    case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => ProductId}) of
                        {ok, Data} ->
                            {ok, Data};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        Value -> {ok, #{<<"results">> => [Value]}}
    end;

get_device(Channel, ProductId, DeviceId, _DevAddr, Query) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            case dgiot_parse:query_object(<<"Timescale">>, Query#{
                <<"where">> => #{
                    <<"product">> => ProductId,
                    <<"device">> => DeviceId
                },
                <<"keys">> => [<<"values">>],
                <<"order">> => <<"-createdAt">>
            }) of
                {ok, #{<<"results">> := Result} = All} when length(Result) > 0 ->
                    {ok, All#{<<"results">> => [maps:get(<<"values">>, X) || X <- Result]}};
                {ok, All} ->
                    {error, All};
                Error ->
                    Error
            end;
        _ ->
            TableName = ?Table(DeviceId),
            case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => ProductId}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% SELECT max(day_electricity) '时间' ,max(charge_current) '日期' FROM _2d26a94cf8._c5e1093e30 WHERE createdat >= now - 1h INTERVAL(1h) limit 10;
get_history_data(Channel, TableName, Query) ->
    dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            Database = maps:get(<<"db">>, Query),
            Function = maps:get(<<"function">>, Query, <<"last">>),
            Keys = maps:get(<<"keys">>, Query, <<"*">>),
            Limit = dgiot_tdengine_select:format_limit(Query),
            Interval = maps:get(<<"interval">>, Query, <<"1d">>),
            Starttime = maps:get(<<"starttime">>, Query, dgiot_utils:to_binary(dgiot_datetime:now_ms() - 604800000)),
            Endtime = maps:get(<<"endtime">>, Query, dgiot_utils:to_binary(dgiot_datetime:now_ms())),
            {Names, Newkeys} = dgiot_product_tdengine:get_keys(Database, Function, Keys),
            DB = dgiot_tdengine_select:format_db(?Database(Database)),
            Tail = <<" where createdat >= ", Starttime/binary, " AND createdat <= ", Endtime/binary, " INTERVAL(", Interval/binary, ") ", Limit/binary, ";">>,
            Sql = <<"SELECT ", Newkeys/binary, " FROM ", DB/binary, TableName/binary, Tail/binary>>,
            ?LOG(error, "Sql ~s", [Sql]),
            {Names, dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)}
        end).

%% SELECT last(*) FROM _2d26a94cf8._c5e1093e30;
get_realtime_data(Channel, TableName, Query) ->
    dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            Database = maps:get(<<"db">>, Query, <<"">>),
            Keys = maps:get(<<"keys">>, Query, <<"*">>),
            {_Names, Newkeys} = dgiot_product_tdengine:get_keys(Database, <<"last">>, Keys),
            DB = dgiot_tdengine_select:format_db(?Database(Database)),
            case size(Newkeys) > 0 of
                true ->
                    Sql = <<"SELECT last(createdat) createdat, ", Newkeys/binary, " FROM ", DB/binary, TableName/binary, ";">>,
                    dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql);
                _ ->
                    {error, #{<<"msg">> => <<"无物模型"/utf8>>}}
            end
        end).

