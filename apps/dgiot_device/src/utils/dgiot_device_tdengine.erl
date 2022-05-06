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
-export([get_chartdata/3, get_appdata/3, get_app/4]).

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
get_chartdata(Channel, TableName, Query) ->
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
get_appdata(Channel, TableName, Query) ->
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

get_app(ProductId, Results, DeviceId, Args) ->
    [Result | _] = Results,
    Keys = maps:get(<<"keys">>, Args, <<"*">>),
    Props = get_Props(ProductId, Keys),
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"name">> := Name, <<"isshow">> := true, <<"identifier">> := Identifier, <<"dataForm">> := #{<<"protocol">> := Protocol}, <<"dataSource">> := DataSource, <<"dataType">> := #{<<"type">> := Typea} = DataType} ->
                Time = maps:get(<<"createdat">>, Result, dgiot_datetime:now_secs()),
                NewTime = dgiot_tdengine_field:get_time(dgiot_utils:to_binary(Time), <<"111">>),
                Devicetype = maps:get(<<"devicetype">>, X, <<"others">>),
                Ico = maps:get(<<"ico">>, X, <<"">>),
                Specs = maps:get(<<"specs">>, DataType, #{}),
                Unit = maps:get(<<"unit">>, Specs, <<"">>),
                case do_hook({Protocol, Identifier}, DataSource#{<<"deviceid">> => DeviceId}) of
                    ignore ->
                        NewV =
                            case maps:find(Identifier, Result) of
                                error ->
                                    <<"--">>;
                                {ok, V} ->
                                    dgiot_product_tdengine:check_field(Typea, V, #{<<"datatype">> => DataType, <<"specs">> => Specs, <<"deviceid">> => DeviceId})
                            end,
                        Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => NewV,
                            <<"time">> => NewTime, <<"unit">> => Unit,
                            <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}];
                    {error, _Reason} ->
                        Acc;
                    V ->
                        NewV = dgiot_product_tdengine:check_field(Typea, V, #{<<"datatype">> => DataType, <<"specs">> => Specs, <<"deviceid">> => DeviceId}),
                        Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => NewV,
                            <<"time">> => NewTime, <<"unit">> => Unit,
                            <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}]
                end;
            _ ->
                Acc
        end
                end, [], Props).

get_Props(ProductId, <<"*">>) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Props;
        _ ->
            []
    end;

get_Props(ProductId, Keys) when Keys == undefined; Keys == <<>> ->
    get_Props(ProductId, <<"*">>);

get_Props(ProductId, Keys) ->
    List =
        case is_list(Keys) of
            true -> Keys;
            false -> re:split(Keys, <<",">>)
        end,
    lists:foldl(fun(Identifier, Acc) ->
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(fun(Prop, Acc1) ->
                    case Prop of
                        #{<<"identifier">> := Identifier} ->
                            Acc1 ++ [Prop];
                        _ ->
                            Acc1
                    end
                            end, Acc, Props);
            _ ->
                Acc
        end
                end, [], List).


do_hook(Key, Args) ->
    case catch dgiot_hook:run_hook(Key, Args) of
        {'EXIT', Reason} ->
            {error, Reason};
        {error, not_find} ->
            ignore;
        {ok, []} ->
            ignore;
        {ok, [{error, Reason} | _]} ->
            {error, Reason};
        {ok, [Rtn | _]} ->
            Rtn
    end.
