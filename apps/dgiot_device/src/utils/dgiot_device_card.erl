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

-module(dgiot_device_card).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").

-export([get_card/4, get_device_card/4]).

get_device_card(Channel, ProductId, DeviceId, Args) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            pass;
        _ ->
            TableName = ?Table(DeviceId),
            case dgiot_device_tdengine:get_realtime_data(Channel, TableName, Args#{<<"db">> => ProductId}) of
                {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
                    Chartdata = get_card(ProductId, Results, DeviceId, Args),
                    {ok, #{<<"data">> => Chartdata}};
                _ ->
                    Chartdata = get_card(ProductId, [#{}], DeviceId, Args),
                    {ok, #{<<"data">> => Chartdata}}
            end
    end.

get_card(ProductId, Results, DeviceId, Args) ->
    [Result | _] = Results,
    Keys = maps:get(<<"keys">>, Args, <<"*">>),
    Props = dgiot_product:get_props(ProductId, Keys),
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"name">> := Name, <<"identifier">> := Identifier, <<"dataForm">> := #{<<"protocol">> := Protocol}, <<"dataSource">> := DataSource, <<"dataType">> := #{<<"type">> := Typea} = DataType} ->
                Time = maps:get(<<"createdat">>, Result, dgiot_datetime:now_secs()),
                NewTime = dgiot_tdengine_field:get_time(dgiot_utils:to_binary(Time), <<"111">>),
                Devicetype =
                    case maps:find(<<"devicetype">>, X) of
                        {ok, <<"">>} ->
                            <<"others">>;
                        {ok, Data} when byte_size(Data) > 0 ->
                            Data;
                        _ ->
                            <<"others">>
                    end,
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
                        Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => <<"--">>,
                            <<"time">> => NewTime, <<"unit">> => Unit,
                            <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}];
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
