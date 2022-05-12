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

-module(dgiot_device_echart).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").

-export([get_echart_data/4]).

get_echart_data(Channel, ProductId, DeviceId, Args) ->
    Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            pass;
        _ ->
            Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
            Interval = maps:get(<<"interval">>, Args),
            TableName = ?Table(DeviceId),
            case dgiot_device_tdengine:get_history_data(Channel, TableName, Query#{
                <<"db">> => ProductId
            }) of
                {Names, {ok, #{<<"results">> := Results}}} ->
                    Chartdata = get_echart(ProductId, Results, Names, Interval),
                    {ok, #{<<"chartData">> => Chartdata}};
                _ ->
                    {ok, #{<<"code">> => 400, <<"msg">> => <<"no data">>}}
            end
    end.

get_echart(ProductId, Results, Names, Interval) ->
    Maps = dgiot_product:get_prop(ProductId),
    Units = dgiot_product:get_unit(ProductId),
    NewMaps = maps:merge(#{<<"createdat">> => <<"日期"/utf8>>}, Maps),
    Columns = [<<"日期"/utf8>>] ++ Names,
    Rows =
        lists:foldl(fun(Line, Lines) ->
            NewLine =
                maps:fold(fun(K, V, Acc) ->
                    case maps:find(K, NewMaps) of
                        error ->
                            Acc;
                        {ok, Name} ->
                            case Name of
                                <<"日期"/utf8>> ->
                                    NewV = dgiot_tdengine_field:get_time(V, Interval),
                                    Acc#{Name => NewV};
                                _ ->
                                    Acc#{Name => V}
                            end
                    end
                          end, #{}, Line),
            Lines ++ [NewLine]
                    end, [], Results),
    ?LOG(debug, "Rows ~p", [Rows]),
    ChildRows = lists:foldl(fun(X, Acc1) ->
        Date = maps:get(<<"日期"/utf8>>, X),
        maps:fold(fun(K1, V1, Acc) ->
            case maps:find(K1, Acc) of
                error ->
                    Acc#{K1 => [#{<<"日期"/utf8>> => Date, K1 => V1}]};
                {ok, V2} ->
                    Acc#{K1 => V2 ++ [#{<<"日期"/utf8>> => Date, K1 => V1}]}
            end
                  end, Acc1, maps:without([<<"日期"/utf8>>], X))
                            end, #{}, Rows),
    ?LOG(debug, "ChildRows ~p", [ChildRows]),
    Child =
        maps:fold(fun(K, V, Acc) ->
            Unit =
                case maps:find(K, Units) of
                    error -> <<"">>;
                    {ok, Unit1} -> Unit1
                end,
            Acc ++ [#{<<"columns">> => [<<"日期"/utf8>>, K], <<"rows">> => V, <<"unit">> => Unit}]
                  end, [], ChildRows),
    ?LOG(debug, "Child ~p", [Child]),
    #{<<"columns">> => Columns, <<"rows">> => Rows, <<"child">> => Child}.

