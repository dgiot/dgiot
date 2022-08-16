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

-module(dgiot_factory_utils).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-export([get_num/2, get_name/2, turn_name/2, turn_num/3]).
-export([get_sheet_type/2]).
get_num(K, V) ->
    Id = dgiot_parse_id:get_dictid(K, K, K, K),
    case dgiot_parse:get_object(<<"Dict">>, Id) of
        {ok, #{<<"data">> := #{<<"end">> := End} = Dict}} ->
            case maps:find(V, Dict) of
                {ok, Num} ->
                    #{K => Num};
                _ ->
                    case dgiot_parse:update_object(<<"Dict">>, Id, #{<<"data">> => Dict#{<<"end">> => End + 1, V => End}}) of
                        {ok, _} ->
                            #{K => End};
                        _ ->
                            error
                    end
            end;
        _ ->
            Map = #{<<"class">> => K,
                <<"title">> => K,
                <<"type">> => K,
                <<"key">> => K,
                <<"data">> => #{<<"end">> => 1, V => 0}
            },
            case dgiot_parse:create_object(<<"Dict">>, Map) of
                {ok, _} ->
                    #{K => 0};
                _ ->
                    error
            end
    end.

get_name(K, Num) ->
    Id = dgiot_parse_id:get_dictid(K, K, K, K),
    case dgiot_parse:get_object(<<"Dict">>, Id) of
        {ok, #{<<"data">> := Dict}} ->
            TupleList = maps:to_list(Dict),
            case lists:keytake(Num, 2, TupleList) of
                {value, {Name, _}, _} ->
                    #{K => Name};
                _ ->
                    error
            end;
        _ ->
            error
    end.


turn_name(Data, ThingMap) when is_list(Data) ->
    lists:foldl(
        fun(X, ACC) ->
            ACC ++ turn_name(X, ThingMap)
        end, [], Data);

turn_name(Data, ThingMap) when is_map(Data) ->
    Res = maps:fold(
        fun(K, V, Acc) ->
            case V of
                <<"enum">> ->
                    case maps:find(K, Acc) of
                        {ok, Num} ->
                            case get_name(K, Num) of
                                error ->
                                    Acc;
                                Map ->
                                    maps:merge(Acc, Map)
                            end;
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end, Data, ThingMap),
    [Res];

turn_name(Data, _) ->
    Data.

turn_num(FlatMap, ProductId, Type) ->
    case dgiot_factory_getdata:get_ThingMap(Type, ProductId) of
        {ok, ThingMap} ->
            maps:fold(
                fun(K, V, Acc) ->
                    case V of
                        <<"enum">> ->
                            case maps:find(K, Acc) of
                                {ok, Data} ->
                                    case dgiot_factory_utils:get_num(K, Data) of
                                        error ->
                                            Acc;
                                        Map ->
                                            maps:merge(Acc, Map)
                                    end;
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end

                end, FlatMap, ThingMap);
        _ ->
            FlatMap
    end.


get_sheet_type(ProductId, Exclude) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        #{<<"devicetype">> := Type} ->
                            case lists:member(Type, Acc) of
                                false ->
                                    Acc ++ [Type];
                                _ ->
                                    Acc
                            end;
                        _ -> Acc
                    end
                end, [], Props),
            lists:foldl(
                fun(X, Acc) ->
                    Acc -- [X]
                end, Res, Exclude);
        _ ->
            #{}
    end.
