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

-module(dgiot_product_enum).
-author("jonliu").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-dgiot_data("ets").
-export([init_ets/0, save_product_enum/1, get_enmu_key/3, get_enmu_value/3, post_enum_value/3]).
-export([turn_name/2, turn_num/2, get_ThingMap/1]).


init_ets() ->
    dgiot_data:init(?MODULE).


%% 设备类型
save_product_enum(ProductId) ->
    delete_product_enum(ProductId),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:map(
                fun
                    (#{<<"identifier">> := Identifier,
                        <<"dataType">> := #{<<"type">> := <<"enum">>, <<"specs">> := Spec}}) ->
                        NewSpec = maps:without([<<"max">>, <<"min">>, <<"step">>], Spec),
                        Reverse = get_reverse(NewSpec),
                        dgiot_data:insert(?MODULE, {ProductId, device_thing, Identifier},
                            #{Identifier => <<"enum">>, <<"specs">> => NewSpec, <<"reverse">> => Reverse});
                    (#{<<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := Type}}) ->
                        dgiot_data:insert(?MODULE, {ProductId, device_thing, Identifier}, #{Identifier => Type});
                    (_) ->
                        pass
                end, Props);

        _Error ->
            []
    end.

delete_product_enum(ProductId) ->
    Fun =
        fun
            ({Key, _}) ->
                case Key of
                    {ProductId, device_thing, _} ->
                        dgiot_data:delete(?MODULE, Key);
                    _ ->
                        pass
                end;
            (_) ->
                pass
        end,
    dgiot_data:loop(?MODULE, Fun).

get_reverse(Spec) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{V => K}
        end, #{}, Spec).

%%#{<<"1">> => <<"name">>}   #{ <<"name">> =><<"1">>}
get_enmu_key(ProductId, Identifier, Value) ->
    case dgiot_data:get(?MODULE, {ProductId, device_thing, Identifier}) of
        #{Identifier := <<"enum">>, <<"reverse">> := #{Value := K}} ->
            #{Value => dgiot_utils:to_int(K)};
        _ ->
            #{}
    end.
get_enmu_value(ProductId, Identifier, NumKey) ->
    BinKey = dgiot_utils:to_binary(NumKey),
    case dgiot_data:get(?MODULE, {ProductId, device_thing, Identifier}) of
        #{Identifier := <<"enum">>, <<"specs">> := #{BinKey := V}} ->
            #{NumKey => V};
        _R ->

            #{}
    end.

post_enum_value(ProductId, Identifier, Name) ->
    case dgiot_data:get(?MODULE, {ProductId, device_thing, Identifier}) of
        #{Identifier := <<"enum">>, <<"specs">> := Spec} ->
            Max = maps:fold(
                fun
                    (<<F:1/binary, _/binary>> = K, _, Acc) ->
                        case dgiot_utils:is_number(F) of
                            true ->
                                case dgiot_utils:to_int(K) > Acc of
                                    true ->
                                        dgiot_utils:to_int(K);
                                    _ ->
                                        Acc
                                end;
                            _ ->
                                Acc
                        end;
                    (_, _, Acc) ->
                        Acc
                end, 0, Spec),
            upadte_thing(ProductId, Identifier, Name, Max);
        _ ->
            upadte_thing(ProductId, Identifier, Name, -1)
    end.
upadte_thing(ProductId, Identifier, Name, Max) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties} = Thing}} ->
            NewProperties = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        #{<<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := <<"enum">>, <<"specs">> := Spec} = DataType} ->
                            NewSpec = maps:merge(Spec, #{dgiot_utils:to_binary(Max + 1) => Name}),
                            NewReverse = get_reverse(NewSpec),
                            dgiot_data:insert(?MODULE, {ProductId, device_thing, Identifier},
                                #{Identifier => <<"enum">>, <<"specs">> => NewSpec, <<"reverse">> => NewReverse}),
                            Acc ++ [X#{<<"dataType">> => DataType#{<<"specs">> => NewSpec}}];
                        _ ->
                            Acc ++ [X]
                    end
                end, [], Properties),
            NewThing = Thing#{<<"properties">> => NewProperties},
            dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"thing">> => NewThing}),
            #{Identifier => Max + 1};
        _ ->

            pass
    end.



turn_name(List, ProductId) when is_list(List) ->
    lists:foldl(
        fun(X, Acc) ->
            Acc ++ [turn_name(X, ProductId)]
        end, [], List);

turn_name(FlatMap, ProductId) when is_map(FlatMap) ->
    case get_ThingMap(ProductId) of
        {ok, ThingMap} ->
            maps:fold(
                fun(K, V, Acc) ->
                    case maps:find(K, ThingMap) of
                        {ok, <<"enum">>} ->
                            case get_enmu_value(ProductId, K, V) of
                                #{V := Value} ->
                                    Acc#{K => Value};
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end
                end, FlatMap, FlatMap);
        _ ->
            FlatMap
    end;

turn_name(Data, _) ->
    Data.


turn_num(FlatMap, ProductId) ->
    case get_ThingMap(ProductId) of
        {ok, ThingMap} ->
            maps:fold(
                fun(K, V, Acc) ->
                    case maps:find(K, ThingMap) of
                        {ok, <<"enum">>} ->
                            case get_enmu_key(ProductId, K, V) of
                                #{V := Value} ->
                                    Acc#{K => Value};
                                _ ->

                                    case post_enum_value(ProductId, K, V) of
                                        #{K := Value} ->
                                            Acc#{K => Value};
                                        _R ->
                                            Acc
                                    end
                            end;
                        _ ->
                            Acc
                    end
                end, FlatMap, FlatMap);
        _ ->
            FlatMap
    end.
get_ThingMap(ProductId) ->
    case dgiot_product:get_devicetype(ProductId) of
        not_find ->
            error;
        List ->
            Res = lists:foldl(
                fun(DeviceType, Acc) ->
                    case dgiot_product:get_device_thing(ProductId, DeviceType) of
                        not_find ->
                            Acc;
                        Res ->
                            maps:merge(Acc, Res)
                    end
                end, #{}, List),
            {ok, Res}
    end.
