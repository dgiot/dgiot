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

-module(dgiot_factory_material).
-author("wolong").
-include("dgiot_factory.hrl").
%% API
-export([get_material_record/2, post_material/2]).
-export([get_warehouse_material/4, put_warehouse_material/1]).

get_material_record(DeviceId, _Depart) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"basedata">> := #{<<"material">> := Material}}} ->
            {ok, Material};
        _ ->
            {ok, #{}}
    end.

post_material(DeviceId, #{<<"material_type">> := Type} = Map) when is_map(Map) ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            func(ProductId, DeviceId, Type, Map);
        _ ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"product">> := #{<<"objectId">> := ProductId}}} ->
                    func(ProductId, DeviceId, Type, Map);
                _ ->
                    {error, <<"not_find_device">>}
            end
    end;

post_material(_, _) ->
    error.

func(ProductId, DeviceId, Type, Map) ->
%%           回调金蝶，确认回调成功后更新系统数据
    case run_material_hook(ProductId, Type, Map) of
        {ok, _} ->
%%            记录领料信息到订单
            record2task(DeviceId, Map),
%%            更新批次数量信息
            update_batch(Map);
        _R ->
            error
    end.

record2task(DeviceId, #{<<"FMaterialId">> := FMaterialId} = Map) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"basedata">> := #{<<"material">> := Material} = Basedata}} ->
            case maps:find(FMaterialId, Material) of
                {ok, V} ->
                    NewV = record2list(V, Map),
                    NewBasedata = dgiot_map:merge(Basedata, #{<<"material">> => #{FMaterialId => NewV}}),
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => NewBasedata});
                _ ->
                    error
            end;
        _ ->
            error

    end;
record2task(_, _) ->
    pass.

update_batch(#{<<"material_type">> := Type, <<"batchId">> := BatchId, <<"FQty">> := Num}) ->
    case dgiot_parse:get_object(<<"Device">>, BatchId) of
        {ok, #{<<"content">> := #{<<"FQty">> := FQty} = Res}} ->
            Remaining = maps:get(<<"Remaining">>, Res, FQty),
            ChangeNum = case Type of
                            <<"picking">> ->
                                -dgiot_utils:to_int(Num);
                            <<"retriving">> ->
                                dgiot_utils:to_int(Num)
                        end,
            NewRemaining = case Remaining + ChangeNum > FQty of
                               true ->
                                   FQty;
                               _ ->
                                   Remaining + ChangeNum
                           end,
            dgiot_parse:update_object(<<"Device">>, BatchId, #{<<"content">> => Res#{<<"Remaining">> => NewRemaining}});
        _ ->
            error
    end;

update_batch(_) ->
    {error,mis_match}.

run_material_hook(ProductId, Type, Map) ->
    case dgiot_hook:run_hook({kingdee, post_material, ProductId, Type}, Map) of
        {ok, [{ok, _}]} ->
            {ok, ok};
        _R ->
            error
    end.

get_warehouse_material(Limit, Skip, Where, ProductId) when is_map(Where) ->
    NewWhere = maps:fold(
        fun(K, V, Acc) ->
            case K of
                <<"isEnable">> ->
                    Acc#{K => V};
                _ ->
                    Acc#{<<"content.", K/binary>> => V}
            end
        end, #{}, Where),
    case dgiot_parse:query_object(<<"Device">>, #{<<"count">> => <<"objectId">>, <<"limit">> => Limit, <<"skip">> => Skip,
        <<"order">> => <<"createdAt">>, <<"where">> => maps:merge(NewWhere, #{<<"product">> => ProductId}),
        <<"keys">> => [<<"content">>]}) of
        {ok, #{<<"count">> := Total, <<"results">> := Device}} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        #{<<"objectId">> := ObjectId, <<"content">> := Content, <<"isEnable">> := IsEnable} ->
                            Acc ++ [maps:merge(Content, #{<<"objectId">> => ObjectId, <<"isEnable">> => IsEnable})];
                        _ ->
                            Acc
                    end
                end, [], Device),
            {ok, {Total, Res}};
        _ ->
            error
    end;

get_warehouse_material(_, _, _, _) ->
    error.

put_warehouse_material(#{<<"objectId">> := Id} = Record) ->
    case dgiot_parse:get_object(<<"Device">>, Id) of
        {ok, #{<<"content">> := Content}} ->
            case maps:find(<<"isEnable">>, Record) of
                {ok, Res} ->
                    dgiot_parse:update_object(<<"Device">>, Id, #{<<"content">> => maps:merge(Content, Record), <<"isEnable">> => Res});
                _ ->
                    dgiot_parse:update_object(<<"Device">>, Id, #{<<"content">> => maps:merge(Content, Record)})
            end;
        _ ->
            pass

    end;
put_warehouse_material(_) ->
    error.

record2list(V, #{<<"material_type">> := <<"picking">>, <<"FQty">> := FQty, <<"batchId">> := BatchId} = Map) ->
    ApplyTime = dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"),
    MaterialPicked = maps:get(<<"material_picked">>, V, 0),
    PickList = maps:get(<<"material_pick">>, V, []),
    NewPickEd = MaterialPicked + FQty,
    NewPickList = PickList ++ [
        Map#{<<"material_date">> => ApplyTime,
%%        <<"material_people">> => People,
            <<"material_batchid">> => BatchId,
%%        <<"material_number">> => Num,
            <<"material_weight">> => FQty}],
    V#{<<"material_pick">> => NewPickList, <<"material_picked">> => NewPickEd};
record2list(_, Map) ->
    Map.
