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
-export([get_depart_material/2]).
get_material_record(DeviceId, _Depart) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"basedata">> := #{<<"material">> := Material}}} ->
%%            DepartMaterial = get_depart_material(Material, Depart),
            {ok, Material};
        _ ->
            {ok, #{}}
    end.
get_depart_material(Material, undefined) ->
    Material;
get_depart_material(Material, Depart) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(<<"Production_workshop">>, V) of
                {ok, MaterialDepart} ->
                    case re:run(MaterialDepart, Depart, [{capture, none}]) of
                        match ->
                            Acc#{K => V};
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end, #{}, Material).



post_material(DeviceId, #{<<"material_type">> := Type} = Map) when is_map(Map) ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            func(ProductId, DeviceId, Type, Map);
        _ ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"product">> := #{<<"objectId">> := ProductId}}} ->
                    func(ProductId, DeviceId, Type, Map);
                _ ->
                    io:format("~s ~p DeviceId = ~p  ~n", [?FILE, ?LINE, DeviceId]),
                    {error, <<"not_find_device">>}
            end
    end;
post_material(_, _) ->
    error.
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
            io:format("~s ~p not_find   ~n", [?FILE, ?LINE]),
            error
    end;
update_batch(_D) ->
    io:format("~s ~p miss_match,D =~p    ~n", [?FILE, ?LINE, _D]).



run_material_hook(ProductId, Type, Map) ->
%%    io:format("~s ~p ProductId = ~p,Type = ~p ,Map = ~p ~n", [?FILE, ?LINE, ProductId,Type,Map]),
    case dgiot_hook:run_hook({kingdee, post_material, ProductId, Type}, Map) of
        {ok, [{ok, _}]} ->
            {ok, ok};
        _ ->
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
%%    io:format("~s ~p NewWhere = ~p  ~n", [?FILE, ?LINE, NewWhere]),
    case dgiot_parse:query_object(<<"Device">>, #{<<"count">> => <<"objectId">>, <<"limit">> => Limit, <<"skip">> => Skip,
        <<"order">> => <<"createdAt">>, <<"where">> => maps:merge(NewWhere, #{<<"product">> => ProductId}),
        <<"keys">> => [<<"content">>]}) of
        {ok, #{<<"count">> := Total, <<"results">> := Device}} ->
%%            io:format("~s ~p ProductId = ~p  ~n", [?FILE, ?LINE, ProductId]),
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
                    handle_ets(Record),
                    dgiot_parse:update_object(<<"Device">>, Id, #{<<"content">> => maps:merge(Content, Record), <<"isEnable">> => Res});
                _ ->
                    handle_ets(Record),
                    dgiot_parse:update_object(<<"Device">>, Id, #{<<"content">> => maps:merge(Content, Record)})
            end;
        _ ->
            pass

    end;
put_warehouse_material(_) ->
    error.

handle_ets(#{<<"isEnable">> := false, <<"FMaterialId">> := Id, <<"FDeliveryDate">> := Date}) ->
    dgiot_data:delete(?MATERIALETS, {Id, Date});

handle_ets(#{<<"isEnable">> := true, <<"FMaterialId">> := Id, <<"FDeliveryDate">> := Date} = Record) ->
%%    io:format("~s ~p Record = ~p  ~n", [?FILE, ?LINE, Record]),
    dgiot_data:insert(?MATERIALETS, {Id, Date}, Record);
handle_ets(_) ->
    io:format("~s ~p here  ~n", [?FILE, ?LINE]),
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

func(ProductId, DeviceId, Type, Map) ->
%%           回调金蝶，确认回调成功后更新系统数据
    case run_material_hook(ProductId, Type, Map) of
        {ok, _} ->
%%            记录领料信息到订单
            record2task(DeviceId, Map),
%%            更新批次数量信息
            update_batch(Map);
        _ ->
            io:format("~s ~p DeviceId = ~p  ~n", [?FILE, ?LINE, DeviceId]),
            error
    end.
