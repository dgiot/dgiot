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
get_material_record(DeviceId, Depart) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"basedata">> := #{<<"material">> := Material}}} ->
            DepartMaterial = get_depart_material(Material, Depart),
            {ok, get_usable_material(DepartMaterial)};
        _ ->
            io:format("~s ~p DeviceId = ~p  ~n", [?FILE, ?LINE, DeviceId]),
            case dgiot_hook:run_hook({factory, get_material}, [DeviceId]) of
                {ok, [{ok, Material}]} ->
                    {ok, get_usable_material(Material)};
                _ ->
                    error
            end
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

get_usable_material(Material) ->
    maps:fold(
        fun(K, V, Acc) ->
            case dgiot_data:match(material, {{K, '_'}, '$1'}) of
                {ok, Res} ->
                    Acc#{K => maps:merge(V, #{<<"usable_material">> => lists:flatten(Res)})};
                _ ->
                    Acc#{K => V}
            end
        end, #{}, Material).




post_material(DeviceId, Data) when is_map(Data) ->
    case get_material_record(DeviceId, undefined) of
        {ok, Material} ->
            io:format("~s ~p Data = ~p ~n", [?FILE, ?LINE, Data]),
            case hanlde_pickandretrive(DeviceId, Data, Material) of
                {ok, NewMaterial} ->
%%                    io:format("~s ~p NewMaterial = ~ts ~n", [?FILE, ?LINE, unicode:characters_to_list(jiffy:encode(NewMaterial))]),
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => #{<<"material">> => NewMaterial}});
                _ ->
                    error
            end;
        _ ->
            error
    end;
post_material(_, _) ->
    error.
%%get_Material(Deviceid, FReplaceGroup, FPrdOrgId,FStockId, FAppQty, FActualQty)
%%FReplaceGroup 行号 FPrdOrgId 组织id FStockId 仓库id  FAppQty 申请数量 FActualQty 实际数量
hanlde_pickandretrive(DeviceId,  #{ <<"FPrdOrgId">> :=FPrdOrgId,<<"FStockId">> := FStockId, <<"FReplaceGroup">> := FReplaceGroup,
    <<"Subitem_BOM_number">> := Name, <<"material_date">> := Date,
    <<"material_people">> := People, <<"material_type">> := Type,
    <<"material_number">> := Num, <<"material_batchid">> := BatchId, <<"material_weight">> := Weight,
    <<"objectId">> := Id}, Material) ->
    case maps:find(Name, Material) of
        {ok, Res} ->
            case Res of
                #{<<"material_pick">> := Pick, <<"material_retrive">> := Retrive, <<"material_picked">> := Picked} ->
                    case Type of
                        <<"picking">> ->
                            case dgiot_kingdee_wuliao:get_Material(DeviceId, FReplaceGroup, FPrdOrgId, FStockId, Weight, Weight) of
                                #{<<"Result">> := #{<<"ResponseStatus">> := #{<<"IsSuccess">> := true}}} ->
                                    io:format("~s ~p Type = ~p ~n", [?FILE, ?LINE, Type]),
                                    After = dgiot_utils:to_float(Picked) + dgiot_utils:to_float(Weight),
                                    handle_warehouse(Id, Type, Num),
                                    NewRes = maps:merge(Res, #{<<"material_picked">> => After, <<"material_pick">> => Pick ++ [#{<<"material_date">> => Date, <<"material_people">> => People, <<"material_number">> => Num,
                                        <<"material_batchid">> => BatchId, <<"material_weight">> => Weight}]}),
                                    {ok, maps:merge(Material, #{Name => NewRes})};
                                Type ->
                                    io:format("~s ~p Type = ~p ~n", [?FILE, ?LINE, Type]),
                                    error

                            end;
                        <<"retriving">> ->
                            case dgiot_kingdee_wuliao:get_Material(DeviceId, FReplaceGroup, FPrdOrgId, FStockId, Weight, Weight) of
                                #{<<"Result">> := #{<<"ResponseStatus">> := #{<<"IsSuccess">> := true}}} ->
                                    After = dgiot_utils:to_float(Picked) - dgiot_utils:to_float(Weight),
                                    handle_warehouse(Id, Type, Num),
                                    NewRes = maps:merge(Res, #{<<"material_picked">> => After, <<"material_retrive">> => Retrive ++ [#{<<"material_date">> => Date, <<"material_people">> => People, <<"material_number">> => Num,
                                        <<"material_batchid">> => BatchId, <<"material_weight">> => Weight}]}),
                                    {ok, maps:merge(Material, #{Name => NewRes})};
                                _ ->
                                    error

                            end;
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end;
hanlde_pickandretrive(_,_, _) ->
%%    io:format("~s ~p R = ~p  ~n", [?FILE, ?LINE,R]),
    error.


handle_warehouse(BatchId, Type, Number) ->
    NewWeight = dgiot_utils:to_float(Number),
    Num = case Type of
              <<"picking">> ->
                  NewWeight;
              <<"retriving">> ->
                  -NewWeight;
              _ ->
                  0
          end,
    io:format("~s ~p BatchId = ~p  ~n", [?FILE, ?LINE, BatchId]),
    case dgiot_parse:get_object(<<"Device">>, BatchId) of
        {ok, #{<<"content">> := #{<<"FQty">> := FQty} = Res}} ->
            Remaining = maps:get(<<"Remaining">>, Res, 0),
            NewRemaining = case Remaining + Num > FQty of
                               true ->
                                   FQty;
                               _ ->
                                   Remaining + Num
                           end,
            io:format("~s ~p NewRemaining = ~p  ~n", [?FILE, ?LINE, NewRemaining]),
            put_warehouse_material(maps:merge(Res, #{<<"Remaining">> => NewRemaining, <<"objectId">> => BatchId}));
        _ ->
            io:format("~s ~p not_find   ~n", [?FILE, ?LINE]),
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




