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
-export([get_warehouse_material/3, put_warehouse_material/1]).
get_material_record(DeviceId, Depart) ->
    io:format("~s ~p Depart= ~p ~n",[?FILE,?LINE,Depart]),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"material">> := Material}} ->
            DepartMaterial = get_depart_material(Material, Depart),
            {ok, get_usable_material(DepartMaterial)};
        _ ->
            case dgiot_hook:run_hook({factory, get_material}, [DeviceId]) of
                {ok, [{ok, Material}]} ->
                    get_usable_material(Material);
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
                    Acc#{K =>V}
            end
        end, #{}, Material).


post_material(DeviceId, Data) when is_list(Data) ->
    case get_material_record(DeviceId,undefined) of
        {ok, Material} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case hanlde_pickandretrive(X, Acc) of
                        {ok, Name, Res} ->
                            Acc#{Name => Res};
                        _ ->
                            Acc
                    end
                end, Material, Data),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"material">> => Res});
        _ ->
            error
    end;
post_material(_, _) ->
    error.






hanlde_pickandretrive(#{<<"Subitem_BOM_number">> := Name, <<"material_date">> := Date,
    <<"material_people">> := People, <<"material_type">> := Type,
    <<"material_number">> := Num, <<"material_batchid">> := BatchId, <<"material_weight">> := Weight, <<"material_batch">> := #{<<"extra">> := #{<<"objectId">> := Id}}}, Material) ->
    case maps:find(Name, Material) of
        {ok, Res} ->
            case Res of
                #{<<"material_pick">> := Pick, <<"material_retrive">> := Retrive, <<"material_picked">> := Picked} ->
                    case Type of
                        <<"picking">> ->
                            After = dgiot_utils:to_float(Picked) + dgiot_utils:to_float(Weight),
                            handle_warehouse(Id, Type, Num),
                            {ok, Name, maps:merge(Res, #{<<"material_picked">> => After, <<"material_pick">> => Pick ++ [#{<<"material_date">> => Date, <<"material_people">> => People, <<"material_number">> => Num,
                                <<"material_batchid">> => BatchId, <<"material_weight">> => Weight}]})};
                        <<"retriving">> ->
                            After = dgiot_utils:to_float(Picked) - dgiot_utils:to_float(Weight),
                            handle_warehouse(Id, Type, Num),
                            {ok, Name, maps:merge(Res, #{<<"material_picked">> => After, <<"material_retrive">> => Retrive ++ [#{<<"material_date">> => Date, <<"material_people">> => People, <<"material_number">> => Num,
                                <<"material_batchid">> => BatchId, <<"material_weight">> => Weight}]})};
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end;

hanlde_pickandretrive(_, _) ->
    io:format("~s ~p hanlde_pickandretrive  ~n", [?FILE, ?LINE]),
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
    case dgiot_parse:get_object(?MATERIALTABLE, BatchId) of
        {ok, #{<<"Remaining">> := Remaining, <<"FQty">> := FQty} = Res} ->
            NewRemaining = case Remaining + Num > FQty of
                               true ->
                                   FQty;
                               _ ->
                                   Remaining + Num
                           end,
            put_warehouse_material(maps:merge(Res, #{<<"Remaining">> => NewRemaining}));
        _ ->
            io:format("~s ~p not_find   ~n", [?FILE, ?LINE]),
            error
    end.
get_warehouse_material(Limit, Skip, undefined) ->
    case dgiot_parse:query_object(?MATERIALTABLE, #{<<"order">> => <<"createdAt">>}) of
        {ok, #{<<"results">> := Res}} ->
            NewRes = remove_time(Res),
            {ok, dgiot_factory_getdata:filter_data(Limit, Skip, NewRes)};
        _ ->
            error
    end;
get_warehouse_material(Limit, Skip, Where) ->
    case dgiot_parse:query_object(?MATERIALTABLE, #{<<"order">> => <<"createdAt">>, <<"where">> => Where}) of
        {ok, #{<<"results">> := Res}} ->
            NewRes = remove_time(Res),
            {ok, dgiot_factory_getdata:filter_data(Limit, Skip, NewRes)};
        _ ->
            error
    end.
remove_time(Res) ->
    lists:foldl(
        fun(X, Acc) ->
            Acc ++ [maps:remove(<<"createdAt">>, maps:remove(<<"updatedAt">>, X))]
        end, [], Res).


put_warehouse_material(#{<<"objectId">> := Id} = Record) ->
    handle_ets(Record),
    dgiot_parse:update_object(?MATERIALTABLE, Id, maps:remove(<<"createdAt">>, maps:remove(<<"updatedAt">>, Record))).

handle_ets(#{<<"validate">> := false} = Record) ->
    case get_id_and_data(Record) of
        {ok, {Id, Date}} ->
            dgiot_data:delete(?MATERIALETS, {Id, Date});
        _ ->
            error
    end;
handle_ets(#{<<"validate">> := true} = Record) ->
    case get_id_and_data(Record) of
        {ok, {Id, Date}} ->

            dgiot_data:insert(?MATERIALETS, {Id, Date}, Record#{<<"FDeliveryDate">> => Date});
        _ ->
            error
    end;

handle_ets(Record) ->
    io:format("~s ~p Record= ~p ~n", [?FILE, ?LINE, Record]).

get_id_and_data(#{<<"FMaterialId">> := Id, <<"FDeliveryDate">> := Date}) ->
    {ok, {Id, Date}};

get_id_and_data(#{<<"objectId">> := ObjectId}) ->
    case dgiot_parse:get_object(<<"material">>, ObjectId) of
        {ok, #{<<"FMaterialId">> := Id, <<"FDeliveryDate">> := Date}} ->
            {ok, {Id, Date}};
        _ ->
            error
    end;


get_id_and_data(_) ->
    error.
%%get_material_record_id(#{<<"FBillNo">> := FBillNo, <<"FCloseStatus">> := FCloseStatus}) ->
%%    BinNum = dgiot_utils:to_binary("1"),
%%    <<DeviceId:10/binary, _/binary>> = dgiot_utils:to_md5(<<FBillNo/binary, FCloseStatus/binary, BinNum/binary>>),
%%    DeviceId.
