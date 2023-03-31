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
%% https://doc.oschina.net/grpc
%% https://www.grpc.io/docs/

-module(dgiot_factory_worker).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_factory.hrl").
-define(WORKERCATEGORY, <<"bf6cbee357">>).

-export([get_new_workernum/1, format_worker/2]).
-export([record_worker_info/7,init_worker_device/3]).
-export([get_wokrer_id/2]).

get_new_workernum(WorkerProduct) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"product">> => WorkerProduct}}) of
        {ok, #{<<"results">> := Res}} ->
            Max = lists:foldl(
                fun(X, Acc) ->
                    case maps:find(<<"devaddr">>, X) of
                        {ok, Devaddr} ->
                            Num = get_worker_num(Devaddr),
                            case Acc > Num of
                                true ->
                                    Acc;
                                _ ->
                                    Num
                            end;
                        _ ->
                            Acc
                    end
                end, 0, Res),
            Max + 1;
        _ ->
            pass
    end.

record_worker_info(BatchProductId, BatchDeviceId, #{<<"quality">> := #{<<"type">> := Type}} = Payload, ChannelId, WorkerList, WorkTime, Num) ->
    Quality = maps:get(<<"quality">>, maps:get(<<"quality">>, Payload, #{}), <<"合格"/utf8>>),
    TypeData = case dgiot_data:get(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, Type}) of
                   not_find ->
                       #{};
                   R ->
                       R
               end,
    OrderId = maps:get(<<"ordername">>, maps:get(<<"person">>, TypeData, #{}), <<"null">>),
    _WorkShop = maps:get(<<"workshop">>, maps:get(Type, TypeData, #{}), <<"null">>),
    Spec = maps:get(<<"spec">>, maps:get(Type, TypeData, #{}), <<"">>),
    RollNum = maps:get(<<"rollnum">>, maps:get(<<"person">>, TypeData, #{}), <<"null">>),
    lists:foldl(
        fun(Worker, _) when is_binary(Worker) ->
            case dgiot_data:get({ChannelId, worker}) of
                not_find ->
                    pass;
                ProductId ->
                    case size(Worker) of
                        0 ->
                            pass;
                        _ ->
                            WorkerId = dgiot_parse_id:get_deviceid(ProductId, Worker),
                            WorkerData = case dgiot_parse:get_object(<<"Device">>, WorkerId) of
                                             {ok, #{<<"name">> := WorkerName}} ->
                                                 #{<<"worker_name">> => WorkerName};
                                             _ ->
                                                 io:format("~s ~p not_find_worker_device,ProductId = ~p  ~n", [?FILE, ?LINE, ProductId]),
                                                 #{}
                                         end,

                            ManufacData = #{
%%                        <<"manufac_type">> => dgiot_utils:to_binary(Type),
                                <<"manufac_quality">> => Quality,
                                <<"manufac_orderid">> => OrderId,
                                <<"manufac_spec">> => Spec,
                                <<"manufac_num">> => Num,
                                <<"manufac_worktime">> => WorkTime,
                                <<"manufac_batchid">> => BatchDeviceId,
                                <<"manufac_rollnum">> => RollNum,
                                <<"manufac_type">> => dgiot_utils:to_binary(Type),
                                <<"base_source">> => <<"质检数据"/utf8>>
                            },
                            NumData = dgiot_product_enum:turn_num(maps:merge(WorkerData, ManufacData), ProductId),
                            dgiot_task:save_td(ProductId, Worker, NumData, #{})
                    end
            end

        end, [], WorkerList);

record_worker_info(_, _, _, _, _, _, _) ->
    pass.

get_wokrer_id(Name, DevType) ->
    dgiot_parse_id:get_productid(?WORKERCATEGORY, DevType, Name).

init_worker_device(ProductId, WorkerNum, WorkerName) ->
    BinNum = dgiot_utils:to_binary(WorkerNum),
    Devaddr = <<WorkerName/binary, "_", BinNum/binary>>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, _} ->
            pass;
        _ ->
            case dgiot_product:get(ProductId) of
                {ok, Product} ->
                    case Product of
                        #{<<"ACL">> := Acl, <<"name">> := Name, <<"devType">> := DevType, <<"dynamicReg">> := true} ->
%%                            以名字+"_"+工号作为工人设备地址，和名称
                            Device = #{
                                <<"profile">> => #{<<"worker_flag">> => 1},
                                <<"status">> => <<"ONLINE">>,
                                <<"brand">> => Name,
                                <<"devModel">> => DevType,
                                <<"name">> => Devaddr,
                                <<"devaddr">> => Devaddr,
                                <<"product">> => ProductId,
                                <<"ACL">> => Acl
                            },
                            io:format("~s ~p DeviceId = ~p ~n", [?FILE, ?LINE, DeviceId]),
                            dgiot_device:create_device(Device),
                            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ACL">> => Acl#{<<"*">> => #{<<"read">> => true}}}),
                            AllData = #{<<"worker_validate">> => true,
                                <<"worker_num">> => WorkerNum,
                                <<"worker_date">> => 0,
                                <<"worker_name">> => WorkerName,
                                <<"product">> => ProductId},
                            NumData = dgiot_product_enum:turn_num(AllData, ProductId),
                            dgiot_data:insert(?WORKER, {ProductId, WorkerNum}, WorkerName),
                            dgiot_task:save_td_no_match(ProductId, Devaddr, NumData, #{});
                        _ ->
                            io:format("~s ~p not_find_product ~n", [?FILE, ?LINE]),
                            pass
                    end;
                _ ->
                    error
            end
    end.


get_worker_num(Devaddr) ->
    [_, Num] = re:split(Devaddr, <<"_">>),
    dgiot_utils:to_int(Num).


format_worker(ProductId, Worker) when is_binary(Worker) ->
    WorkerList = re:split(Worker, <<",">>),
    lists:foldl(
        fun(X, Acc) ->
            case dgiot_data:get(?WORKER, {ProductId, X}) of
                not_find ->
                    WorkerId = dgiot_parse_id:get_deviceid(ProductId, Worker),
                    case dgiot_parse:get_object(<<"Device">>, WorkerId) of
                        {ok, #{<<"name">> := WorkerName}} ->
                            dgiot_data:insert(?WORKER, {ProductId, X}, WorkerName),
                            <<Acc/binary, ",", WorkerName/binary>>;
                        _ ->
                            <<Acc/binary, ",", X/binary>>
                    end;
                Res ->
                    <<Acc/binary, " ", Res/binary>>
            end
        end, <<"">>, WorkerList);
format_worker(_, Worker) ->
    Worker.
