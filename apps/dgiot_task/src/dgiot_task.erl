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

-module(dgiot_task).
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    start/1,
    stop/1,
    load/1,
    get_control/3,
    get_collection/4,
    get_calculated/2,
    string2value/1,
    save_pnque/4,
    get_pnque/1
]).

%% 查询指标队列
load(#{
    <<"page_index">> := PageIndex,
    <<"page_size">> := PageSize,
    <<"total">> := Total,
    <<"product">> := ProductId,
    <<"vcaddr">> := <<"all">>
} = Args) ->
%%    ?LOG(info,"Args ~p", [Args]),
    Success = fun(Page) ->
        lists:map(fun(X) ->
            #{<<"objectId">> := DtuId, <<"devaddr">> := DtuAddr} = X,
            start(Args#{<<"dtuid">> => DtuId, <<"dtuaddr">> => DtuAddr})
                  end, Page)
              end,
    Query = #{<<"where">> => #{<<"product">> => ProductId}},
    dgiot_parse_loader:start(<<"Device">>, Query, PageIndex, PageSize, Total, Success);

%% 查询指标队列
load(#{
    <<"product">> := ProductId,
    <<"channel">> := Channel,
    <<"vcaddr">> := Dtu} = Args) ->
    dgiot_product:load(ProductId),
    Consumer = <<"task/", Channel/binary, "/", Dtu/binary>>,
    dgiot_data:set_consumer(Consumer, 10),
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => Dtu}),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"objectId">> := DtuId, <<"devaddr">> := DtuAddr}} ->
            start(Args#{<<"dtuid">> => DtuId, <<"dtuaddr">> => DtuAddr});
        _ -> pass
    end.

start(#{
    <<"freq">> := Freq,
    <<"start_time">> := Start_time,
    <<"end_time">> := End_time,
    <<"channel">> := Channel,
    <<"dtuid">> := DtuId
} = Args) ->
    %% 设备没上线则不加入到采集任务队列中
    dgiot_data:set_consumer(<<"taskround/", Channel/binary, "/", DtuId/binary>>, 1000000),
    Callback =
        fun(_X) ->
            case dgiot_task:get_pnque(DtuId) of
                not_find ->
%%                    ?LOG(info,"task ~p",[DtuId]),
                    {ok, _X};
                _ ->
%%                    ?LOG(info,"task ~p",[Args]),
                    {ok, supervisor:start_child(dgiot_task, [Args])}
            end
        end,
    Task = #{
        <<"freq">> => Freq,
        <<"unit">> => 5,
        <<"start_time">> => dgiot_datetime:to_localtime(Start_time),
        <<"end_time">> => dgiot_datetime:to_localtime(End_time),
        <<"id">> => <<"task/", Channel/binary, "/", DtuId/binary>>,
        <<"callback">> => Callback
    },
    dgiot_cron:save(default_task, Task);

start(_) ->
    ok.

stop(#{
    <<"page_index">> := PageIndex,
    <<"page_size">> := PageSize,
    <<"total">> := Total,
    <<"product">> := ProductId,
    <<"channel">> := Channel,
    <<"vcaddr">> := <<"all">>}
) ->
    Consumer = <<"task/", Channel/binary>>,
    dgiot_data:set_consumer(Consumer, 10),
    Success = fun(Page) ->
        lists:map(fun(X) ->
            #{<<"objectId">> := DtuId} = X,
            dgiot_cron:save(default_task, #{
                <<"id">> => <<"task/", Channel/binary, "/", DtuId/binary>>,
                <<"count">> => 0}),
                dgiot_task_worker:stop(#{<<"channel">> => Channel, <<"dtuid">> => DtuId})
                  end, Page)
              end,
    Query = #{
        <<"where">> => #{<<"product">> => ProductId
        }
    },
    dgiot_parse_loader:start(<<"Device">>, Query, PageIndex, PageSize, Total, Success);

stop(#{
    <<"page_index">> := PageIndex,
    <<"page_size">> := PageSize,
    <<"total">> := Total,
    <<"product">> := ProductId,
    <<"channel">> := Channel,
    <<"vcaddr">> := VcAddr}
) ->
    Consumer = <<"task/", Channel/binary>>,
    dgiot_data:set_consumer(Consumer, 10),
    Success = fun(Page) ->
        lists:map(fun(X) ->
            #{<<"objectId">> := DtuId} = X,
            dgiot_cron:save(default_task, #{
                <<"id">> => <<"task/", Channel/binary, "/", DtuId/binary>>,
                <<"count">> => 0}),
            dgiot_task_worker:stop(#{<<"channel">> => Channel, <<"dtuid">> => DtuId})
                  end, Page)
              end,
    Query = #{
        <<"where">> => #{<<"product">> => ProductId, <<"devaddr">> => VcAddr
        }
    },
    dgiot_parse_loader:start(<<"Device">>, Query, PageIndex, PageSize, Total, Success).

%%获取计算值，必须返回物模型里面的数据表示，不能用寄存器地址
get_calculated(ProductId, Ack) ->
    case dgiot_shadow:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"dataForm">> := #{
                        <<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection}} ->
                        Str1 = maps:fold(fun(K, V, Acc2) ->
                            Str = re:replace(Acc2, dgiot_utils:to_list(<<"%%", K/binary>>), "(" ++ dgiot_utils:to_list(V) ++ ")", [global, {return, list}]),
                            re:replace(Str, "%s", "(" ++ dgiot_utils:to_list(V) ++ ")", [global, {return, list}])
                                         end, dgiot_utils:to_list(Collection), Ack),
                        Acc#{Identifier => string2value(Str1)};
                    _ ->
                        Acc
                end
                        end, Ack, Props);
        _Error ->
            Ack
    end.

%%转换设备上报值，必须返回物模型里面的数据表示，不能用寄存器地址
get_collection(ProductId, Identifier, Payload, Ack) ->
    case dgiot_shadow:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"dataForm">> := #{<<"strategy">> := Strategy, <<"collection">> := Collection},
                        <<"identifier">> := Identifier} when Strategy =/= <<"计算值"/utf8>> ->
                        case maps:find(Identifier, Payload) of
                            {ok, Value} ->
                                Str = re:replace(Collection, dgiot_utils:to_list(<<"%%", Identifier/binary>>), "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                                Str1 = re:replace(Str, "%s", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                                Acc#{Identifier => string2value(Str1)};
                            _ -> Acc
                        end
                end
                        end, Ack, Props);
        _Error ->
            Ack
    end.

%% 获取控制值
get_control(Round, Data, Control) ->
    case Data of
        <<"null">> ->
            <<"null">>;
        Data ->
            Str = re:replace(dgiot_utils:to_list(Control), "%d", "(" ++ dgiot_utils:to_list(Data) ++ ")", [global, {return, list}]),
            Str1 = re:replace(Str, "%r", "(" ++ dgiot_utils:to_list(Round) ++ ")", [global, {return, list}]),
            dgiot_task:string2value(Str1)
    end.

string2value(Str) ->
    %% eralng语法中. 表示事务结束
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.

save_pnque(DtuProductId, DtuAddr, ProductId, DevAddr) ->
    DtuId = dgiot_parse:get_deviceid(DtuProductId, DtuAddr),
    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary>>,
    dgiot_mqtt:subscribe(Topic),
    case dgiot_data:get(?DGIOT_TASK, DtuId) of
        not_find ->
            dgiot_data:insert(?DGIOT_TASK, DtuId, [{ProductId, DevAddr}]);
        Pn_que ->
            New_Pn_que = dgiot_utils:unique_2(Pn_que ++ [{ProductId, DevAddr}]),
            dgiot_data:insert(?DGIOT_TASK, DtuId, New_Pn_que)
    end.

get_pnque(DtuId) ->
    case dgiot_data:get(?DGIOT_TASK, DtuId) of
        not_find ->
            not_find;
        PnQue when length(PnQue) > 0 ->
            Head = lists:nth(1, PnQue),
            dgiot_data:insert(?DGIOT_TASK, DtuId, lists:nthtail(1, PnQue) ++ [Head]),
            Head;
        _ ->
            not_find
    end.

