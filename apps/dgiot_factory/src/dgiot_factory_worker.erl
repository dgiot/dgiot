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

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_factory.hrl").

-export([put_worker_shift/1, get_work_shop_workers/2, get_new_workernum/1, format_worker/2]).
-export([duplicate_shift/4]).
-export([record_worker_info/7]).

put_worker_shift(#{<<"product">> := ProductId, <<"ids">> := Ids, <<"shift">> := Shift} = Data) ->
    WorkShop = maps:get(<<"workshop">>, Data, <<"">>),
    WorkTeam = maps:get(<<"workteam">>, Data, <<"">>),
    _Validate = maps:get(<<"worker_validate">>, Data, true),
    _Leader = maps:get(<<"leader">>, Data, <<"">>),
    WorkerList = re:split(Ids, <<",">>),
    _Today = dgiot_datetime:get_today_stamp(),
    case maps:find(<<"items">>, Data) of
        {ok, Items} ->
            put_relax(Items, ProductId);
        _ ->
            pass
    end,
    case format_time(Data, ProductId) of
        {ok, {Day, ShiftStartTime, ShiftEndTime}} ->
            lists:foldl(
                fun(Worker, _) ->
                    WorkerId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_binary(Worker)),
                    WorkerName = case dgiot_parse:get_object(<<"Device">>, WorkerId) of
                                     {ok, #{<<"name">> := Name}} ->
                                         Name;
                                     _ ->
                                         <<"null">>
                                 end,
                    AllData = #{
                        <<"base_source">> => 1,
%%                        <<"worker_validate">> => Validate,
                        <<"worker_num">> => Worker,
                        <<"worker_name">> => WorkerName,
                        <<"worker_workshop">> => WorkShop,
                        <<"worker_date">> => Day,
                        <<"worker_shiftstarttime">> => ShiftStartTime,
                        <<"worker_shiftendtime">> => ShiftEndTime,
                        <<"worker_shift">> => Shift,
                        <<"worker_team">> => WorkTeam,
                        <<"worker_monitor">> => _Leader,
                        <<"product">> => ProductId},
                    put_one_shift(AllData, ProductId, Worker)
                end, [], WorkerList),
            {ok, ok};
        _ ->
            error
    end;
put_worker_shift(_) ->
    io:format("~s ~p here   ~n", [?FILE, ?LINE]),
    error.

put_one_shift(AllData, ProductId, Worker) ->
    BinWorker = dgiot_utils:to_binary(Worker),
    NumTd = dgiot_product_enum:turn_num(AllData, ProductId),
    dgiot_task:save_td_no_match(ProductId, BinWorker, NumTd, #{}),
    case dgiot_data:get(?WORKER, BinWorker) of
        not_find ->
            dgiot_data:insert(?WORKER, BinWorker, [AllData]);
        List ->
            CheckedList = check_shift(AllData, List),
            NewList = sort_shift(CheckedList),
            dgiot_data:insert(?WORKER, BinWorker, NewList)
    end.
format_time(#{<<"date">> := Date, <<"startTime">> := S, <<"endTime">> := E, <<"shift">> := Shift}, ProductId) ->
    update_shift_info(ProductId, Shift, S, E),
    Day = dgiot_datetime:get_today_stamp(dgiot_utils:to_int(Date)),
    case get_new_time(S) of
        error ->
            error;
        SatrtTimeStamp ->
            case get_new_time(E) of
                error ->
                    error;
                EndTimeStamp ->
                    ShiftStartTime = Day + SatrtTimeStamp,
                    ShiftEndTime = case EndTimeStamp < SatrtTimeStamp of
                                       true ->
                                           Day + EndTimeStamp + ?ONEDAY;
                                       _ ->
                                           Day + EndTimeStamp
                                   end,
                    {ok, {Day, ShiftStartTime, ShiftEndTime}}
            end
    end;
format_time(_, _) ->
    error.
get_new_time(<<H:2/binary, ":", M:2/binary>>) ->
    Hour = dgiot_utils:to_int(H),
    Minute = dgiot_utils:to_int(M),
    Hour * 60 * 60 + Minute * 60;
get_new_time(_) ->
    error.


get_work_shop_workers(WorkeShop, _ProductId) ->
    Now = dgiot_datetime:nowstamp(),
    WorkerList = dgiot_data:keys(?WORKER),
    lists:foldl(
        fun(WorkerNum, Acc) ->
            case dgiot_data:lookup(?WORKER, WorkerNum) of
                {ok, ShiftList} ->
                    NewList = get_new_shift(Now, ShiftList),
                    case length(NewList) > 0 of
                        true ->
                            First = lists:nth(1, NewList),
                            case First of
                                #{<<"worker_workshop">> := Shop, <<"worker_shiftstarttime">> := Start, <<"worker_shiftendtime">> := End, <<"worker_name">> := WorkerName} ->
                                    case ((Now > Start) and (Now < End)) and (WorkeShop == Shop) of
                                        true ->
                                            Acc ++ [#{<<"label">> => WorkerName, <<"value">> => WorkerNum}];
                                        _ ->
                                            Acc
                                    end;
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end, [], WorkerList).

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


get_shift_time(ProductId, Shift) ->
    case dgiot_data:get(?FACTORY, {ProductId, shift}) of
        nut_find ->
            #{};
        Res ->
            maps:get(Shift, Res, #{})
    end.
get_latest_shift(Res) ->
    lists:foldl(
        fun(#{<<"worker_shiftstarttime">> := StartTime} = X, #{<<"worker_shiftstarttime">> := OldStart} = Acc) ->
            case StartTime < OldStart of
                true ->
                    X;
                _ ->
                    Acc
            end;
            (_, Acc) ->
                Acc
        end, #{<<"worker_shiftstarttime">> => 0}, Res).



sort_shift(List) ->
    Fun = fun(#{<<"worker_shiftstarttime">> := Start1}, #{<<"worker_shiftstarttime">> := Start2}) ->
        Start1 =< Start2
          end,
    lists:sort(Fun, List).


update_shift_info(ProductId, Shift, ShiftStartTime, ShiftEndTime) ->
    case dgiot_data:get(?FACTORY, {ProductId, shift}) of
        not_find ->
            dgiot_data:insert(?FACTORY, {ProductId, shift}, #{Shift => #{<<"startTime">> => ShiftStartTime, <<"endTime">> => ShiftEndTime}});
        Res ->
            dgiot_data:insert(?FACTORY, {ProductId, shift}, maps:merge(Res, #{Shift => #{<<"startTime">> => ShiftStartTime, <<"endTime">> => ShiftEndTime}}))
    end.
get_new_shift(Now, ShiftList) ->
    case length(ShiftList) of
        0 ->
            [];
%%        1 ->
%%            First = lists:nth(1, ShiftList),
%%            EndTime = maps:get(<<"worker_shiftendtime">>, First, 99999999999999),
%%            case EndTime < Now of
%%                true ->
%%                    NewData = duplicate_shift(First),
%%                    Worker = maps:get(<<"worker_name">>, First, <<"null">>),
%%                    ProductId = maps:get(<<"product">>, First, <<"null">>),
%%                    NumTd = dgiot_product_enum:turn_num(NewData, ProductId),
%%                    dgiot_task:save_td(ProductId, Worker, NumTd, #{}),
%%                    dgiot_data:insert(?WORKER, Worker, [NewData]);
%%                _ ->
%%                    ShiftList
%%            end;
        _ ->
            First = lists:nth(1, ShiftList),
            EndTime = maps:get(<<"worker_shiftendtime">>, First, 99999999999999),
            case EndTime < Now of
                true ->
                    get_new_shift(Now, lists:delete(First, ShiftList));
                _ ->
                    ShiftList
            end
    end.
check_shift(#{<<"worker_validate">> := <<"false">>} = Data, List) ->
    Day = maps:get(<<"worker_date">>, Data, 0),
    lists:foldl(
        fun(#{<<"worker_date">> := OldDay} = X, Acc) ->
            case Day == OldDay of
                true ->
                    Acc;
                _ ->
                    Acc ++ [X]
            end;
            (_, Acc) ->
                Acc
        end, [], List);
check_shift(#{<<"worker_shift">> := <<"休班"/utf8>>} = Data, List) ->
    Day = maps:get(<<"worker_date">>, Data, 0),
    lists:foldl(
        fun(#{<<"worker_date">> := OldDay} = X, Acc) ->
            case Day == OldDay of
                true ->
                    Acc;
                _ ->
                    Acc ++ [X]
            end;
            (_, Acc) ->
                Acc
        end, [], List);

check_shift(Data, List) ->
    Day = maps:get(<<"worker_date">>, Data, 0),
    Res = lists:foldl(
        fun(#{<<"worker_date">> := OldDay} = X, Acc) ->
            io:format("~s ~p OldDay: ~p~n", [?FILE, ?LINE, OldDay]),
            io:format("~s ~p Day: ~p~n", [?FILE, ?LINE, Day]),
            case Day == OldDay of
                true ->
                    io:format("~s ~p here~n", [?FILE, ?LINE]),
                    Acc;
                _ ->
                    io:format("~s ~p here~n", [?FILE, ?LINE]),
                    Acc ++ [X]
            end;
            (_, Acc) ->
                Acc
        end, [], List),
    Res ++ [Data].


duplicate_shift(Shift) ->
    NewEndTime = Shift + ?ONEDAY,
    NewSatrtTime = maps:get(<<"worker_shiftstarttime">>, Shift, 0) + ?ONEDAY,
    NewDay = maps:get(<<"worker_date">>, Shift, 0) ++ ?ONEDAY,
    maps:merge(Shift, #{<<"worker_shiftendtime">> => NewEndTime, <<"worker_shiftstarttime">> => NewSatrtTime, <<"worker_date">> => NewDay}).


%%get_history_data(ProductId, DeviceId, Type, Function, FunctionMap, Group, Having, Where, Order, Channel, Limit, Skip) ->
%%duplicate_shift(#{<<"sink_date">> := SinkDate,<<"source_date">> := SourceDate,<<"where">> := Where},ProductId) ->
duplicate_shift(SinkDate, Where, ProductId, Channel) ->
    case dgiot_factory_data:get_history_data(ProductId, undefined, <<"worker">>, <<"last">>, #{}, <<"worker_num">>, undefined, Where, undefined, Channel, undefined, undefined) of
        {ok, {_, Res}} ->
            Results = lists:foldl(
                fun(#{<<"worker_date">> := Day, <<"worker_shiftstarttime">> := ShiftStartTime, <<"worker_shiftendtime">> := ShiftEndTime,
                    <<"worker_num">> := WorkerNum, <<"worker_name">> := Name} = OldShift, Acc) ->
                    Gap = dgiot_utils:to_int(SinkDate) - dgiot_utils:to_int(Day),
                    NewDay = SinkDate,
                    NewStartTime = ShiftStartTime + Gap,
                    NewEndTime = ShiftEndTime + Gap,
                    NewData = maps:merge(OldShift, #{<<"worker_date">> => NewDay, <<"worker_shiftstarttime">> => NewStartTime, <<"worker_shiftendtime">> => NewEndTime}),
                    put_one_shift(NewData, ProductId, WorkerNum),
                    Acc#{WorkerNum => Name};
                    (_, Acc) ->
                        Acc
                end, #{}, Res),
            {ok, Results};
        _ ->
            {ok, #{}}
    end.


put_relax(Items, ProductId) ->
    lists:foldl(
        fun
            (#{<<"worker_num">> := Worker} = Item, _) ->
                put_one_shift(Item#{<<"worker_shift">> => 0}, ProductId, Worker);
            (_, _) ->
                pass
        end, [], Items).



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
    io:format("~s ~p WorkerList = ~p  ~n", [?FILE, ?LINE, WorkerList]),
    lists:foldl(
        fun(Worker, _) when is_binary(Worker) ->
            case dgiot_data:get({ChannelId, worker}) of
                not_find ->
                    pass;
                ProductId ->
                    case size(Worker) of
                        0 ->
                            pass;
                        _->
                    WorkerId = dgiot_parse_id:get_deviceid(ProductId, Worker),
                    WorkerData = case dgiot_parse:get_object(<<"Device">>, WorkerId) of
                                     {ok, #{<<"name">> := WorkerName}} ->
                                         #{<<"worker_name">> => WorkerName};
                                     _ ->
                                         io:format("~s ~p not_find_worker_device,ProductId = ~p  ~n", [?FILE, ?LINE,ProductId]),
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


%%record_worker_info(BatchProductId, BatchDeviceId, #{<<"quality">> := #{<<"type">> := Type}} = Payload, ChannelId, WorkerList, WorkTime, Num) ->
%%    Quality = maps:get(<<"quality">>, maps:get(<<"quality">>, Payload, #{}), <<"合格"/utf8>>),
%%    TypeData = case dgiot_data:get(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, Type}) of
%%                   not_find ->
%%                       #{};
%%                   R ->
%%                       R
%%               end,
%%    OrderId = maps:get(<<"ordername">>, maps:get(<<"person">>, TypeData, #{}), <<"null">>),
%%    _WorkShop = maps:get(<<"workshop">>, maps:get(Type, TypeData, #{}), <<"null">>),
%%    Spec = maps:get(<<"spec">>, maps:get(Type, TypeData, #{}), <<"">>),
%%    RollNum = maps:get(<<"rollnum">>, maps:get(<<"person">>, TypeData, #{}), <<"null">>),
%%    lists:foldl(
%%        fun(Worker, _) ->
%%            case dgiot_data:get({ChannelId, worker}) of
%%                not_find ->
%%                    pass;
%%                ProductId ->
%%                    WorkerData = case dgiot_data:get(?WORKER, Worker) of
%%                                     not_find ->
%%                                         #{};
%%                                     N ->
%%                                         lists:nth(1, N)
%%                                 end,
%%                    ManufacData = #{
%%%%                        <<"manufac_type">> => dgiot_utils:to_binary(Type),
%%                        <<"manufac_quality">> => Quality,
%%                        <<"manufac_orderid">> => OrderId,
%%                        <<"manufac_spec">> => Spec,
%%                        <<"manufac_num">> => Num,
%%                        <<"manufac_worktime">> => WorkTime,
%%                        <<"manufac_batchid">> => BatchDeviceId,
%%                        <<"manufac_rollnum">> => RollNum,
%%                        <<"manufac_type">> => dgiot_utils:to_binary(Type),
%%                        <<"base_source">> => <<"质检数据"/utf8>>
%%                    },
%%                    NumData = dgiot_product_enum:turn_num(maps:merge(WorkerData, ManufacData), ProductId),
%%                    dgiot_task:save_td(ProductId, Worker, NumData, #{})
%%            end
%%
%%        end, [], WorkerList);


get_worker_num(Devaddr)->
    [_,Num] =re:split(Devaddr,<<"_">>),
    dgiot_utils:to_int(Num).
