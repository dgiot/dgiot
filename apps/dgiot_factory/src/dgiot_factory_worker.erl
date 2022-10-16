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

-export([put_worker_shift/1, get_work_shop_workers/1,get_new_workernum/1]).
put_worker_shift(#{<<"product">> := ProductId, <<"ids">> := Ids, <<"workteam">> := WorkTeam,
    <<"shift">> := Shift, <<"workshop">> := WorkShop, <<"leader">> := Leader} = Data) ->
    WorkerList = re:split(Ids, <<",">>),
    Today = dgiot_datetime:get_today_stamp(),
    io:format("~s ~p WorkerList ~p  ~n", [?FILE, ?LINE, WorkerList]),
    case format_time(Data) of
        {ok, {Day, ShiftStartTime, ShiftEndTime}} ->
            case dgiot_data:get(?FACTORY, {ProductId, shift}) of
                not_find ->
                    dgiot_data:insert(?FACTORY, {ProductId, shift}, #{Shift => #{<<"startTime">> => ShiftStartTime, <<"endTime">> => ShiftEndTime}});
                Res ->

                    dgiot_data:insert(?FACTORY, {ProductId, shift_info}, maps:merge(Res, #{Shift => #{<<"startTime">> => ShiftStartTime, <<"endTime">> => ShiftEndTime}}))
            end,
            io:format("~s ~p Day ~p  ~n", [?FILE, ?LINE, Day]),
            lists:foldl(
                fun(Worker, _) ->
                    WorkerId = dgiot_parse_id:get_deviceid(ProductId, Worker),
                    IfMonitor = case Worker == Leader of
                                    true ->
                                        true;
                                    _ ->
                                        false
                                end,
                    WorkerName = case dgiot_parse:get_object(<<"Device">>, WorkerId) of
                                     {ok, #{<<"name">> := Name}} ->
                                         Name;
                                     _ ->
                                         <<"null">>
                                 end,
                    AllData = #{<<"worker_name">> => WorkerName,
                        <<"worker_workshop">> => WorkShop,
                        <<"worker_date">> => Day,
                        <<"worker_shift">> => Shift,
                        <<"worker_team">> => WorkTeam,
                        <<"worker_monitor">> => IfMonitor,
                        <<"worker_shiftstarttime">> => ShiftStartTime,
                        <<"worker_shiftendtime">> => ShiftEndTime},
                    NumTd = dgiot_factory_utils:turn_num(AllData, ProductId),
                    dgiot_task:save_td(ProductId, Worker, NumTd, #{}),

                    case Day == Today of
                        true ->
                            dgiot_parse:update_object(<<"Device">>, WorkerId, #{<<"content">> => Data}),
                            dgiot_data:insert(?WORKER, Worker, AllData);
                        _ ->
                            pass
                    end
                end, [], WorkerList),
            {ok, ok};
        _ ->
            error
    end;
put_worker_shift(_) ->
    io:format("~s ~p here   ~n", [?FILE, ?LINE]),
    error.
format_time(#{<<"date">> := Date, <<"startTime">> := S, <<"endTime">> := E}) ->
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
format_time(_) ->
    error.
get_new_time(<<H:2/binary, ":", M:2/binary>>) ->
    Hour = dgiot_utils:to_int(H),
    Minute = dgiot_utils:to_int(M),
    Hour * 60 * 60 + Minute * 60;
get_new_time(_) ->
    error.


get_work_shop_workers(WorkeShop) ->
    Now = dgiot_datetime:nowstamp(),
    WorkerList = dgiot_data:keys(?WORKER),
    lists:foldl(
        fun(WorkerNum, Acc) ->
            case dgiot_data:lookup(?WORKER, WorkerNum) of
                not_find ->
                    Acc;
                {ok, #{<<"worker_workshop">> := Shop, <<"worker_shiftstarttime">> := Start, <<"worker_shiftendtime">> := End, <<"worker_name">> := WorkerName}}
                    ->
                    case ((Now > Start) and (Now < End)) and (WorkeShop == Shop) of
                        true ->
                            Acc ++ [#{<<"label">> => WorkerName, <<"value">> => WorkerNum}];
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
                        {ok, WorkerNum} ->
                            Num = dgiot_utils:to_int(WorkerNum),
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


get_shift_time(ProductId,Shift) ->
    case dgiot_data:get(?FACTORY,{ProductId,shift}) of
        nut_find ->
            #{};
        Res ->
            maps:get(Shift,Res,#{})
    end.
