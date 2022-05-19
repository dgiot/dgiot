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

-module(dgiot_cron_timer).
-include_lib("stdlib/include/ms_transform.hrl").
-include("dgiot_cron.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([init_ets/0, start/0, start/2]).
-export([handle_call/3, handle_info/2]).
-export([save/2, do_task/2, match_task/2]).
-export([test/0]).

-dgiot_data("ets").
init_ets() ->
    init(?CRON_DB).

%% 初始化数据,可以多共用一个
init(Name) ->
    dgiot_data:init(Name, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]).

start() ->
    Clock = get_clock(),
    start(?DGIOT_TIMER, Clock).

%% 可以启动不同时区的定时器,设置自己的定时计算函数
start(Name, Clock) ->
    Fun =
        fun() ->
            Now = Clock(),
            ?MODULE:do_task(Name, Now),
            {ok, Now}
        end,
    %% 启动一个频率为990毫秒的定时器,保证整点秒不会错过
    Args = [?CRON_NAME(Name), [{<<"gettime">>, Clock}, {<<"mod">>, ?MODULE}, {<<"frequency">>, 990}], Fun],
    supervisor:start_child(dgiot_cron_sup, Args).

get_clock() ->
    GetTimeFun = fun() -> dgiot_datetime:nowstamp() end,
    get_clock(GetTimeFun).

%% GetTimeFun 获取当前时间，与任务时间比较
get_clock(GetTimeFun) ->
    fun() -> dgiot_datetime:to_unixtime(GetTimeFun()) end.

save(_Name, #{
    <<"id">> := Id,
    <<"end_time">> := EndTime,
    <<"next_time">> := NextTime
}) when EndTime < NextTime ->
    dgiot_data:delete(?CRON_DB, Id);

save(_Name, #{<<"id">> := Id, <<"count">> := 0}) ->
    dgiot_data:delete(?CRON_DB, Id);

save(Name, Task) ->
    save(Name, Task, dgiot_datetime:nowstamp()).

save(Name, #{<<"id">> := Id, <<"start_time">> := {{_Y, M, D}, {H, N, S}}} = Task, _Now) when M == '_'; D == '_'; H == '_'; N == '_'; S == '_' ->
    Enable = maps:get(<<"enable">>, Task, true),
    Count = maps:get(<<"count">>, Task, forever),
    dgiot_data:insert(?CRON_DB, Id, Task#{<<"name">> => Name, <<"count">> => Count, <<"enable">> => Enable});

save(Name, #{<<"id">> := Id, <<"freq">> := Freq, <<"unit">> := Unit} = Task, Now) ->
    StartTime = maps:get(<<"start_time">>, Task, dgiot_datetime:unixtime_to_localtime(Now)),
    UnixStartTime = dgiot_datetime:to_unixtime(StartTime),
    RunTime = maps:get(<<"run_time">>, Task, StartTime),
    NextTime = dgiot_cron_worker:get_next_time(RunTime, Freq, Unit),
    Enable = maps:get(<<"enable">>, Task, true),
    Count = maps:get(<<"count">>, Task, forever),
    NewTask = Task#{
        <<"name">> => Name,
        <<"count">> => Count,
        <<"enable">> => Enable,
        <<"start_time">> => StartTime,
        <<"next_time">> => NextTime
    },
    NewTask1 =
        case maps:get(<<"end_time">>, NewTask, false) of
            false ->
                NewTask;
            EndTime ->
                NewTask#{<<"end_time">> => dgiot_datetime:to_unixtime(EndTime)}
        end,
    if
        Now < UnixStartTime ->
            dgiot_data:insert(?CRON_DB, Id, NewTask1#{<<"next_time">> => UnixStartTime});
        Now == NextTime ->
            dgiot_data:insert(?CRON_DB, Id, NewTask1#{<<"next_time">> => NextTime + 1});
        Now < NextTime ->
            dgiot_data:insert(?CRON_DB, Id, NewTask1);
        true ->
            case dgiot_cron_worker:get_sec_by_unit(Freq, Unit) of
                error ->
                    {error, start_time_error};
                SecFreq ->
                    NextTime1 = UnixStartTime + ((Now - UnixStartTime) div SecFreq + 1) * SecFreq,
                    dgiot_data:insert(?CRON_DB, Id, NewTask1#{<<"next_time">> => NextTime1})
            end
    end.

match_task(Name, Now, Fun) ->
    Handle = ets:table(?CRON_DB),
    Filter =
        fun({ID, Task}) ->
            case Task of
                #{<<"name">> := Name, <<"enable">> := true} ->
                    LocalTime = dgiot_datetime:to_localtime(Now),
                    case match_task(LocalTime, Task) of
                        true ->
                            Fun({ID, Task}),
                            false;
                        false ->
                            false
                    end;
                _ ->
                    false
            end
        end,
    dgiot_pager:all(Handle, Filter, none).

match_task({{_Y, M, D}, {H, N, S}} = Now, #{<<"start_time">> := StartTime} = Task) ->
    if
        StartTime == {{'_', '_', '_'}, {'_', '_', S}} ->
            true;
        StartTime == {{'_', '_', '_'}, {'_', N, S}} ->
            true;
        StartTime == {{'_', '_', '_'}, {H, N, S}} ->
            true;
        StartTime == {{'_', '_', D}, {H, N, S}} ->
            true;
        StartTime == {{'_', M, D}, {H, N, S}} ->
            true;
        true ->
            case maps:get(<<"next_time">>, Task, false) of
                false ->
                    false;
                NextTime ->
                    NextTime == dgiot_datetime:to_unixtime(Now)
            end
    end.

do_task(Name, Now) ->
    Fun =
        fun({ID, Task}) ->
            case catch (run_task(Name, Task#{<<"id">> => ID, <<"run_time">> => Now})) of
                {'EXIT', Reason} ->
                    ?LOG(error, "Task execute error, ~p~n", [Reason]);
                _ ->
                    ok
            end
        end,
    NowTime = dgiot_datetime:to_unixtime(Now),
    proc_lib:spawn(fun() -> match_task(Name, NowTime, Fun) end).

% 回调三种类型
% 匿名函数写法  fun(Task) -> end
% MF 写法   Mod:Fun(Task)
% MFA 写法  Mod:Fun([Task|Args])
run_task(Name, #{<<"callback">> := Caller, <<"count">> := Count} = Task) ->
    NewCount = case is_integer(Count) of true -> Count - 1; false -> Count end,
    NewTask = Task#{
        <<"count">> => NewCount,
        <<"exe_times">> => maps:get(<<"exe_times">>, Task, 0) + 1},
    dgiot_cron:save(Name, NewTask),
    case Caller of
        {Mod, Fun} ->
            apply(Mod, Fun, [Task]);
        {Mod, Fun, Args} ->
            apply(Mod, Fun, [Task | Args]);
        Fun ->
            Fun(Task)
    end.

%% 消息处理函数
handle_call(now, _From, #{<<"gettime">> := GetTimeFun} = State) ->
    {reply, {ok, GetTimeFun()}, State}.

%% 集中器时间改变, 对当前任务进行更正
handle_info({change_time, Name, Sec}, #{<<"gettime">> := GetTimeFun} = State) ->
    change_task_by_time(Name, Sec, GetTimeFun),
    {noreply, State}.

change_task_by_time(Name, Sec, GetTimeFun) ->
    ?LOG(info, "Task[~p], change time ~p~n", [Name, Sec]),
    Fun = fun
              ({_ID, #{<<"name">> := NewName, <<"next_time">> := NextTime} = Task}) ->
                  case NextTime > GetTimeFun() of
                      true ->
                          ok;
                      false ->
                          save(NewName, Task, GetTimeFun())
                  end;
              (_) ->
                  ok
          end,
    dgiot_data:search(?CRON_DB, Fun).

%% === 测试代码 =====
%% 测试一分钟内每一秒都能进入
test() ->
    spawn(fun() ->
        timer:sleep(3 * 1000),
        lists:foreach(
            fun
                (I) ->
                    save(<<"secondTimer">>, #{
                        <<"callback">> => fun(_T) ->
                            io:format("secondTimer freq ~p ~p ~n", [I, dgiot_datetime:now_secs()]) end,
                        <<"freq">> => I,
                        <<"unit">> => second,
                        <<"id">> => I
                    })
            end, lists:seq(1, 60))
          end).


