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

-module(dgiot_cron).
-include_lib("stdlib/include/ms_transform.hrl").
-include("dgiot_cron.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([test/0, childspec/1, childspec/2, do_task/2, change_time/2]).
-export([start/1, start/2, start_timer/2, start_timer/3, handle_call/3, handle_info/2, get_next_time/3]).
-export([save/2, match_task/2,now/1,push/1]).

-include_lib("eunit/include/eunit.hrl").
start(Sup) ->
    {ok, _} = start(Sup, ?DEFAULT_CRON).

start(Sup, Name) ->
    ChildSpec = childspec(Name),
    supervisor:start_child(Sup, ChildSpec).

start_timer(StartTime, Callback) ->
    ID = dgiot_utils:guid(),
    start_timer(ID, StartTime, Callback).

start_timer(ID, {{Y,M,D}, {H,N,S}}, Callback) ->
    dgiot_cron:save(?DEFAULT_CRON, #{
        <<"id">> => ID,
        <<"start_time">> => {{Y, M, D}, {H, N, S}},
        <<"callback">> => Callback
    });

start_timer(ID, Secs, Callback) when is_integer(Secs) ->
    dgiot_cron:save(?DEFAULT_CRON, #{
        <<"id">> => ID,
        <<"freq">> => Secs,
        <<"unit">> => 5,
        <<"count">> => 1,
        <<"callback">> => Callback
    }).


%% 返回任务进程描述，可以挂到sup下面
childspec(Name) ->
    childspec(Name, fun() -> dgiot_datetime:nowstamp() end).

%% GetTimeFun 获取当前时间，与任务时间比较
childspec(Name, GetTimeFun) ->
    NewGetTimeFun = fun() -> dgiot_datetime:to_unixtime(GetTimeFun()) end,
    Fun =
        fun() ->
            Now = NewGetTimeFun(),
            ?MODULE:do_task(Name, Now),
            {ok, Now}
        end,
    {
        dgiot_cron_woker,
        {dgiot_cron_woker, start_link, [?CRON_NAME(Name), [{<<"gettime">>, NewGetTimeFun}, {<<"mod">>, ?MODULE}, {<<"frequency">>, 900}], Fun]},
        permanent, 5000, worker, [dgiot_cron_woker]
    }.


% 任务当前时间
now(Name) ->
    gen_server:call(?CRON_NAME(Name), now, 500).

change_time(Name, Sec) ->
    erlang:send_after(500, ?CRON_NAME(Name), {change_time, Name, Sec}).


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

save(Name, #{ <<"id">> := Id, <<"start_time">> := {{_Y,M,D},{H,N,S}}} = Task, _) when
    M == '_'; D == '_'; H == '_'; N == '_'; S =='_' ->
    Enable = maps:get(<<"enable">>, Task, true),
    Count = maps:get(<<"count">>, Task, forever),
    dgiot_data:insert(?CRON_DB, Id, Task#{ <<"name">> => Name, <<"count">> => Count, <<"enable">> => Enable });

save(Name, #{<<"id">> := Id, <<"freq">> := Freq, <<"unit">> := Unit} = Task, Now) ->
    StartTime = maps:get(<<"start_time">>, Task, dgiot_datetime:unixtime_to_localtime(Now)),
    UnixStartTime = dgiot_datetime:to_unixtime(StartTime),
    RunTime = maps:get(<<"run_time">>, Task, StartTime),
    NextTime = get_next_time(RunTime, Freq, Unit),
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
            case get_sec_by_unit(Freq, Unit) of
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


get_next_time(StartTime, Freq, Unit) ->
    NewStartTime = dgiot_datetime:to_unixtime(StartTime),
    case Unit of
        0 ->
            NewStartTime + get_sec_by_unit(Freq, Unit); % 分
        1 ->
            NewStartTime + get_sec_by_unit(Freq, Unit); % 时
        2 ->
            NewStartTime + get_sec_by_unit(Freq, Unit); % 日
        3 ->
            {{Y, M, D}, {H, N, S}} = dgiot_datetime:unixtime_to_localtime(NewStartTime),
            Year =
                case M + Freq =< 12 of
                    true -> Y;
                    false -> Y + (M + Freq) rem 12
                end,
            Month =
                case (M + Freq) rem 12 of
                    0 ->
                        12;
                    Any ->
                        Any
                end,
            LastDay = calendar:last_day_of_the_month(Year, Month),
            Day = case LastDay > D of true -> D; false -> LastDay end,
            dgiot_datetime:localtime_to_unixtime({{Year, Month, Day}, {H, N, S}}); % 月
        4 ->
            {{Y, M, D}, {H, N, S}} = dgiot_datetime:unixtime_to_localtime(NewStartTime),
            Year = Y + Freq,
            LastDay = calendar:last_day_of_the_month(Year, M),
            Day = case LastDay > D of true -> D; false -> LastDay end,
            dgiot_datetime:localtime_to_unixtime({{Year, M, Day}, {H, N, S}}); % 年
        5 ->
            NewStartTime + Freq
    end.

get_sec_by_unit(Freq, 0) ->
    Freq * 60;
get_sec_by_unit(Freq, 1) ->
    Freq * 60 * 60;
get_sec_by_unit(Freq, 2) ->
    Freq * 24 * 60 * 60;
get_sec_by_unit(Freq, 5) ->
    Freq;
get_sec_by_unit(_, _) ->
    error.

do_task(Name, Now) ->
    Fun =
        fun({ID, Task}) ->
            case catch (run_task(Name, Task#{<<"id">> => ID, <<"run_time">> => Now})) of
                {'EXIT', Reason} ->
                    ?LOG(error,"Task execute error, ~p~n", [Reason]);
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
    save(Name, NewTask),
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
    ?LOG(info,"Task[~p], change time ~p~n", [Name, Sec]),
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



test() ->
    spawn(fun() ->
        timer:sleep(3 * 1000),
        lists:foreach(
            fun(I) ->
                dgiot_cron:save(<<"111111111111">>, #{
                    <<"callback">> => fun(_T) ->
                        ?LOG(info,"~p ",[I]), ok end,
                    <<"freq">> => I,
                    <<"unit">> => 5,
                    <<"id">> => I + 10000
                })
            end, lists:seq(1, 60))
          end).

push([]) -> ok;
push([Sec | Secs]) ->
    Callback =
        fun(_Task) ->
            io:format("call:~p~n", [Sec]),
            %% todo
            %% if fail
            push(Secs)
        end,
    dgiot_cron:start_timer(dgiot_utils:guid(), Sec, Callback).

%% === 测试代码 =====
get_next_time_test() ->
    NextTime = dgiot_datetime:to_unixtime(get_next_time({{2018, 10, 16}, {12, 30, 12}}, 1, 0)),
    ?assertEqual({{2018, 10, 16}, {12, 31, 12}}, dgiot_datetime:unixtime_to_localtime(NextTime)),

    NextTime1 = dgiot_datetime:to_unixtime(get_next_time({{2018, 10, 16}, {12, 30, 12}}, 1, 1)),
    ?assertEqual({{2018, 10, 16}, {13, 30, 12}}, dgiot_datetime:unixtime_to_localtime(NextTime1)),

    NextTime2 = dgiot_datetime:to_unixtime(get_next_time({{2018, 10, 16}, {12, 30, 12}}, 1, 2)),
    ?assertEqual({{2018, 10, 17}, {12, 30, 12}}, dgiot_datetime:unixtime_to_localtime(NextTime2)),

    NextTime3 = dgiot_datetime:to_unixtime(get_next_time({{2018, 12, 16}, {12, 30, 12}}, 1, 3)),
    ?assertEqual({{2019, 1, 16}, {12, 30, 12}}, dgiot_datetime:unixtime_to_localtime(NextTime3)),

    NextTime4 = dgiot_datetime:to_unixtime(get_next_time({{2018, 1, 31}, {12, 30, 12}}, 1, 3)),
    ?assertEqual({{2018, 2, 28}, {12, 30, 12}}, dgiot_datetime:unixtime_to_localtime(NextTime4)),

    NextTime5 = dgiot_datetime:to_unixtime(get_next_time({{2020, 2, 29}, {12, 30, 12}}, 1, 4)),
    ?assertEqual({{2021, 2, 28}, {12, 30, 12}}, dgiot_datetime:unixtime_to_localtime(NextTime5)).
