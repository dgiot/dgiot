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

%% @doc
%%
%% task_server:
%%
%%
%% Body^@
%%
%% @end

%%
%% 执行任务调用
%%
%% task_server:run(Module, Fun, Args, Seconds)
%%
%% 例：5秒为周期打印，Hello, zwx
%% Name = "zwx",
%% task_server:run(io, format, ["Hello, ~p~n", [Name]], 5).
%%
%%
%% 如果要自己控制任务的起停（TaskName为元子）
%%
%% 开启任务
%% task_server:run(TaskName, Module, Fun, Args, Seconds),
%%
%% 停止任务
%% task_server:clean(TaskName).
%%
%% 停止所有任务
%% task_server:clean().
%%
%% 如果要更细的控制任务，比如执行次数或运行总时间
%% Config =
%% [
%%  {frequency, 5}, %% 频率，必填，根据unit为判断单位
%%
%%  {unit, 0}, %% 频率单位， 0，1，2代表频率分别为单位为分 时 天，不填单位为秒
%%
%%  {start_time, {{2017,11,19},{15,16,0}}, %% 启动时间，非必填，不填就为当前时间
%%
%%  {run_time, 5 * 60} %% 任务运行总时间，非必填，不填无限制周期运行下去
%%
%%  {count, 100} %% 执行次数，非必填，不填无限制周期运行下去
%% ]
%%
%% task_server:run(TaskName, Config, {Module, Fun, Args})
%%
%% task_server:fun(TaskName, Config, fun() ->  end)
%%
%% 例 以当前时间，5秒周期打印
%% Name = "zwx",
%% Config = [{frequency, 5}],
%%
%% task_server:run(task_test, Config, {io, format, ["Hello, ~p~n", [Name]]})
%% 或
%% task_server:run(task_test1, Config, fun() -> io:format("Hello, ~p~n", [Name]) end).


-module(dgiot_cron_worker).
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_cron.hrl").
-include_lib("eunit/include/eunit.hrl").

% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
-export([start/3, start/4, start/5, stop/1, start_link/3, get_next_time/3, get_sec_by_unit/2]).

-export([test/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%-record(state, { name, start_time, frequency, callback, end_time, tref, mod, gettime }).
-record(config, {start_time, unit, frequency, run_time, count}).

-define(DETS, task_config).
-define(DETS_PATH, "data/task.config").


% Config =
%[
%  {start_time, {{2017,11,19},{15,16,0}},
%  {frequency, 5},
%  {unit, 0},
%  {run_time, 5 * 60}
%  {count, 100}
%]

start_link(TaskName, Config, Fun) ->
    gen_server:start_link({local, ?CRON_NAME(TaskName)}, ?MODULE, [TaskName, Config, Fun], []).

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([TaskName, Config, Fun]) ->
    ?LOG(info, "TaskName ~p, Config ~p, Fun ~p", [TaskName, Config, Fun]),
    case parse_config(Config) of
        {ok, {StartTime, Frequency, EndTime}} ->
            Waitting = waitting(StartTime, Frequency),
            case is_complete(Waitting, EndTime) of
                true ->
                    {stop, normal};
                false ->
                    {ok, #{
                        <<"name">> => TaskName,
                        <<"mod">> => proplists:get_value(<<"mod">>, Config),
                        <<"gettime">> => proplists:get_value(<<"gettime">>, Config),
                        <<"start_time">> => StartTime,
                        <<"frequency">> => Frequency,
                        <<"callback">> => Fun,
                        <<"end_time">> => EndTime,
                        <<"tref">> => {0, next_time(Waitting)}
                    }}
            end;
        {error, Reason} ->
            {stop, Reason}
    end;

init(Other) ->
    io:format("~s ~p ~p ~n", [?FILE, ?LINE, Other]),
    ok.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(clean, _From, State) ->
    {stop, normal, ok, State};

handle_call(Request, From, #{<<"mod">> := Mod} = State) ->
    case Mod == undefined of
        true ->
            {reply, noreply, State};
        false ->
            case catch (apply(Mod, handle_call, [Request, From, State])) of
                {'EXIT', Reason} ->
                    {reply, Reason, State};
                Reply ->
                    Reply
            end
    end.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(Msg, #{<<"mod">> := Mod} = State) ->
    case Mod == undefined of
        true ->
            {noreply, State};
        false ->
            case catch (apply(Mod, handle_cast, [Msg, State])) of
                {'EXIT', _Reason} ->
                    {noreply, State};
                Reply ->
                    Reply
            end
    end.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info({timeout, _TimerRef, do}, State) ->
    handle_info(do, State);

handle_info(do, State = #{
    <<"end_time">> := EndTime,
    <<"frequency">> := Frequency,
    <<"tref">> := {Pre, TRef}}) ->
    NewTRef = next_time(TRef, Frequency),
    Now = dgiot_datetime:nowstamp(),
    case Now == Pre of
        false ->
            NewState = do_task(State#{<<"tref">> => {Now, NewTRef}}),
            case is_complete(Frequency, EndTime) of
                true ->
                    {stop, normal, NewState};
                false ->
                    {noreply, NewState}
            end;
        true ->
            {noreply, State#{<<"tref">> => {Now, NewTRef}}}
    end;

handle_info(Info, #{<<"mod">> := Mod} = State) ->
    case Mod == undefined of
        true ->
            {noreply, State};
        false ->
            case catch (apply(Mod, handle_info, [Info, State])) of
                {'EXIT', _Reason} ->
                    {noreply, State};
                Reply ->
                    Reply
            end
    end.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

to_int(<<V:8>>) ->
    V;
to_int(V) when is_list(V) ->
    list_to_integer(V);
to_int(V) when is_integer(V) ->
    V;
to_int(V) ->
    V.


is_complete(Waitting, EndTime) when is_integer(EndTime) andalso is_integer(Waitting) ->
    dgiot_datetime:nowstamp() + Waitting div 1000 > EndTime;
is_complete(_, _) ->
    false.


next_time(TRef, Timeout) ->
    _ = erlang:cancel_timer(TRef),
    next_time(Timeout).
next_time(Timeout) ->
    erlang:send_after(Timeout, self(), do).


% Config =
%[
%  {start_time, {{2017,11,19},{15,16,0}},
%  {frequency, 5},
%  {unit, 0},
%  {run_time, 5 * 60}
%  {count, 100}
%]
parse_config(Config) ->
    case to_int(proplists:get_value(<<"frequency">>, Config)) of
        undefined ->
            {error, frequency_is_invalid};
        Frequency ->
            NewFrequency = get_sec_by_unit(Frequency, dgiot_utils:to_atom(proplists:get_value(<<"unit">>, Config))),
            StartTime = dgiot_datetime:localtime_to_unixtime(proplists:get_value(<<"start_time">>, Config, calendar:local_time())),
            EndTime1 =
                case proplists:get_value(<<"count">>, Config) of
                    undefined ->
                        undefined;
                    Count ->
                        StartTime + (Count - 1) * NewFrequency
                end,
            EndTime2 =
                case proplists:get_value(<<"run_time">>, Config) of
                    undefined -> undefined;
                    V -> StartTime + V
                end,
            EndTime = min(EndTime1, EndTime2),
            {ok, {StartTime, NewFrequency, EndTime}}
    end.

do_task(State = #{<<"name">> := _TaskName, <<"callback">> := Callback, <<"tref">> := {_Now, _}}) ->
%%    io:format("~s ~p Task:~p, Callback ~p _Now ~p ~n", [?FILE, ?LINE, _TaskName, Callback, _Now]),
    safe_execute(Callback),
    State.

safe_execute(Callback) ->
    try execute(Callback) of
        Result -> Result
    catch
        Error:Reason:Stacktrace ->
            ?LOG(error, "Failed to execute ~0p: ~0p", [Callback, {Error, Reason, Stacktrace}]),
            ok
    end.

% 回调三种类型
% 匿名函数写法  fun(Task) -> end
% MF 写法   Mod:Fun(Task)
% MFA 写法  Mod:Fun([Task|Args])
execute({M, F, A}) ->
    apply(M, F, A);
execute({F, A}) ->
    apply(F, A);
execute(F) ->
    F().

waitting(StartTime, Frequency) ->
    Now = dgiot_datetime:nowstamp(),
    if
        StartTime >= Now ->
            (StartTime - Now) * 1000;
        true ->
            Frequency - (Now - StartTime) * 1000 rem Frequency
    end.


get_next_time(StartTime, Freq, 0) ->
    get_next_time(StartTime, Freq, minute);
get_next_time(StartTime, Freq, 1) ->
    get_next_time(StartTime, Freq, hour);
get_next_time(StartTime, Freq, 2) ->
    get_next_time(StartTime, Freq, day);
get_next_time(StartTime, Freq, 3) ->
    get_next_time(StartTime, Freq, month);
get_next_time(StartTime, Freq, 4) ->
    get_next_time(StartTime, Freq, year);
get_next_time(StartTime, Freq, 5) ->
    get_next_time(StartTime, Freq, second);
get_next_time(StartTime, Freq, Unit) ->
    NewStartTime = dgiot_datetime:to_unixtime(StartTime),
    case Unit of
        second ->
            NewStartTime + Freq;                        % 秒
        minute ->
            NewStartTime + get_sec_by_unit(Freq, Unit); % 分
        hour ->
            NewStartTime + get_sec_by_unit(Freq, Unit); % 时
        day ->
            NewStartTime + get_sec_by_unit(Freq, Unit); % 日
        month ->
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
            dgiot_datetime:localtime_to_unixtime({{Year, Month, Day}, {H, N, S}});  % 月
        year ->
            {{Y, M, D}, {H, N, S}} = dgiot_datetime:unixtime_to_localtime(NewStartTime),
            Year = Y + Freq,
            LastDay = calendar:last_day_of_the_month(Year, M),
            Day = case LastDay > D of true -> D; false -> LastDay end,
            dgiot_datetime:localtime_to_unixtime({{Year, M, Day}, {H, N, S}});      % 年
        _ ->
            NewStartTime + Freq                                                     % 秒
    end.


get_sec_by_unit(Freq, second) -> % 秒
    Freq;
get_sec_by_unit(Freq, minute) -> % 分
    Freq * 60;
get_sec_by_unit(Freq, hour) ->  % 小时
    Freq * 60 * 60;
get_sec_by_unit(Freq, day) -> % 天
    Freq * 24 * 60 * 60;
get_sec_by_unit(Freq, _) ->
    Freq.

% --------------------------------------------------------------------
% External API
% --------------------------------------------------------------------
start(M, F, A, Seconds) ->
    TaskName = list_to_atom(lists:concat([task_, dgiot_datetime:nowstamp()])),
    start(TaskName, M, F, A, Seconds).

start(TaskName, M, F, A, Seconds) ->
    start(TaskName, [{<<"frequency">>, Seconds}], {M, F, A}).

start(TaskName, Config, Fun) when is_record(Config, config) ->
    NewConfig =
        [
            {<<"start_time">>, Config#config.start_time},
            {<<"frequency">>, Config#config.frequency},
            {<<"unit">>, Config#config.unit},
            {<<"run_time">>, Config#config.run_time},
            {<<"count">>, Config#config.count}
        ],
    start(TaskName, NewConfig, Fun);

start(TaskName, Config, Fun) when is_list(Config) ->
    case supervisor:start_child(dgiot_cron_sup, [TaskName, Config, Fun]) of
        {error, Why} ->
            {error, Why};
        {ok, Pid} ->
            ?LOG(info, "Task[~p, ~p] run successful!~n", [TaskName, Pid]),
            {ok, Pid}
    end.

stop(TaskName) ->
    gen_server:call(TaskName, clean, 500).


%% 测试代码
test() ->
    Config = [
        {<<"start_time">>, {{2022, 05, 17}, {18, 00, 01}}},
        {<<"frequency">>, 5},
        {<<"unit">>, second},
        {<<"run_time">>, 5 * 60 * 10000},
        {<<"count">>, 10000000}
    ],
    Fun = fun() -> io:format("~p ~n", [dgiot_datetime:now_secs()]) end,
    dgiot_cron_worker:start(<<"crontest44">>, Config, Fun).

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