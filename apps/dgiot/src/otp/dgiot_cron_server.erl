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


-module(dgiot_cron_server).

-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").

% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
-export([run/3, run/4, run/5, clean/1, start_link/3]).

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

% --------------------------------------------------------------------
% External API
% --------------------------------------------------------------------


run(Module, Fun, Args, Seconds) ->
    TaskName = list_to_atom(lists:concat([task_, dgiot_datetime:nowstamp()])),
    run(TaskName, Module, Fun, Args, Seconds).

run(TaskName, Module, Fun, Args, Seconds) ->
    run(TaskName, [{<<"frequency">>, Seconds}], {Module, Fun, Args}).

run(TaskName, Config, Fun) when is_record(Config, config) ->
    NewConfig = [
        {<<"start_time">>, Config#config.start_time},
        {<<"frequency">>, Config#config.frequency},
        {<<"unit">>, Config#config.unit},
        {<<"run_time">>, Config#config.run_time},
        {<<"count">>, Config#config.count}
    ],
    run(TaskName, NewConfig, Fun);

run(TaskName, Config, Fun) when is_list(Config) ->
    case supervisor:start_child(dgiot_task_manager_sup, [TaskName, Config, Fun]) of
        {error, Why} ->
            {error, Why};
        {ok, Pid} ->
            ?LOG(info,"Task[~p, ~p] run successful!~n", [TaskName, Pid]),
            {ok, Pid}
    end.

clean(TaskName) ->
    gen_server:call(TaskName, clean, 500).


start_link(TaskName, Config, Fun) ->
    gen_server:start_link({local, TaskName}, ?MODULE, [TaskName, Config, Fun], []).



% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([TaskName, Config, Fun]) ->
    ?LOG(info,"TaskName ~p, Config ~p, Fun ~p",[TaskName, Config, Fun]),
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
    end.

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

handle_call(Request, From, #{ <<"mod">> := Mod } = State) ->
    case Mod == undefined of
      true ->
          {reply, noreply, State};
      false ->
          case catch(apply(Mod, handle_call, [Request, From, State])) of
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
handle_cast(Msg, #{ <<"mod">> := Mod } = State) ->
    case Mod == undefined of
      true ->
          {noreply, State};
      false ->
          case catch(apply(Mod, handle_cast, [Msg, State])) of
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
    <<"tref">> := {Pre, TRef} }) ->
    NewTRef = next_time(TRef, Frequency),
    Now = dgiot_datetime:nowstamp(),
    case Now == Pre of
      false ->
          NewState = do_task(State#{ <<"tref">> => {Now, NewTRef} }),
          case is_complete(Frequency, EndTime) of
              true ->
                  {stop, normal, NewState};
              false ->
                  {noreply, NewState}
          end;
      true ->
          {noreply, State#{ <<"tref">> => {Now, NewTRef} } }
    end;


handle_info(Info, #{ <<"mod">> := Mod } = State) ->
    case Mod == undefined of
      true ->
          {noreply, State};
      false ->
          case catch(apply(Mod, handle_info, [Info, State])) of
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
%%    erlang:start_timer(Timeout, self(), do).


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
          NewFrequency = case to_int(proplists:get_value(<<"unit">>, Config)) of
              0 ->
                 Frequency * 60 * 1000; % 分
              1 ->
                 Frequency * 60 * 60 * 1000; % 时
              2 ->
                 Frequency * 60 * 60 * 24 * 1000; % 天
              3 ->
                 Frequency * 1000; % 秒
              _ ->
                 Frequency % 毫秒
          end,
          StartTime = dgiot_datetime:localtime_to_unixtime(proplists:get_value(<<"start_time">>, Config, calendar:local_time())),
          EndTime1 = case proplists:get_value(<<"count">>, Config) of
              undefined ->
                  undefined;
              Count ->
                  StartTime + (Count - 1)  * NewFrequency div 1000
          end,
          EndTime2 = case proplists:get_value(<<"run_time">>, Config) of
              undefined -> undefined;
              V -> StartTime + V
          end,
          EndTime = min(EndTime1, EndTime2),
          {ok, {StartTime, NewFrequency , EndTime}}
   end.


do_task(State = #{ <<"name">> := TaskName, <<"callback">> := Callback, <<"tref">> := {_Now, _} }) ->
   % io:format("Task:~p, Now:~p~n", [TaskName, dgiot_datetime:unixtime_to_localtime(Now)]),
   case catch(execute(Callback)) of
      {ok, _Time} ->
          State;
      {_, Reason} ->
          ?LOG(error,"Task[~p] execute error why:~p~n", [TaskName, Reason]),
          State
  end.

execute({M,F, A}) -> apply(M, F, A);
execute(Callback) -> Callback().

waitting(StartTime, Frequency) ->
    Now = dgiot_datetime:nowstamp(),
    if
        StartTime >= Now ->
            (StartTime - Now) * 1000;
        true ->
            Frequency - (Now - StartTime) * 1000 rem Frequency
    end.



