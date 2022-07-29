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
-export([save/1, save/2, push/2, push/3]).
-export([test/0, test/1]).

save(Task) ->
    save(?DGIOT_TIMER, Task).

save(Name, Task) ->
    dgiot_cron_timer:save(Name, Task).

push(StartTime, Callback) ->
    ID = dgiot_utils:guid(),
    push(ID, StartTime, Callback).

%% 定时启停任务
push(ID, {{Y, M, D}, {H, N, S}}, Callback) ->
    save(?DGIOT_TIMER, #{
        <<"id">> => ID,
        <<"freq">> => 30,
        <<"unit">> => second,
        <<"count">> => 3,
        <<"start_time">> => {{Y, M, D}, {H, N, S}},
        <<"callback">> => Callback
    });

push(ID, Secs, Callback) when is_integer(Secs) ->
    save(?DGIOT_TIMER, #{
        <<"id">> => ID,
        <<"freq">> => Secs,
        <<"unit">> => second,
        <<"count">> => 1,
        <<"callback">> => Callback
    }).

%% === 测试代码 =====
test() ->
    spawn(fun() ->
        timer:sleep(3 * 1000),
        lists:foreach(
            fun(I) ->
                save(#{
                    <<"callback">> => fun(_T) ->
                        io:format("dgiot_cron ~p ~p ~n", [I, dgiot_datetime:local_time()]), ok end,
                    <<"freq">> => I,
                    <<"unit">> => second,
                    <<"id">> => I + 1000
                })
            end, lists:seq(1, 60))
          end).


%% 指定几秒后执行任务
test([]) -> ok;
test([Sec | Secs]) ->
    Callback =
        fun(_Task) ->
            io:format("call:~p ~p~n", [Sec, dgiot_datetime:local_time()]),
            %% todo
            %% if fail
            test(Secs)
        end,
    push(dgiot_utils:guid(), Sec, Callback).
