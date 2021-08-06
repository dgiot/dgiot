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
-module(dashboard_worker).
-author("johnliu").
-include("dgiot_topo.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#{<<"sessionToken">> := SessionToken} = State) ->
    case dgiot_data:lookup({dashboard, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    gen_server:start_link(?MODULE, [State], [])
            end;
        _Reason ->
            gen_server:start_link(?MODULE, [State], [])
    end;


start_link(State) ->
    ?LOG(info, "State ~p", [State]),
    ok.

stop(#{<<"sessionToken">> := SessionToken}) ->
    case dgiot_data:lookup({dashboard, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"data">> := Que, <<"sessionToken">> := SessionToken}]) ->
    dgiot_data:insert({dashboard, SessionToken}, self()),
    case length(Que) of
        0 ->
            erlang:send_after(300, self(), stop);
        _ ->
            Topic = <<"dashboard/", SessionToken/binary, "/heart">>,
            dgiot_mqtt:subscribe(Topic),
            erlang:send_after(30 * 1000, self(), heart),
            erlang:send_after(100, self(), retry)
    end,
    {ok, #task{oldque = Que, newque = Que, freq = 1, sessiontoken = SessionToken}};

init(A) ->
    ?LOG(info, "A ~p ", [A]).

handle_call(stop, _From, State) ->
    erlang:garbage_collect(self()),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    erlang:garbage_collect(self()),
    {stop, Reason, State};

%% 任务结束
handle_info(retry, #task{newque = Que} = State) when length(Que) == 0 ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

%% 定时触发抄表指令
handle_info(retry, State) ->
    {noreply, send_msg(State)};

%% 定时触发抄表指令
handle_info(heart, #task{heart = Heart} = State) when Heart < 4 ->
    erlang:send_after(30 * 1000, self(), heart),
    {noreply, State#task{heart = Heart + 1}};

%% 定时触发抄表指令
handle_info(heart, State) ->
    {stop, normal, State};

%% 任务结束
handle_info({deliver, _, _Msg}, State) ->
    {noreply, State#task{heart = 0}};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_msg(#task{newque = Que} = State) ->
    Task = lists:nth(1, Que),
    dgiot_dashboard:do_task(Task, State),
    NewQue = lists:nthtail(1, Que),
    erlang:send_after(300, self(), retry),
    State#task{newque = NewQue}.



