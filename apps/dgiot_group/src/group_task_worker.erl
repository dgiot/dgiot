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

-module(group_task_worker).
-author("johnliu").
-include("dgiot_group.hrl").
-include_lib("dgiot/include/logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(task, {tid, mid, di, mod, func,
               usedata = none, delay = 2, retry = none, que = none, times = 3600, ts = 0}).
%%%===================================================================
%%% API
%%%===================================================================
start_link(#{<<"tid">> := Tid, <<"di">> := Di, <<"mid">> := Mid} = State) ->
    case dgiot_data:lookup(?dgiot_GROUP_TASK, {Mid, Tid, Di}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    gen_server:start_link(?MODULE, [State], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"tid">> := Tid, <<"di">> := Di, <<"mid">> := Mid,<<"mod">> := Mod, <<"fun">> := Fun,
    <<"delay">> := Delay, <<"retry">> := Retry, <<"begin">> := Begin, <<"times">> := Times,
    <<"que">> := Que, <<"quelen">> := QueLen, <<"usedata">> := Usedata}]) ->
    BinMid = dgiot_utils:to_binary(Mid),
    dgiot_data:insert(?dgiot_GROUP_TASK, {Mid, Tid, Di}, self()),
    Consumer = <<"task/",Di/binary, "/",BinMid/binary>>,
    dgiot_data:set_consumer(Consumer, QueLen),
    TimeToLive = <<"task/times/",BinMid/binary>>,
    dgiot_data:set_consumer(TimeToLive, Times),
    erlang:send_after(Delay * Mid + Begin, self(), delay),
    {ok, #task{tid = Tid, mid = Mid, di = Di, mod = Mod, func = Fun,
        delay = Delay, retry = Retry, que = Que, usedata  = Usedata, times = Times}};

init(A) ->
    ?LOG(info,"A ~p ",[A]).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

handle_info(stop, State) ->
    {stop, normal, State};

%延迟启动
handle_info(delay, State) ->
    erlang:send_after(100, self(), retry),
    {noreply, State};

% 定时执行
handle_info(retry, #task{di = Di, mid = Mid, mod = Mod, func = Fun,
    retry = Retry, que = Que, usedata = Usedata} = State) ->
    BinMid = dgiot_utils:to_binary(Mid),
    Consumer = <<"task/",Di/binary, "/",BinMid/binary>>,
    Offset = dgiot_data:get_consumer(Consumer,1),
    TimeConsumer = <<"task/times/",BinMid/binary>>,
    TimeToLive = dgiot_data:get_consumer(TimeConsumer, 1),
    Mod:Fun(#{<<"que">> => Que, <<"offset">> => Offset, <<"usedata">> => {Mid, Usedata, TimeToLive}}),
    erlang:send_after(Retry, self(), retry),
    {noreply, State};

handle_info({td_gc, Pid}, State) ->
    case is_process_alive(Pid) of
        true ->
            erlang:garbage_collect(Pid),
            erlang:exit(Pid, brutal_kill);
        _ -> ok
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason,  _State ) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
