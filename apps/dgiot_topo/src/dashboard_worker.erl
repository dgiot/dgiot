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

start_link(#{<<"dashboardId">> := DashboardId, <<"sessionToken">> := SessionToken} = State) ->
    case dgiot_data:lookup({dashboard, DashboardId, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    gen_server:call(Pid, stop, 5000),
                    erlang:garbage_collect(Pid);
                false ->
                    pass
            end;
        _Reason ->
            pass
    end,
    gen_server:start_link(?MODULE, [State], []);

start_link(_State) ->
    ok.

stop(#{<<"dashboardId">> := DashboardId, <<"sessionToken">> := SessionToken}) ->
    case dgiot_data:lookup({dashboard, DashboardId, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"dashboardId">> := DashboardId, <<"sessionToken">> := SessionToken}]) ->
    dgiot_data:insert({dashboard, DashboardId, SessionToken}, self()),
    dgiot_mqtt:subscribe(<<"dashboard_ack/", SessionToken/binary>>),
    dgiot_data:delete({dashboard_heart, SessionToken}),
    Que = dgiot_topo:get_que(DashboardId),
    case length(Que) of
        0 ->
            erlang:send_after(3000, self(), stop);
        _ ->
            erlang:send_after(1000, self(), topo)
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
handle_info(dashboard, #task{newque = Que} = State) when length(Que) == 0 ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

handle_info(dashboard, State) ->
    {noreply, send_dashboard(State)};

%% 任务结束
handle_info(topo, #task{newque = Que} = State) when length(Que) == 0 ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

handle_info(topo, State) ->
    {noreply, send_topo(State)};

%%
handle_info({deliver, _, Msg}, State) ->
    Topic = dgiot_mqtt:get_topic(Msg),
    case Topic of
        <<"dashboard_ack/", SessionToken:34/binary>> ->
%%            io:format("~s ~p SessionToken = ~p. ~p~n ~n", [?FILE, ?LINE, SessionToken, dgiot_datetime:now_secs()]),
            dgiot_data:insert({dashboard_heart, SessionToken}, dgiot_datetime:now_secs());
        _ ->
            pass
    end,
    {noreply, State#task{heart = 0}};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_dashboard(#task{newque = Que} = State) ->
    Task = lists:nth(1, Que),
    dgiot_dashboard:do_task(Task, State),
    NewQue = lists:nthtail(1, Que),
    erlang:send_after(10 * 1000, self(), dashboard),
    State#task{newque = NewQue}.

send_topo(#task{newque = Que, sessiontoken = SessionToken} = State) ->
    Now = dgiot_datetime:now_secs(),
    Task = lists:nth(1, Que),
    dgiot_topo:send_topo(Task, SessionToken),
    NewQue = lists:nthtail(1, Que) ++ [Task],
    erlang:send_after(10 * 1000, self(), topo),
%%    ets:lookup(emqx_channel, SessionToken)
    case dgiot_data:get({dashboard_heart, SessionToken}) of
        Oldtime when Now - Oldtime > 30 ->
%%            停止任务
            State#task{newque = []};
        _ ->
            State#task{newque = NewQue}
    end.
