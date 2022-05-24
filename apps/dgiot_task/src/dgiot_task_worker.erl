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

-module(dgiot_task_worker).
-author("johnliu").
-include("dgiot_task.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(device_task, {
    pnque_len = 0 :: integer(),                              %% 本轮任务剩余的设备队列数
    product :: atom(),                                       %% 当前任务网关设备或者网关子设备的产品ID
    devaddr :: binary(),                                     %% 当前任务网关设备或者网关子设备的设备地址
    dique = [] :: list(),                                    %% 当前任务网关设备或者网关子设备下的指令队列
    interval = 3 :: integer(),                               %% 指令队列的间隔，
    appdata = #{} :: map()                                   %% 用户自定义的一些控制参数
}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%%%===================================================================a
%%% APIa
%%%===================================================================
start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"starttime">> := StartTime, <<"endtime">> := EndTime, <<"freq">> := Freq}]) ->
    dgiot_client:add(ChannelId, ClientId),
    erlang:send_after(20, self(), init),
    dgiot_metrics:inc(dgiot_task, <<"task">>, 1),
    NextTime = dgiot_client:get_nexttime(StartTime, Freq),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    io:format("~s ~p ChannelId ~p ClientId ~p  NextTime = ~p  Freq ~p Count = ~p.~n", [?FILE, ?LINE, ChannelId, ClientId, NextTime, Freq, Count]),
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED,
        clock = #dclock{nexttime = NextTime, freq = Freq, count = Count,  round = 0}},
    {ok, Dclient};

init(A) ->
    ?LOG(error, "A ~p ", [A]).

handle_call(stop, _From, State) ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    erlang:garbage_collect(self()),
    {stop, Reason, State};

handle_info(stop, State) ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

%% 动态修改任务启动时间和周期
handle_info({change_clock, NextTime, EndTime, Freq}, #dclient{clock = Clock} = Dclient) ->
    {noreply, Dclient#dclient{clock = Clock#dclock{nexttime = NextTime, count  = dgiot_client:get_count(NextTime, EndTime, Freq), freq = Freq}}};

%% 定时触发网关及网关任务, 在单个任务轮次中，要将任务在全局上做一下错峰操作
handle_info(next_time, #dclient{ channel = Channel, client = Client, userdata =  UserData,
    clock = #dclock{round = Round, nexttime = NextTime, count  = Count, freq = Freq, rand = Rand} = Clock} = Dclient) ->
    io:format("~s ~p DtuId = ~p.~n", [?FILE, ?LINE, Client]),
    dgiot_client:stop(Channel, Client, Count), %% 检查是否需要停止任务
    NewNextTime = dgiot_client:get_nexttime(NextTime, Freq),
    case dgiot_task:get_pnque(Client) of
        not_find ->
            {noreply, Dclient#dclient{clock = Clock#dclock{ nexttime = NewNextTime, count = Count - 1}}};
        {ProductId, DevAddr} ->
            NewRound = Round + 1,
            PnQueLen = dgiot_task:get_pnque_len(Client),
            DiQue = dgiot_task:get_instruct(ProductId, NewRound),
            dgiot_client:send_after(10, Freq, Rand, read), % 每轮任务开始时，做一下随机开始
            {noreply, Dclient#dclient{userdata = UserData#device_task{product = ProductId, devaddr = DevAddr,  pnque_len = PnQueLen, dique = DiQue},
                clock = Clock#dclock{nexttime = NewNextTime, count = Count - 1, round = NewRound}}}
    end;

%% 开始采集下一个子设备的指令集
handle_info(read, #dclient{userdata = #device_task{dique = DiQue} } = State) when length(DiQue) == 0 ->
    {noreply, get_next_pn(State)};

%% 发送采集指令
handle_info(retry, State) ->
    {noreply, send_msg(State)};

%% ACK消息触发进行新的指令发送
handle_info({dclient_ack, Topic, Payload}, #dclient{userdata = Usedata} = State) ->
    dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"thing">>, ProductId, DevAddr, <<"properties">>, <<"report">>] ->
            dgiot_task:save_td(ProductId, DevAddr, Payload, #{}),
            {noreply, send_msg(State#dclient{userdata = Usedata#device_task{product = ProductId, devaddr = DevAddr}})};
        _ ->
            {noreply, send_msg(State)}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dgiot_metrics:dec(dgiot_task, <<"task">>, 1),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_msg(#dclient{channel = Channel, userdata = #device_task{product = Product, devaddr = DevAddr, dique = DisQue} = UserData} = State) ->
    {InstructOrder, Interval, _Identifier, _AccessMode, _Data, _NewDataSource} = lists:nth(1, DisQue),
    {NewCount, _Payload, _Dis} =
        lists:foldl(fun(X, {Count, Acc, Acc1}) ->
            case X of
                {InstructOrder, _, _, _, error, _, _} ->
                    {Count + 1, Acc, Acc1};
                {InstructOrder, _, Identifier1, _AccessMode, NewData, DataSource, _} ->
                    Payload1 = DataSource#{<<"data">> => NewData},
                    Topic = <<"$dg/device/", Product/binary, "/", DevAddr/binary, "/properties">>,
                    dgiot_mqtt:publish(Channel, Topic, jsx:encode(Payload1)),
                    dgiot_bridge:send_log(Channel, Product, DevAddr, "to_dev=> ~s ~p ~ts: ~ts", [?FILE, ?LINE, unicode:characters_to_list(Topic), unicode:characters_to_list(jsx:encode(Payload1))]),
                    {Count + 1, Acc ++ [Payload1], Acc1 ++ [Identifier1]};
                _ ->
                    {Count, Acc, Acc1}
            end
                    end, {0, [], []}, DisQue),
    NewDisQue = lists:nthtail(NewCount, DisQue),
    dgiot_metrics:inc(dgiot_task, <<"task_send">>, 1),
    erlang:send_after(Interval * 1000, self(), retry),
    State#dclient{userdata = UserData#device_task{dique = NewDisQue, interval = Interval}}.

get_next_pn(#dclient{client = CLient, clock  = #dclock{round = Round}, userdata = UserData} = State) ->
    {NextProductId, NextDevAddr} = dgiot_task:get_pnque(CLient),
    DisQue = dgiot_task:get_instruct(NextProductId, Round),
    NewState = State#dclient{ userdata = UserData#device_task{product = NextProductId, devaddr = NextDevAddr, dique = DisQue}},
    send_msg(NewState).
