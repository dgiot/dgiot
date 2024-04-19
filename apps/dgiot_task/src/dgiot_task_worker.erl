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
    ref = undefined,
    pnque_len = 0 :: integer(),                              %% 本轮任务剩余的设备队列数
    product :: binary()|atom(),                              %% 当前任务网关设备或者网关子设备的产品ID
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
init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"starttime">> := StartTime, <<"endtime">> := EndTime, <<"freq">> := Freq} = Args]) ->
    dgiot_client:add(ChannelId, ClientId),
    dgiot_metrics:inc(dgiot_task, <<"task">>, 1),
    NextTime = dgiot_client:get_nexttime(StartTime, Freq),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    Rand =
        case maps:get(<<"rand">>, Args, true) of
            true ->
                dgiot_client:get_rand(Freq);
            _ ->
                0
        end,
%%    io:format("~s ~p ChannelId ~p ClientId ~p  NextTime = ~p  Freq ~p Count = ~p.~n", [?FILE, ?LINE, ChannelId, ClientId, NextTime, Freq, Count]),
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, userdata = #device_task{},
        clock = #dclock{nexttime = NextTime + Rand, freq = Freq, count = Count, round = 0}},
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

handle_info(stop, #dclient{channel = Channel, client = Client} = Dclient) ->
    dgiot_client:stop(Channel, Client),
    erlang:garbage_collect(self()),
    {stop, normal, Dclient};

%% 动态修改任务启动时间和周期
handle_info({change_clock, NextTime, EndTime, Freq}, #dclient{clock = Clock} = Dclient) ->
    {noreply, Dclient#dclient{clock = Clock#dclock{nexttime = NextTime, count = dgiot_client:get_count(NextTime, EndTime, Freq), freq = Freq}}};

%% 定时触发网关及网关任务, 在单个任务轮次中，要将任务在全局上做一下错峰操作
handle_info(next_time, #dclient{channel = Channel, client = Client, userdata = UserData,
    clock = #dclock{round = Round, nexttime = NextTime, count = Count, freq = Freq} = Clock} = Dclient) ->
    dgiot_client:stop(Channel, Client, Count), %% 检查是否需要停止任务
    NewNextTime = dgiot_client:get_nexttime(NextTime, Freq),
    case dgiot_task:get_pnque(Client) of
        not_find ->
            {noreply, Dclient#dclient{clock = Clock#dclock{nexttime = NewNextTime, count = Count - 1}}};
        {ProductId, DevAddr} ->
            NewRound = Round + 1,
            DiQue = dgiot_task:get_instruct(ProductId, NewRound),
            PnQueLen = dgiot_task:get_pnque_len(Client),
%%            io:format("~s ~p DiQue = ~p.~n", [?FILE, ?LINE, DiQue]),
            erlang:send_after(100, self(), read), % 每轮任务开始时，做一下随机开始
            {noreply, Dclient#dclient{userdata = UserData#device_task{product = ProductId, devaddr = DevAddr, pnque_len = PnQueLen, dique = DiQue},
                clock = Clock#dclock{nexttime = NewNextTime, count = Count - 1, round = NewRound}}}
    end;

%% 发送指令集
handle_info(read, State) ->
    {noreply, send_msg(State)};

%% ACK消息触发进行新的指令发送
handle_info({dclient_ack, Topic, Payload}, #dclient{channel = _ChannelId, userdata = Usedata} = State) ->
    dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"thing">>, ProductId, DevAddr, <<"properties">>, <<"report">>] ->
%%            dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), ProductId, DevAddr, "~s ~p recv => ~p ~ts ", [?FILE, ?LINE, Topic, unicode:characters_to_list(dgiot_json:encode(Payload))]),
            dgiot_task:save_td(ProductId, DevAddr, Payload, #{}),
            {noreply, send_msg(State#dclient{userdata = Usedata#device_task{product = ProductId, devaddr = DevAddr}})};
        _ ->
            io:format("~s ~p Topic = ~p.~n", [?FILE, ?LINE, Topic]),
            {noreply, send_msg(State)}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #dclient{channel = ChannelId, client = ClientId} = _State) ->
    dgiot_client:stop(ChannelId, ClientId),
    dgiot_metrics:dec(dgiot_task, <<"task">>, 1),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 开始采集下一个子设备的指令集
send_msg(#dclient{userdata = #device_task{dique = DisQue, pnque_len = PnQueLen} = UserData} = State) when length(DisQue) == 0 ->
    get_next_pn(State#dclient{userdata = UserData#device_task{pnque_len = PnQueLen - 1}});

%% 发送指令集
send_msg(#dclient{channel = ChannelId, clock = #dclock{freq = Freq}, userdata = #device_task{ref = Ref, product = Product, devaddr = DevAddr, dique = DisQue} = UserData} = State) ->
    {InstructOrder, Interval, _Identifier, _NewDataSource} = lists:nth(1, DisQue),
    {NewCount, _Payload, _Dis} =
        lists:foldl(fun(X, {Count, Acc, Acc1}) ->
            case X of
                {InstructOrder, _, Identifier1, DataSource} ->
                    Topic = <<"$dg/device/", Product/binary, "/", DevAddr/binary, "/properties">>,
                    Payload = dgiot_json:encode(DataSource#{<<"identifier">> => Identifier1, <<"_dgiotTaskFreq">> => Freq}),
%%                  io:format("~s ~p DataSource = ~p.~n", [?FILE, ?LINE, DataSource]),
                    dgiot_mqtt:publish(dgiot_utils:to_binary(ChannelId), Topic, Payload),
%%                    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), Product, DevAddr, "~s ~p to dev => ~ts: ~ts", [?FILE, ?LINE, unicode:characters_to_list(Topic), unicode:characters_to_list(Payload)]),
                    {Count + 1, Acc ++ [DataSource], Acc1 ++ [Identifier1]};
                _ ->
                    {Count, Acc, Acc1}
            end
                    end, {0, [], []}, DisQue),
    %%  在超时期限内，回报文，就取消超时定时器
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    NewDisQue = lists:nthtail(NewCount, DisQue),
    dgiot_metrics:inc(dgiot_task, <<"task_send">>, 1),
    State#dclient{userdata = UserData#device_task{ref = erlang:send_after(Interval * 1000, self(), read), dique = NewDisQue, interval = Interval}}.

%% 本轮任务结束
get_next_pn(#dclient{channel = ChannelId, clock = #dclock{round = Round}, userdata = #device_task{product = Product, devaddr = DevAddr, pnque_len = PnQueLen}} = State) when PnQueLen < 1 ->
    case PnQueLen of
        0 ->
            dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), Product, DevAddr, "~s ~p time: ~p, round: ~p end ~n", [?FILE, ?LINE, dgiot_datetime:format(dgiot_datetime:now_secs(), <<"YY-MM-DD HH:NN:SS">>), Round]);
        _ ->
            pass
    end,
    State;

get_next_pn(#dclient{client = CLient, clock = #dclock{round = Round}, userdata = #device_task{product = ProductId, devaddr = DevAddr} = UserData} = State) ->
    case dgiot_task:get_pnque(CLient) of
        not_find ->
            State;
        {ProductId, DevAddr} ->
            State;
        {NextProductId, NextDevAddr} ->
            DisQue = dgiot_task:get_instruct(NextProductId, Round),
            NewState = State#dclient{userdata = UserData#device_task{product = NextProductId, devaddr = NextDevAddr, dique = DisQue}},
            send_msg(NewState)
    end.

