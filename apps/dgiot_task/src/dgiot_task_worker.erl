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
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(task, {mode = thing, tid, firstid, dtuid, product, devaddr, dis = [], que, round, ref, ack = #{}, appdata = #{}, ts = 0, freq = 0, interval = 5}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(State) ->
    dgiot_client:start_link(?MODULE, State).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"channel">> := ChannelId, <<"client">> := DtuId, <<"mode">> := Mode, <<"freq">> := Freq} = State]) ->
    case dgiot_task:get_pnque(DtuId) of
        not_find ->
            io:format("~s ~p State ~p ~n", [?FILE, ?LINE, State]),
            {stop, normal, State};
        {ProductId, DevAddr} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Que = dgiot_instruct:get_instruct(ProductId, DeviceId, 1, dgiot_utils:to_atom(Mode)),
%%          ChildQue = dgiot_instruct:get_child_instruct(DeviceId, 1, dgiot_utils:to_atom(Mode)),
            Nowstamp = dgiot_datetime:nowstamp(),
            erlang:send_after(1000, self(), init),
            Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/#">>,
            dgiot_mqtt:subscribe(Topic),
            dgiot_metrics:inc(dgiot_task, <<"task">>, 1),
            dgiot_client:save(ChannelId, DtuId),
            {ok, #task{mode = dgiot_utils:to_atom(Mode), dtuid = DtuId, product = ProductId, devaddr = DevAddr,
                tid = ChannelId, firstid = DeviceId, que = Que, round = 1, ts = Nowstamp, freq = Freq}}
    end;

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

handle_info(init, #task{dtuid = DtuId, mode = Mode, round = Round} = State) ->
%%    io:format("~s ~p DtuId = ~p.~n", [?FILE, ?LINE, DtuId]),
    case dgiot_task:get_pnque(DtuId) of
        not_find ->
            ?LOG(info, "DtuId ~p", [DtuId]),
            {noreply, State};
        {ProductId, DevAddr} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            NewRound = Round + 1,
            Que = dgiot_instruct:get_instruct(ProductId, DeviceId, NewRound, dgiot_utils:to_atom(Mode)),
            {noreply, State#task{product = ProductId, devaddr = DevAddr, round = NewRound, firstid = DeviceId, que = Que}}
    end;

%% 定时触发抄表指令
handle_info(retry, State) ->
    {noreply, send_msg(State)};

%% 任务结束
handle_info({deliver, _, Msg}, #task{tid = Channel, dis = Dis, product = _ProductId1, devaddr = _DevAddr1, ack = Ack, que = Que} = State) when length(Que) == 0 ->
    Payload = jsx:decode(dgiot_mqtt:get_payload(Msg), [return_maps]),
    case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"thing">>, ProductId, DevAddr, <<"properties">>, <<"report">>] ->
            dgiot_bridge:send_log(Channel, ProductId, DevAddr, "~s ~p  ~ts: ~ts ", [?FILE, ?LINE, unicode:characters_to_list(dgiot_mqtt:get_topic(Msg)), unicode:characters_to_list(dgiot_mqtt:get_payload(Msg))]),
            NewPayload =
                maps:fold(fun(K, V, Acc) ->
                    case dgiot_data:get({protocol, K, ProductId}) of
                        not_find ->
                            Acc#{K => V};
                        Identifier ->
                            Acc#{Identifier => V}
                    end
                          end, #{}, Payload),
            NewAck = dgiot_task:get_collection(ProductId, Dis, NewPayload, maps:merge(Ack, NewPayload)),
            dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1),
            {noreply, get_next_pn(State#task{ack = NewAck, product = ProductId, devaddr = DevAddr})};
        [<<"$dg">>, <<"thing">>, _ProductId, _DevAddr, <<"events">>] -> % todo
            {noreply, get_next_pn(State#task{ack = Ack})};
        _ ->
            {noreply, get_next_pn(State#task{ack = Ack})}
    end;


%% ACK消息触发抄表指令
handle_info({deliver, _, Msg}, #task{tid = Channel, dis = Dis, product = _ProductId1, devaddr = _DevAddr1, ack = Ack} = State) ->
    Payload = jsx:decode(dgiot_mqtt:get_payload(Msg), [return_maps]),
    dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1),
    case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"thing">>, ProductId, DevAddr, <<"properties">>, <<"report">>] ->
            dgiot_bridge:send_log(Channel, ProductId, DevAddr, "~s ~p  ~ts: ~ts ",
                [?FILE, ?LINE, unicode:characters_to_list(dgiot_mqtt:get_topic(Msg)), unicode:characters_to_list(dgiot_mqtt:get_payload(Msg))]),
            NewPayload =
                maps:fold(fun(K, V, Acc) ->
                    case dgiot_data:get({protocol, K, ProductId}) of
                        not_find ->
                            Acc#{K => V};
                        Identifier ->
                            Acc#{Identifier => V}
                    end
                          end, #{}, Payload),
            NewAck = dgiot_task:get_collection(ProductId, Dis, NewPayload, maps:merge(Ack, NewPayload)),
            {noreply, send_msg(State#task{ack = NewAck, product = ProductId, devaddr = DevAddr})};
        [<<"$dg">>, <<"thing">>, _ProductId, _DevAddr, <<"events">>] -> % todo
            {noreply, get_next_pn(State#task{ack = Ack})};
        _ ->
            {noreply, send_msg(State#task{ack = Ack})}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dgiot_metrics:dec(dgiot_task, <<"task">>, 1),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_msg(#task{ref = Ref, que = Que} = State) when length(Que) == 0 ->
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    get_next_pn(State);

send_msg(#task{tid = Channel, product = Product, devaddr = DevAddr, ref = Ref, que = Que} = State) ->
    {InstructOrder, Interval, _, _, _, Protocol, _, _} = lists:nth(1, Que),
    {NewCount, Payload, Dis} =
        lists:foldl(fun(X, {Count, Acc, Acc1}) ->
            case X of
                {InstructOrder, _, _, _, error, _, _, _} ->
                    {Count + 1, Acc, Acc1};
                {InstructOrder, _, Identifier1, _AccessMode, NewData, Protocol, DataSource, _} ->
                    Payload1 = DataSource#{<<"data">> => NewData},
                    Topic = <<"$dg/device/", Product/binary, "/", DevAddr/binary, "/properties/report">>,
                    dgiot_mqtt:publish(Channel, Topic, jsx:encode(Payload1)),
                    {Count + 1, Acc ++ [Payload1], Acc1 ++ [Identifier1]};
                _ ->
                    {Count, Acc, Acc1}
            end
                    end, {0, [], []}, Que),
    Newpayload = jsx:encode(Payload),
    Topic = <<"$dg/device/", Product/binary, "/", DevAddr/binary, "/properties/report">>,
    dgiot_bridge:send_log(Channel, Product, DevAddr, "to_dev=> ~s ~p ~ts: ~ts", [?FILE, ?LINE, unicode:characters_to_list(Topic), unicode:characters_to_list(Newpayload)]),
    %%  在超时期限内，回报文，就取消超时定时器
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    NewQue = lists:nthtail(NewCount, Que),
    dgiot_metrics:inc(dgiot_task, <<"task_send">>, 1),
    State#task{que = NewQue, dis = Dis, ref = erlang:send_after(Interval * 1000, self(), retry), interval = Interval}.

get_next_pn(#task{tid = _Channel, mode = Mode, dtuid = DtuId, firstid = DeviceId, product = _ProductId, devaddr = _DevAddr, round = Round, ref = Ref, interval = Interval} = State) ->
    save_td(State),
    {NextProductId, NextDevAddr} = dgiot_task:get_pnque(DtuId),
    NextDeviceId = dgiot_parse_id:get_deviceid(NextProductId, NextDevAddr),
    Que = dgiot_instruct:get_instruct(NextProductId, NextDeviceId, Round, Mode),
%%    dgiot_bridge:send_log(Channel, NextProductId, NextDevAddr, "to_dev=> ~s ~p NextProductId ~p NextDevAddr ~p NextDeviceId ~p", [?FILE, ?LINE, NextProductId, NextDevAddr, NextDeviceId]),
    NextTopic = <<"$dg/device/", NextProductId/binary, "/", NextDevAddr/binary, "/properties/report">>,
    dgiot_mqtt:subscribe(NextTopic),
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    timer:sleep(20),
    NewRef =
        case NextDeviceId of
            DeviceId ->
                erlang:send_after(1000, self(), init);
            _ ->
                erlang:send_after(Interval * 1000 - 20, self(), retry)
        end,
    State#task{product = NextProductId, devaddr = NextDevAddr, que = Que, dis = [], ack = #{}, ref = NewRef}.

save_td(#task{tid = Channel, product = ProductId, devaddr = DevAddr, ack = Ack, appdata = AppData}) ->
    Data = dgiot_task:save_td(ProductId, DevAddr, Ack, AppData),
    dgiot_bridge:send_log(Channel, ProductId, DevAddr, "save_td=> ~s ~p ProductId ~p DevAddr ~p : ~ts ", [?FILE, ?LINE, ProductId, DevAddr, unicode:characters_to_list(jsx:encode(Data))]).

