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

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3, stop/1]).

-record(task, {mode = thing, tid, app, firstid, dtuid, product, devaddr, dis, que, round, ref, ack = #{}, appdata = #{}, ts = 0, endtime = 0, freq = 0}).
%%%===================================================================
%%% API
%%%===================================================================
start_link(#{<<"channel">> := ChannelId, <<"dtuid">> := DtuId} = State) ->
    case dgiot_data:lookup(?DGIOT_TASK, {ChannelId, DtuId}) of
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
    ?LOG(error, "State ~p", [State]),
    ok.

stop(#{<<"channel">> := Channel, <<"dtuid">> := DtuId}) ->
    case dgiot_data:lookup(?DGIOT_TASK, {Channel, DtuId}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"app">> := App, <<"channel">> := ChannelId, <<"dtuid">> := DtuId, <<"mode">> := Mode, <<"freq">> := Freq, <<"end_time">> := Endtime} = _Args]) ->
    dgiot_data:insert(?DGIOT_TASK, {ChannelId, DtuId}, self()),
%%    Round = dgiot_data:get_consumer(<<"taskround/", ChannelId/binary, "/", DtuId/binary>>, 1),
    {ProductId, DevAddr} = dgiot_task:get_pnque(DtuId),
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    Que = dgiot_instruct:get_instruct(ProductId, DeviceId, 1, dgiot_utils:to_atom(Mode)),
%%    ?LOG(info, "Que ~p", [Que]),
    Tsendtime = dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(Endtime)),
    Nowstamp = dgiot_datetime:nowstamp(),
    case length(Que) of
        0 ->
            erlang:send_after(300, self(), stop);
        _ ->
            case Tsendtime > Nowstamp of
                true ->
                    erlang:send_after(1000, self(), retry);
                false ->
                    erlang:send_after(300, self(), stop)
            end
    end,
    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
    dgiot_mqtt:subscribe(Topic),
    AppData = maps:get(<<"appdata">>, _Args, #{}),
    {ok, #task{mode = dgiot_utils:to_atom(Mode), app = App, dtuid = DtuId, product = ProductId, devaddr = DevAddr,
        tid = ChannelId, firstid = DeviceId, que = Que, round = 1, appdata = AppData, ts = Nowstamp, freq = Freq, endtime = Tsendtime}};

init(A) ->
    ?LOG(error, "A ~p ", [A]).

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

handle_info(init, #task{dtuid = DtuId, mode = Mode, round = Round, ts = Oldstamp, freq = Freq, endtime = Tsendtime} = State) ->
    dgiot_datetime:now_secs(),
    {ProductId, DevAddr} = dgiot_task:get_pnque(DtuId),
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    NewRound = Round + 1,
    Que = dgiot_instruct:get_instruct(ProductId, DeviceId, NewRound, dgiot_utils:to_atom(Mode)),
    Nowstamp = dgiot_datetime:nowstamp(),
    Newfreq = Nowstamp - Oldstamp,
    case length(Que) of
        0 ->
            erlang:send_after(300, self(), stop);
        _ ->
            case Tsendtime > Nowstamp of
                true ->
                    case Newfreq > Freq of
                        true ->
                            erlang:send_after(1000, self(), retry);
                        false ->
                            erlang:send_after((Freq - Newfreq) * 1000, self(), retry)
                    end;
                false ->
                    erlang:send_after(300, self(), stop)
            end
    end,
    {noreply, State#task{product = ProductId, devaddr = DevAddr, round = NewRound, firstid = DeviceId, que = Que, ts = Nowstamp}};

%% 定时触发抄表指令
handle_info(retry, State) ->
    {noreply, send_msg(State)};

%% 任务结束
handle_info({deliver, _, Msg}, #task{tid = Channel, dis = Dis, product = ProductId, ack = Ack, que = Que} = State) when length(Que) == 0 ->
    Payload = jsx:decode(dgiot_mqtt:get_payload(Msg), [return_maps]),
    dgiot_bridge:send_log(Channel, "to_dev=> ~ts: ~ts ~s ~p ", [unicode:characters_to_list(dgiot_mqtt:get_topic(Msg)), unicode:characters_to_list(dgiot_mqtt:get_payload(Msg)), ?FILE, ?LINE]),
    NewAck = dgiot_task:get_collection(ProductId, Dis, Payload, Ack),
%%    ?LOG(info, "NewAck ~p", [NewAck]),
    {noreply, get_next_pn(State#task{ack = NewAck})};

%% ACK消息触发抄表指令
handle_info({deliver, _, Msg}, #task{tid = Channel, dis = Dis, product = ProductId, ack = Ack} = State) ->
    Payload = jsx:decode(dgiot_mqtt:get_payload(Msg), [return_maps]),
    dgiot_bridge:send_log(Channel, "to_dev=> ~ts: ~ts ~s ~p ", [unicode:characters_to_list(dgiot_mqtt:get_topic(Msg)), unicode:characters_to_list(dgiot_mqtt:get_payload(Msg)), ?FILE, ?LINE]),
    NewAck = dgiot_task:get_collection(ProductId, Dis, Payload, Ack),
%%    ?LOG(info, "NewAck ~p", [NewAck]),
    {noreply, send_msg(State#task{ack = NewAck})};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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

send_msg(#task{tid = Channel, product = Product, devaddr = DevAddr, ref = Ref, que = Que, appdata = AppData} = State) ->
    {InstructOrder, Interval, _, _, _, _, _, Protocol, _} = lists:nth(1, Que),
    {NewCount, Payload, Dis} =
        lists:foldl(fun(X, {Count, Acc, Acc1}) ->
            case X of
                {InstructOrder, _, _, _, _, _, error, _, _} ->
                    {Count + 1, Acc, Acc1};
                {InstructOrder, _, Identifier1, Pn1, Address1, Command1, Data1, Protocol, _} ->
                    Payload1 = #{
                        <<"appdata">> => AppData,
                        <<"thingdata">> => #{
                            <<"product">> => Product,
                            <<"devaddr">> => DevAddr,
                            <<"pn">> => Pn1,
                            <<"di">> => Address1,
                            <<"command">> => Command1,
                            <<"data">> => Data1,
                            <<"protocol">> => Protocol
                        }
                    },
                    {Count + 1, Acc ++ [Payload1], Acc1 ++ [Identifier1]};
                _ ->
                    {Count, Acc, Acc1}
            end
                    end, {0, [], []}, Que),
    Newpayload = jsx:encode(Payload),
    Topic = <<"thing/", Product/binary, "/", DevAddr/binary>>,
    dgiot_bridge:send_log(Channel, "to_dev=> ~ts: ~ts ~s ~p ", [unicode:characters_to_list(Topic), unicode:characters_to_list(Newpayload), ?FILE, ?LINE]),
    dgiot_mqtt:publish(Channel, Topic, Newpayload),
%%    在超时期限内，回报文，就取消超时定时器
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    NewQue = lists:nthtail(NewCount, Que),
    State#task{que = NewQue, dis = Dis, ref = erlang:send_after(Interval * 1000, self(), retry)}.


get_next_pn(#task{mode = Mode, dtuid = DtuId, firstid = DeviceId, product = ProductId, devaddr = DevAddr, round = Round, ref = Ref} = State) ->
    save_td(State),
    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
    dgiot_mqtt:unsubscribe(Topic),
    {NextProductId, NextDevAddr} = dgiot_task:get_pnque(DtuId),
    NextDeviceId = dgiot_parse:get_deviceid(NextProductId, NextDevAddr),
    Que = dgiot_instruct:get_instruct(NextProductId, DeviceId, Round, Mode),
    NextTopic = <<"thing/", NextProductId/binary, "/", NextDevAddr/binary, "/post">>,
    dgiot_mqtt:subscribe(NextTopic),
    case Ref of
        undefined -> pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    timer:sleep(200),
    NewRef =
        case NextDeviceId of
            DeviceId ->
                erlang:send_after(1000, self(), init);
            _ ->
                erlang:send_after(2 * 1000, self(), retry)
        end,
    State#task{product = NextProductId, devaddr = NextDevAddr, que = Que, ack = #{}, ref = NewRef}.

save_td(#task{app = _App, tid = Channel, product = ProductId, devaddr = DevAddr, ack = Ack, appdata = AppData}) ->
    case length(maps:to_list(Ack)) of
        0 -> pass;
        _ ->
            Data = dgiot_task:get_calculated(ProductId, Ack),
            ?LOG(info,"ProductId ~p",[ProductId]),
            ?LOG(info,"Ack ~p ~t",[Ack]),
            ?LOG(info,"Data ~p",[Data]),
            case length(maps:to_list(Data)) of
                0 -> pass;
                _ ->
                    case lists:member(error, maps:values(Data)) of
                        false ->
                            Payload = jsx:encode(#{<<"thingdata">> => Data, <<"appdata">> => AppData}),
                            Topic = <<"topo/", ProductId/binary, "/", DevAddr/binary, "/post">>,
                            dgiot_mqtt:publish(DevAddr, Topic, Payload),
                            dgiot_tdengine_adapter:save(ProductId, DevAddr, Data),
                            NotificationTopic = <<"notification/", ProductId/binary, "/", DevAddr/binary, "/post">>,
                            dgiot_mqtt:publish(DevAddr, NotificationTopic, jsx:encode(Data)),
                            dgiot_bridge:send_log(Channel, "from_Notification=> ~ts: ~ts ", [unicode:characters_to_list(NotificationTopic), unicode:characters_to_list(jsx:encode(Data))]);
                        true ->
                            pass
                    end
            end
    end.

