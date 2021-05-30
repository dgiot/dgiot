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

-record(task, {mode = thing, tid, app, firstid, dtuid, product, devaddr, di, que, round, ref, ack = #{}, appdata = #{}}).
%%%===================================================================
%%% API
%%%===================================================================
start_link(#{<<"channel">> := ChannelId, <<"dtuid">> := DtuId} = State) ->
    case dgiot_data:lookup(?DGIOT_TASK, {ChannelId, DtuId}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    case dgiot_task:get_pnque(DtuId) of
        not_find ->
            {ok, DtuId};
        _ ->
            gen_server:start_link(?MODULE, [State], [])
    end.

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
init([#{<<"app">> := App, <<"channel">> := ChannelId, <<"dtuid">> := DtuId, <<"mode">> := Mode} = _Args]) ->
    dgiot_data:insert(?DGIOT_TASK, {ChannelId, DtuId}, self()),
    Round = dgiot_data:get_consumer(<<"taskround/", ChannelId/binary, "/", DtuId/binary>>, 1),

    {ProductId, DevAddr} = dgiot_task:get_pnque(DtuId),

    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),

%%    ?LOG(info,"dgiot_instruct:get_instruct( ~p , ~p , ~p , ~p", [ProductId, DeviceId, Round, Mode]),
    Que = dgiot_instruct:get_instruct(ProductId, DeviceId, Round, dgiot_utils:to_atom(Mode)),
%%    ?LOG(info,"Que ~p", [Que]),

    case length(Que) of
        0 ->
            erlang:send_after(300, self(), stop);
        _ ->
            erlang:send_after(1000, self(), delay)
    end,

    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
    dgiot_mqtt:subscribe(Topic),

    AppData = maps:get(<<"appdata">>, _Args, #{}),

    {ok, #task{mode = dgiot_utils:to_atom(Mode), app = App, dtuid = DtuId, product = ProductId, devaddr = DevAddr,
        tid = ChannelId, firstid = DeviceId, que = Que, round = Round, appdata = AppData}};

init(A) ->
    ?LOG(info,"A ~p ", [A]).

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

%% 延迟启动
handle_info(delay, State) ->
    erlang:send_after(100, self(), retry),
    {noreply, State};

%% 任务结束
handle_info(retry, #task{que = Que} = State) when length(Que) == 0 ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

%% 定时触发抄表指令
handle_info(retry, State) ->
    {noreply, send_msg(State)};

%% 任务结束
handle_info({deliver, _, Msg}, #task{di = Identifier, product = ProductId, ack = Ack, que = Que} = State) when length(Que) == 0 ->
    Payload = jsx:decode(dgiot_mqtt:get_payload(Msg), [return_maps]),
    NewAck = dgiot_task:get_collection(ProductId, Identifier, Payload, Ack),
    {noreply, get_next_pn(State#task{ack = NewAck})};

%% ACK消息触发抄表指令
handle_info({deliver, _, Msg}, #task{di = Identifier, product = ProductId, ack = Ack} = State) ->
    Payload = jsx:decode(dgiot_mqtt:get_payload(Msg), [return_maps]),
    NewAck = dgiot_task:get_collection(ProductId, Identifier, Payload, Ack),
    {noreply, send_msg(State#task{ack = NewAck})};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_msg(#task{tid = Channel, product = Product, devaddr = DevAddr, ref = Ref, que = Que, appdata = AppData} = State) ->
    {_InstructOrder, Interval, Identifier, Pn, Address, Command, Data, Protocol, _} = lists:nth(1, Que),
    Payload = jsx:encode(#{
        <<"appdata">> => AppData,
        <<"thingdata">> => #{
            <<"product">> => Product,
            <<"devaddr">> => DevAddr,
            <<"pn">> => Pn,
            <<"di">> => Address,
            <<"command">> => Command,
            <<"data">> => Data,
            <<"protocol">> => Protocol
        }
    }),
    Topic = <<"thing/", Product/binary, "/", DevAddr/binary>>,
    dgiot_bridge:send_log(Channel, "~s ~p to_dev=> ~ts: ~ts ", [?FILE, ?LINE, unicode:characters_to_list(Topic), unicode:characters_to_list(Payload)]),
    dgiot_mqtt:publish(Channel, Topic, Payload),
    NewQue = lists:nthtail(1, Que),
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    State#task{que = NewQue, di = Identifier, ref = erlang:send_after(Interval * 1000, self(), retry)}.

get_next_pn(#task{mode = Mode, dtuid = DtuId, firstid = DeviceId, product = ProductId, devaddr = DevAddr, round = Round, ref = Ref} = State) ->
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
    save_td(State),
    timer:sleep(200),
    NewRef =
        case NextDeviceId of
            DeviceId ->
                erlang:send_after(1000, self(), stop);
            _ ->
                erlang:send_after(1000, self(), retry)
        end,
    State#task{product = NextProductId, devaddr = NextDevAddr, que = Que, ack = #{}, ref = NewRef}.

save_td(#task{app = App, tid = Channel, product = ProductId, devaddr = DevAddr, ack = Ack, appdata = AppData}) ->
    Data = dgiot_task:get_calculated(ProductId, Ack),
    case length(maps:to_list(Data)) of
        0 -> pass;
        _ ->
            Payload = jsx:encode(#{<<"thingdata">> => Data, <<"appdata">> => AppData}),
            Topic = <<App/binary, "/", ProductId/binary, "/", DevAddr/binary>>,
            dgiot_mqtt:publish(ProductId, Topic, Payload),
            dgiot_tdengine_adapter:save(ProductId, DevAddr, Data),
            dgiot_bridge:send_log(Channel, "from_dev=> ~ts: ~ts ", [unicode:characters_to_list(Topic), unicode:characters_to_list(jsx:encode(Data))])
    end.
