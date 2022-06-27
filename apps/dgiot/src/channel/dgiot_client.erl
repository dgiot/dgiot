%%--------------------------------------------------------------------
%% Copyright (c) 2021-2022 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_client).
-author("johnliu").
-include("dgiot.hrl").
-include("dgiot_client.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([register/3, unregister/1, start_link/2, add_clock/3, notify/3, add/2, set_consumer/2, get_consumer/1]).
-export([start/2, start/3, stop/1, stop/2, stop/3, restart/2, get/2, send/4, count/1]).
-export([get_time/1, get_nexttime/2, get_count/3, get_rand/1]).
-export([get_que/2, save_que/3, start_que/1, stop_que/1]).
-export([get_pnque_len/2, save_pnque/5, get_pnque/2, del_pnque/2, start_pnque/2, stop_pnque/2]).

-type(result() :: any()).   %% todo 目前只做参数检查，不做结果检查

%% @doc 注册client的通道管理池子
-spec register(atom() | binary(), atom(), map()) -> result().
register(ChannelId, Sup, State) when is_binary(ChannelId) ->
    register(binary_to_atom(ChannelId), Sup, State);
register(ChannelId, Sup, State) ->
    case dgiot_data:get({client, ChannelId}) of
        not_find ->
            dgiot_data:insert({client, ChannelId}, State#{<<"channel">> => ChannelId});
        _ ->
            pass
    end,
    set_consumer(ChannelId, 100),
    dgiot_data:init(ChannelId),
    dgiot_data:init(?DCLINET_QUE(ChannelId)),
    dgiot_data:init(?DCLINET_PNQUE(ChannelId)),
    dgiot_data:delete({start_client, ChannelId}),
    dgiot_data:delete({stop_client, ChannelId}),
    ChildSpec = dgiot:child_spec(Sup, supervisor, [ChannelId]),
    [ChildSpec].

unregister(ChannelId) when is_binary(ChannelId) ->
    dgiot_client:unregister(binary_to_atom(ChannelId));

unregister(ChannelId) ->
    dgiot_data:delete({client, ChannelId}),
    dgiot_data:delete(ChannelId),
    dgiot_data:delete({start_client, ChannelId}),
    dgiot_data:delete({stop_client, ChannelId}).

save_pnque(ChannelId, DtuProductId, DtuAddr, ProductId, DevAddr) ->
    DtuId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
    case dgiot_data:get(?DCLINET_PNQUE(ChannelId), DtuId) of
        not_find ->
            dgiot_data:insert(?DCLINET_PNQUE(ChannelId), DtuId, [{ProductId, DevAddr}]);
        Pn_que ->
            New_Pn_que = dgiot_utils:unique_2(Pn_que ++ [{ProductId, DevAddr}]),
            dgiot_data:insert(?DCLINET_PNQUE(ChannelId), DtuId, New_Pn_que)
    end.

get_pnque_len(ChannelId, DtuId) ->
    case dgiot_data:get(?DCLINET_PNQUE(ChannelId), DtuId) of
        not_find ->
            0;
        PnQue ->
            length(PnQue)
    end.

get_pnque(ChannelId, DtuId) ->
    case dgiot_data:get(?DCLINET_PNQUE(ChannelId), DtuId) of
        not_find ->
            not_find;
        PnQue when length(PnQue) > 0 ->
            Head = lists:nth(1, PnQue),
            dgiot_data:insert(?DCLINET_PNQUE(ChannelId), DtuId, lists:nthtail(1, PnQue) ++ [Head]),
            Head;
        _ ->
            not_find
    end.

del_pnque(ChannelId, DtuId) ->
    case dgiot_data:get(?DCLINET_PNQUE(ChannelId), DtuId) of
        not_find ->
            pass;
        PnQue when length(PnQue) > 0 ->
            dgiot_data:delete(?DCLINET_PNQUE(ChannelId), DtuId);
        _ ->
            pass
    end.

start_pnque(ChannelId, ClinetId) when is_atom(ChannelId) ->
    start_pnque(dgiot_utils:to_binary(ChannelId), ClinetId);
start_pnque(ChannelId, ClinetId) ->
    case dgiot_data:get(?DCLINET_PNQUE(ChannelId), ClinetId) of
        not_find ->
            not_find;
        PnQue ->
            lists:map(
                fun
                    ({ProductId, DevAddr}) ->
                        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
                        %% io:format("~s ~p ChannelId ~p, Type ~p , ClinetId ~p ~n",[?FILE, ?LINE, ChannelId, Type, ClinetId]),
                        dgiot_client:start(<<ProductId/binary, "_", ChannelId/binary>>, DeviceId,
                            #{<<"child">> => #{<<"productid">> => ProductId, <<"devaddr">> => DevAddr, <<"dtuid">> => ClinetId}});
                    (_) ->
                        pass
                end, PnQue)
    end.

stop_pnque(ChannelId, ClinetId) when is_atom(ChannelId) ->
    stop_pnque(dgiot_utils:to_binary(ChannelId), ClinetId);
stop_pnque(ChannelId, ClinetId) ->
    case dgiot_data:get(?DCLINET_PNQUE(ChannelId), ClinetId) of
        not_find ->
            not_find;
        PnQue ->
            lists:map(
                fun
                    ({ProductId, DevAddr}) ->
                        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
                        %% io:format("~s ~p ChannelId ~p, Type ~p , ClinetId ~p ~n",[?FILE, ?LINE, ChannelId, Type, ClinetId]),
                        dgiot_client:stop(<<ProductId/binary, "_", ChannelId/binary>>, DeviceId,
                            #{<<"child">> => #{<<"productid">> => ProductId, <<"devaddr">> => DevAddr, <<"dtuid">> => ClinetId}});
                    (_) ->
                        pass
                end, PnQue),
            del_pnque(ChannelId, ClinetId)
    end.

save_que(ChannelId, ProductId, DevAddr) ->
    DtuId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    dgiot_data:insert(?DCLINET_QUE(ChannelId), DtuId, {ProductId, DevAddr}).

get_que(ChannelId, DeviceId) when is_atom(ChannelId) ->
    get_que(atom_to_binary(ChannelId), DeviceId);
get_que(ChannelId, DeviceId) ->
    dgiot_data:get(?DCLINET_QUE(ChannelId), DeviceId).

start_que(ChannelId) ->
    Fun = fun
              ({Key, _Value}) ->
                  dgiot_client:start(ChannelId, Key);
              (_) ->
                  pass
          end,
    dgiot_data:loop(?DCLINET_QUE(ChannelId), Fun).

stop_que(ChannelId) ->
    Fun = fun
              ({ClientId, _Value}) ->
                  dgiot_client:stop_pnque(ChannelId, ClientId),
                  dgiot_client:stop(ChannelId, ClientId);
              (_) ->
                  pass
          end,
    dgiot_data:loop(?DCLINET_QUE(ChannelId), Fun),
    timer:sleep(3000),
    dgiot_data:destroy(?DCLINET_QUE(ChannelId)).

%% @doc 在通道管理池子中增加client的Pid号
-spec add(atom() | binary(), binary()) -> result().
add(ChannelId, ClientId) when is_binary(ChannelId) ->
    add(binary_to_atom(ChannelId), ClientId);
add(ChannelId, ClientId) ->
    dgiot_data:insert(ChannelId, ClientId, self()).

%% @doc 启动client
-spec start(atom() | binary(), binary()) -> result().
start(ChannelId, ClientId) when is_binary(ChannelId) ->
    start(binary_to_atom(ChannelId), ClientId);
start(ChannelId, ClientId) ->
    start(ChannelId, ClientId, #{}).

%% @doc 启动client, 自定义启动参数
-spec start(atom() | binary(), binary(), map()) -> result().
start(ChannelId, ClientId, Args) when is_binary(ChannelId) ->
    start(binary_to_atom(ChannelId), ClientId, Args);
start(ChannelId, ClientId, Args) ->
    case dgiot_data:get({client, ChannelId}) of
        State when is_map(State) ->
            supervisor:start_child(ChannelId, [maps:merge(State#{<<"client">> => ClientId}, Args)]);
        _ ->
            pass
    end.

%% @doc 停止通道下所有的client
-spec stop(atom() | binary()) -> result().
stop(ChannelId) when is_binary(ChannelId) ->
    stop(binary_to_atom(ChannelId));
stop(ChannelId) ->
    case ets:info(ChannelId) of
        undefined ->
            pass;
        _ ->
            Fun =
                fun
                    ({_Key, Pid}) when is_pid(Pid) ->
                        supervisor:terminate_child(ChannelId, Pid);
                    (_) ->
                        pass
                end,
            dgiot_data:loop(ChannelId, Fun),
            dgiot_data:clear(ChannelId),
            dgiot_client:unregister(ChannelId)
    end.

%% @doc stop client
-spec stop(atom() | binary(), binary()) -> result().
stop(ChannelId, ClientId) when is_binary(ChannelId) ->
    stop(binary_to_atom(ChannelId), ClientId);
stop(ChannelId, ClientId) ->
    case dgiot_data:get(ChannelId, ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
%%                    io:format("~s ~p DtuId = ~p. Pid ~p ~n", [?FILE, ?LINE, ChannelId, Pid]),
                    supervisor:terminate_child(ChannelId, Pid);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

%% @doc stop client
-spec stop(atom() | binary(), binary(), non_neg_integer()) -> result().
stop(ChannelId, ClientId, Count) when is_binary(ChannelId) ->
    stop(binary_to_atom(ChannelId), ClientId, Count);
stop(ChannelId, ClientId, Count) ->
    case Count =< 0 of
        true ->
            stop(ChannelId, ClientId);
        _ ->
            pass
    end.

%% @doc restart client
-spec restart(atom() | binary(), binary()) -> result().
restart(ChannelId, ClientId) when is_binary(ChannelId) ->
    restart(binary_to_atom(ChannelId), ClientId);
restart(ChannelId, ClientId) ->
    case dgiot_data:get(ChannelId, ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    pass;
                false ->
                    stop(ChannelId, ClientId),
                    start(ChannelId, ClientId)
            end;
        _ ->
            start(ChannelId, ClientId)
    end.

%% @doc get client info
-spec get(atom() | binary(), binary()) -> result().
get(ChannelId, ClientId) when is_binary(ChannelId) ->
    get(binary_to_atom(ChannelId), ClientId);
get(ChannelId, ClientId) ->
    case dgiot_data:get(ChannelId, ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    offline
            end;
        _ ->
            offline
    end.

%% @doc send message to client
-spec send(atom() | binary(), binary(), binary(), binary() | map()) -> result().
send(ChannelId, ClientId, Topic, Payload) when is_binary(ChannelId) ->
    send(binary_to_atom(ChannelId), ClientId, Topic, Payload);
send(ChannelId, ClientId, Topic, Payload) ->
    case dgiot_data:get(ChannelId, ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pid ! {dclient_ack, Topic, Payload},
                    ok;
                false ->
                    fasle
            end;
        _ ->
            fasle
    end.

%% @doc client start_link
-spec start_link(atom(), map()) -> result().
start_link(Module, #{<<"channel">> := ChannelId, <<"client">> := Client} = State) ->
    case dgiot_data:lookup(dgiot_utils:to_atom(ChannelId), Client) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    gen_server:start_link(Module, [State], [])
            end;
        _Reason ->
            gen_server:start_link(Module, [State], [])
    end.

%% @doc 该通道下客户端数量
-spec count(atom() | binary()) -> result().
count(ChannelId) when is_binary(ChannelId) ->
    count(binary_to_atom(ChannelId));
count(ChannelId) ->
    case ets:info(ChannelId) of
        undefined ->
            0;
        Info ->
            proplists:get_value(size, Info)
    end.

%% @doc 做一下全局的错峰处理
-spec get_rand(non_neg_integer()) -> non_neg_integer().
get_rand(Freq) ->
    Seed = Freq * 1000, %  默认用采样周期的20%的时间来做随机
    rand:uniform(Seed) div 1000.

%% @doc 获取闹铃执行次数
-spec get_count(integer(), integer(), integer()) -> result().
get_count(StartTime, EndTime, _Freq) when EndTime =< StartTime ->
    0;
get_count(_StartTime, _EndTime, Freq) when Freq =< 0 ->
    0;
get_count(StartTime, EndTime, Freq) ->
    (EndTime - StartTime) div Freq.

get_time(Time) when is_integer(Time) ->
    Time;
get_time(Time) ->
    dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(Time)).

get_nexttime(NextTime, Freq) ->
    NowTime = dgiot_datetime:nowstamp(),
    get_nexttime(NowTime, Freq, NextTime).

get_nexttime(NowTime, Freq, NextTime) when (NextTime =< NowTime) ->
    RetryTime = (NowTime - NextTime) rem Freq,
    erlang:send_after(RetryTime * 1000, self(), next_time),
    NowTime + RetryTime + Freq;

get_nexttime(NowTime, Freq, NextTime) ->
    RetryTime = NextTime - NowTime,
    erlang:send_after(RetryTime * 1000, self(), next_time),
    NextTime + Freq.

%% @doc 设置消费组大小
-spec set_consumer(binary() | atom(), integer()) -> result().
set_consumer(ChannelId, PoolSize) when is_binary(ChannelId) ->
    set_consumer(binary_to_atom(ChannelId), PoolSize);
set_consumer(ChannelId, PoolSize) ->
    dgiot_data:set_consumer(ChannelId, PoolSize).

%% @doc 获取消费组值
-spec get_consumer(binary() | atom()) -> result().
get_consumer(ChannelId) when is_binary(ChannelId) ->
    get_consumer(binary_to_atom(ChannelId));
get_consumer(ChannelId) ->
    dgiot_data:get_consumer(ChannelId, 1).

%% 定时检查启动, 10s
%% @doc 添加闹铃
-spec add_clock(binary() | atom(), binary() | integer(), binary() | integer()) -> result().
add_clock(Channel, Start_time, End_time) when is_binary(Channel) ->
    add_clock(dgiot_utils:to_atom(Channel), Start_time, End_time);
add_clock(Channel, Start_time, End_time) when is_binary(Start_time) ->
    add_clock(Channel, dgiot_datetime:to_localtime(Start_time), dgiot_datetime:to_localtime(End_time));
add_clock(Channel, Start_time, End_time) when is_integer(Start_time) ->
    add_clock(Channel, dgiot_datetime:unixtime_to_localtime(Start_time), dgiot_datetime:unixtime_to_localtime(End_time));
add_clock(Channel, Start_time, End_time) ->
    BinChannel = dgiot_utils:to_binary(Channel),
    dgiot_cron:push(BinChannel, Start_time, {?MODULE, notify, [Channel, start_client]}),
    dgiot_cron:push(<<BinChannel/binary, "_stop">>, End_time, {?MODULE, notify, [Channel, stop_client]}).

%% 定时检查启动, 10s
%% @doc 闹铃通知回调函数
-spec notify(any(), binary() | atom(), atom()) -> result().
notify(_Task, Channel, Type) when is_binary(Channel) ->
    notify(_Task, binary_to_atom(Channel), Type);
notify(_Task, Channel, Type) ->
    dgiot_channelx:do_message(atom_to_binary(Channel), Type),
    timer:sleep(50),
    dgiot_data:insert({Type, Channel}, Type).

