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

-module(dgiot_client).
-author("kenneth").
-include("dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([register/3, start_link/2, start/2, stop/1, stop/2, get/2, restart/2, save/2, notify/3, set_consumer/2, get_consumer/1]).

-export([add_clock/3]).

register(ChannelId, Sup, State) ->
    case dgiot_data:get({client, ChannelId}) of
        not_find ->
            dgiot_data:insert({client, ChannelId}, State#{<<"channel">> => ChannelId});
        _ ->
            pass
    end,
    set_consumer(ChannelId, 100),
    dgiot_data:init(?DGIOT_CLIENT(ChannelId)),
    dgiot_data:delete({start_client, ChannelId}),
    dgiot_data:delete({stop_client, ChannelId}),
    ChildSpec = dgiot:child_spec(Sup, supervisor, [?DGIOT_CLIENT(ChannelId)]),
    [ChildSpec].

save(ChannelId, ClientId) ->
    dgiot_data:insert(?DGIOT_CLIENT(ChannelId), ClientId, self()).

start(ChannelId, ClientId) ->
    case dgiot_data:get({client, ChannelId}) of
        State when is_map(State) ->
            supervisor:start_child(?DGIOT_CLIENT(ChannelId), [State#{<<"client">> => ClientId}]);
        _ ->
            pass
    end.

stop(ChannelId, ClientId) ->
    case dgiot_data:get(?DGIOT_CLIENT(ChannelId), ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    supervisor:terminate_child(?DGIOT_CLIENT(ChannelId), Pid);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

%% 停止通道下所有的client
stop(ChannelId) ->
    case ets:info(?DGIOT_CLIENT(ChannelId)) of
        undefined ->
            pass;
        _ ->
            Fun =
                fun
                    ({_Key, Pid}) when is_pid(Pid) ->
                        supervisor:terminate_child(?DGIOT_CLIENT(ChannelId), Pid);
                    (_) ->
                        pass
                end,
            dgiot_data:loop(?DGIOT_CLIENT(ChannelId), Fun),
            dgiot_data:clear(?DGIOT_CLIENT(ChannelId))
    end.

restart(ChannelId, ClientId) ->
    case dgiot_data:get(?DGIOT_CLIENT(ChannelId), ClientId) of
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

get(ChannelId, ClientId) ->
    case dgiot_data:get(?DGIOT_CLIENT(ChannelId), ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    online;
                false ->
                    offline
            end;
        _ ->
            offline
    end.

start_link(Module, #{<<"channel">> := ChannelId, <<"client">> := Client} = State) ->
    case dgiot_data:lookup(?DGIOT_CLIENT(ChannelId), Client) of
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

set_consumer(ChannelId, PoolSize) ->
    dgiot_data:set_consumer(?DGIOT_CLIENT(ChannelId), PoolSize).

get_consumer(ChannelId) ->
    dgiot_data:get_consumer(?DGIOT_CLIENT(ChannelId), 1).

%% 定时检查启动, 10s 检查一次
add_clock(Channel, Start_time, End_time) ->
    io:format("Channel ~p, Start_time ~p, End_time ~p",[Channel, Start_time, End_time]),
    dgiot_cron:push(Channel, dgiot_datetime:to_localtime(Start_time), {?MODULE, notify, [Channel, start_client]}),
    dgiot_cron:push(<<Channel/binary,"_stop">>, dgiot_datetime:to_localtime(End_time),   {?MODULE, notify, [Channel, stop_client]}).

notify(_Task, Channel, Type) ->
    dgiot_channelx:do_message(Channel, Type),
    timer:sleep(50),
    dgiot_data:insert({Type,Channel}, Type).

