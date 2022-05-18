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

-module(dgiot_bridge_client).
-author("kenneth").
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([get_client/2, start_client/1, start_client/2, stop_client/2, restart_client/2, save_client/3, stop_clients/1, check_init/1, set_consumer/2, get_consumer/1]).

stop_clients(ChannelId) ->
    stop_pid(ChannelId).

stop_pid(ChannelId) ->
    case ets:info(ChannelId) of
        undefined ->
            pass;
        _ ->
            Fun =
                fun
                    ({_Key, Pid}) when is_pid(Pid) ->
                        is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
                    (_) ->
                        pass
                end,
            dgiot_data:loop(ChannelId, Fun),
            dgiot_data:clear(ChannelId)
    end.

get_client(ChannelId, ClientId) ->
    case dgiot_data:get(ChannelId, ClientId) of
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

start_client(#{<<"channel">> := ChannelId} = State, Module) ->
    save_client(ChannelId, Module, State),
    start_client(State).

start_client(#{<<"channel">> := ChannelId, <<"client">> := Client} = State) ->
    case dgiot_data:lookup(ChannelId, Client) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    case dgiot_data:get(?DGIOT_CHANNEL_CLIENT, ChannelId) of
                        {Module, State} ->
                            gen_server:start_link(Module, [State], []);
                        _ ->
                            ok
                    end
            end;
        _Reason ->
            case dgiot_data:get(?DGIOT_CHANNEL_CLIENT, ChannelId) of
                {Module, State} ->
                    gen_server:start_link(Module, [State], []);
                _ ->
                    ok
            end
    end;

start_client(_State) ->
    ok.

restart_client(ChannelId, ClientId) ->
    case dgiot_data:get(ChannelId, ClientId) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    pass;
                false ->
                    restart_client_(ChannelId, ClientId)
            end;
        _ ->
            restart_client_(ChannelId, ClientId)
    end.

stop_client(ChannelId, ClientId) ->
    case dgiot_data:lookup(ChannelId, ClientId) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _ ->
            ok
    end,
    dgiot_data:delete(ChannelId, ClientId).

save_client(ChannelId, Module, State) ->
    case dgiot_data:get(?DGIOT_CHANNEL_CLIENT, ChannelId) of
        not_find ->
            dgiot_data:insert(?DGIOT_CHANNEL_CLIENT, ChannelId, {Module, State});
        _ ->
            pass
    end.

restart_client_(ChannelId, ClientId) ->
    case dgiot_data:get(?DGIOT_CHANNEL_CLIENT, ChannelId) of
        {Module, State} ->
            gen_server:start_link(Module, [State#{<<"client">> => ClientId}], []),
            dgiot_data:insert(ChannelId, ClientId, self());
        _ ->
            pass
    end.

check_init(ChannelId) ->
    case dgiot_data:get(?DGIOT_BRIDGE, {ChannelId, init}) of
        not_find ->
            dgiot_data:insert(?DGIOT_BRIDGE, {ChannelId, init}, self()),
            false;
        _ ->
            true
    end.

set_consumer(ChannelId, PoolSize) ->
    dgiot_data:set_consumer(?DGIOT_CHANNEL_CONSUMER(ChannelId), PoolSize).

get_consumer(ChannelId) ->
    dgiot_data:get_consumer(?DGIOT_CHANNEL_CONSUMER(ChannelId), 1).

%%%% 定时启动
%%start_timer(Channel, #{
%%    <<"freq">> := _Freq,
%%    <<"start_time">> := Start_time,
%%    <<"end_time">> := End_time,
%%    <<"channel">> := Channel
%%} = Args) ->
%%    Fun =
%%        fun(_X) ->
%%            lists:map(fun(Y) ->
%%                case Y of
%%                    {DtuId, _} ->
%%                        supervisor:start_child(?TASK_SUP(Channel), [Args#{<<"client">> => DtuId}]);
%%                    _ ->
%%                        pass
%%                end
%%                      end, ets:tab2list(?DGIOT_PNQUE))
%%        end,
%%    Task = #{
%%        <<"freq">> => 5,
%%        <<"unit">> => 0,
%%        <<"start_time">> => dgiot_datetime:to_localtime(Start_time),
%%        <<"end_time">> => dgiot_datetime:to_localtime(End_time),
%%        <<"id">> => <<"task/", Channel/binary>>,
%%        <<"callback">> => Fun
%%    },
%%    dgiot_cron:save(default_task, Task).


