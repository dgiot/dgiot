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

-module(dgiot_app).
-author("johnliu").
-include("dgiot.hrl").

-behaviour(application).
-dgiot_plugin(?MODULE).
-ifdef(DGIOT_WITH_EMQX).
-emqx_plugin(?MODULE).
-endif.

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dgiot_data:init(),
    dgiot_data:search_data(),
    dgiot_datetime:start_time(),
    start_mnesia(),
    init_broker(),
    dgiot:init_plugins(),
    {ok, Sup} = dgiot_sup:start_link(),
    dgiot_cron_timer:start(),
    start_plugin(Sup),
    dgiot_pushgateway:start_link(),
    dgiot_metrics:start(dgiot),
    start_mqtt_listeners(),
    {ok, Sup}.

start_mqtt_listeners() ->
    try esockd:start() catch _:_ -> ok end,
    MqttOpts = [{tcp_options,[{backlog,512},{keepalive,true},{nodelay,true},{reuseaddr,true},binary,{packet,raw},{exit_on_close,true}]},{acceptors,8},{max_connections,1024000}],
    try esockd:open(external, 1883, MqttOpts, {dgiot_broker_native, start_link, []}) catch _:_ -> ok end.

init_broker() ->
    try dgiot_broker_native:init() catch _:_ -> ok end,
    try dgiot_mqtt:init_ets() catch _:_ -> ok end.

stop(_State) ->
    ok.

start_mnesia() ->
    dgiot_mnesia:mnesia(boot),
    try dgiot_mnesia_helper:mnesia(boot) catch _:_ -> ok end,
    Env = [
        {mnesia_monitor, dc_dump_limit, 50000},
        {mnesia_monitor, dump_log_time_threshold, 300000},
        {mnesia_monitor, dump_log_write_threshold, 50000}
    ],
    lists:foreach(fun({Mod, Key, Value}) -> apply(Mod, set_env, [Key, Value]) end, Env).

start_plugin(Sup) ->
    Fun =
        fun({_App, _Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) of
                false ->
                    Acc;
                _ ->
                    lists:foldl(fun(X, Acc1) ->
                        case X of
                            {dgiot_plugin, [Order]} ->
                                Acc1 ++ [{Order, Mod}];
                            _ ->
                                Acc1
                        end
                                end, Acc, Mod:module_info(attributes))
            end
        end,
    NewAcc = dgiot_plugin:check_module(Fun, []),
    lists:map(fun(X) ->
        {_Order, Mod} = X,
        case erlang:function_exported(Mod, start, 1) of
            true ->
                Mod:start(Sup);
            false ->
                pass
        end
              end, lists:ukeysort(1, NewAcc)).
