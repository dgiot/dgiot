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

-module(sinmahe_mqttc).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").
-record(state, {addr, client, key, secret, count}).
%% API
-export([start_link/3, init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

start(Tid) ->
    Count = 100,
    Sup = list_to_atom(lists:concat([Tid, mqttc])),
    NewCount =
        case Count of
            1 ->
                2;
            _ ->
                Count
        end,
    lists:map(fun(Id) ->
        {ok, _} = supervisor:start_child(Sup, [Tid, Id, NewCount])
              end, lists:seq(1, NewCount)).

%%推送协议：MQTT
%%➢ 连接地址：服务器 IP
%%➢ 连接端口：1883
%%➢ 连接方式：TCP
%%➢ 认证参数
%%用户名：api_key（企业编码）
%%密码：api_secret（企业秘钥）
%%clientID：api_key:api_secret: + 三位随机数字。 如：api_key:api_secret125
%%注：服务器 IP 由平台运营商提供，企业编码、企业秘钥可在 ZETA 网管平台->权限管理->企业信
%%息中获得

start_link(_Name, Id, _Count) ->
    _Options = [
        {clientid, <<"ClientId">>},
        {host, binary_to_list(<<"Host">>)},
        {port, <<"Port">>},
        {username, binary_to_list(<<"Key">>)},
        {password, binary_to_list(<<"Secret">>)},
        {proto_ver, v3},
        {keepalive, 60},
        {clean_start, true}
    ],

%%   ?LOG(info,"Options ~p ",[Options]),
    case dgiot_data:lookup({Id, mqttc}) of
        {ok, Pid} ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
%%                    dgiot_mqtt_client:start_link(?MODULE, [Id, Count], Options)
                    pass
            end;
        {error, not_find} ->
%%            dgiot_mqtt_client:start_link(?MODULE, [Id, Count], Options)
            pass
    end.

init([Id, Key, Secret, Count]) ->
    dgiot_data:insert({Id, mqttc}, self()),
%%    ?LOG(info,"Id ~p Count ~p ", [Id, Count]),
    {ok, #state{addr = Id, key = Key, secret = Secret, count = Count}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #state{addr = Id, count = Count} = State) ->
    erlang:send_after(2000, self(), {subscribe, Client}),
    ?LOG(info, "Id ~p Count ~p ", [Id, Count]),
    dgiot_metrics:inc(dgiot_sinmahe, <<"mqtt_online">>, 1),
    update(Count),
    {noreply, State#state{client = Client}};

handle_info({subscribe, Client}, #state{count = Count, key = Key, addr = 1} = State) ->
    Topic = <<Key/binary, "/v1/ms/#">>,
    ?LOG(info, "Topic ~p Count ~p ", [Topic, Count]),
    emqtt:subscribe(Client, {Topic, 1}),
    {noreply, State};

handle_info({subscribe, Client}, #state{count = Count, key = Key, addr = 2} = State) ->
    Topic = <<Key/binary, "/v1/ap/#">>,
    ?LOG(info, "Topic ~p Count ~p ", [Topic, Count]),
    emqtt:subscribe(Client, {Topic, 1}),
    {noreply, State};

handle_info(disconnect, State) ->
    dgiot_metrics:dec(dgiot_sinmahe, <<"mqtt_online">>, 1),
    ?LOG(info, "disconnect ~p ", [State]),
    {noreply, State#state{client = disconnect}};

handle_info({publish, #{payload := _Payload} = _Msg}, State) ->
    dgiot_metrics:inc(dgiot_sinmahe, <<"mqtt_recv">>, 1),
    {noreply, State};

handle_info(_Info, State) ->
%%    ?LOG(info,"ecapturer ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{count = Count, addr = Id} = _State) ->
%%    ?LOG(info,"_Reason ~p~n", [_Reason]),
    update(Count),
    dgiot_data:delete({Id, mqttc}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update(Count) ->
    OnlineCount = lists:foldl(fun(Id, Acc) ->
        case dgiot_data:lookup({Id, mqttc}) of
            {ok, Pid} ->
                case is_pid(Pid) andalso is_process_alive(Pid) of
                    true ->
                        Acc + 1;
                    false ->
                        Acc
                end;
            _ ->
                Acc
        end
                              end, 0, lists:seq(1, Count)),
    dgiot_data:insert({<<"mqtt_online">>, dgiot_metrics}, OnlineCount).
