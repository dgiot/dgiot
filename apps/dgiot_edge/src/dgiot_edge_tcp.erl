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
-module(dgiot_edge_tcp).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").
-define(EDGE_PRODUCTID, <<"dc85acdfec">>).
-export([childspec/2, start/3]).

%% API
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3, check_serials/2, start_serial/2]).

%% {ok, Pid} = dgiot_client:get(<<"ecdb414366">>, <<"001521_DG_usb1">>).
start(ChannelId, ClientId, Data) ->
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    dgiot_client:start(ChannelId, ClientId, Data#{<<"child">> => Data})
            end;
        _Reason ->
            dgiot_client:start(ChannelId, ClientId, Data#{<<"child">> => Data})
    end.

childspec(ChannelId, ChannelArgs) ->
    Args = #{
        <<"channel">> => ChannelId,
        <<"mod">> => ?MODULE,
        <<"ip">> => maps:get(<<"ip">>, ChannelArgs, <<"127.0.0.1">>),
        <<"port">> => maps:get(<<"port">>, ChannelArgs, 5080)
    },
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 5000, dgiot_datetime:now_secs() + 300000),
    dgiot_client:register(ChannelId, tcp_client_sup, Args).

%%  callback
init(#dclient{channel = ChannelId, client = ClientId, child = Child} = State) ->
%%  初始化串口
%%    io:format("~s ~p Child = ~p.~n", [?FILE, ?LINE, Child]),
    dgiot_serial_port:init(Child),
    dgiot_data:insert({serials, ClientId}, Child),
    {ok, State#dclient{channel = dgiot_utils:to_binary(ChannelId)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(connection_ready, #dclient{channel = ChannelId, client = ClientId} = Dclient) ->
    dgiot_tcp_client:send(ChannelId, ClientId, ClientId),
    {noreply, Dclient};

%% 接收串口开启后返回的FD
handle_info({serial_open, #{<<"fd">> := FD}}, #dclient{channel = ChannelId, client = ClientId, child = #{<<"serialport">> := Serialport, <<"edgeaddr">> := Ifaddr} = Child} = Dclient) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p ClientId ~p FD ~p serial_open_success ~n", [?FILE, ?LINE, ClientId, FD]),
    dgiot_metrics:inc(dgiot_edge, <<"edge_online">>, 1),
    case dgiot_parse:get_object(<<"Channel">>, ChannelId) of
        {ok, #{<<"config">> := Config}} ->
            Serials = maps:get(<<"serials">>, Config, #{}),
            dgiot_task:save_td(?EDGE_PRODUCTID, Ifaddr, #{<<Serialport/binary, "_state">> => 0}, #{}),
            dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"config">> => Config#{<<"serials">> => Serials#{ClientId => Child#{<<"action">> => <<"enable">>}}}});
        _ ->
            pass
    end,
    Pubtopic = <<"$dg/user/edge/", Serialport/binary>>,
    dgiot_mqtt:publish(self(), Pubtopic, <<" OPEN SUCCESS">>),
    {noreply, Dclient#dclient{child = Child#{<<"fd">> => FD}}};

%% 接收串口开启失败
handle_info({serial_open_error, _}, #dclient{channel = ChannelId, client = ClientId} = Dclient) ->
    dgiot_client:stop(ChannelId, ClientId),
    dgiot_bridge:send_log(ChannelId, "~s ~p ClientId ~p serial_open_failure ~n", [?FILE, ?LINE, ClientId]),
    {stop, serial_open_error, Dclient};

%% 接收串口返回的数据
handle_info({serial_data, _, Data}, #dclient{channel = ChannelId, client = ClientId, child = #{<<"serialport">> := Serialport}} = Dclient) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p ClientId ~p recv ~p", [?FILE, ?LINE, ClientId, dgiot_utils:binary_to_hex(Data)]),
    case dgiot_data:get({autoresv, Serialport}) of
        {true, AutoData} ->
            dgiot_serial_client:write(self(), Serialport, AutoData);
        _ ->
            pass
    end,
    dgiot_tcp_client:send(ChannelId, ClientId, Data),
    put_heart(recv, ChannelId, ClientId),
    {noreply, Dclient};

%% 接收tcp server下发的数据
handle_info({tcp, Binary}, #dclient{channel = ChannelId, client = ClientId, child = #{<<"serialport">> := Serialport}} = Dclient) ->
    dgiot_serial_client:write(self(), Serialport, Binary),
    dgiot_bridge:send_log(ChannelId, "~s ~p ClientId ~p write ~p", [?FILE, ?LINE, ClientId, dgiot_utils:binary_to_hex(Binary)]),
    put_heart(send, ChannelId, ClientId),
    {noreply, Dclient};

handle_info(_Info, Dclient) ->
%%    io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
%%    io:format("~s ~p Dclient = ~p.~n", [?FILE, ?LINE, Dclient]),
    {noreply, Dclient}.

terminate(_Reason, #dclient{channel = ChannelId, client = ClientId}) ->
    start_serial(ChannelId, ClientId),
    ok.

code_change(_OldVsn, Dclient, _Extra) ->
    {ok, Dclient}.

put_heart(recv, _ChannelId, ClientId) ->
    case dgiot_data:get({serial_heart, ClientId}) of
        {_, Time} ->
            dgiot_data:insert({serial_heart, ClientId}, {0, Time});
        _ ->
            dgiot_data:insert({serial_heart, ClientId}, {0, dgiot_datetime:now_secs()})
    end;

put_heart(send, ChannelId, ClientId) ->
    case dgiot_data:get({serial_heart, ClientId}) of
        not_find ->
            dgiot_data:insert({serial_heart, ClientId}, {1, dgiot_datetime:now_secs()});
        {Num, _} when Num > 3 ->
            start_serial(ChannelId, ClientId),
            dgiot_data:insert({serial_heart, ClientId}, {0, dgiot_datetime:now_secs()});
        {Num, _} ->
            dgiot_data:insert({serial_heart, ClientId}, {Num + 1, dgiot_datetime:now_secs()})
    end.

%%  erlang:exit(dgiot_data:get(dgiot_serial_ets, <<"usb7">>), kill).
%%  is_process_alive(dgiot_data:get(dgiot_serial_ets, <<"usb7">>)).
check_serials(ChannelId, ClientIds) ->
    Now = dgiot_datetime:now_secs(),
    lists:foldl(fun(ClientId, _) ->
        case dgiot_data:get({serial_heart, ClientId}) of
            {Num, _} when Num > 3 ->
                start_serial(ChannelId, ClientId);
            {_, Last} when (Now - Last) > 10 ->
                start_serial(ChannelId, ClientId);
            _ ->
                pass
        end
                end, #{}, ClientIds).

%% dgiot_edge_tcp:start_serial(<<"c93b23863e">>, <<"317300_DG_usb5">>).
%% dgiot_data:get({serials, <<"317300_DG_usb5">>}).
start_serial(ChannelId, ClientId) ->
    case dgiot_data:get({serials, ClientId}) of
        #{<<"action">> := <<"enable">>} = Child ->
            dgiot_edge_tcp:start(ChannelId, ClientId, Child);
        _ ->
            pass
    end.
