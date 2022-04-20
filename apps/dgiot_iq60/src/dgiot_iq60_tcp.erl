%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_iq60_tcp).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_iq60.hrl").
%% API
-export([start/2]).

%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

%% =======================
%% tcp server start
%% {ok, State} | {stop, Reason}
init(TCPState) ->

    dgiot_metrics:inc(dgiot_iq60, <<"dtu_login">>, 1),
    {ok, TCPState}.

%%设备登录报文，登陆成功后，开始搜表
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, dtuaddr = <<>>, search = _Search} = State} = TCPState) ->
    ?LOG(info,"Buff ~p",[Buff]),
    DTUIP = dgiot_utils:get_ip(Socket),
    {Protocol, DtuAddr} =
        case Buff of
            <<16#68, _:4/bytes, 16#68,_A1:8/bytes,_Rest/binary>> ->
                {_, [Acc | _]} = dlt376_decoder:parse_frame(Buff, []),
                #{<<"msgtype">> := Protocol1,<<"addr">> := MeterAddr} = Acc,
                dgiot_iq60:create_iq60(MeterAddr, ChannelId, DTUIP,Buff),
                {Protocol1, MeterAddr};
            _ ->
                {?IQ60, Buff}
        end,
    case Protocol of
        ?IQ60 ->
            {ProductId, _, _} = dgiot_data:get({meter, ChannelId}),
            {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
            Topic = <<"thing/", ProductId/binary, "/", DtuAddr/binary>>,
            dgiot_mqtt:subscribe(Topic),  %为这个设备订阅一个mqtt
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "from dev ~p (登录)", [dgiot_utils:binary_to_hex(Buff)]),
            {NewRef, NewStep} = {undefined, read_meter},
            DtuId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
            dgiot_metrics:inc(dgiot_iq60, <<"dtu_online">>, 1),
            {noreply, TCPState#tcp{buff = <<>>, register = true, clientid = DtuId, state = State#state{dtuaddr = DtuAddr, protocol = ?IQ60, ref = NewRef, step = NewStep}}};
       _ ->
            {noreply, TCPState}
    end;


%% 异常报文丢弃
%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    ?LOG(info, "GGM 999 Meter Rev Buff========ERROR========= dgiot_iq60_tcp, handle_info9, ~p,~p~n~n~n ", [_Info, TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    dgiot_metrics:dec(dgiot_iq60, <<"dtu_online">>, 1),
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.
