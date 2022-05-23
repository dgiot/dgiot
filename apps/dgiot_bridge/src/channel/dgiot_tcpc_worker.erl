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
-module(dgiot_tcpc_worker).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-record(child_state, {buff = <<>>, product, devaddr, hb = 30 }).


%% tcp client callback
-define(MAX_BUFF_SIZE, 10 * 1024).
-export([init/1, handle_info/2, terminate/2]).

%% tcp client  callback
init(_Dclient) ->
    {ok, #child_state{}}.

handle_info(connection_ready, #child_state{product = ProductId} = ChildState) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    case do_cmd(ProductId, connection_ready, <<>>, ChildState) of
        default ->
            erlang:send_after(Time, self(), login);
        _ ->
            pass
    end,
    {noreply, ChildState};

handle_info(#{<<"cmd">> := Cmd, <<"data">> := Data, <<"productId">> := ProductId}, ChildState) ->
    case do_cmd(ProductId, Cmd, Data, ChildState) of
        default ->
            {noreply, ChildState};
        Result ->
            Result
    end;

handle_info(tcp_closed, #child_state{product = ProductId}  = ChildState) ->
    case do_cmd(ProductId, tcp_closed, <<>>, ChildState) of
        default ->
            {noreply, ChildState};
        Result ->
            Result
    end;

handle_info({tcp, Buff}, #child_state{buff = Old, product = ProductId} = ChildState) ->
    Data = <<Old/binary, Buff/binary>>,
    case do_cmd(ProductId, tcp, Data, ChildState) of
        default ->
            {noreply, ChildState};
        {noreply, Bin, NewChildState} ->
            {noreply, NewChildState#child_state{buff = Bin}};
        {stop, Reason, NewChildState} ->
            {stop, Reason, NewChildState};
        Result ->
            Result
    end;

handle_info({deliver, _Topic, _Msg}, ChildState) ->
    {noreply, ChildState};

handle_info(login, #child_state{hb = Hb} = ChildState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    dgiot_tcp_client:send(<<"login">>),
    {noreply, ChildState};

handle_info(heartbeat, #child_state{hb = Hb} = TCPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    dgiot_tcp_client:send(<<"heartbeat">>),
    {noreply, TCPState};

handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

do_cmd(ProductId, Cmd, Data, ChildState) ->
    case dgiot_hook:run_hook({tcp, ProductId}, [Cmd, Data, ChildState]) of
        {ok, NewChildState} ->
            {noreply, NewChildState};
        {reply, ProductId, Payload, NewChildState} ->
            case dgiot_tcp_client:send(Payload) of
                ok ->
                    ok;
                {error, _Reason} ->
                    pass
            end,
            {noreply, NewChildState};
        _ ->
            default
    end.