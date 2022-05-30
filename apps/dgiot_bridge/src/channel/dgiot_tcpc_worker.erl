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
-include_lib("dgiot/include/dgiot_client.hrl").
-record(child_state, {buff = <<>>, product, hb = 30, device}).

%% tcp client callback
-define(MAX_BUFF_SIZE, 10 * 1024).
-export([init/1, handle_info/2, terminate/2]).

%% tcp client  callback
init(#dclient{child = ChildState} = Dclient) when is_map(ChildState) ->
    case maps:find(<<"productid">>, ChildState) of
        {ok, ProductId} ->
            case do_cmd(ProductId, init, ChildState, Dclient) of
                default ->
                    {noreply, #child_state{product = ProductId}};
                Result ->
                    Result
            end;
        _ ->
            {stop, <<"not find product">>}
    end;

init(#dclient{channel = ChannelId}) ->
    case dgiot_product:get_channel(ChannelId) of
        not_find ->
            {stop, <<"not find product">>};
        ProductId ->
            io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, ChannelId, ProductId]),
            {noreply, #child_state{product = ProductId}}
    end.

handle_info(connection_ready, #dclient{child = #child_state{product = ProductId} = ChildState} = Dclient) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, ProductId]),
    case do_cmd(ProductId, connection_ready, <<>>, Dclient) of
        default ->
            erlang:send_after(Time, self(), login),
            {noreply, ChildState};
        Result ->
            Result
    end;

handle_info(#{<<"cmd">> := Cmd, <<"data">> := Data, <<"productId">> := ProductId}, #dclient{child = #child_state{} = ChildState} = Dclient) ->
    case do_cmd(ProductId, Cmd, Data, Dclient) of
        default ->
            {noreply, ChildState};
        Result  ->
            Result
    end;

handle_info(tcp_closed, #dclient{child = #child_state{product = ProductId} = ChildState} = Dclient) ->
    case do_cmd(ProductId, tcp_closed, <<>>, Dclient) of
        default ->
            {noreply, ChildState};
        Result  ->
            Result
    end;

handle_info({tcp, Buff}, #dclient{child = #child_state{buff = Old, product = ProductId} = ChildState} = Dclient) ->
    Data = <<Old/binary, Buff/binary>>,
    case do_cmd(ProductId, tcp, Data, Dclient) of
        default ->
            {noreply, ChildState};
        Result ->
            Result
    end;

handle_info({deliver, _Topic, _Msg}, #dclient{child = ChildState}) ->
    {noreply, ChildState};

handle_info(login, #dclient{child = #child_state{hb = Hb} = ChildState}) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    dgiot_tcp_client:send(<<"login">>),
    {noreply, ChildState};

handle_info(heartbeat, #dclient{child = #child_state{hb = Hb} = ChildState}) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    dgiot_tcp_client:send(<<"heartbeat">>),
    {noreply, ChildState};

handle_info(_Info, Dclient) ->
    {noreply, Dclient}.

terminate(_Reason, _Dclient) ->
    ok.

do_cmd(ProductId, Cmd, Data, #dclient{child = ChildState} = Dclient) ->
    io:format("~s ~p ~p Cmd ~p  ~n", [?FILE, ?LINE, ProductId, Cmd]),
    case dgiot_hook:run_hook({tcp, ProductId}, [Cmd, Data, Dclient]) of
        {ok, Device} ->
            {noreply, ChildState#child_state{device = Device}};
        {noreply, Device} ->
            {noreply, ChildState#child_state{device = Device}};
        {noreply, Bin, Device} ->
            {noreply, ChildState#child_state{buff = Bin, device = Device}};
        {error, Reason, Device} ->
            {stop, Reason, ChildState#child_state{device = Device}};
        {stop, Reason, Device} ->
            {stop, Reason, ChildState#child_state{device = Device}};
        _ ->
            default
    end.