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

-module(dgiot_tcp2dlink_worker).
-author("kenneth").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"TCP2DLINK">>).
-define(MAX_BUFF_SIZE, 1024).
-define(AREA(R), 3.14159265 * R * R).
-record(state, {
    id,
    productId,
    devaddr,
    taskdeviceid,
    out_area_mj,
    out_distend,
    ratedspeed
}).


%% TCP callback
-export([child_spec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).


child_spec(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, Port, State).

%% =======================
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            ProductId = lists:nth(1, ProductIds),
            NewState = State#state{productId = ProductId},
            {ok, TCPState#tcp{state = NewState}};
        {error, not_find} ->
            {error, not_find_channel}
    end.

% device2task 上行
handle_info({tcp, _Buff}, #tcp{state = #state{id = _ChannelId, productId = _ProductId} = _State} = TCPState) ->
    {noreply, TCPState};

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{state = #state{productId = _ProductId}} = _TCPState) ->
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.
