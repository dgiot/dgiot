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
-module(dgiot_hjt212_tcp).
-author("stoneliu").
-include("dgiot_hjt212.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(MAX_BUFF_SIZE, 1024).

-export([
    start/2
]).

%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

init(#tcp{state = #state{id = ChannelId}} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, _ProductIds} ->
            {ok, TCPState};
        {error, not_find} ->
            {stop, not_find_channel}
    end.

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, product = ProductId} = _State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, ProductId, "~s ~p ~p revice from ~p => ProductId ~p ", [?FILE, ?LINE, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), Buff, ProductId]),
    case dgiot_hjt212_decoder:parse_frame(Buff, []) of
        {ok, [#{<<"devaddr">> := Devaddr} = Data | _]} ->
            NewData = dgiot_dlink_proctol:parse_payload(ProductId, Data),
            dgiot_bridge:send_log(ChannelId, ProductId, Devaddr, "~s ~p ~p revice from ~p~n save td => ProductId ~p DevAddr ~p ~ts ", [?FILE, ?LINE, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), Buff, ProductId, Devaddr, unicode:characters_to_list(dgiot_json:encode(NewData))]),
            dgiot_task:save_td(ProductId, Devaddr, NewData, #{});
        _O ->
            dgiot_bridge:send_log(ChannelId, ProductId, "~s ~p ~p revice from => ~p ", [?FILE, ?LINE, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), _O]),
            pass
    end,
    {noreply, TCPState};

handle_info({deliver, _, _Msg}, #tcp{state = #state{id = _ChannelId} = _State} = TCPState) ->

    {noreply, TCPState};
%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    ?LOG(info, "TCPState ~p", [TCPState]),
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

