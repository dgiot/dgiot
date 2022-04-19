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

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "revice from  ~p", [dgiot_utils:binary_to_hex(Buff)]),
    case dgiot_hjt212_decoder:parse_frame(Buff, State) of
        {ok, #{<<"ack">> := Ack} = _Result} ->
            dgiot_tcp_server:send(TCPState, Ack);
        {ok, Result} ->
            dgiot_bridge:send_log(ChannelId, "~s ~p ~ts", [?FILE, ?LINE, unicode:characters_to_list(jiffy:encode(Result))]);
%%            R = dgiot_hjt212_decoder:to_frame(Result),
%%            case R =:= Buff of
%%                true ->
%%                    io:format("success to_frame Buff ~p~n", [Buff]),
%%                    dgiot_bridge:send_log(ChannelId, "success to_frame Buff ~p", [Buff]);
%%                _ ->
%%                    io:format("error to_frame R ~p~n", [R]),
%%                    dgiot_bridge:send_log(ChannelId, "error to_frame Buff ~p", [R])
%%            end;
        _ ->
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

