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

-module(dgiot_bacnet_broadcast).
-author("johnliu").
-include_lib("dgiot/include/dgiot_client.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([init/1, handle_info/2, terminate/2]).
-export([childspec/2, start_connect/2]).

-define(MAX_BUFF_SIZE, 10 * 1024).

childspec(ChannelId, Args) ->
    dgiot_client:register(<<ChannelId/binary, "_broacast">>, udp_broadcast_sup, Args).

start_connect(ChannelId, #{<<"ip">> := Ip}) ->
    Opts = #{<<"ip">> => Ip, <<"mod">> => ?MODULE, <<"child">> => #{<<"channel">> => ChannelId}},
    dgiot_client:start(<<ChannelId/binary, "_broacast">>, dgiot_utils:get_ip(Ip), Opts).

init(#dclient{child = ChildState} = Dclient) when is_map(ChildState) ->
    {ok, Dclient};

init(_) ->
    {ok, #{}}.

handle_info(connection_ready, #dclient{child = ChildState} = Dclient) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), whois),
    {noreply, Dclient#dclient{child = ChildState}};

handle_info(whois, #dclient{channel = ChannelId, client = ClientId} = Dclient) ->
    WhoIs = dgiot_bacnet_utils:whois(),
    dgiot_udp_broadcast:send(ChannelId, ClientId, WhoIs),
    dgiot_bridge:send_log(ChannelId, "~s ~p 1 send WhoIs => ~p", [?FILE, ?LINE, dgiot_utils:to_hex(WhoIs)]),
    {noreply, Dclient};

handle_info({udp, Ip, Port, Buff}, #dclient{child = #{<<"channel">> := ChannelId}} = Dclient) ->
    io:format("~s ~p ~p send from ~p:~p : ~p ~n", [?FILE, ?LINE, self(), dgiot_utils:get_ip(Ip), Port, dgiot_utils:to_hex(Buff)]),
    dgiot_channelx:do_message(<<"BACNET">>, ChannelId, {whois, dgiot_utils:get_ip(Ip), Port, Buff}, 30000),
    {noreply, Dclient};

handle_info(_Info, Dclient) ->
    {noreply, Dclient}.

terminate(_Reason, _Dclient) ->
    ok.
