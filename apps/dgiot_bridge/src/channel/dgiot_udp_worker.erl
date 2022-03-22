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
-module(dgiot_udp_worker).
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-record(state, {id, ip, port, transport, env, product, log}).

-define(MAX_BUFF_SIZE, 1024).

-export([
    get_deviceid/2,
    child_spec/2
]).
-define(SOCKOPTS, [binary, {reuseaddr, true}]).
%% udp callback

-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

child_spec(Port, State) ->
    dgiot_udp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

init(#udp{state = #state{id = ChannelId}} = UDPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, _ProductIds} ->
            {ok, UDPState};
        {error, not_find} ->
            {stop, not_find_channel}
    end.

%%设备登录报文，登陆成功后
handle_info({udp, Buff}, #udp{state = #state{id = _ChannelId} = _State} = UDPState) ->
    io:format("~s ~p Buff: ~p~n", [?FILE, ?LINE, Buff]),
%%    io:format("~s ~p UDPState: ~p~n", [?FILE, ?LINE, UDPState]),
%%    DTUIP = dgiot_utils:get_ip(Sock),
    Data = dgiot_utils:hex_to_binary(<<"32 30 32 32 2d 30 33 2d 32 31 20 31 30 3a 30 37 3a 32 38 20 20 20 20 4f 6e 6c 69 6e 65 3a 20 38 2c 54 6f 74 61 6c 3a 20 39 0a d2 bb c2 a5 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 31 09 d4 da cf df 09 37 35 09 31 45 2d 33 30 2d 33 30 2d 30 30 2d 34 36 2d 46 44 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 2d 39 36 2e 30 64 42 09 0a b6 fe c2 a5 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 32 09 d4 da cf df 09 37 35 09 31 45 2d 33 30 2d 33 30 2d 30 30 2d 34 36 2d 36 33 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 2d 39 36 2e 30 64 42 09 0a c8 fd c2 a5 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 33 09 d4 da cf df 09 37 35 09 31 45 2d 33 30 2d 33 30 2d 30 30 2d 34 37 2d 42 34 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 2d 39 36 2e 30 64 42 09 0a bc e0 cc fd d2 f4 cf e4 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 34 09 d4 da cf df 09 35 30 09 31 45 2d 32 30 2d 41 30 2d 31 30 2d 43 31 2d 31 39 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 30 2e 30 64 42 09 0a d1 b0 ba f4 bb b0 cd b2 31 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 35 09 c0 eb cf df 09 2d 09 2d 09 2d 09 2d 09 2d 09 2d 09 2d 09 0a d1 b0 ba f4 bb b0 cd b2 32 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 36 09 d4 da cf df 09 30 09 31 45 2d 33 30 2d 36 43 2d 30 31 2d 33 44 2d 30 31 09 2d 09 2d 09 2d 09 2d 09 2d 35 31 2e 36 64 42 09 0a b5 d8 cf c2 ca d2 31 09 31 39 32 2e 31 36 38 2e 39 2e 31 38 31 09 d4 da cf df 09 37 35 09 31 45 2d 33 30 2d 33 30 2d 30 30 2d 34 35 2d 43 45 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 2d 39 36 2e 30 64 42 09 0a b5 d8 cf c2 ca d2 32 09 31 39 32 2e 31 36 38 2e 39 2e 31 38 32 09 d4 da cf df 09 37 35 09 31 45 2d 33 30 2d 33 30 2d 30 30 2d 34 36 2d 37 46 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 2d 39 36 2e 30 64 42 09 0a b5 d8 cf c2 ca d2 33 09 31 39 32 2e 31 36 38 2e 38 2e 31 30 37 09 d4 da cf df 09 37 35 09 31 45 2d 33 30 2d 33 30 2d 30 30 2d 34 35 2d 43 32 09 c1 d9 ca b1 c8 ce ce f1 3a c1 d6 d2 e4 c1 ab 20 2d 20 cc fd cb b5 b0 ae c7 e9 bb d8 c0 b4 b9 fd 2e 6d 70 33 09 31 09 c1 d9 ca b1 c8 ce ce f1 3a 20 39 34 37 38 32 09 31 09 2d 39 36 2e 30 64 42 09 0a">>),
    dgiot_udp_server:send(UDPState, Data),
    {noreply, UDPState};

%% 异常报文丢弃
handle_info(_Info, UDPState) ->
    io:format("~s ~p _Info: ~p~n", [?FILE, ?LINE, _Info]),
    io:format("~s ~p UDPState: ~p~n", [?FILE, ?LINE, UDPState]),
    {noreply, UDPState}.

handle_call(_Msg, _From, UDPState) ->
    {reply, ok, UDPState}.

handle_cast(_Msg, UDPState) ->
    {noreply, UDPState}.

terminate(_Reason, _UDPState) ->
    ok.

code_change(_OldVsn, UDPState, _Extra) ->
    {ok, UDPState}.

get_deviceid(ProdcutId, DevAddr) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProdcutId, <<"devaddr">> => DevAddr}),
    DeviceId.
