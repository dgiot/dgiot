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
-module(dgiot_modbusc_tcp).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
%% API
-export([init/1, handle_info/2, terminate/2]).
-export([start_connect/1]).
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot.hrl").
-define(MAX_BUFF_SIZE, 10 * 1024).
-include_lib("dgiot/include/logger.hrl").

start_connect(_Opts =
    #{
        <<"auto_reconnect">> := Recon,
        <<"reconnect_times">> := ReTimes,
        <<"ip">> := Ip,
        <<"port">> := Port,
        <<"channelid">> := ChannelId,
        <<"hb">> := HB,
        <<"filename">> := FileName,
        <<"minaddr">> := MinAddr,
        <<"maxaddr">> := Maxaddr
    }) ->
    State = #state{
        id = ChannelId,
        hb = HB,
        env = #{
            data => <<>>,
            filename => FileName,
            minaddr => MinAddr,
            maxaddr => Maxaddr
        }
    },
    dgiot_tcp_client:start_link(?MODULE, Ip, Port, Recon, ReTimes, State).

init(TCPState) ->
    {ok, TCPState}.

handle_info(connection_ready, TCPState) ->
    io:format("~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), read),
    {noreply, TCPState};

handle_info(tcp_closed, TCPState) ->
    {noreply, TCPState};

handle_info(read, #tcp{state = #state{id = ChannelId, env = #{minaddr := MinAddr, maxaddr := Maxaddr} = Env} = State} = TCPState) ->
    Address = modbus_tcp:get_addr(ChannelId, MinAddr, Maxaddr, 120),
    DataSource =
        #{
            <<"registersnumber">> => <<"120">>,
            <<"slaveid">> => <<"0X01">>,
            <<"operatetype">> => <<"readHregs">>,
            <<"address">> => Address
        },
    Data = modbus_tcp:to_frame(DataSource),
    dgiot_tcp_server:send(TCPState, Data),
%%    erlang:send_after(10 * 1000, self(), read),
    {noreply, TCPState#tcp{state = State#state{env = Env#{maxaddr => Maxaddr, di => Address, step => 120}}}};

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, env = #{maxaddr := Maxaddr, di := Address, filename := FileName, data := OldData, step := Step} = Env} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "returns [~p] to Channel", [dgiot_utils:binary_to_hex(Buff)]),
    Data = modbus_tcp:parse_frame(Buff),
    erlang:send_after(3 * 1000, self(), read),
    case Address + Step >= Maxaddr of
        true ->
            EndData = <<OldData/binary, Data/binary>>,
%%            io:format("~s ~p EndData = ~p.~n", [?FILE, ?LINE, EndData]),
            modbus_tcp:parse_frame(FileName, EndData),
            {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = Env#{data => <<>>}}}};
        _ ->
            {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = Env#{data => <<OldData/binary, Data/binary>>}}}}

    end;

handle_info(_Info, TCPState) ->
    io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
    io:format("~s ~p TCPState = ~p.~n", [?FILE, ?LINE, TCPState]),
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

