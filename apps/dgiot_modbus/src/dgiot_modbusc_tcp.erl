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
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot.hrl").
-define(MAX_BUFF_SIZE, 10 * 1024).
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").


%% tcp client  callback
init(#dclient{child = ChildState} = Dclient) when is_map(ChildState) ->
    {ok, Dclient};

init(_) ->
    {ok, #{}}.

handle_info(connection_ready, #dclient{child = ChildState} = Dclient) ->
%%    io:format("~s ~p ChildState = ~p.~n", [?FILE, ?LINE, ChildState]),
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), read),
    {noreply, Dclient#dclient{child = ChildState}};

handle_info(tcp_closed, #dclient{child = ChildState} = Dclient) ->
    {noreply, Dclient#dclient{child = ChildState}};

handle_info(read, #dclient{channel = ChannelId, client = ClientId, child = #{slaveid := SlaveId, function := Operatetype, address := StartAddr, maxaddr := Maxaddr} = ChildState} = Dclient) ->
    Address = maps:get(di, ChildState, StartAddr),
    Step = maps:get(step, ChildState, 100),
    Registersnumber =
        case (Address + Step) >= Maxaddr of
            true ->
                Maxaddr - Address + 2;
            _ ->
                100
        end,
    DataSource =
        #{
            <<"registersnumber">> => Registersnumber,
            <<"slaveid">> => SlaveId,
            <<"operatetype">> => Operatetype,
            <<"address">> => Address
        },
    Data = modbus_tcp:to_frame(DataSource),
    dgiot_tcp_client:send(ChannelId, ClientId, Data),
    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "~p Channel sends ~p to DTU", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), dgiot_utils:binary_to_hex(Data)]),
%%    io:format("~s ~p Address = ~p.~n", [?FILE, ?LINE, Address]),
    {noreply, Dclient#dclient{child = ChildState#{di => Address, step => Step}}};

handle_info({tcp, Buff}, #dclient{channel = ChannelId,
    child = #{freq := Freq, minaddr := MinAddr, maxaddr := Maxaddr, di := Address, filename := FileName, address := StartAddr, data := OldData, step := Step} = ChildState} = Dclient) ->
%%    io:format("~s ~p Buff = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Buff)]),
    Data = modbus_tcp:parse_frame(Buff),
    case Address + Step >= Maxaddr of
        true ->
            EndData = <<OldData/binary, Data/binary>>,
%%            io:format("~s ~p EndData = ~p.~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(EndData)]),
            AllData = modbus_tcp:parse_frame(StartAddr, FileName, EndData, MinAddr),
            dgiot_data:insert({check_connection, dgiot_utils:to_binary(ChannelId), FileName}, dgiot_datetime:now_secs()),
            dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "~p recv data ~ts => ~p", [dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), unicode:characters_to_list(dgiot_json:encode(AllData)), dgiot_utils:binary_to_hex(EndData)]),
            erlang:send_after(Freq * 1000, self(), read),
            {noreply, Dclient#dclient{child = ChildState#{di => StartAddr, data => <<>>}}};
        _ ->
            erlang:send_after(100, self(), read),
            {noreply, Dclient#dclient{child = ChildState#{di => Address + Step, data => <<OldData/binary, Data/binary>>}}}
    end;

handle_info(_Info, #dclient{child = _ChildState} = Dclient) ->
%%    io:format("~s ~p _Info = ~p.~n", [?FILE, ?LINE, _Info]),
%%    io:format("~s ~p Dclient = ~p.~n", [?FILE, ?LINE, Dclient]),
%%    io:format("~s ~p ChildState = ~p.~n", [?FILE, ?LINE, ChildState]),
    {noreply, Dclient}.

terminate(_Reason, _Dclient) ->
    ok.
