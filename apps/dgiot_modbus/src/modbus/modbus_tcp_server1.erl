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

-module(modbus_tcp_server1).
%%-include("dgiot_modbus.hrl").
%%-behaviour(exo_socket_server).
%%-export([init/2, data/3, close/2, error/3, control/4]).
%%
%%-export([start_link/1]).
%%-export([stop/1]).
%%
%%-record(state,
%%{
%%    proto_id = 0,
%%    unit_ids = [255],
%%    buf = <<>>,
%%    callback
%%}).
%%
%%-define(DEFAULT_TCP_PORT, 502).
%%
%%start_link(Opts0) ->
%%    Opts = Opts0 ++ application:get_all_env(modbus),
%%    ?LOG(debug,"start options ~p", [Opts]),
%%    Port = proplists:get_value(port, Opts, ?DEFAULT_TCP_PORT),
%%    Callback = proplists:get_value(callback, Opts),
%%    UnitIDs = case proplists:get_value(unit_ids, Opts) of
%%                  List when is_list(List) -> List;
%%                  I when is_integer(I) -> [I];
%%                  undefined ->
%%                      %% old syntax
%%                      case proplists:get_value(unit_id, Opts, 255) of
%%                          List when is_list(List) -> List;
%%                          I when is_integer(I) -> [I]
%%                      end
%%              end,
%%    exo_app:start(),
%%    exo_socket_server:start_link(Port, [tcp],
%%        [{active,once},{mode,binary},
%%            {reuseaddr,true},{nodelay,true}],
%%        ?MODULE, [Callback,UnitIDs]).
%%
%%stop(Pid) when is_pid(Pid) ->
%%    exo_socket_server:stop(Pid).
%%
%%%% init(Socket::socket(), Args::[term()]
%%%%   -> {ok,state()} | {stop,reason(),state()}
%%init(_Socket, [Callback,UnitIDs]) ->
%%    ?LOG(debug,"unit ids ~p", [UnitIDs]),
%%    {ok,
%%        #state{
%%            callback = Callback,
%%            unit_ids = UnitIDs
%%        }}.
%%
%%%% data(Socket::socket(), Data::io_list(), State::state())
%%%%   -> {ok,state()}|{close,state()}|{stop,reason(),state()}
%%data(Socket, Data, State) ->
%%    Buf = <<(State#state.buf)/binary, Data/binary>>,
%%    case Buf of
%%        <<TransID:16,_ProtoID:16,Length:16,Data1:Length/binary, Buf1/binary>> ->
%%            case Data1 of
%%                <<UnitID, Func, Params/binary>> ->
%%                    ?LOG(debug,"unit_id ~p received, check list ~p",
%%                        [UnitID, State#state.unit_ids]),
%%                    case lists:member(UnitID,State#state.unit_ids) of
%%                        true ->
%%                            try handle_pdu(Socket,UnitID,TransID,Func,Params,
%%                                State#state { buf = Buf1 }) of
%%                                State1 ->
%%                                    {ok, State1}
%%                            catch
%%                                error:_Reason ->
%%                                    send(Socket,TransID,State#state.proto_id,
%%                                        UnitID,
%%                                        16#80 + (Func band 16#7f),
%%                                        <<?SLAVE_DEVICE_FAILURE>>),
%%                                    {ok, State}
%%                            end;
%%                        false ->
%%                            ?LOG(warning,"pdu not for us", []),
%%                            {ok, State#state { buf = Buf1 }}
%%                    end;
%%                _ ->
%%                    ?LOG(warning,"pdu too short", []),
%%                    {ok, State#state { buf = Buf1 }}
%%            end;
%%        _ ->
%%            %% FIXME: throw if too big
%%            {ok, State#state { buf = Buf }}
%%    end.
%%
%%%% close(Socket::socket(), State::state())
%%%%   -> {ok,state()}
%%close(_Socket, State) ->
%%    {ok, State}.
%%
%%%% error(Socket::socket(),Error::error(), State:state())
%%%%   -> {ok,state()} | {stop,reason(),state()}
%%
%%error(_Socket, Error,State) ->
%%    {stop, Error, State}.
%%
%%%% control(Socket::socket(), Request::term(),
%%%%         From::term(), State:state())
%%%%   -> {reply, Reply::term(),state() [,Timeout]} |
%%%%      {noreply,state() [,Timeout]} |
%%%%      {ignore,state()[,Timeout]} |
%%%%      {send, Bin::binary(),state()[,Timeout]} |
%%%%      {data, Data::term()[,Timeout]} |
%%%%      {stop,reason(), Reply::term(),state()]}
%%
%%control(_Socket, _Request, _From, State) ->
%%    {reply, {error, no_control}, State}.
%%
%%%%
%%%% Handle modbus command
%%%%
%%handle_pdu(Socket, UnitID, TransID, ?READ_DISCRETE_INPUTS,
%%        <<Addr:16,N:16>>, State) ->
%%    Coils = apply(State#state.callback,read_discrete_inputs,[Addr,N]),
%%    Bin = modbus:coils_to_bin(Coils),
%%    Len = byte_size(Bin),
%%    send(Socket, TransID, State#state.proto_id, UnitID,
%%        ?READ_DISCRETE_INPUTS, <<Len, Bin/binary>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?READ_COILS,
%%        <<Addr:16,N:16>>, State) ->
%%    Coils = apply(State#state.callback,read_coils,[Addr,N]),
%%    Bin = modbus:coils_to_bin(Coils),
%%    Len = byte_size(Bin),
%%    send(Socket, TransID, State#state.proto_id, UnitID,
%%        ?READ_COILS, <<Len, Bin/binary>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?WRITE_SINGLE_COIL,
%%        <<Addr:16,Value:16>>, State) ->
%%    Value1 = apply(State#state.callback, write_single_coil, [Addr,Value]),
%%    send(Socket, TransID, State#state.proto_id, UnitID,
%%        ?WRITE_SINGLE_COIL, <<Addr:16, Value1:16>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?WRITE_MULTIPLE_COILS,
%%        <<Addr:16, N:16, _M, Data/binary>>, State) ->
%%    Coils = modbus:bits_to_coils(N, Data),
%%    N1 = apply(State#state.callback, write_multiple_coils, [Addr,Coils]),
%%    send(Socket, TransID, State#state.proto_id, UnitID,
%%        ?WRITE_MULTIPLE_COILS, <<Addr:16, N1:16>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?READ_INPUT_REGISTERS,
%%        <<Addr:16,N:16>>, State) ->
%%    Regs = apply(State#state.callback, read_input_registers, [Addr,N]),
%%    RegData = << <<Reg:16>> || Reg <- Regs >>,
%%    Len = byte_size(RegData),
%%    send(Socket, TransID, State#state.proto_id,UnitID,
%%        ?READ_INPUT_REGISTERS,<<Len, RegData/binary>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?READ_HOLDING_REGISTERS,
%%        <<Addr:16,N:16>>, State) ->
%%    Regs = apply(State#state.callback, read_holding_registers, [Addr,N]),
%%    RegData = << <<Reg:16>> || Reg <- Regs >>,
%%    Len = byte_size(RegData),
%%    send(Socket, TransID, State#state.proto_id,UnitID,
%%        ?READ_HOLDING_REGISTERS,<<Len, RegData/binary>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?WRITE_SINGLE_HOLDING_REGISTER,
%%        <<Addr:16,Value:16>>, State) ->
%%    Value1 = apply(State#state.callback, write_single_holding_register,
%%        [Addr,Value]),
%%    send(Socket, TransID, State#state.proto_id,  UnitID,
%%        ?WRITE_SINGLE_HOLDING_REGISTER, <<Addr:16, Value1:16>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, ?WRITE_MULTIPLE_HOLDING_REGISTERS,
%%        <<Addr:16,_N:16,_M,Data/binary>>, State) ->
%%    Values = [ V ||  <<V:16>> <= Data ], %% check M? and check N!
%%    N1 = apply(State#state.callback, write_multiple_holding_registers,
%%        [Addr,Values]),
%%    send(Socket,TransID, State#state.proto_id,UnitID,
%%        ?WRITE_MULTIPLE_HOLDING_REGISTERS,<<Addr:16, N1:16>>),
%%    State;
%%handle_pdu(Socket, UnitID, TransID, Func,<<_/binary>>, State) ->
%%    send(Socket,TransID,State#state.proto_id, UnitID,
%%        16#80 + (Func band 16#7f), <<?ILLEGAL_FUNCTION>>).
%%
%%send(Socket,TransID,ProtoID,UnitID,Func,Bin) ->
%%    Length  = byte_size(Bin) + 2,
%%    Data    = <<TransID:16, ProtoID:16, Length:16, UnitID, Func, Bin/binary>>,
%%    exo_socket:send(Socket, Data).
