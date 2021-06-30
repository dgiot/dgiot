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

-module(modbus_demo_callback).

-export([read_discrete_inputs/2,
    read_coils/2,
    write_single_coil/2,
    write_multiple_coils/2,
    read_input_registers/2,
    read_holding_registers/2,
    write_single_holding_register/2,
    write_multiple_holding_registers/2]).

read_discrete_inputs(Addr,N) ->
    io:format("demo_callback: read_discrete_inputs: addr=~w, n=~w\n", [Addr,N]),
    [ (rand:uniform(2)-1) || _ <- lists:seq(1, N)].

read_coils(Addr,N) ->
    io:format("demo_callback: read_coils: addr=~w, n=~w\n", [Addr,N]),
    [ (rand:uniform(2)-1) || _ <- lists:seq(1, N)].

write_single_coil(Addr, Value) ->
    io:format("demo_callback: write_single_coil: addr=~w, value=~w\n",
        [Addr,Value]),
    Value.

write_multiple_coils(Addr,BitList) ->
    io:format("demo_callback: write_multiple_coils: addr=~w, values=~w\n",
        [Addr,BitList]),
    length(BitList).

read_input_registers(Addr, N) ->
    io:format("demo_callback: read_input_registers: addr=~w, n=~w\n", [Addr,N]),
    [ (rand:uniform(16#10000)-1) || _ <- lists:seq(1, N)].

read_holding_registers(Addr, N) ->
    io:format("demo_callback: read_holding_registers: addr=~w, n=~w\n",
        [Addr,N]),
    [ (rand:uniform(16#10000)-1) || _ <- lists:seq(1, N)].

write_single_holding_register(Addr, Value) ->
    io:format("demo_callback: write_single_holding_register: addr=~w, value=~w\n", [Addr,Value]),
    Value.

write_multiple_holding_registers(Addr, Values) ->
    io:format("demo_callback: write_multiple_holding_registers: addr=~w, values=~w\n", [Addr,Values]),
    length(Values).
