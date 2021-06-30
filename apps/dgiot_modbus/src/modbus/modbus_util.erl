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

-module(modbus_util).
-export([
    binary_to_coils/1,
    binary_to_int16/1,
    binary_to_int16s/1,
    binary_to_int32/1,
    binary_to_int32s/1,
    binary_to_float32/1,
    binary_to_ascii/1,
    coils_to_binary/1,
    int16_to_binary/1
]).

%% @doc Function to convert bytes to coils.
%% @end
-spec binary_to_coils(Bin::binary()) -> [0|1].
binary_to_coils(Bin) ->
    lists:append([ lists:reverse([ Y || <<Y:1>> <= <<X>>]) || <<X:8>> <= Bin]).

%% @doc Function to convert bytes to 16bits integer.
%% @end
-spec binary_to_int16(Bin::binary()) -> [integer()].
binary_to_int16(Bin) ->
    [ X || <<X:16/integer>> <= Bin ].

%% @doc Function to convert bytes to 16bits signed integer.
%% @end
-spec binary_to_int16s(Bin::binary()) -> [integer()].
binary_to_int16s(Bin) ->
    [ X || <<X:16/signed-integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits integer.
%% @end
-spec binary_to_int32(Bin::binary()) -> [integer()].
binary_to_int32(Bin) ->
    [ X || <<X:32/integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits signed integer.
%% @end
-spec binary_to_int32s(Bin::binary()) -> [integer()].
binary_to_int32s(Bin) ->
    [ X || <<X:32/signed-integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits float number.
%% @end
-spec binary_to_float32(Bin::binary()) -> [float()].
binary_to_float32(Bin) ->
    [ X || <<X:32/float>> <= Bin ].

%% @doc Function to convert bytes to ASCII.
%% @end
-spec binary_to_ascii(Bin::binary()) -> list().
binary_to_ascii(Bin) ->
    erlang:binary_to_list(Bin).

%% @doc Function to convert a list of coils to binary.
%% @end
-spec coils_to_binary(Values::list()) -> binary().
coils_to_binary(Values) ->
    coils_to_binary(Values, <<>>).

coils_to_binary([], Acc) ->
    Acc;
coils_to_binary([B0, B1, B2, B3, B4, B5, B6, B7 | T], Acc) ->
    coils_to_binary(T, <<Acc/binary, B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>);
coils_to_binary(Values, Acc) ->
    coils_to_binary(Values ++ [0], Acc).

%% @doc Function to convert a list of 16bits integer to binary.
%% @end
-spec int16_to_binary(Values::list()) -> binary().
int16_to_binary(Values) ->
    << <<X:16>> || X <- Values >>.
