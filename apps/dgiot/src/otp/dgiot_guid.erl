%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_guid).
-author("dgiot").
-export([gen/0, new/0, to_hexstr/1]).

-define(TO_HEX(X), (X >= 10 andalso X =< 15 andalso X - 10 + $a) orelse X + $0).

gen() ->
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> =
        crypto:strong_rand_bytes(12),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>.

new() ->
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> =
        crypto:strong_rand_bytes(12),
    {I1, I2, I3}.

to_hexstr(Guid) when is_tuple(Guid) ->
    {A, B, C} = Guid,
    to_hexstr(<<A:32, B:32, C:32>>);
to_hexstr(Guid) when is_binary(Guid) ->
    << <<(hex(H)), (hex(L))>> || <<H:4, L:4>> <= Guid >>.

hex(C) when C >= 0, C =< 9 -> C + $0;
hex(C) when C >= 10, C =< 15 -> C - 10 + $a.
