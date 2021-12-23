%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dlt645 Protocol Processor.
-module(dlt376_proctol).
-author("gugm").

-include_lib("dgiot_meter.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    binary_to_hex/1,
    concrat_binary/2,
    encode_of_addr/2,
    decode_of_addr/1]).

% -record(di_data_A6, {di1, di2, di3, di4,di5}).


binary_to_hex(Id) ->
    <<<<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X, 16)>>.

% binary 相连接
concrat_binary(Acc, <<>>) -> Acc;
concrat_binary(Acc, <<H:1/binary, Rest/binary>>) ->
    concrat_binary(<<Acc/binary, H/binary>>, Rest).


split_head_bytes(<<Head:2/binary, Rest/binary>>) ->
    {Head, Rest}.


% 把地址转化成binary
encode_of_addr(A1, A2) ->
    AA = concrat_binary(dlt645_proctol:reverse(A1), dlt645_proctol:reverse(A2)),
    AA1 = concrat_binary(<<16#00, 16#00>>, AA),
    AA1.

%把binary转化成地址
decode_of_addr(A) ->
    {_, Rest} = split_head_bytes(A),
    {A1, A2} = split_head_bytes(Rest),
    AA = concrat_binary(dlt645_proctol:reverse(A1), dlt645_proctol:reverse(A2)),
    AA1 = concrat_binary(AA, <<16#14>>),
    AA1.
