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

-module(dgiot_edge).
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-export([is_even/1, get_writeData/2, convert_data/2, push_cloud_log/0]).

is_even(Number) ->
    case Number rem 2 of
        0 -> true;
        _ -> false
    end.

get_writeData(<<"HEX">>, Data) ->
    Len = size(Data),
    NewData =
        case dgiot_edge:is_even(Len) of
            true ->
                Data;
            _ ->
%%                最后一位补0
                L = Len - 1,
                <<H:L/binary, W/binary>> = Data,
                <<H/binary, "0", W/binary>>
        end,
    case catch dgiot_utils:hex_to_binary(NewData) of
        {_, _} ->
            NewData;
        Binary ->
            Binary
    end;

get_writeData(_, Data) ->
    Data.

convert_data(<<"ASCII">>, Data) ->
    Data;

convert_data(_, Data) ->
    case catch dgiot_utils:hex_to_binary(Data) of
        {_, _} ->
            dgiot_utils:binary_to_hex(Data);
        H when size(H) =:= 0 ->
            dgiot_utils:binary_to_hex(Data);
        _ ->
            Data
    end.

push_cloud_log() ->


    ok.
