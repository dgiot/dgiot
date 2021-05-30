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

-module(dgiot_device_utils).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([send/3, encode/1, decode/3, sock_debug/3]).
-import(dgiot_utils, [binary_to_hex/1]).



send(Frame, Fun, State) ->
    case encode(Frame) of
        {ok, Bin} ->
            case Fun(Bin) of
                ok ->
                    sock_debug(send, #{<<"frame">> => Frame, <<"status">> => ok, <<"data">> => binary_to_hex(Bin)}, State),
                    ok;
                {error, Why} ->
                    sock_debug(send, #{<<"frame">> => Frame, <<"status">> => Why, <<"data">> => binary_to_hex(Bin)}, State),
                    {error, Why}
            end;
        {ignore, NewFrame} ->
            ?LOG(warning,"Ignore send Frame:~p, State:~p~n", [NewFrame, State]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

encode(Frame) when is_map(Frame) ->
    case maps:get(<<"data">>, Frame, no) of
        no ->
            dgiot_protocol:encode(Frame);
        Data ->
            HxData = dgiot_utils:binary_to_hex(Data),
            case re:run(HxData, <<"0000180080E1">>) of
                nomatch ->
                    dgiot_protocol:encode(Frame);
                _ ->
                    {ignore, Frame}
            end
    end;
encode(Data) when is_binary(Data) ->
    {ok, Data}.

decode([], _, _State) ->
    {error, not_decode};
decode([MsgType | Other], Bin, State) ->
    case dgiot_protocol:decode(MsgType, Bin, State) of
        {ok, Rest, Messages} ->
            {ok, Rest, Messages};
        {error, _} ->
            %?LOG(error,"decode:~p, not this protocol:~p", [Bin, MsgType]),
            decode(Other, Bin, State)
    end.


sock_debug(Type, Message, State) ->
    Fun =
        case maps:get(<<"debug">>, State, no) of
            no ->
                BaseData = maps:get(<<"basedata">>, State, #{}),
                maps:get(<<"debug">>, BaseData, no);
            Fun1 ->
                Fun1
        end,
    case Fun of
        no ->
            ok;
        {Mod, Fun2, Args} ->
            apply(Mod, Fun2, Args ++ [Type, Message#{<<"state">> => State}]);
        _ ->
            Fun(Type, Message#{<<"state">> => State})
    end.
