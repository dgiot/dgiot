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

-module(dgiot_protocol).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([start/0, lookup/1, add/1, add/2, decode/3, encode/1]).

start() ->
    dgiot_plugin:check_module(
        fun({_App, _Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) =/= false of
                true ->
                    case lists:keyfind(protocol, 1, Mod:module_info(attributes)) of
                        false ->
                            Acc;
                        {protocol, MsgTypes} ->
                            lists:foldl(
                                fun(MsgType, Acc1) ->
                                    [add(MsgType, Mod) | Acc1]
                                end, Acc, MsgTypes)
                    end;
                false ->
                    Acc
            end
        end, []).


lookup(MsgType) ->
    case dgiot_data:lookup({MsgType, decoder}) of
        {ok, Mod} ->
            {ok, Mod};
        {error, Reason} ->
            {error, Reason}
    end.


% 新增解码器
add(Mod) when is_atom(Mod) ->
    [add(MsgType, Mod) || {protocol, MsgTypes} <- Mod:module_info(attributes), MsgType <- MsgTypes].
add(MsgType, Mod) ->
    dgiot_data:insert({MsgType, decoder}, Mod),
    dgiot_hook:add({MsgType, parse_frame}, {Mod, parse_frame}),
    dgiot_hook:add({MsgType, to_frame}, {Mod, to_frame}),
    {MsgType, Mod}.


decode(MsgType, Buff, Env) ->
    case dgiot_hook:run_hook({MsgType, parse_frame}, [Buff, Env]) of
        {ok, []} ->
            {ok, <<>>, []};
        {ok, [{<<>>, []}]} ->
            {error, {MsgType, maybe_not_this_type}};
        {ok, [{Rest, Messages} | _]} ->
            {ok, Rest, Messages};
        {error, not_find} ->
            {error, {MsgType, not_find_decoder}}
    end.


encode(Msg) ->
    case maps:get(<<"msgtype">>, Msg, not_find) of
        not_find ->
            ?LOG(warning,"encode msg error, msgtype not find,~p~n", [Msg]),
            {error, msgtype_not_find};
        MsgType ->
            case dgiot_hook:run_hook({MsgType, to_frame}, [Msg]) of
                {ok, [Payload | _]} ->
                    {ok, Payload};
                {ok, []} ->
                    {error, {MsgType, not_find_encoder}};
                {error, not_find} ->
                    {error, {MsgType, not_find_encoder}}
            end
    end.
