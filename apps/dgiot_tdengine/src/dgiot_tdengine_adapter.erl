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

-module(dgiot_tdengine_adapter).
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("jonliu").

%% API
-export([save/1, save/2, save/3, do_save/2, do_save/4]).

save(Product, Devaddr, Msg) when is_map(Msg) ->
    do_channel(Product,
        fun(Channel) ->
            do_save(Channel, Product, Devaddr, Msg)
        end).

save(Product, Msg) when is_map(Msg) ->
    do_channel(Product,
        fun(Channel) ->
            do_save(Channel, Msg#{<<"productId">> => Product})
        end).

save(#{<<"product">> := Product} = Msg) ->
    case format(Msg) of
        ignore ->
            ignore;
        {ok, Batch} ->
            save(Product, Batch);
        {error, Reason} ->
            ?LOG(warning, "~p format error ~p!", [Msg, Reason]),
            {error, Reason}
    end.

do_save(Channel, Product, Devaddr, Msg) ->
    dgiot_channelx:do_message(?TYPE, Channel, {data, Product, Devaddr, Msg, #{}}, 30000).

do_save(Channel, Msg) ->
    dgiot_channelx:do_message(?TYPE, Channel, {data, Msg, #{}}, 30000).

do_channel(Product, Fun) ->
    case dgiot_data:lookup({Product, ?TYPE}) of
        {ok, Channel} ->
            Fun(Channel);
        {error, not_find} ->
            ?LOG(warning, "~s not find tdengine channel!", [Product]),
            {error, not_find_tdengine}
    end.

format(#{
    <<"devaddr">> := DevAddr,
    <<"dtuaddr">> := DtuAddr,
    <<"product">> := Product,
    <<"thing">> := Things
}) ->
    Data = format_thing(Things),
    case maps:size(Data) == 0 of
        true ->
            ignore;
        false ->
            {ok, Data#{
                <<"dtuaddr">> => DtuAddr,
                <<"product">> => Product,
                <<"addr">> => DevAddr
            }}
    end;
format(_) ->
    {error, not_match}.

format_thing(Things) -> format_thing(Things, #{}).
format_thing([], Acc) -> Acc;
format_thing([#{<<"value">> := Value, <<"identifier">> := Id} | Other], Acc) ->
    format_thing(Other, Acc#{Id => Value}).


