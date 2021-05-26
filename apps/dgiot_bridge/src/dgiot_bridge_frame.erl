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

-module(dgiot_bridge_frame).
-author("kenneth").
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
%% API
-export([do/3]).

do(_, _, []) -> ok;
do(ChannelId, ProductId, [Frame | Frames]) ->
    do_message(ChannelId, ProductId, Frame),
    do(ChannelId, ProductId, Frames).


do_message(ChannelId, ProductId, Frame) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {error, not_find} ->
            ?LOG(warning,"Product[~s] not find!", [ProductId]);
        {ok, Product} ->
            Thing = maps:get(<<"thing">>, Product, #{}),
            Props = maps:get(<<"properties">>, Thing, []),
            Fun =
                fun(Key, Msg) ->
                    case maps:get(<<"addr">>, Frame, no) of
                        no ->
                            ok;
                        Addr ->
                            Topic = <<"thing/", ProductId/binary, "/", Addr/binary, "/post">>,
                            dgiot_mqtt:publish(ChannelId,Topic,jsx:encode(Msg)),
                            dgiot_bridge:send_log(ChannelId, ProductId, Addr, "~s", [jsx:encode(Msg)]),
                            dgiot_data:insert(?ETS, {ProductId, Addr, Key}, Msg)
                    end
                end,
            do_message(ChannelId, ProductId, Frame, Props, Fun)
    end.

do_message(_ChannelId, _ProductId, _Frame, [], _) -> ok;
do_message(ChannelId, ProductId, #{<<"property">> := Property}, Props, Fun) ->
    do_message(ChannelId, ProductId, Property, Props, Fun);
do_message(ChannelId, ProductId, Frame, [#{<<"identifier">> := Key} = Prop | Props], Fun) ->
    case maps:get(Key, Frame, no) of
        no ->
            ok;
        Value ->
            Msg = Prop#{
                <<"value">> => Value,
                <<"time">> => dgiot_datetime:nowstamp()
            },
            Fun(Key, Msg)
    end,
    do_message(ChannelId, ProductId, Frame, Props, Fun).
