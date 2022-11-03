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

-module(dgiot_product_channel).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-export([do_td_message/1, save_channel/1, save_tdchannel/1, save_taskchannel/1, get_channel/1, get_tdchannel/1, get_taskchannel/1]).

-type(result() :: any()).   %% todo 目前只做参数检查，不做结果检查

%% 发消息通知td 重载超级表、字段
do_td_message(ProfuctId) ->
    ChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
    dgiot_channelx:do_message(ChannelId, {sync_product, <<"Product">>, ProfuctId}).

-spec save_tdchannel(binary()) -> result().
save_tdchannel(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"channel">> := #{<<"tdchannel">> := Channel}}} ->
            dgiot_data:insert({tdchannel_product, binary_to_atom(ProductId)}, Channel);
        _ ->
            pass
    end.

-spec save_taskchannel(binary()) -> result().
save_taskchannel(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"channel">> := #{<<"taskchannel">> := Channel}}} ->
            dgiot_data:insert({taskchannel_product, binary_to_atom(ProductId)}, Channel);
        _ ->
            pass
    end.

-spec save_channel(binary()) -> result().
save_channel(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"channel">> := #{<<"otherchannel">> := [Channel | _]}}} ->
            dgiot_data:insert({channel_product, binary_to_atom(Channel)}, ProductId);
        {ok, #{<<"channel">> := #{<<"otherchannel">> := Channel}}} ->
            dgiot_data:insert({channel_product, binary_to_atom(Channel)}, ProductId);
        _ ->
            pass
    end.

-spec get_channel(binary() | atom()) -> result().
get_channel(ChannelId) when is_binary(ChannelId) ->
    get_channel(binary_to_atom(ChannelId));
get_channel(ChannelId) ->
    dgiot_data:get({channel_product, ChannelId}).

-spec get_tdchannel(binary() | atom()) -> result().
get_tdchannel(ProductId) when is_binary(ProductId) ->
    get_tdchannel(binary_to_atom(ProductId));
get_tdchannel(ProductId) ->
    dgiot_data:get({tdchannel_product, ProductId}).

-spec get_taskchannel(binary() | atom()) -> result().
get_taskchannel(ProductId) when is_binary(ProductId) ->
    get_taskchannel(binary_to_atom(ProductId));
get_taskchannel(ProductId) ->
    dgiot_data:get({taskchannel_product, ProductId}).
