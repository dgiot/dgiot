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

-module(dgiot_common_channel).
-behavior(dgiot_channelx).
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"COMM">>).
-author("kenneth").
-record(state, {id, env, product }).
%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"通用通道"/utf8>>
    },
    description => #{
        zh => <<"通用通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{}).


start(ChannelId, ChannelArgs) ->
    Size = maps:get(<<"Size">>, ChannelArgs, 5),
    MaxOverFlow = maps:get(<<"MaxOverFlow">>, ChannelArgs, 10),
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => Size,
        <<"MaxOverFlow">> => MaxOverFlow
    }).


%% 通道初始化
init(?TYPE, ChannelId, _ChannelArgs) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            State = #state{
                id = ChannelId,
                env = #{},
                product = ProductIds
            },
            {ok, State};
        {error, not_find} ->
            {stop, not_find_product}
    end.


handle_event(EventType, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventType, Event]),
    ok.

handle_message(Message, #state{ id = _ChannelId, product = _ProductId } = State) ->
    % ?LOG(info,"Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
    do_product(handle_info, [Message], State).

stop(ChannelType, ChannelId, _) ->
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


do_product(Fun, Args, #state{id = ChannelId, env = Env, product = ProductIds} = State) ->
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, Fun, Args, Env) of
        {ok, NewEnv} ->
            {ok, update_state(NewEnv, State)};
        {stop, Reason, NewEnv} ->
            {stop, Reason, update_state(NewEnv, State)};
        {reply, ProductId, Reply, NewEnv} ->
            {reply, ProductId, Reply, update_state(NewEnv, State)}
    end.


update_state(Env, State) ->
    State#state{env = Env}.


