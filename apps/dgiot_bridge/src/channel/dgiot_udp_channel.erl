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

-module(dgiot_udp_channel).
-behavior(dgiot_channelx).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_bridge.hrl").
-define(TYPE, <<"UDP">>).
-author("kenneth").
-record(state, {id, ip, port, transport, env, product, log}).
%% API
-export([start/2]).
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-define(SOCKOPTS, [binary, {reuseaddr, true}]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"UDP采集通道"/utf8>>
    },
    description => #{
        zh => <<"UDP采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 3456,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/UdpIcon.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    ok = esockd:start(),
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).


%% 通道初始化
init(?TYPE, ChannelId, #{<<"port">> := Port} = _ChannelArgs) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            State = #state{
                id = ChannelId,
                env = #{},
                product = ProductIds
            },
%%            Name = dgiot_channelx:get_name(?TYPE, ChannelId),
%%            MFArgs = {?MODULE, start_link, [State]},
%%            ChildSpec = esockd:udp_child_spec(binary_to_atom(Name, utf8), Port, [{udp_options, ?SOCKOPTS}], MFArgs),
            ChildSpec = dgiot_udp_worker:child_spec(Port, State),
            {ok, State, ChildSpec};
        {error, not_find} ->
            {stop, not_find_product}
    end;

init(?TYPE, _ChannelId, _Args) ->
    io:format("~s ~p _Args: ~p~n", [?FILE, ?LINE, _Args]),
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(Message, #state{id = ChannelId, product = ProductId} = State) ->
    ?LOG(info, "Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
    do_product(handle_info, [Message], State).

stop(ChannelType, ChannelId, _) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
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
    State#state{env = maps:without([<<"send">>], Env)}.




