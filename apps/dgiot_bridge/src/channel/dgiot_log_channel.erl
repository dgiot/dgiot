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
-module(dgiot_log_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"LOG">>).
-record(state, {id, env}).
%% API
-export([
    start/2
]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

-channel(?TYPE).
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"LOG资源通道"/utf8>>
    },
    description => #{
        zh => <<"LOG资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/log.png">>,
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
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{}).

%% 通道初始化
init(?TYPE, ChannelId, Args) ->
    State = #state{id = ChannelId, env = Args},
    {ok, State, []}.

handle_init(State) ->
    dgiot_mqtt:subscribe(<<"dashboard_task/#">>),
%%    io:format("~s ~p topics ~n ", [?FILE, ?LINE]),
    timer:sleep(100),
    dgiot_mqtt:subscribe(<<"dgiot_topics/#">>),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message({deliver, _, Msg}, State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case catch jsx:decode(Payload, [{labels, binary}, return_maps]) of
        {'EXIT', Reason} ->
            ?LOG(error, "Payload:~p, error:~p", [Payload, Reason]);
        Message ->
            case re:run(Topic, <<"[^//]+">>, [{capture, all, binary}, global]) of
                {match, [[<<"dashboard_task">>], [_DashboardId]]} ->
                    supervisor:start_child(dashboard_task, [Message]);
                {match, [[<<"dgiot_topics">>], [SessionToken]]} ->
                    subscribe_topic(SessionToken, Message);
                _ ->
                    ?LOG(error, "~p, Payload:~p", [Topic, Payload])
            end
    end,
    {noreply, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    dgiot_mqtt:unsubscribe(<<"dashboard_task/#">>),
    dgiot_mqtt:unsubscribe(<<"dgiot_topics/#">>),
    ok.

subscribe_topic(SessionToken, #{<<"topic">> := Topic, <<"topickey">> := TopicKey}) ->
    case is_list(Topic) of
        true ->
            case dgiot_data:get({page_router_key, SessionToken, TopicKey}) of
                not_find ->
                    pass;
                OldTopic ->
                    lists:foldl(
                        fun(X, _Acc) ->
%%                            dgiot_mqtt:unsubscribe(SessionToken, X),
                            emqx_mgmt:unsubscribe(SessionToken, X),
                            []
                        end, [], OldTopic
                    )
            end,
            lists:foldl(
                fun(X, _Acc) ->
%%                    dgiot_mqtt:subscribe(SessionToken, X),
                    emqx_mgmt:subscribe(SessionToken, [{X, #{qos => 0}}]),
                    []
                end, [], Topic
            ),
            dgiot_data:insert({page_router_key, SessionToken, TopicKey}, Topic);
        false ->
            case dgiot_data:get({page_router_key, SessionToken, TopicKey}) of
                not_find ->
                    pass;
                OldTopic ->
                    emqx_mgmt:unsubscribe(SessionToken, [{OldTopic, #{qos => 0}}])
            end,
%%            io:format("~s ~p SessionToken ~p Topic ~p ~n ", [?FILE, ?LINE, SessionToken, Topic]),
            emqx_mgmt:subscribe(SessionToken, [{Topic, #{qos => 0}}]),
%%            dgiot_mqtt:subscribe(SessionToken, Topic),
            dgiot_data:insert({page_router_key, SessionToken, TopicKey}, Topic)
    end;

subscribe_topic(_SessionToken, _) ->
    pass.
