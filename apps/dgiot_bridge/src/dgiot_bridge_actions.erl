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

-module(dgiot_bridge_actions).
-author("zwx").
-include_lib("dgiot/include/logger.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx_rule_engine/include/rule_engine.hrl").
-include_lib("emqx_rule_engine/include/rule_actions.hrl").
-define(RESOURCE_TYPE, 'dgiot_resource').

-define(RESOURCE_CONFIG_SPEC, #{
    channel => #{
        order => 1,
        type => string,
        required => true,
        default => <<"">>,
        title => #{
            en => <<"DGIOT Channel ID">>,
            zh => <<"数蛙通道ID"/utf8>>
        },
        description => #{
            en => <<"DGIOT Channel ID">>,
            zh => <<"数蛙通道ID"/utf8>>}
    }
}).

-resource_type(#{
    name => ?RESOURCE_TYPE,
    provider => dgiot,
    create => on_resource_create,
    status => on_get_resource_status,
    destroy => on_resource_destroy,
    params => ?RESOURCE_CONFIG_SPEC,
    title => #{en => <<"DGIOT Channel">>, zh => <<"数蛙通道"/utf8>>},
    description => #{en => <<"MQTT message to  DGIOT Channel">>, zh => <<"MQTT消息桥接到数蛙通道"/utf8>>}
}).


-rule_action(#{name => dgiot,
    category => data_forward,
    for => '$any',
    types => [],
    create => on_action_create_dgiot,
    params => ?RESOURCE_CONFIG_SPEC,
    title => #{en => <<"DGIOT Channel">>,
        zh => <<"数蛙通道"/utf8>>},
    description => #{en => <<"Republish a MQTT message to DGIOT Channel">>,
        zh => <<"重新发布消息到数蛙通道"/utf8>>}
}).


-export([
    on_resource_create/2,
    on_get_resource_status/2,
    on_resource_destroy/2
]).

%% callbacks for rule engine
-export([on_action_create_dgiot/2
    , on_action_dgiot/2
]).

-spec(on_resource_create(binary(), map()) -> map()).
on_resource_create(ResId, #{<<"channel">> := _ObjectId} = Params) ->
    ?LOG(error, "on_resource_destroy ~p,~p", [ResId, Params]),
    #{<<"channel">> => false}.
%%    application:ensure_all_started(dgiot_parse),
%%    case dgiot_parse:get_object(<<"Channel">>, ObjectId) of
%%        {ok, Channel} ->
%%            #{<<"channel">> => Channel};
%%        {error, #{<<"code">> := 101, <<"error">> := <<"Object not found.">>}} ->
%%            case catch emqx_rule_registry:remove_resource(ResId) of
%%                {'EXIT', {{throw, {dependency_exists, {rule, RuleId}}}, _}} ->
%%                    ok = emqx_rule_registry:remove_rule(RuleId),
%%                    ok = emqx_rule_registry:remove_resource(ResId);
%%                _ ->
%%                    ok
%%            end,
%%            #{<<"channel">> => false};
%%        {error, Reason} ->
%%            ?LOG(error, "Resource create error, Channel:~s, Reason:~p", [ObjectId, Reason]),
%%            #{<<"channel">> => false}
%%    end.

on_get_resource_status(ResId, #{<<"channel">> := Channel} = Params) ->
    ?LOG(error, "on_resource_destroy ~p,~p", [ResId, Params]),
    case Channel of
        false ->
            #{is_alive => false};
        _ ->
            #{is_alive => true}
    end.

on_resource_destroy(ResId, Params) ->
    ?LOG(error, "on_resource_destroy ~p,~p", [ResId, Params]),
    ok.

%%------------------------------------------------------------------------------
%% Action 'republish'
%%------------------------------------------------------------------------------
-spec on_action_create_dgiot(action_instance_id(), Params :: map()) -> {bindings(), NewParams :: map()}.
on_action_create_dgiot(Id, #{<<"channel">> := #{<<"cType">> := CType, <<"objectId">> := ChannelId}} = Params) ->
    ?LOG(error, "Id ~p", [Id, Params]),
    fun(Msg, Env) ->
        dgiot_channelx:do_message(CType, ChannelId, {rule, Msg, Env}, 60000)
    end.

on_action_dgiot(Selected, _Envs) ->
    ?LOG(debug, "[republish] republish to, Payload: ~p", [Selected]).

