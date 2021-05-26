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
-define(RESOURCE_TYPE, 'data_resource').
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    on_resource_create/2,
    on_get_resource_status/2,
    on_resource_destroy/2,
    on_action_create/2
]).

-export([create_resource/1]).


-define(RESOURCE_CONFIG_SPEC, #{
    channel => #{
        order => 1,
        type => string,
        required => true,
        default => <<"">>,
        title => #{
            en => <<"Channel objectId">>,
            zh => <<"资源/采集通道ID"/utf8>>
        },
        description => #{
            en => <<"Channel objectId">>,
            zh => <<"资源/采集通道ID"/utf8>>}
    }
}).

-define(ACTION_PARAM_RESOURCE, #{
    type => string,
    required => true,
    title => #{
        en => <<"Resource ID">>,
        zh => <<"资源/采集通道ID"/utf8>>
    },
    description => #{
        en => <<"Bind a resource to this action">>,
        zh => <<"给动作绑定一个资源/采集通道"/utf8>>
    }
}).

-rule_action(#{
    name => data_to_resource,
    for => 'message.publish',
    types => [?RESOURCE_TYPE],
    create => on_action_create,
    params => #{'$resource' => ?ACTION_PARAM_RESOURCE},
    title => #{
        en => <<"Data bridge to Data Resource">>,
        zh => <<"桥接数据到资源/采集通道"/utf8>>
    },
    description => #{
        en => <<"Bridge Data to Data Resource">>,
        zh => <<"桥接数据到资源/采集通道"/utf8>>
    }
}).

-resource_type(#{
    name => ?RESOURCE_TYPE,
    create => on_resource_create,
    status => on_get_resource_status,
    destroy => on_resource_destroy,
    params => ?RESOURCE_CONFIG_SPEC,
    title => #{
        en => <<"Data Resource">>,
        zh => <<"资源/采集通道"/utf8>>
    },
    description => #{
        en => <<"Data Resource">>,
        zh => <<"资源/采集通道"/utf8>>
    }
}).


on_resource_create(ResId, #{<<"channel">> := ObjectId}) ->
    application:ensure_all_started(dgiot_parse),
    case dgiot_parse:get_object(<<"Channel">>, ObjectId) of
        {ok, Channel} ->
%%            ?LOG(info,"on_resource_create ~p,~p", [ResId, Channel]),
            #{<<"channel">> => Channel};
        {error, #{<<"code">> := 101, <<"error">> := <<"Object not found.">>}} ->
            case catch emqx_rule_registry:remove_resource(ResId) of
                {'EXIT', {{throw, {dependency_exists, {rule, RuleId}}}, _}} ->
                    ok = emqx_rule_registry:remove_rule(RuleId),
                    ok = emqx_rule_registry:remove_resource(ResId);
                _ ->
                    ok
            end,
            #{<<"channel">> => false};
        {error, Reason} ->
            ?LOG(error,"Resource create error, Channel:~s, Reason:~p", [ObjectId, Reason]),
            #{<<"channel">> => false}
    end.

on_get_resource_status(_ResId, #{<<"channel">> := Channel}) ->
    case Channel of
        false ->
            #{is_alive => false};
        _ ->
            #{is_alive => true}
    end.

on_resource_destroy(ResId, Params) ->
    ?LOG(info,"on_resource_destroy ~p,~p", [ResId, Params]),
    ok.

on_action_create(Id, #{<<"channel">> := #{<<"cType">> := CType, <<"objectId">> := ChannelId}} = Params) ->
    ?LOG(info,"on_action_create ~p,~p", [Id, Params]),
    fun(Msg, Env) ->
        dgiot_channelx:do_message(CType, ChannelId, {rule, Msg, Env}, 60000)
    end.



create_resource(ChannelId) ->
    Args = #{
        id => <<"resource:", ChannelId/binary>>,
        type => ?RESOURCE_TYPE,
        config => #{<<"channel">> => ChannelId},
        description => ChannelId
    },
    try emqx_rule_engine:create_resource(Args) of
        {ok, Resource} ->
            {ok, Resource};
        {error, {resource_type_not_found, Type}} ->
            {error, {resource_type_not_found, Type}}
    catch
        throw:{resource_type_not_found, Type} ->
            {error, {resource_type_not_found, Type}};
        throw:{init_resource_failure, Reason} ->
            {error, {init_resource_failure, Reason}};
        throw:Reason ->
            {error, Reason};
        _Error:Reason:_StackT ->
            {error, Reason}
    end.
