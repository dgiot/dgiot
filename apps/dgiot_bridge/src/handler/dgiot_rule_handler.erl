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

-module(dgiot_rule_handler).
-author("dgiot").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("emqx_rule_engine/include/rule_engine.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(ERR_UNKNOWN_COLUMN(COLUMN), list_to_binary(io_lib:format("Unknown Column: ~s", [(COLUMN)]))).
-define(ERR_NO_ACTION(NAME), list_to_binary(io_lib:format("Action ~s Not Found", [(NAME)]))).
-define(ERR_NO_RESOURCE(RESID), list_to_binary(io_lib:format("Resource ~s Not Found", [(RESID)]))).
-define(ERR_NO_HOOK(HOOK), list_to_binary(io_lib:format("Hook ~s Not Found", [(HOOK)]))).
-define(ERR_NO_RESOURCE_TYPE(TYPE), list_to_binary(io_lib:format("Resource Type ~s Not Found", [(TYPE)]))).
-define(ERR_BADARGS(REASON), list_to_binary(io_lib:format("Bad Arguments: ~0p", [REASON]))).

%% API
-export([swagger_rule/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/rule">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_rule.json">>, ?MODULE, [], priv)
swagger_rule() ->
    [
        dgiot_http_server:bind(<<"/swagger_rule.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            ?LOG(error, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> list_to_binary(io_lib:format("~p", [Reason]))
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================


%% Rule 概要: 获取acl编辑器提示语
%% OperationId:get_provider
%% 请求:GET /iotapi/provider}

do_request(get_provider, #{<<"language">> := Language}, _Context, _Req) ->
    {ok, get_dictLanguage(Language)};


%% Rule 概要: 获取规则引擎 描述:获取规则引擎
%% OperationId:get_rule_id
%% 请求:GET /iotapi/rule/:{id}
do_request(get_rule_id, #{<<"id">> := RuleID}, _Context, _Req) ->
    emqx_rule_engine_api:show_rule(#{id => RuleID}, []);


%% Rule 概要: 修改规则引擎 描述:修改规则引擎
%% OperationId:put_rules_id
%% 请求:PUT /iotapi/rule/:{id}
do_request(put_rule_id, #{<<"id">> := RuleID} = Params, _Context, _Req) ->
    emqx_rule_engine_api:update_rule(#{id => RuleID}, maps:to_list(maps:without([<<"id">>],Params)));

%% Rule 概要: 删除规则引擎 描述:删除规则引擎
%% OperationId:delete_rule_id
%% 请求:DELETE /iotapi/rule/:{id}
do_request(delete_rule_id, #{<<"id">> := RuleID}, _Context, _Req) ->
    emqx_rule_engine_api:delete_rule(#{id => RuleID}, []);

%% Rule 概要: 测试规则引擎 描述:测试规则引擎
do_request(post_rules, #{<<"test">> := <<"true">>} = Params, _Context, _Req) ->
    emqx_rule_engine_api:create_rule(#{}, maps:to_list(Params));
%% Rule 概要: 创建规则引擎 描述:创建规则引擎
%% OperationId:post_rules
%% 请求:POST /iotapi/rules
do_request(post_rules, Params, _Context, _Req) ->
    Actions = maps:get(<<"actions">>, Params),
    NewActions = lists:foldl(fun(X, Acc) ->
        ?LOG(error, "X ~p ", [X]),
        #{<<"params">> := #{<<"$resource">> := Resource}} = X,
        <<"resource:", Channel/binary>> = Resource,
        emqx_rule_engine_api:create_resource(#{},
            [
                {<<"id">>, Resource},
                {<<"type">>, <<"dgiot_resource">>},
                {<<"config">>, [{<<"channel">>, Channel}]},
                {<<"description">>, Resource}]),
        Acc ++ [X]
                             end, [], Actions),
    ?LOG(error, "Params ~p ", [Params#{<<"actions">> => NewActions}]),
    R = emqx_rule_engine_api:create_rule(#{}, maps:to_list(Params#{<<"actions">> => NewActions})),
    R;

%% Rule 概要: 获取规则引擎列表 描述:获取规则引擎列表
%% OperationId:get_rules
%% 请求:GET /iotapi/rules
do_request(get_rules, _Args, _Context, _Req) ->
    emqx_rule_engine_api:list_rules(#{}, []);

%% OperationId:get_actions
do_request(get_actions, Args, _Context, _Req) ->
    ?LOG(info, "~p", [Args]),
%%    {ok, #{data := Data} = Result} = emqx_rule_engine_api:list_actions(#{}, []),
%%    NewData =
%%        lists:foldl(fun(X, Acc) ->
%%            case X of
%%                #{<<"app">> := <<"dgiot_bridge">>} ->
%%                    Acc ++ [X#{<<"dgiot_channel">> => get_channel()}];
%%                _ ->
%%                    Acc ++ [X]
%%
%%            end
%%                    end, [], Data),
%%    ?LOG(error, "NewData ~p ", [NewData]),
%%    {ok, Result#{data => NewData}};
    emqx_rule_engine_api:list_actions(#{}, []);

do_request(get_actions_id, #{<<"id">> := RuleID}, _Context, _Req) ->
    emqx_rule_engine_api:show_action(#{id => RuleID}, []);

do_request(get_resources, _Args, _Context, _Req) ->
    {ok, #{data := Data} = Result} = emqx_rule_engine_api:list_resources(#{}, []),
    {ok, Result#{data => get_channel(Data)}};

%% OperationId:post_rule_resource
do_request(post_resources, Params, _Context, _Req) ->
    ?LOG(info, "Params ~p ", [Params]),
    Actions = maps:get(<<"actions">>, Params),
    ?LOG(info, "Actions ~p ", [Actions]),
    lists:map(fun(Action) ->
        ?LOG(info, "Action ~p ", [Action])
              end, Actions),
    emqx_rule_engine_api:create_resource(#{}, maps:to_list(Params));

do_request(get_resources_id, #{<<"id">> := ResId}, _Context, _Req) ->
    emqx_rule_engine_api:show_resource(#{id => ResId}, []);

do_request(delete_resources_id, #{<<"id">> := Id}, _Context, _Req) ->
    emqx_rule_engine_api:delete_resource(#{id => Id}, #{});

do_request(get_resource_types, _Args, _Context, _Req) ->
    Resources = dgiot_bridge:get_all_channel(),
    {200, Resources};


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

%%save_rule_to_dict(Key, Params) ->
%%    Dict = #{
%%        <<"key">> => Key,
%%        <<"type">> => <<"ruleengine">>,
%%        <<"data">> => #{<<"rule">> => jsx:encode(Params)}
%%    },
%%    #{<<"objectId">> := ObjectId} = dgiot_parse:get_objectid(<<"Dict">>, Dict),
%%    case dgiot_parse:create_object(<<"Dict">>, Dict#{<<"objectId">> => ObjectId}) of
%%        {ok, #{<<"objectId">> := ObjectId}} ->
%%            ?LOG(info,"ObjectId ~p",[ObjectId]),
%%            {ok, #{<<"objectId">> => ObjectId}};
%%        {error, Reason1} ->
%%            ?LOG(info,"Reason1 ~p",[Reason1]),
%%            {error, Reason1}
%%    end.

get_channel(Data) ->
    ?LOG(error, "~p", [Data]),
    case dgiot_parse:query_object(<<"Channel">>, #{<<"keys">> => [<<"name">>]}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun(#{<<"objectId">> := ChannelId,<<"name">> := Name}, Acc) ->
                Acc ++ [#{
                    <<"config">> => #{<<"channel">> => ChannelId},
                    <<"description">> => Name,
                    <<"id">> => <<"resource:", ChannelId/binary>>,
                    <<"status">> => true,
                    <<"type">> => <<"dgiot_resource">>
                }]
                        end, [], Results);
        _ -> []
    end.

%% Acc ++ [maps:without([<<"applicationtText">>,<<"description">>,<<"enable">>,<<"templateId">>,<<"templateName">>,<<"templateTypekey">>],Data)]  %% without过滤掉不需要的字段
%%Acc ++ [maps:with([<<"name">>, <<"value">>, <<"caption">>, <<"meta">>, <<"type">>, <<"score">>], Data)] %% with 只取需要的字段
%% ;结尾是分支 .结尾是结束
get_dictLanguage(Language) ->
    Type = dgiot_parse:get_dictid(Language, <<"dict_template">>),
    case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"type">> => Type}}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun(#{<<"data">> := Data}, Acc) ->
                Acc ++ [maps:with([<<"name">>, <<"value">>, <<"caption">>, <<"meta">>, <<"type">>, <<"score">>], Data)]
                        end, [], Results);
        _ -> []
    end.

