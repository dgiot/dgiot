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
-include("dgiot_bridge.hrl").
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
-export([handle/4, sysc_rules/0, save_rule_to_dict/2, device_sql/4]).

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
    save_rule_to_dict(RuleID, Params),
    emqx_rule_engine_api:update_rule(#{id => RuleID}, maps:to_list(maps:without([<<"id">>], Params)));

%% Rule 概要: 删除规则引擎 描述:删除规则引擎
%% OperationId:delete_rule_id
%% 请求:DELETE /iotapi/rule/:{id}
do_request(delete_rule_id, #{<<"id">> := RuleID}, _Context, _Req) ->
    ObjectId = dgiot_parse_id:get_dictid(RuleID, <<"ruleengine">>, <<"Rule">>, <<"Rule">>),
    dgiot_parse:del_object(<<"Dict">>, ObjectId),
    dgiot_data:delete(?DGIOT_RUlES, RuleID),
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
        #{<<"params">> := #{<<"$resource">> := Resource}} = X,
        <<"resource:", Channel/binary>> = Resource,
        emqx_rule_engine_api:create_resource(#{},
            [
                {<<"id">>, Resource},
                {<<"type">>, <<"dgiot_resource">>},
                {<<"config">>, [{<<"channel">>, Channel}]},
                {<<"description">>, Resource}
            ]),
        Acc ++ [X]
                             end, [], Actions),
    R = emqx_rule_engine_api:create_rule(#{}, maps:to_list(Params#{<<"actions">> => NewActions})),
    case R of
        {ok, #{data := #{id := RuleID}}} ->
            save_rule_to_dict(RuleID, Params#{<<"actions">> => NewActions});
        _ -> pass
    end,
    R;

%% Rule 概要: 获取规则引擎列表 描述:获取规则引擎列表
%% OperationId:get_rules
%% 请求:GET /iotapi/rules
do_request(get_rules, _Args, _Context, _Req) ->
    dgiot_rule_handler:sysc_rules(),
    emqx_rule_engine_api:list_rules(#{}, []);

%% OperationId:get_actions
do_request(get_actions, _Args, _Context, _Req) ->
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

%% Rule 概要: 生成sql参数
%% OperationId:post_rulesql
%% 请求:POST /iotapi/rulesql
%%do_request(post_rulesql, #{<<"select">> := Select, <<"from">> := From, <<"where">> := Where, <<"method">> := Method}, _Context, _Req) ->
%%    device_sql(Select, From, Where, Method);

do_request(post_rulesql, #{<<"trigger">> := Trigger, <<"condition">> := Condition, <<"action">> := Action}, _Context, _Req) ->
%%    device_sql(Select, From, Where, Method);
    sql_tpl(Trigger, Condition, Action);



do_request(get_actions_id, #{<<"id">> := RuleID}, _Context, _Req) ->
    emqx_rule_engine_api:show_action(#{id => RuleID}, []);

do_request(get_resources, _Args, _Context, _Req) ->
    {ok, #{data := Data} = Result} = emqx_rule_engine_api:list_resources(#{}, []),
    {ok, Result#{data => get_channel(Data)}};

%% OperationId:post_rule_resource
do_request(post_resources, Params, _Context, _Req) ->
    Actions = maps:get(<<"actions">>, Params),
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

%%getSelect(#{<<"mqttEvent">> := MqttEvent,<<"mqttEvent">> := MqttEvent}->Select) ->

%%doc-api https://help.aliyun.com/document_detail/256392.html#section-yqn-9ue-cwa
%%阿里云topic pub_payload 实例
%%{\"id\":\"1\",\"version\":\"1.0\",\"params\":{\"LightSwitch\":0}}

%%desc 消息通信数据格式
%%doc-api https://help.aliyun.com/document_detail/73736.htm?spm=a2c4g.11186623.0.0.353d7a48CvEOwF#concept-ap3-lql-b2b

sql_tpl(Trigger, Condition, Action) ->
    SELECT = generateSelect(Condition, Trigger, Action),
    FROM = generateFrom(Trigger),
    WHERE = generateWhere(Condition, Trigger, FROM),
%%    {200, #{<<"code">> => 200, <<"Action">> => Action, <<"TopicTpl">> => FROM, <<"WHERE">> => WHERE}}.

%%    WhereSql = lists:foldl(fun(X, Acc) ->
%%        case X of
%%            #{<<"identifier">> := Id, <<"operator">> := Op, <<"value">> := Value} ->
%%                case Acc of
%%                    <<"">> ->
%%                        <<Id/binary, "  ", Op/binary, "  ", Value/binary>>;
%%                    _ ->
%%                        <<Acc/binary, ", \r\n   ", Id/binary, "  ", Op/binary, "  ", Value/binary>>
%%                end;
%%            _ -> Acc
%%        end
%%                           end, <<"">>, Where),
    DefaultSql =
        <<"SELECT", "\r\n",
            SELECT/binary, "\r\n",
            "FROM ", "\r\n",
            "   \"", FROM/binary, "\"", "\r\n",
            "WHERE", "\r\n     ",
            WHERE/binary>>,
    {ok, #{<<"template">> => DefaultSql}}.

%% 根据设备条件生成sql模板
%% Select
%% From
%% Where

device_sql(Select, From, Where, _Method) ->
    %%
    %% 生成sql Where参数
%%    WhereTpl = [#{<<"identifier">> => <<"flow">>, <<"operator">> => <<"==">>, <<"value">> => <<"test">>},
%%        #{<<"identifier">> => <<"temp">>, <<"operator">> => <<"==">>, <<"value">> => <<"test">>},
%%        #{<<"identifier">> => <<"flow">>, <<"operator">> => <<"==">>, <<"value">> => <<"test">>}],
%%    Select = <<"sql">>,

    SelectTpl = getSelect(Select, <<"">>),
    TopicTpl = case From of
                   #{<<"productid">> := ProductId, <<"devaddr">> := Devaddr} ->
                       case Devaddr of
                           <<"#">> ->
                               <<"$dg/user/", ProductId/binary, "/", Devaddr/binary>>;
                           _ ->
                               <<"$dg/user/", ProductId/binary, "/", Devaddr/binary, "/#">>
                       end;
                   _ ->
                       <<"thing/", "test/#">>
               end,
    WhereSql = lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"identifier">> := Id, <<"operator">> := Op, <<"value">> := Value} ->
                case Acc of
                    <<"">> ->
                        <<Id/binary, "  ", Op/binary, "  ", Value/binary>>;
                    _ ->
                        <<Acc/binary, ", \r\n   ", Id/binary, "  ", Op/binary, "  ", Value/binary>>
                end;
            _ -> Acc
        end
                           end, <<"">>, Where),
    DefaultSql = <<"SELECT", "\r\n",
        SelectTpl/binary, "\r\n",
        "FROM ", "\r\n",
        "   \"", TopicTpl/binary, "\"", "\r\n",
        "WHERE", "\r\n     ",
        WhereSql/binary>>,
    {ok, #{<<"template">> => DefaultSql}}.

%% 根据cron表达式生成sql模板



save_rule_to_dict(RuleID, Params) ->
    Rule =
        case emqx_rule_engine_api:show_rule(#{id => RuleID}, []) of
            {ok, #{message := <<"Not Found">>}} ->
                Params;
            {ok, #{code := 0, data := Data}} ->
                Nedata = maps:merge(Data, Params),
                emqx_rule_engine_api:update_rule(#{id => RuleID}, maps:to_list(Nedata)),
                Nedata
        end,
%%    todo class title key type 多个channel 存多条dict ,title都用RuleID
    Dict = #{
        <<"class">> => <<"Rule">>,
        <<"title">> => <<"Rule">>,
        <<"key">> => RuleID,
        <<"type">> => <<"ruleengine">>,
        <<"data">> => #{<<"rule">> => jsx:encode(Rule)}
    },
    dgiot_data:insert(?DGIOT_RUlES, Dict),
    ObjectId = dgiot_parse_id:get_dictid(RuleID, <<"ruleengine">>, <<"Rule">>, <<"Rule">>),
    case dgiot_parse:get_object(<<"Dict">>, ObjectId) of
        {ok, _} ->
            dgiot_parse:update_object(<<"Dict">>, ObjectId, Dict);
        _ ->
            case dgiot_parse:create_object(<<"Dict">>, Dict) of
                {ok, #{<<"objectId">> := ObjectId}} ->
                    {ok, #{<<"objectId">> => ObjectId}};
                {error, Reason1} ->
                    ?LOG(info, "Reason1 ~p", [Reason1]),
                    {error, Reason1}
            end
    end.

get_channel(_Data) ->
    case dgiot_parse:query_object(<<"Channel">>, #{<<"keys">> => [<<"name">>]}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun(#{<<"objectId">> := ChannelId, <<"name">> := Name}, Acc) ->
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
    Type = dgiot_parse_id:get_dictid(Language, <<"dict_template">>, <<"Dict">>, <<"Dict">>),
    case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"type">> => Type}}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun(#{<<"data">> := Data}, Acc) ->
                Acc ++ [maps:with([<<"name">>, <<"value">>, <<"caption">>, <<"meta">>, <<"type">>, <<"score">>], Data)]
                        end, [], Results);
        _ -> []
    end.

sysc_rules() ->
    case dgiot_datetime:start_time() < 15 of
        true ->
            pass;
        false ->
            case ets:tab2list(?DGIOT_RUlES) of
                Result when length(Result) == 0 ->
                    case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"type">> => <<"ruleengine">>}}) of
                        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
                            lists:map(fun(#{<<"key">> := RuleID, <<"data">> := Data}) ->
                                #{<<"rule">> := Rule} = Data,
                                NewRule = jsx:decode(Rule, [return_maps]),
                                Actions = maps:get(<<"actions">>, NewRule),
                                lists:foldl(fun(X, Acc) ->
                                    #{<<"params">> := #{<<"$resource">> := Resource}} = X,
                                    Channel =
                                        case Resource of
                                            <<"resource:", Channel1/binary>> ->
                                                Channel1;
                                            <<"channel:", Channel2/binary>> ->
                                                Channel2
                                        end,
                                    emqx_rule_engine_api:create_resource(#{}, [
                                        {<<"id">>, <<"resource:", Channel/binary>>},
                                        {<<"type">>, <<"dgiot_resource">>},
                                        {<<"config">>, [{<<"channel">>, Channel}]},
                                        {<<"description">>, Resource}
                                    ]),
                                    Acc ++ [X]
                                            end, [], Actions),
                                case emqx_rule_engine_api:show_rule(#{id => RuleID}, []) of
                                    {ok, #{message := <<"Not Found">>}} ->
                                        ?LOG(debug, "NewRule ~p", [NewRule]),
                                        emqx_rule_engine_api:create_rule(#{}, maps:to_list(NewRule#{<<"id">> => RuleID}));
                                    _ ->
                                        emqx_rule_engine_api:update_rule(#{id => RuleID}, maps:to_list(NewRule))
                                end,
                                dgiot_data:insert(RuleID, NewRule)
                                      end, Results);
                        _ -> pass
                    end;
                _ ->
                    pass
            end
    end.

%%
%%
getSelect(#{<<"payload">> := Payload} = Select, Acc1) ->
    NewAcc = lists:foldl(fun(Id, Acc) ->
        case Acc of
            <<"">> ->
                <<"    payload.", Id/binary, "  as ", Id/binary>>;
            _ ->
                <<Acc/binary, ", \r\n   ", "    payload.", Id/binary, "  as ", Id/binary>>
        end

                         end, Acc1, Payload),
    getSelect(maps:without([<<"payload">>], Select), NewAcc);

getSelect(Select, Acc1) ->
    lists:foldl(fun(Id, Acc) ->
        case Acc of
            <<"">> ->
                <<Id/binary>>;
            _ ->
                <<Acc/binary, " , ", Id/binary>>
        end
                end, Acc1, lists:flatten(maps:values(Select))).

generateFrom(Trigger) ->
%%    Trigger: [
%%        {
%%            label: '设备属性触发',
%%            value: 'trigger/product/property',
%%            },
%%        {
%%            label: '设备事件触发',
%%            value: 'trigger/product/event',
%%            },
%%        {
%%            label: 'mqtt事件触发',
%%            value: 'trigger/mqtt/event',
%%            },
%%        {
%%            label: '定时触发',
%%            value: 'trigger/timer',
%%            },
%%        ],
%%
    Firstfrom = lists:nth(1, maps:get(<<"items">>, Trigger)),
    Uri = maps:get(<<"uri">>, Firstfrom),
    Params = maps:get(<<"params">>, Firstfrom, #{}),
    case Uri of
        <<"trigger/product/property">> ->
            case Params of
                #{<<"productKey">> := ProductId, <<"deviceName">> := Devaddr} ->
                    case Devaddr of
                        <<"">> ->
                            <<"$dg/user/", ProductId/binary, "/", Devaddr/binary>>;
                        _ ->
                            <<"$dg/user/", ProductId/binary, "/", Devaddr/binary, "/#">>
                    end;
                _ ->
                    <<"$dg/user/", "test/#">>
            end;
        <<"trigger/product/event">> ->
            case Params of
                #{<<"productKey">> := ProductId, <<"deviceName">> := Devaddr} ->
                    case Devaddr of
                        <<"">> ->
                            <<"$dg/user/", ProductId/binary, "/", Devaddr/binary>>;
                        _ ->
                            <<"$dg/user/", ProductId/binary, "/", Devaddr/binary, "/#">>
                    end;
                _ ->
                    <<"$dg/user/", "test/#">>
            end;
        <<"trigger/mqtt/event">> ->
            case Params of
                #{<<"productKey">> := ProductId, <<"mqtt">> := Mqtt, <<"deviceName">> := Devaddr} ->
                    case ProductId of
                        <<"">> ->
                            <<"$events/", Mqtt/binary, "/", ProductId/binary, "/", Devaddr/binary>>;
                        _ ->
                            <<"$events/", Mqtt/binary, "/", ProductId/binary, "/", Devaddr/binary, "/#">>
                    end;
                _ ->
                    <<"$dg/user/", "test/#">>
            end;
        <<"trigger/timer">> ->
            <<"$dg/user/", "test/#">>;
        _ ->
            <<"$dg/user/", "test/#">>
    end.

generateSelect(Condition, _Trigger, _FROM) ->
    lists:foldl(fun(Item, Acc) ->
        case Item of
            #{<<"uri">> := <<"condition/device/deviceState">>, <<"params">> := Params} ->
                PropertyName = maps:get(<<"propertyName">>, Params, <<"test">>),
                case Acc of
                    <<"">> ->
                        PropertyName;
                    _ ->
                        <<Acc/binary, ",\r\n   ", PropertyName/binary>>
                end;
            #{<<"uri">> := <<"condition/device/stateContinue">>, <<"params">> := Params} ->
                PropertyName = maps:get(<<"propertyName">>, Params, <<"test">>),
                case Acc of
                    <<"">> ->
                        PropertyName;
                    _ ->
                        <<Acc/binary, ",\r\n   ", PropertyName/binary>>
                end;
            #{<<"uri">> := <<"condition/device/time">>, <<"params">> := Params} ->
                PropertyName = maps:get(<<"propertyName">>, Params, <<"test">>),
                case Acc of
                    <<"">> ->
                        PropertyName;
                    _ ->
                        <<Acc/binary, ",\r\n   ", PropertyName/binary>>
                end;
            _ ->
                Acc
        end
                end, <<"">>, maps:get(<<"items">>, Condition, [])).

generateWhere(Condition, _Trigger, _FROM) ->
%%    Condition: [
%%        {
%%            label: '状态持续时长判断',
%%            value: 'condition/device/stateContinue',
%%            },
%%        {
%%            label: '设备状态',
%%            value: 'condition/device/deviceState',
%%            },
%%        { label: '时间范围', value: 'condition/device/time' },
%%        // { label: '设备属性值', value: 'condition/device/property' },
%%    ],
    L1 = lists:foldl(fun(Item, Acc) ->
        case Item of
            #{<<"uri">> := <<"condition/device/deviceState">>, <<"params">> := Params} ->
                CompareValue = maps:get(<<"compareValue">>, Params, <<"0">>),
                CompareType = maps:get(<<"compareType">>, Params, <<"=">>),
                PropertyName = maps:get(<<"propertyName">>, Params, <<"test">>),
                DeviceState = <<PropertyName/binary, "  ", CompareType/binary, "  ", CompareValue/binary>>,
                case Acc of
                    <<"">> ->
                        DeviceState;
                    _ ->
                        <<Acc/binary, ",\r\n   ", DeviceState/binary>>
                end;
            _ ->
                Acc
        end
                     end, <<"">>, maps:get(<<"items">>, Condition, [])),
    L2 = lists:foldl(fun(Item, Acc) ->
        case Item of
            #{<<"uri">> := <<"condition/device/stateContinue">>, <<"params">> := #{
                <<"state">> := State, <<"times">> := Times}} ->
                Continue = <<State/binary, "  =  ", Times/binary>>,
                case Acc of
                    <<"">> ->
                        Continue;
                    _ ->
                        <<Acc/binary, ",\r\n   ", Continue/binary>>
                end;
            _ ->
                Acc
        end
                     end, L1, maps:get(<<"items">>, Condition, [])),

    lists:foldl(fun(Item, Acc) ->
        case Item of
            #{<<"uri">> := <<"condition/device/time">>, <<"params">> := #{<<"time">> := Time}} ->
                {TimesStart, TimesEnd} =
                    lists:foldl(fun(X, Acc1) ->
                        case X of
                            {Start, End} ->
                                IntStart = dgiot_utils:to_int(Start),
                                IntEnd = dgiot_utils:to_int(End),
                                case IntStart > IntEnd of
                                    true ->
                                        {dgiot_utils:to_binary(End), dgiot_utils:to_binary(Start)};
                                    false ->
                                        {dgiot_utils:to_binary(Start), dgiot_utils:to_binary(End)}
                                end;
                            _ ->
                                Acc1
                        end
                                end, {<<"1646064000000">>, <<"1649088000000">>}, Time),
                TimesLg = <<"timestamp  <     ", TimesStart/binary, ", \r\n   ", "timestamp >     ", TimesEnd/binary>>,
                case Acc of
                    <<"">> ->
                        TimesLg;
                    _ ->
                        <<Acc/binary, " , ", TimesLg/binary>>
                end;
            _ ->
                Acc
        end
                end, L2, maps:get(<<"items">>, Condition, [])).
