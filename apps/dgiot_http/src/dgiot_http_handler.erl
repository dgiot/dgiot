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

-module(dgiot_http_handler).
-author("kenneth").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_http/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/http">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_http.json">>, ?MODULE, [], priv)
swagger_http() ->
    [
        dgiot_http_server:bind(<<"/swagger_http.json">>, ?MODULE, [], priv)
    ].

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            ?LOG(debug, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
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

do_request(get_file_signature, Args, _Context, _Req) ->
    case maps:get(<<"type">>, Args, null) of
        <<"aliyun">> -> {200, dgiot_aliyun_auth:aliyun_upload()};
        _ -> {404, #{<<"code">> => 1001, <<"error">> => <<"not support this type">>}}
    end;

%dgiot通用告警
do_request(post_handlewarnsendsms, #{<<"appId">> := AppId, <<"appKey">> := AppKey, <<"tplId">> := TplId, <<"sign">> := Sign, <<"params">> := Params}, _Context, _Req) ->
    %通用
    application:set_env(dgiot_http, tencent_sms_appid, AppId),
    application:set_env(dgiot_http, tencent_sms_appkey, AppKey),
    application:set_env(dgiot_http, tencent_sms_notification_templateId, TplId),
    %测试告警通知的部门，默认为数蛙部门
    application:set_env(dgiot_http, tencent_sms_sign, Sign),
    application:set_env(dgiot_http, tencent_sms_params, Params),

    case dgiot_parse:get_object(<<"_Role">>, Sign) of
        {ok, #{<<"objectId">> := RolesId}} ->
            %循环得到部门下所有的手机号
            Users = dgiot_parse_auth:get_UserIds(unicode:characters_to_binary(RolesId)),
            UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => Users}}},
            {ok, #{<<"results">> := Row}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
            PhoneList = lists:foldl(fun(X, Acc) ->
                Phone = unicode:characters_to_binary(dgiot_utils:to_list(maps:get(<<"phone">>, X))),
                dgiot_notification:send_sms(Phone, application:get_env(dgiot_http, tencent_sms_params, Params)),
                Acc ++ [unicode:characters_to_binary(dgiot_utils:to_list(maps:get(<<"phone">>, X)))]
                                    end, [], Row),
%      模板格式：时间：{1} {2}（发起人：{3}）（单据编号{4}）（车间：{5}）产生异常,警告等级为:{6}。
            Json = #{<<"phones">> => PhoneList},
            {ok, #{
                <<"status">> => 200,
                <<"msg">> => <<"success">>,
                <<"data">> => Json
            }};
        _ ->
            {error, #{<<"status">> => 404, <<"msg">> => <<"部门ID错误"/utf8>>, <<"result">> => <<"_Role info null">>}}
    end;

%数字工厂告警
do_request(post_warnsendsms, #{<<"objectId">> := DeviceId, <<"department">> := Department,<<"branchId">> := BranchId,"orderId":= OrderId, <<"datetimes">> := DateTimes, <<"docnumber">> := Docnumber, <<"username">> := UserName, <<"workshop">> := Workshop, <<"level">> := Level, <<"desc">> := Desc, <<"file">> := FileInfo}, _Context, _Req) ->

    case Level of
        <<"1">> ->
            Warn = <<"待首检"/utf8>>,
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 3});
        <<"2">> ->
            Warn = <<"待尾检"/utf8>>,
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 5});
        _ ->
            Warn = <<"告警"/utf8>>,
            {error, #{code => 1, error => ("level错误")}}
    end,
    Warns = Warn,
    case dgiot_parse:get_object(<<"_Role">>, BranchId) of
        {ok, #{<<"objectId">> := RolesId, <<"ACL">> := Acl}} ->

            Map = #{
                <<"type">> => DeviceId,
                <<"name">> => <<"Manual alarm">>,
                <<"status">> => 0,
                <<"content">> => #{
                    <<"alarm">> => #{
                        <<"deviceId"/utf8>> => DeviceId,
                        <<"orderId"/utf8>> => OrderId,
                        <<"department"/utf8>> => Department,
                        <<"docnumber"/utf8>> => Docnumber,
                        <<"datetimes"/utf8>> => DateTimes,
                        <<"username"/utf8>> => UserName,
                        <<"workshop"/utf8>> => Workshop,
                        <<"level"/utf8>> => Level,
                        <<"desc"/utf8>> => Desc,
                        <<"imgurl"/utf8>> => FileInfo
                    },
                    <<"alertstatus">> => 1
                },
                <<"ACL">> => Acl
            },
            #{<<"objectId">> := ObjectId} = dgiot_parse_id:get_objectid(<<"Notification">>, Map),
            NewMap = Map#{
                <<"objectId">> => ObjectId
            },
            dgiot_parse:create_object(<<"Notification">>, NewMap),
            %循环得到部门下所有的手机号
            Users = dgiot_parse_auth:get_UserIds(unicode:characters_to_binary(RolesId)),
            UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => Users}}},
            {ok, #{<<"results">> := Row}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
            PhoneList = lists:foldl(fun(X, Acc) ->
                Phone = unicode:characters_to_binary(dgiot_utils:to_list(maps:get(<<"phone">>, X))),
                dgiot_notification:send_sms(Phone, [DateTimes, <<"-">>, UserName, Docnumber, Workshop, Warns]),
                Acc ++ [unicode:characters_to_binary(dgiot_utils:to_list(maps:get(<<"phone">>, X)))]
                                    end, [], Row),
%      模板格式：时间：{1} {2}（发起人：{3}）（单据编号{4}）（车间：{5}）产生异常,警告等级为:{6}。
            Json = #{<<"phones">> => PhoneList},
            {ok, #{
                <<"status">> => 200,
                <<"msg">> => <<"success">>,
                <<"data">> => Json
            }};
        _ ->
            {error, #{<<"status">> => 404, <<"msg">> => <<"部门ID错误"/utf8>>, <<"result">> => <<"_Role info null">>}}
    end;

%% iot_hub 概要: 查询平台api资源 描述:jwt回调
%% OperationId:get_jwtlogin
%% 请求:POST /iotapi/get_jwtlogin
do_request(get_jwtlogin, #{<<"id_token">> := Idtoken}, _Context, _Req) ->
    dgiot_aliyun_auth:jwtlogin(Idtoken);

%% iot_hub 概要: 查询平台api资源 描述:wechat登陆
%% OperationId:get_wechat
%% 请求:GET /iotapi/get_wechat
do_request(get_wechat, #{<<"jscode">> := Jscode}, _Context, _Req) ->
    dgiot_wechat:get_sns(Jscode);

%% iot_hub 概要: 查询平台api资源 描述:wechat绑定
%% OperationId:post_wechat
%% 请求:POST /iotapi/post_wechat
do_request(post_wechat, #{<<"username">> := UserName, <<"password">> := Password, <<"openid">> := OpenId}, _Context, _Req) ->
    dgiot_wechat:post_sns(UserName, Password, OpenId);

%% iot_hub 概要: 查询平台api资源 描述:wechat解绑
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(get_wechat_unbind, _Args, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"objectId">> := UserId} ->
            dgiot_wechat:unbind_sns(UserId);
        _ ->
            {error, <<"Not Allowed.">>}
    end;

%% iot_hub 概要: 查询平台api资源 描述:总控台
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(get_wechat_index, _Args, #{<<"sessionToken">> := SessionToken}, _Req) ->
    dgiot_wechat:get_wechat_index(SessionToken);


%% iot_hub 概要: 查询平台api资源 描述:设备地图
%% OperationId:get_wechat_map
%% 请求:GET /iotapi/get_wechat_map
do_request(get_wechat_map, _Args, #{<<"sessionToken">> := SessionToken}, _Req) ->
    dgiot_wechat:get_wechat_map(SessionToken);

%% iot_hub 概要: 查询平台api资源 描述:设备详情
%% OperationId:get_device_info
%% 请求:GET /iotapi/get_device_info
do_request(get_device_info, #{<<"deviceid">> := Deviceid}, #{<<"sessionToken">> := SessionToken}, _Req) ->
    dgiot_wechat:get_device_info(Deviceid, SessionToken);

%% iot_hub 概要: 查询平台api资源 描述:设备详情
%% OperationId:get_notification
%% 请求:GET /iotapi/get_notification
do_request(get_notification, #{<<"productid">> := ProductId, <<"order">> := Order, <<"limit">> := Limit, <<"skip">> := Skip, <<"isprocess">> := Isprocess}, #{<<"sessionToken">> := SessionToken}, _Req) ->
    ?LOG(info, "SessionToken = ~p ", [SessionToken]),
    Where =
        case Isprocess of
            <<"0">> ->
                #{<<"status">> => 0};
            <<"1">> ->
                #{<<"status">> => 1};
            <<"2">> ->
                #{<<"status">> => 2};
            _ ->
                #{}
        end,
    dgiot_wechat:get_notification(ProductId, SessionToken, Order, Limit, Skip, Where);

%% iot_hub 概要: 查询平台api资源 描述:发送订阅消息
%% OperationId:post_sendsubscribe
%% 请求:POST /iotapi/post_sendsubscribe
do_request(post_sendsubscribe, Args, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"objectId">> := UserId} ->
            dgiot_wechat:sendSubscribe_test(UserId, Args);
        _ ->
            {error, <<"Not Allowed.">>}
    end;

%% iot_hub 概要: 查询平台api资源 描述:发送邮件
%% OperationId:post_sendsubscribe
%% 请求:POST /iotapi/post_sendsubscribe
do_request(post_sendemail, Args, #{<<"sessionToken">> := _SessionToken}, _Req) ->
    case dgiot_notification:send_email(Args) of
        {ok, _R} ->
            {ok, #{<<"msg">> => <<"send success">>}};
        _Ot ->
            {error, #{<<"msg">> => <<"send fail">>}}
    end;

%% iot_hub 概要: 查询平台api资源 描述:工单结束发送
%% OperationId:get_maintenancefinish
%% 请求:POST /iotapi/get_maintenancefinish
do_request(get_maintenancefinish, #{<<"number">> := Number}, #{<<"sessionToken">> := _SessionToken}, _Req) ->
    Topic = <<"/workOrderCompletion/up">>,
    dgiot_mqtt:publish(Number, <<"bridge/", Topic/binary>>, jsx:encode(#{<<"id">> => Number})),
    dgiot_mqtt:publish(Number, Topic, jsx:encode(#{<<"id">> => Number}));

%% iot_hub 概要: 查询平台api资源 描述:创建工单
%% OperationId:post_maintenance
%% 请求:POST /iotapi/post_maintenance
do_request(post_maintenance, Args, #{<<"sessionToken">> := SessionToken}, _Req) ->
    Isbridge = application:get_env(dgiot_http, isbridge, false),
    Url = application:get_env(dgiot_http, dgiot_bridge_url, "https://prod.iotn2n.com") ++ "/iotapi/maintenance",
    Result = dgiot_umeng:create_maintenance(Args, SessionToken),
    case Isbridge of
        true ->
            case catch httpc:request(post, {Url, [], "application/json", jsx:encode(Args)}, [], []) of
                {'EXIT', _Reason} ->
                    pass;
                {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
                    jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]),
                    pass;
                _ ->
                    pass
            end;
        _ ->
            pass
    end,
    case Result of
        {error, #{<<"code">> := 137}} ->
            {ok, #{<<"code">> => 137, <<"error">> => <<"ID REPEAT">>}};
        {ok, Result1} ->
            {ok, Result1};
        _ ->
            {ok, #{<<"code">> => 500, <<"error">> => <<"SYSTEM ERROR">>}}
    end;

%% iot_hub 概要: 获取运维管理列表 描述:获取运维管理列表
%% OperationId:get_operations
%% 请求:POST /iotapi/get_operations
do_request(get_operations, _Args, #{<<"sessionToken">> := _SessionToken}, _Req) ->
    Data = dgiot_umeng:get_operations(),
    {ok, #{<<"data">> => Data}};


%% iot_hub 概要: 查询平台api资源 描述:修改运维管理列表
%% OperationId:post_operations
%% 请求:POST /iotapi/post_operations
do_request(post_operations, Args, #{<<"sessionToken">> := _SessionToken}, _Req) ->
%%    dgiot_mqtt:publish(<<"">>, <<"">>, jsx:decode(Args)),
    {ok, #{<<"msg">> => <<"modify successfully">>, <<"data">> => Args}};

%% iot_hub 概要: 查询平台api资源 描述:触发告警
%% OperationId:post_triggeralarm
%% 请求:POST /iotapi/post_triggeralarm
do_request(post_triggeralarm, #{<<"deviceid">> := DeviceId} = Args, #{<<"sessionToken">> := _SessionToken}, _Req) ->
%%    dgiot_mqtt:publish(<<"">>, <<"">>, jsx:decode(Args)),
    dgiot_umeng:triggeralarm(DeviceId),
    {ok, #{<<"msg">> => <<"trigger successfully">>, <<"data">> => Args}};


%% System 概要: 发送短信验证码 描述:发送短信,短信验证码发送成功后,则会在缓存中写入action + mobile, 用户下一步提交时，可以根据此键查询验证通过
%% OperationId:post_sendsms_action
%% 请求:POST /iotapi/sendsms/:Action
do_request(post_sendsms, #{<<"account">> := Account, <<"nationcode">> := NationCode}, _Context, _Req) ->
    case dgiot_notification:send_verification_code(NationCode, Account) of
        {error, Reason} ->
            {500, #{code => 1, error => Reason}};
        {ok, Map} ->
            {ok, Map}
    end;

%% System 概要: 验证手机号/邮箱是否通过 描述:验证手机号/邮箱是否通过
%% OperationId:post_verify_code
%% 请求:POST /iotapi/verify_code
do_request(post_verify_code_action, #{<<"account">> := Account, <<"code">> := Code} = Args, _Context, Req) ->
    case dgiot_notification:check_verification_code(Account, Code) of
        true ->
            dgiot_verify_code:handle(Args, Req);
        false ->
            {400, unicode:characters_to_binary(<<"验证码未通过！"/utf8>>)}
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.


