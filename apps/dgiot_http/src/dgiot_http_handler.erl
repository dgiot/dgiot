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

%% iot_hub 概要: 查询平台api资源 描述:jwt回调
%% OperationId:get_jwtlogin
%% 请求:POST /iotapi/get_jwtlogin
do_request(get_jwtlogin, #{<<"id_token">> := Idtoken}, _Context, _Req) ->
    dgiot_aliyun_auth:jwtlogin(Idtoken);

%% iot_hub 概要: 查询平台api资源 描述:wechat登陆
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(get_wechat, #{<<"jscode">> := Jscode}, _Context, _Req) ->
    dgiot_wechat:get_sns(Jscode);

%% iot_hub 概要: 查询平台api资源 描述:wechat绑定
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
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
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(get_wechat_map, _Args, #{<<"sessionToken">> := SessionToken}, _Req) ->
    dgiot_wechat:get_wechat_map(SessionToken);

%% iot_hub 概要: 查询平台api资源 描述:设备详情
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(get_device_info, #{<<"deviceid">> := Deviceid}, #{<<"sessionToken">> := SessionToken}, _Req) ->
    dgiot_wechat:get_device_info(Deviceid, SessionToken);

%% iot_hub 概要: 查询平台api资源 描述:设备详情
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
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

%% iot_hub 概要: 查询平台api资源 描述:发送订阅消息
%% OperationId:post_sendsubscribe
%% 请求:POST /iotapi/post_sendsubscribe
do_request(post_sendemail, Args, #{<<"sessionToken">> := _SessionToken}, _Req) ->
    case dgiot_notification:send_email(Args) of
        {ok, _R} ->
            {ok, <<"send success">>};
        _Ot ->
            {error, <<"send fail">>}
    end;

%%  服务器不支持的API接口
do_request(OperationId, Args, _Context, _Req) ->
    ?LOG(error, "do request ~p, ~p~n", [OperationId, Args]),
    {error, <<"Not Allowed.">>}.


