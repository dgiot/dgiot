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
-export([do_request/4]).
-export([swagger_http/0]).

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

do_request(get_file_signature, Args, _Context, _Req) ->
    case maps:get(<<"type">>, Args, null) of
        <<"aliyun">> -> {200, dgiot_aliyun_auth:aliyun_upload()};
        _ -> {404, #{<<"code">> => 1001, <<"error">> => <<"not support this type">>}}
    end;

%% iot_hub 概要: 查询平台api资源 描述:查询平台api资源
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(get_wechat, #{<<"openid">> := Openid}, _Context, _Req) ->
    dgiot_wechat:get_sns(Openid);

do_request(post_wechat, #{<<"jscode">> := JSCODE}, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"objectId">> := UserId} ->
            dgiot_wechat:post_sns(UserId, JSCODE);
        _ ->
            {error, <<"Not Allowed.">>}
    end;

%%  服务器不支持的API接口
do_request(OperationId, Args, _Context, _Req) ->
    ?LOG(error, "do request ~p,~p~n", [OperationId, Args]),
    {error, <<"Not Allowed.">>}.


