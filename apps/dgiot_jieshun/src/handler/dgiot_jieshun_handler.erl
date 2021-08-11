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

-module(dgiot_jieshun_handler).
-author("dgiot").
-dgiot_rest(all).
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").


%% API
-export([swagger_jieshun/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/zeta">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_zeta.json">>, ?MODULE, [], priv)
swagger_jieshun() ->
    [
        dgiot_http_server:bind(<<"/swagger_jieshun.json">>, ?MODULE, [], priv)
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
            ?LOG(info, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_ctl:format("~p", [Reason])
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
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.

%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% System 概要: 服务器文件上传 描述:上传所有区域剩余车位数
%% OperationId:post_parktotalplace
%% 请求:POST /iotapi/parktotalplace
do_request(post_parktotalplace, Body, _Context, _Req) ->
    ?LOG(info, "Body ~p", [Body]),
    ?LOG(info, "_Context ~p", [_Context]),
    dgiot_park:parktotalplace(Body),
    {ok, #{<<"code">> => <<"0">>, <<"msg">> => <<"">>}};

%% System 概要: 服务器文件上传 描述:接收车辆收费记录
%% OperationId:post_parkcharge
%% 请求:POST /iotapi/parkcharge
do_request(post_parkcharge, Body, _Context, _Req) ->
    ?LOG(info, "Body ~p", [Body]),
    ?LOG(info, "_Context ~p", [_Context]),
    dgiot_park:parkcharge(Body),
    {ok, #{<<"code">> => <<"0">>, <<"msg">> => <<"">>}};

%% System 概要: 服务器文件上传 描述:接收车辆入场识别记录
%% OperationId:post_inrecognition
%% 请求:POST /iotapi/inrecognition
do_request(post_inrecognition, Body, _Context, _Req) ->
    ?LOG(info, "Body ~p", [Body]),
    ?LOG(info, "_Context ~p", [_Context]),
    dgiot_park:inrecognition(Body),
    {ok, #{<<"code">> => <<"0">>, <<"msg">> => <<"">>}};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
