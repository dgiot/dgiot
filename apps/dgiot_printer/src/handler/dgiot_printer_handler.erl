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
-module(dgiot_printer_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_printer/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_printer.json">>, ?MODULE, [], priv)
swagger_printer() ->
    [
        dgiot_http_server:bind(<<"/swagger_printer.json">>, ?MODULE, [], priv)
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
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_utils:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            {200, Headers, #{}, Req};
        {ok, Res} ->
            {200, Headers, Res, Req};
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
%%  视频取证的API接口
%% FFMPEG 概要: 获取视频取证任务信息 描述:json文件导库
%% OperationId:get_printer
%% 请求:GET /iotapi/printer
do_request(get_printer, #{<<"type">> := Type, <<"devaddr">> := DevAddr, <<"product">> := Product} = _Body,
        #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"_Body ~p", [_Body]),
    get_printer(Type, DevAddr, Product, SessionToken);

%%  视频取证的API接口
%% FFMPEG 概要: 修改视频取证任务 描述:json文件导库
%% OperationId:put_printer
%% 请求:PUT /iotapi/printer
do_request(put_printer, _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"_Body ~p", [_Body]),
    put_printer(_Body, SessionToken);

do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.


get_printer(_Type, _DevAddr, _Product, _SessionToken) ->
    ok.


put_printer(#{<<"type">> := _Type, <<"devaddr">> := _DevAddr} = _Body, _SessionToken) ->
   ok.

