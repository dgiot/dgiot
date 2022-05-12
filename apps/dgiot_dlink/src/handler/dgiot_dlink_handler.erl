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
-module(dgiot_dlink_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_dlink/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/dlink">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_dlink.json">>, ?MODULE, [], priv)
swagger_dlink() ->
    [
        dgiot_http_server:bind(<<"/swagger_dlink.json">>, ?MODULE, [], priv)
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
%% Proctol 概要: 获取Dlink协议列表
%% OperationId:protocol
%% 请求:GET /iotapi/protocol
do_request(get_protocol, _Body, _Context, _Req) ->
    Protocols = dgiot_dlink:get_all_protocol(),
    {200, Protocols};

%% Proctol 概要: 获取Dlink json信息
%% OperationId:dlinkjson
%% 请求:GET /iotapi/dlinkjson
do_request(get_dlinkjson, #{<<"type">> := <<"swaggerTree">>}, _Context, _Req) ->
    {ok, SwaggerTree} = dgiot_swagger:tree(),
    {200, SwaggerTree};

do_request(get_dlinkjson, #{<<"type">> := <<"Table">>}, _Context, _Req) ->
    {ok, Tables} = dgiot_parse:get_schemas(),
    {200, Tables};

do_request(get_dlinkjson, #{<<"type">> := Type}, _Context, _Req) ->
    DlinkJson = dgiot_utils:get_JsonFile(?MODULE, <<Type/binary, ".json">>),
    {200, DlinkJson};

do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
