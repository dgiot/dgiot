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

-module(dgiot_role_handler).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_rest).
-dgiot_rest(all).

%% API
-export([swagger_role/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/iotdev">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_iot.json">>, ?MODULE, [], priv)
swagger_role() ->
    [
        dgiot_http_server:bind(<<"/swagger_role.json">>, ?MODULE, [], priv)
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
            ?LOG(debug, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_utils:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% iot_hub 概要: 查询平台api资源 描述:查询平台api资源
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(post_login, #{<<"username">> := UserName, <<"password">> := Password}, _Context, _Req) ->
    ?LOG(debug, "UserName ~p ", [UserName]),
    dgiot_parse_handler:login_by_account(UserName, Password);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:post_role
%% 请求:POST /iotapi/role
do_request(post_role, #{<<"name">> := _Name, <<"tempname">> := _TempName} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:post_role(Body, SessionToken);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:delete_role
%% 请求:DELETE /iotapi/role
do_request(delete_role, #{<<"name">> := _Name, <<"tempname">> := _TempName} = Body, _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:post_role
%% 请求:POST /iotapi/role
do_request(put_role, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:put_role(Body, SessionToken);

%% Role 概要: 导库 描述:json文件导库
%% OperationId:get_role
%% 请求:GET /iotapi/role
do_request(get_role, #{<<"name">> := Name} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:get_role(Name, SessionToken);

%% RoleUser 概要: 导库 描述:json文件导库
%% OperationId:get_roleuser
%% 请求:GET /iotapi/roleuser
do_request(get_roleuser, #{<<"where">> := Where} = Filter, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    dgiot_role:get_roleuser(Filter#{<<"where">> => jsx:decode(Where, [return_maps])}, SessionToken);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:put_roleuser
%% 请求:POST /iotapi/roleuser
do_request(put_roleuser, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:put_roleuser(Body, SessionToken);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:delete_roleuser
%% 请求:POST /iotapi/roleuser
do_request(delete_roleuser, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
%%    io:format("Body ~p~n", [Body]),
    dgiot_role:del_roleuser(Body, SessionToken);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:delete_roleuser
%% 请求:POST /iotapi/roleuser
do_request(post_roleuser, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:post_roleuser(Body, SessionToken);

%% Role 概要: 导库 描述:json文件导库
%% OperationId:get_roletree
%% 请求:GET /iotapi/roletree
do_request(get_roletree, _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "SessionToken ~p ", [SessionToken]),
    dgiot_parse_handler:get_classtree(<<"_Role">>, <<"parent">>, #{}, SessionToken);
%%    dgiot_role:get_roletree(SessionToken);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:post_roletemp
%% 请求:GET /iotapi/roletemp
do_request(get_roletemp, #{<<"name">> := Name} = Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    FileName = dgiot_utils:to_list(Name) ++ ".zip",
    case dgiot_role:get_roletemp(FileName, Name, SessionToken) of
        {ok, ZipFile} ->
            Headers = #{
                <<"content-type">> => <<"application/zip">>,
                <<"Content-Disposition">> => list_to_binary("attachment;filename=" ++ FileName)
            },
            {200, Headers, ZipFile};
        Err ->
            Err
    end;

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:post_roletemp
%% 请求:POST /iotapi/roletemp
do_request(post_roletemp, #{<<"name">> := Name, <<"tempname">> := TempName} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:post_roletemp(Name, TempName, SessionToken);


%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:post_roletemp
%% 请求:POST /iotapi/roletemp
do_request(put_roletemp, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    ?LOG(debug, "Body ~p ", [Body]),
    dgiot_role:put_roletemp(Body, SessionToken);


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    ?LOG(debug, "_Args ~p", [_Args]),
    {error, <<"Not Allowed.">>}.
