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

-module(dgiot_user_handler).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_rest).
-dgiot_rest(all).
-define(DEFUser, re:split(application:get_env(dgiot_parse, default_user, ""), ",")).
%% API
-export([swagger_user/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/iotdev">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_user.json">>, ?MODULE, [], priv)
swagger_user() ->
    [
        dgiot_http_server:bind(<<"/swagger_user.json">>, ?MODULE, [], priv)
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

%% IoTDevice 概要: 获取子角色token 描述:子角色Token查询
%% OperationId: get_token
%% GET /token
do_request(get_token, #{<<"name">> := Name} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parse:query_object(<<"_Role">>, #{
        <<"order">> => <<"updatedAt">>, <<"limit">> => 1,
        <<"where">> => #{<<"name">> => Name}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            Result = dgiot_parse_auth:check_roles(Name),
            case Result of
                {200, #{<<"access_token">> := Depart_token}} ->
                    dgiot_parse_auth:put_usersession(SessionToken, Depart_token),
                    Result;
                _ ->
                    Result
            end;
        {ok, #{<<"results">> := Roles}} when length(Roles) == 0 ->
            {404, #{<<"code">> => 101, <<"error">> => <<"User not found.">>}};
        {error, Error} ->
            {404, #{<<"code">> => 101, <<"error">> => Error}}
    end;

%% IoTDevice 概要: 获取应用token 描述:Token查询
%% OperationId: post_token
%% POST /token
do_request(post_token, #{<<"appid">> := AppId, <<"secret">> := Secret}, _Context, _Req) ->
    case dgiot_parse_auth:login_by_token(AppId, Secret) of
        {ok, UserInfo} ->
            {200, maps:with([<<"access_token">>, <<"expires_in">>, <<"desc">>, <<"name">>], UserInfo)};
        {error, #{<<"code">> := 101} = Err} ->
            {404, Err};
        {error, Err} -> {500, Err}
    end;

%% IoTDevice 概要: 刷新Token 描述:刷新Token
%% OperationId: get_refresh_session
%% POST /token
do_request(get_refresh_session_sessiontoken, #{<<"sessionToken">> := SessionToken}, _Context, _Req) ->
    case dgiot_parse_auth:refresh_session(SessionToken) of
        {ok, Ref} ->
            {200, Ref};
        {error, Err} ->
            {500, Err}
    end;

%% IoTDevice 概要: 获取应用token 描述:更新token
%% OperationId: put_token
%% PUT /token
do_request(put_token, #{<<"appid">> := AppId, <<"secret">> := Secret}, _Context, _Req) ->
    case dgiot_parse_auth:login_by_token(AppId, Secret) of
        {ok, UserInfo} ->
            {200, maps:with([<<"access_token">>, <<"expires_in">>, <<"desc">>, <<"name">>], UserInfo)};
        {error, #{<<"code">> := 101} = Err} ->
            {404, Err};
        {error, Err} -> {500, Err}
    end;

%% IoTDevice 概要: 创建新用户 描述:更新token
%% OperationId: put_token
%% PUT /token
do_request(post_user, #{<<"username">> := _UserName, <<"password">> := _Password} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_parse_auth:create_user(Body, SessionToken) of
        {ok, Data} ->
            dgiot_role:load_user(),
            dgiot_parse_auth:load_roleuser(),
            {200, Data};
        {error, Error} -> {500, Error}
    end;

do_request(delete_user, Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_parse_auth:delete_user(Body, SessionToken) of
        {ok, Data} ->
            dgiot_role:load_user(),
            dgiot_parse_auth:load_roleuser(),
            {200, Data};
        {error, Error} ->
            {error, Error}
    end;

do_request(put_user, #{<<"username">> := UserName} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    DefaultUsers = ?DEFUser,
    case lists:member(UserName, DefaultUsers) of
        true ->
            {ok, #{<<"code">> => 401, <<"msg">> => <<UserName/binary, " PROHIBITED Modify">>}};
        _ ->
            case dgiot_parse_auth:put_user(Body, SessionToken) of
                {ok, Data} ->
                    dgiot_role:load_user(),
                    dgiot_parse_auth:load_roleuser(),
                    {200, Data};
                {error, Error} -> {500, Error}
            end
    end;

do_request(get_users_id, Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_parse_auth:get_user(Body, SessionToken) of
        {ok, Data} ->
%%            dgiot_role:load_user(),
%%            dgiot_parse_auth:load_roleuser(),
            {200, Data};
        {error, Error} -> {500, Error}
    end;

%% IoTDevice 概要: 禁用账号
%% OperationId: get_disableuser
%% Disuserid 被禁用账号
%% GET /token
%% dgiot_parse:load().
do_request(get_disableuser, #{<<"userid">> := Disuserid, <<"action">> := Action} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"objectId">> := UserId, <<"username">> := UserName} ->
            DefaultUsers = ?DEFUser,
            case lists:member(UserName, DefaultUsers) of
                true ->
                    {ok, #{<<"code">> => 401, <<"msg">> => <<UserName/binary, " PROHIBITED Disable">>}};
                _ ->
                    case dgiot_parse_auth:disableusere(UserId, Disuserid, Action) of
                        {ok, Data} ->
                            {200, Data};
                        {error, Error} -> {500, #{<<"code">> => 101, <<"error">> => Error}}
                    end
            end;
        _ ->
            {500, <<"Not Allowed.">>}
    end;

%% iot_hub 概要: 查询平台api资源 描述:查询平台api资源
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(post_login, #{<<"username">> := UserName, <<"password">> := Password}, _Context, _Req) ->
    dgiot_parse_auth:login_by_account(UserName, Password);

%% iot_hub 概要: 用户管理 描述: 用户管理
%% OperationId:post_logout
%% 请求:POST /iotapi/post_logout
do_request(post_logout, #{<<"sessionToken">> := SessionToken}, _Context, _Req) ->
    dgiot_auth:delete_session(SessionToken),
    SessionId = dgiot_parse_id:get_sessionId(SessionToken),
    dgiot_mqtt:unsubscribe_route_key(SessionToken, <<"all">>),
    dgiot_parse_auth:del_usersession(SessionToken),
    dgiot_parse_auth:del_cookie(SessionToken),
    dgiot_parse:del_object(<<"_Session">>, SessionId);

%% RoleUser 概要: 导库 描述:查询部门下用户
%% OperationId:get_roleuser
%% 请求:GET /iotapi/roleuser
do_request(get_roleuser, #{<<"where">> := Where} = Filter, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    UserFilter =
        case maps:get(<<"userfilter">>, Filter, <<"{}">>) of
            undefined ->
                <<"{}">>;
            V ->
                V
        end,
    dgiot_parse_auth:get_roleuser(Filter#{<<"where">> => dgiot_json:decode(Where, [return_maps]), <<"userfilter">> => dgiot_json:decode(UserFilter, [return_maps])}, SessionToken);

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:put_roleuser
%% 请求:POST /iotapi/roleuser
do_request(put_roleuser, #{<<"userid">> := UserId} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
%%  ?LOG(debug, "Body ~p ", [Body]),
    DefaultUsers = ?DEFUser,
    case lists:member(UserId, DefaultUsers) of
        true ->
            {ok, #{<<"code">> => 401, <<"result">> => <<UserId/binary, " Cannot be transferred">>}};
        _ ->
            dgiot_parse_auth:put_roleuser(Body, SessionToken)
    end;

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:delete_roleuser
%% 请求:POST /iotapi/roleuser
do_request(delete_roleuser, #{<<"userid">> := UserId} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    DefaultUsers = ?DEFUser,
    case lists:member(UserId, DefaultUsers) of
        true ->
            {ok, #{<<"code">> => 401, <<"result">> => <<UserId/binary, " Cannot be Resignation">>}};
        _ ->
            dgiot_parse_auth:del_roleuser(Body, SessionToken)
    end;

%% Role模版 概要: 导库 描述:json文件导库
%% OperationId:delete_roleuser
%% 请求:POST /iotapi/roleuser
do_request(post_roleuser, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    dgiot_parse_auth:post_roleuser(Body, SessionToken);

do_request(get_usertree, _Arg, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    Data = dgiot_parse_auth:get_usertree(SessionToken),
    {ok, #{
        <<"status">> => 0,
        <<"msg">> => <<"ok">>,
        <<"data">> => #{<<"options">> => Data}
    }};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
%%    io:format("~s ~p _OperationId = ~p.~n", [?FILE, ?LINE, _OperationId]),
%%    io:format("~s ~p _Args = ~p.~n", [?FILE, ?LINE, _Args]),
    {error, <<"Not Allowed.">>}.

