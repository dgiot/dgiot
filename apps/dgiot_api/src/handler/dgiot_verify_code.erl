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

-module(dgiot_verify_code).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([handle/2]).


%% 注册/登录验证码校验成功
handle(#{<<"Action">> := Action, <<"account">> := Account} = Args, Req)
    when Action == <<"register">>; Action == <<"login">> ->
    case dgiot_parse_handler:login_by_mail_phone(Account) of
        {ok, #{<<"sessionToken">> := Token} = UserInfo} ->
            NewReq = dgiot_parse_handler:set_cookies("sessionToken", Token, Req),
            case maps:get(<<"callback">>, Args, undefined) of
                undefined ->
                    {200, #{}, UserInfo, NewReq};
                Callback ->
                    {302, #{<<"Location">> => Callback}, UserInfo, NewReq}
            end;
        {error, #{<<"code">> := 101} = Err} ->
            {404, Err};
        {error, Reason} ->
            {error, Reason}
    end;

%% 重置密码
handle(#{<<"Action">> := <<"passwordreset">>, <<"account">> := Account, <<"password">> := Password }, _Req) ->
    Query = #{
      <<"where">> => #{
          <<"phone">> => Account
      }
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{ <<"results">> := [] }} ->
            {404, #{ <<"error">> => <<"Not Found">> }};
        {ok, #{ <<"results">> := [#{ <<"objectId">> := UserId }] }} ->
            dgiot_parse:update_object(<<"_User">>, UserId, #{ <<"password">> => Password });
        {error, Reason} ->
            {error, Reason}
    end;

%% 修改用户信息
handle(#{<<"Action">> := <<"modifyuser">>, <<"account">> := Account, <<"user">> := User } = _Body, _Req) ->
    Query = #{
        <<"where">> => #{
            <<"phone">> => Account
        }
    },
    ?LOG(info,"_Body ~p ",[_Body]),
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{ <<"results">> := [] }} ->
            {404, #{ <<"error">> => <<"Not Found">> }};
        {ok, #{ <<"results">> := [#{ <<"objectId">> := UserId }] }} ->
            dgiot_parse:update_object(<<"_User">>, UserId, User);
        {error, Reason} ->
            {error, Reason}
    end.


