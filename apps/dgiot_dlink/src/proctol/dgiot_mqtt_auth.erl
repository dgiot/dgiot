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

-module(dgiot_mqtt_auth).

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_dlink.hrl").

-define(TABLE, emqx_user).
%% Auth callbacks
-export([
    check/3
    , description/0
]).

check(#{username := Username}, AuthResult, _)
    when Username == <<"anonymous">> orelse Username == undefined orelse Username == <<>> ->
%%    io:format("~s ~p Username: ~p~n", [?FILE, ?LINE, Username]),
    {ok, AuthResult#{anonymous => true, auth_result => success}};

%% 当 clientid 和 password 为token 且相等的时候为用户登录
check(#{clientid := Token, username := UserId, password := Token}, AuthResult, #{hash_type := _HashType}) ->
%%    io:format("~s ~p UserId: ~p~n", [?FILE, ?LINE, UserId]),
    case dgiot_auth:get_session(Token) of
        #{<<"objectId">> := UserId} ->
            {stop, AuthResult#{anonymous => false, auth_result => success}};
        _ ->
            {stop, AuthResult#{anonymous => false, auth_result => password_error}}
    end;

%% ClientID 为deviceID 或者 deviceAddr Username 为 ProductID
%% 1、 尝试1型1密认证
%% 2、 尝试ClientID 为deviceID的1机1密认证
%% 3、 尝试ClientID 为deviceAddr的1机1密认证
check(#{clientid := ClientId, username := ProductID, password := Password}, AuthResult, #{hash_type := _HashType}) ->
%%    io:format("~s ~p ProductID: ~p ClientId ~p Password ~p ~n", [?FILE, ?LINE, ProductID, ClientId, Password]),
    case dgiot_product:lookup_prod(ProductID) of
        {ok, #{<<"productSecret">> := Password}} ->
            {stop, AuthResult#{anonymous => false, auth_result => success}};
        _ ->
            case dgiot_device:lookup(ClientId) of
                {ok, #{<<"devicesecret">> := Password}} ->
                    {stop, AuthResult#{anonymous => false, auth_result => success}};
                _ ->
                    DeviceID = dgiot_parse_id:get_deviceid(ProductID, ClientId),
                    case dgiot_device:lookup(DeviceID) of
                        {ok, #{<<"devicesecret">> := Password, <<"devaddr">> := ClientId}} ->
                            {stop, AuthResult#{anonymous => false, auth_result => success}};
                        _ ->
                            {stop, AuthResult#{anonymous => false, auth_result => password_error}}
                    end
            end
    end;

check(#{username := _Username}, AuthResult, _) ->
%%    io:format("~s ~p Username: ~p~n", [?FILE, ?LINE, _Username]),
    {stop, AuthResult#{anonymous => false, auth_result => password_error}}.

description() -> "Authentication with Mnesia".
