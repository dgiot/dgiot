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

-module(dgiot_auth).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
%% API
-export([pre_check/4]).
-export([check_auth/3]).
-export([put_session/2]).
-export([put_session/3]).
-export([get_session/1]).

-export([ttl/0]).


%% session 期限
ttl() ->
    application:get_env(dgiot_api, session_expiration, 86400).

%%%===================================================================
%%% default check auth Callback
%%%===================================================================

-spec check_auth(OperationID :: atom(), Args :: map(), Req :: dgiot_req:req()) ->
    {true, Context :: #{binary() => any()}, Req :: dgiot_req:req()} |
    {false, Result :: #{binary() => any()}, Req :: dgiot_req:req()} |
    {switch_handler, Mod :: module(), Req :: dgiot_req:req()}.

check_auth(OperationID, #{<<"username">> := UserName, <<"password">> := Password}, Req) ->
    is_authorized(OperationID, {UserName, Password}, Req);
check_auth(OperationID, #{<<"apiKey">> := ApiKey}, Req) ->
    is_authorized(OperationID, ApiKey, Req).


%%%===================================================================
%%% pre check args
%%%===================================================================

-spec pre_check(OperationID::atom(), LogicHandler :: atom(), AuthList :: list() | atom(), Req :: dgiot_req:req()) ->
    {ok, Args :: #{binary() => any()}, Req :: dgiot_req:req()} |
    {error, Err :: binary(), Req :: dgiot_req:req()}.

pre_check(_OperationID, _LogicHandler, [], Req) ->
    {error, <<"unauthorized">>, Req};

pre_check(OperationID, LogicHandler, [{<<"BasicAuth">> = Key, _Map} | AuthList], Req) ->
    case dgiot_req:get_value(<<"header">>, <<"authorization">>, Req) of
        {undefined, Req1} ->
            pre_check(OperationID, LogicHandler, AuthList, Req1);
        {<<"Basic ", Bin/binary>>, Req1} ->
            case re:split(base64:decode(Bin), <<":">>) of
                [UserName, Password] when byte_size(UserName) > 0, byte_size(Password) > 0 ->
                    Args = #{
                        <<"type">> => Key,
                        <<"username">> => UserName,
                        <<"password">> => Password
                    },
                    {ok, Args, Req1};
                _ ->
                    pre_check(OperationID, LogicHandler, AuthList, Req1)
            end
    end;

pre_check(OperationID, LogicHandler, [{Key, #{<<"in">> := From, <<"name">> := Name}} | AuthList], Req) ->
    case dgiot_req:get_value(From, Name, Req) of
        {undefined, Req1} ->
            pre_check(OperationID, LogicHandler, AuthList, Req1);
        {ApiKey, Req1} ->
            Args = #{
                <<"type">> => Key,
                <<"apiKey">> => ApiKey
            },
            {ok, Args, Req1}
    end.


%%%===================================================================
%%% 内部函数
%%%===================================================================

%% Basic auth
is_authorized(OperationID, {UserName, Password}, Req) ->
    Key = erlang:md5(binary_to_list(<<UserName/binary, Password/binary>>)),
    Token = case catch dgiot_cache:get(Key) of {'EXIT', _} -> undefined;  T -> T end,
    case is_binary(Token) andalso byte_size(Token) > 0 andalso get_session(Token) =/= undefined of
        true ->
            is_authorized(OperationID, Token, Req);
        false ->
            % login
            case dgiot_parse_auth:login_by_account(UserName, Password) of
                {ok, #{<<"sessionToken">> := SessionToken}} ->
                    dgiot_cache:set(Key, SessionToken, ttl()),
                    is_authorized(OperationID, SessionToken, Req);
                {error, Msg} ->
                    ?LOG(error,"~p,~p~n", [UserName, Msg]),
                    {false, Msg, Req}
            end
    end;

%% Token auth, in body, query, header, cookie
is_authorized(OperationID, Token, Req) ->
    case get_session(Token) of
        undefined ->
            {false, #{<<"code">> => 209, <<"error">> => <<"unauthorized">>}, Req};
        #{<<"rules">> := Rules} = UserInfo ->
            put_session(UserInfo#{<<"sessionToken">> => Token}, ttl()),
            %% 检查操作权限
            Action = list_to_binary(string:to_upper(atom_to_list(OperationID))),
            case lists:member(Action, Rules) of
                false ->
                    {forbidden, #{<<"code">> => 119, <<"error">> => <<Action/binary, " Forbidden">>}, Req};
                true ->
                    {true, #{<<"user">> => UserInfo, <<"sessionToken">> => Token}, Req}
            end
    end.

put_session(#{<<"sessionToken">> := SessionToken} = UserInfo, TTL) ->
    put_session(SessionToken, maps:remove(<<"sessionToken">>, UserInfo), dgiot_utils:to_int(TTL)).

put_session(SessionToken, UserInfo, TTL) ->
    dgiot_cache:set({SessionToken, parse}, jsx:encode(UserInfo), dgiot_utils:to_int(TTL)),
    timer:sleep(500),
    ok.

get_session(Token) ->
    case catch dgiot_cache:get({Token, parse}) of
        <<>> ->
            undefined;
        {'EXIT', Err} ->
            ?LOG(error,"dgiot_cache get, ~p~n", [Err]),
            undefined;
        User when is_binary(User) ->
            jsx:decode(User, [{labels, binary}, return_maps])
    end.
