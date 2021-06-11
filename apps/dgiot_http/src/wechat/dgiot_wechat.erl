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


%% 集成指南 https://developer.umeng.com/docs/67966/detail/149296#h1--i-9
%% 常见错误码 https://developer.umeng.com/docs/67966/detail/149332

-module(dgiot_wechat).
-author("jonhl").

-include_lib("dgiot/include/logger.hrl").

-export([
    post_sns/2,
    get_sns/1
]).

%%https://api.weixin.qq.com/sns/jscode2session?appid=APPID&secret=SECRET&js_code=JSCODE&grant_type=authorization_code
post_sns(UserId, JSCODE) ->
    inets:start(),
    AppId = dgiot_utils:to_binary(dgiot:get_env(wechat_appid)),
    Secret = dgiot_utils:to_binary(dgiot:get_env(wechat_secret)),
    Url = "https://api.weixin.qq.com/sns/jscode2session?appid=" ++ dgiot_utils:to_list(AppId) ++ "&secret=" ++ dgiot_utils:to_list(Secret) ++
        "&js_code=" ++ dgiot_utils:to_list(JSCODE) ++ "&grant_type=authorization_code",
    case httpc:request(Url) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Json = list_to_binary(Body),
            ?LOG(info, "Json ~p", [Json]),
            case jsx:is_json(Json) of
                true ->
                    case jsx:decode(Json, [{labels, binary}, return_maps]) of
                        #{<<"openid">> := OPENID, <<"session_key">> := _SESSIONKEY} ->
                            ?LOG(info, "~p ~p", [OPENID, _SESSIONKEY]),
                            NewTag =
                                case dgiot_parse:get_object(<<"_User">>, UserId) of
                                    {ok, #{<<"tag">> := Tag}} ->
                                        Tag;
                                    _ -> #{}
                                end,
                            dgiot_parse:update_object(<<"_User">>, UserId, NewTag#{<<"wechat">> => #{<<"openid">> => OPENID}});
                        _Result ->
                            {error, <<"not find openid">>}
                    end;
                false -> {error, <<"not find openid">>}
            end;
        _Error ->
            _Error
    end.

get_sns(OPENID) ->
    case dgiot_parse:query_object(<<"_User">>, #{<<"where">> => #{<<"tag.wxopenid">> => OPENID}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := UserId, <<"username">> := Name} | _]}} ->
            {ok, UserInfo} = dgiot_parse_handler:create_session(UserId, dgiot_auth:ttl(), Name),
            {ok, UserInfo#{<<"openid">> => OPENID, <<"status">> => <<"bind">>}};
        _ ->
            {ok, #{<<"openid">> => OPENID, <<"status">> => <<"unbind">>}}
    end.



