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
    post_sns/3,
    get_sns/1,
    unbind_sns/1,
    get_wechat_index/1
]).

%% https://api.weixin.qq.com/sns/jscode2session?appid=APPID&secret=SECRET&js_code=JSCODE&grant_type=authorization_code
%% wechat绑定
post_sns(UserName, Password, OpenId) ->
    case dgiot_parse:login(UserName, Password) of
        {ok, #{<<"objectId">> := _UserId, <<"tag">> := #{<<"wechat">> := #{<<"openid">> := OpenId}}}} when size(OpenId) > 0 ->
            {error, <<"is bind">>};
        {ok, #{<<"objectId">> := UserId, <<"tag">> := Tag, <<"username">> := Name}} ->
            dgiot_parse:update_object(<<"_User">>, UserId, #{<<"tag">> => Tag#{<<"wechat">> => #{<<"openid">> => OpenId}}}),
            {ok, UserInfo} = dgiot_parse_handler:create_session(UserId, dgiot_auth:ttl(), Name),
            {ok, UserInfo};
        {error, Msg} ->
            {error, Msg}
    end.

%% wechat解绑
unbind_sns(UserId) ->
    NewTag =
        case dgiot_parse:get_object(<<"_User">>, UserId) of
            {ok, #{<<"tag">> := Tag}} ->
                Tag;
            _ -> #{}
        end,
    dgiot_parse:update_object(<<"_User">>, UserId, #{<<"tag">> => NewTag#{<<"wechat">> => #{<<"openid">> => <<"">>}}}),
    {ok, #{<<"msg">> => <<"succeed">>}}.

%% wechat登陆
get_sns(Jscode) ->
    inets:start(),
    AppId = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_appid, <<"">>)),
    Secret = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_secret, <<"">>)),
    Url = "https://api.weixin.qq.com/sns/jscode2session?appid=" ++ dgiot_utils:to_list(AppId) ++ "&secret=" ++ dgiot_utils:to_list(Secret) ++
        "&js_code=" ++ dgiot_utils:to_list(Jscode) ++ "&grant_type=authorization_code",
    ?LOG(info, "Url ~s", [Url]),
    case httpc:request(Url) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Json = list_to_binary(Body),
            case jsx:is_json(Json) of
                true ->
                    case jsx:decode(Json, [{labels, binary}, return_maps]) of
                        #{<<"openid">> := OPENID, <<"session_key">> := _SESSIONKEY} ->
                            ?LOG(info, "~p ~p", [OPENID, _SESSIONKEY]),
                            case dgiot_parse:query_object(<<"_User">>, #{<<"where">> => #{<<"tag.wechat.openid">> => OPENID}}) of
                                {ok, #{<<"results">> := [#{<<"objectId">> := UserId, <<"username">> := Name} | _]}} ->
                                    {ok, UserInfo} = dgiot_parse_handler:create_session(UserId, dgiot_auth:ttl(), Name),
                                    {ok, UserInfo#{<<"openid">> => OPENID, <<"status">> => <<"bind">>}};
                                _ ->
                                    {ok, #{<<"openid">> => OPENID, <<"status">> => <<"unbind">>}}
                            end;
                        _Result ->
                            {error, <<"not find openid">>}
                    end;
                false -> {error, <<"not find openid">>}
            end;
        _Error ->
            _Error
    end.


%% Data 消息内容
%% page       要跳转到小程序的页面
%% templateId 模板消息编号
%% touser     消息接收者openId
%% POST https://api.weixin.qq.com/cgi-bin/message/wxopen/template/uniform_send?access_token=ACCESS_TOKEN
%%sendMessage(Data, Touser, Page, TemplateId) ->
%%    AppId = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_appid, <<"">>)),
%%    Secret = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_secret, <<"">>)),
%%%%  "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=wx62de5b17388a361f&secret=bd71815d6ff657b0f8a0032848c9079b",
%%    Url = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=" ++ dgiot_utils:to_list(AppId) ++ "&secret=" ++ dgiot_utils:to_list(Secret),
%%    case httpc:request(Url) of
%%        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
%%            Json = list_to_binary(Body),
%%            case jsx:is_json(Json) of
%%                true ->
%%                    case jsx:decode(Json, [{labels, binary}, return_maps]) of
%%                        #{<<"access_token">> := AccessToken, <<"expires_in">> := _ExpiresIn} ->
%%                            TemplateUrl = "https://api.weixin.qq.com/cgi-bin/message/template/send?access_token=" + dgiot_utils:to_list(AccessToken),
%%                            ?LOG(info, "~p ~p", [OPENID, _SESSIONKEY]);
%%                        _Result ->
%%                            {error, <<"not find access_token">>}
%%                    end;
%%                false -> {error, <<"not find access_token">>}
%%            end;
%%        _Error ->
%%            _Error
%%    end.


get_wechat_index(SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 1000}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results} = R} ->
            ?LOG(info, "Results ~p", [Results]),
            ?LOG(info, "R ~p", [R]),
            {ONLINE, OFFLINE} =
                lists:foldl(fun(X, {Online, Offline}) ->
                    case X of
                        #{<<"status">> := <<"ONLINE">>} ->
                            {Online ++ [X], Offline};
                        #{<<"status">> := <<"OFFLINE">>} ->
                            {Online, Offline ++ [X]};
                        _ ->
                            {Online, Offline}
                    end
                            end, {[], []}, Results),
            {ok, #{<<"deviceCount">> => Count, <<"devicelist">> => Results,
                <<"onlineCount">> => length(ONLINE), <<"onlinelist">> => ONLINE,
                <<"offlineCount">> => length(OFFLINE), <<"offlinelist">> => OFFLINE,
                <<"panalarmDevice">> => 0, <<"unPanalarmDevice">> => 0,
                <<"carousel">> => [#{<<"imgurl">> => <<"https://www.baidu.com/img/flexible/logo/pc/peak-result.png">>, <<"webUrl">> => <<"www.baidu.com">>}]}};
        _ ->
            pass
    end.




