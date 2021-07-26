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
    get_wechat_index/1,
    sendSubscribe/2,
    sendTemplate/0,
    get_wechat_map/1,
    get_device_info/2
]).

%% https://api.weixin.qq.com/sns/jscode2session?appid=APPID&secret=SECRET&js_code=JSCODE&grant_type=authorization_code
%% wechat绑定
post_sns(UserName, Password, OpenId) ->
    case dgiot_parse:query_object(<<"_User">>, #{<<"where">> => #{<<"tag.wechat.openid">> => OpenId}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := _UserId, <<"username">> := Name} | _]}} ->
%%            {ok, UserInfo} = dgiot_parse_handler:create_session(UserId, dgiot_auth:ttl(), Name),
            {error, <<OpenId/binary, " is bind ", Name/binary>>};
        _ ->
            case dgiot_parse:login(UserName, Password) of
                {ok, #{<<"objectId">> := _UserId, <<"tag">> := #{<<"wechat">> := #{<<"openid">> := OPENID}}}} when size(OPENID) > 0 ->
                    {error, <<UserName/binary, "is bind">>};
                {ok, #{<<"objectId">> := UserId, <<"tag">> := Tag, <<"username">> := Name}} ->
                    dgiot_parse:update_object(<<"_User">>, UserId, #{<<"tag">> => Tag#{<<"wechat">> => #{<<"openid">> => OpenId}}}),
                    {ok, UserInfo} = dgiot_parse_handler:create_session(UserId, dgiot_auth:ttl(), Name),
                    {ok, UserInfo};
                {error, Msg} ->
                    {error, Msg}
            end
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

%% 发送小程序订阅消息
%% Data 小程序订阅消息内容
%% page       要跳转到小程序的页面
%% templateId 模板消息编号
%% touser     消息接收者openId
%% POST https://api.weixin.qq.com/cgi-bin/message/wxopen/template/uniform_send?access_token=ACCESS_TOKEN
%% dgiot_wechat:sendSubscribe().
sendSubscribe(UserId, #{<<"lang">> := Lang,
    <<"miniprogramstate">> := Miniprogramstate,
    <<"page">> := Page,
    <<"templateid">> := Templateid,
    <<"data">> := _Data} = _Args
) ->
    case dgiot_parse:get_object(<<"_User">>, UserId) of
        {ok, #{<<"tag">> := #{<<"wechat">> := #{<<"openid">> := OpenId}}}} when size(OpenId) > 0 ->
            AppId = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_appid, <<"">>)),
            Secret = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_secret, <<"">>)),
            Url = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=" ++ dgiot_utils:to_list(AppId) ++ "&secret=" ++ dgiot_utils:to_list(Secret),
            case httpc:request(Url) of
                {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                    Json = list_to_binary(Body),
                    case jsx:is_json(Json) of
                        true ->
                            case jsx:decode(Json, [{labels, binary}, return_maps]) of
                                #{<<"access_token">> := AccessToken, <<"expires_in">> := _ExpiresIn} ->
                                    SubscribeUrl = "https://api.weixin.qq.com/cgi-bin/message/subscribe/send?access_token=" ++ dgiot_utils:to_list(AccessToken),
                                    ?LOG(info, "SubscribeUrl ~p", [SubscribeUrl]),
                                    Subscribe = #{<<"touser">> => OpenId,
                                        <<"template_id">> => Templateid,
                                        <<"page">> => Page,
                                        <<"miniprogram_state">> => Miniprogramstate,
                                        <<"lang">> => Lang,
                                        <<"data">> =>
                                        #{<<"thing1">> => #{<<"value">> => <<"太阳能控制器"/utf8>>},
                                            <<"character_string8">> => #{<<"value">> => <<"0123456789">>},
                                            <<"thing5">> => #{<<"value">> => <<"杭州 余杭区 良渚"/utf8>>},
                                            <<"date4">> => #{<<"value">> => <<"2021年7月07日 15:30:30"/utf8>>},
                                            <<"thing12">> => #{<<"value">> => <<"没电了"/utf8>>}}},
                                    ?LOG(info, "Subscribe ~p", [Subscribe]),
                                    Data1 = dgiot_utils:to_list(jsx:encode(Subscribe)),
                                    R = httpc:request(post, {SubscribeUrl, [], "application/x-www-form-urlencoded", Data1}, [{timeout, 5000}, {connect_timeout, 10000}], [{body_format, binary}]),
                                    ?LOG(info, "R ~p", [R]);
                                _Result ->
                                    {error, <<"not find access_token">>}
                            end;
                        false -> {error, <<"not find access_token">>}
                    end;
                _Error ->
                    _Error
            end;
        _ -> {error, <<"user unbind openid">>}
    end.

%% 总控台
get_wechat_index(SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 1000}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results}} ->
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
            {error, <<"no device">>}
    end.


%% 发送模板消息
%% Data 消息内容
%% templateId 模板消息编号
%% touser     消息接收者openId
%% POST https://api.weixin.qq.com/cgi-bin/message/wxopen/template/uniform_send?access_token=ACCESS_TOKEN
sendTemplate() ->
    AppId = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_appid, <<"">>)),
    Secret = dgiot_utils:to_binary(application:get_env(dgiot_http, wechat_secret, <<"">>)),
%%  "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=wx62de5b17388a361f&secret=bd71815d6ff657b0f8a0032848c9079b",
    Url = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=" ++ dgiot_utils:to_list(AppId) ++ "&secret=" ++ dgiot_utils:to_list(Secret),
    case httpc:request(Url) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Json = list_to_binary(Body),
            case jsx:is_json(Json) of
                true ->
                    case jsx:decode(Json, [{labels, binary}, return_maps]) of
                        #{<<"access_token">> := AccessToken, <<"expires_in">> := _ExpiresIn} ->
                            TemplateUrl = "https://api.weixin.qq.com/cgi-bin/message/template/send?access_token=" ++ dgiot_utils:to_list(AccessToken),
                            Data = #{<<"touser">> => <<"o_8aC4mM0KhYpIe7ddDV8_0NZUZY">>,
                                <<"template_id">> => <<"ngqIpbwh8bUfcSsECmogfXcV14J0tQlEpBO27izEYtY">>,
                                <<"url">> => <<"www.baidu.com">>,
                                <<"data">> => #{<<"first">> => #{<<"value">> => <<"thing2">>, <<"color">> => <<"#173177">>},
                                    <<"thing3">> => #{<<"value">> => <<"thing3">>, <<"color">> => <<"#173177">>},
                                    <<"thing4">> => #{<<"value">> => <<"thing4">>, <<"color">> => <<"#173177">>},
                                    <<"thing5">> => #{<<"value">> => <<"thing5">>, <<"color">> => <<"#173177">>},
                                    <<"thing6">> => #{<<"value">> => <<"thing6">>, <<"color">> => <<"#173177">>}}},
                            Data1 = dgiot_utils:to_list(Data),
                            ?LOG(info, "SubscribeUrl ~s Data1 ~s", [TemplateUrl, Data1]),
                            R = httpc:request(post, {TemplateUrl, [], "application/x-www-form-urlencoded", Data1}, [{timeout, 5000}, {connect_timeout, 10000}], [{body_format, binary}]),
                            ?LOG(info, "~p", [R]);
                        _Result ->
                            {error, <<"not find access_token">>}
                    end;
                false -> {error, <<"not find access_token">>}
            end;
        _Error ->
            _Error
    end.

get_wechat_map(SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 1000}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            NewResult =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"status">> := Status, <<"location">> := #{<<"latitude">> := Latitude, <<"longitude">> := Longitude}} ->
                            Acc ++ [#{<<"id">> => ObjectId, <<"title">> => Name, <<"status">> => Status, <<"latitude">> => Latitude, <<"longitude">> => Longitude, <<"joinCluster">> => true}];
                        _ ->
                            Acc
                    end
                            end, [], Results),
            {ok, #{<<"results">> => NewResult}};
        _ ->
            {error, <<"no device">>}
    end.

get_device_info(Deviceid, SessionToken) ->
    case dgiot_parse:get_object(<<"Device">>, Deviceid, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"product">> := #{<<"objectId">> := ProductId}, <<"basedata">> := Basedata} = Result} ->
            {NewParams, ProductName} =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"config">> := #{<<"basedate">> := #{<<"params">> := Params}}, <<"name">> := Productname}} ->
                        lists:foldl(fun(Param, {Acc, _Acc1}) ->
                            Identifier = maps:get(<<"identifier">>, Param),
                            case maps:find(Identifier, Basedata) of
                                error ->
                                    {Acc, Productname};
                                {ok, Value} ->
                                    {Acc ++ [Param#{<<"value">> => Value}], Productname}
                            end
                                    end, {[], <<>>}, Params);
                    _ ->
                        {[], <<" ">>}
                end,
            {ok, Result#{<<"params">> => NewParams, <<"productname">> => ProductName}};
        _ ->
            {error, []}
    end.
