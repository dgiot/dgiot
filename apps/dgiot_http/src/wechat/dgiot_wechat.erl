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
    get_device_info/2,
    get_notification/6
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
sendSubscribe(UserId, Data) ->
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
                                    ?LOG(debug, "SubscribeUrl ~p", [SubscribeUrl]),
                                    Subscribe = #{<<"touser">> => OpenId,
                                        <<"template_id">> => <<"9Fmc0vtA7vnh_HtoVtXJy_cRDOnIk1ubniO_Oe3WatU">>,
                                        <<"page">> => <<"pages/alarm/alarm">>,
                                        <<"miniprogram_state">> => <<"formal">>,
                                        <<"lang">> => <<"zh_CN">>,
                                        <<"data">> => Data},
                                    Data1 = dgiot_utils:to_list(jiffy:encode(Subscribe)),
                                    R = httpc:request(post, {SubscribeUrl, [], "application/x-www-form-urlencoded", Data1}, [{timeout, 5000}, {connect_timeout, 10000}], [{body_format, binary}]),
                                    ?LOG(debug, "R ~p", [R]);
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
            NotificationCount =
                case dgiot_parse:query_object(<<"Notification">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 1}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                    {ok, #{<<"count">> := NotCount, <<"results">> := _}} ->
                        NotCount;
                    _ ->
                        0
                end,
            UnPanalarmCount =
                case dgiot_parse:query_object(<<"Notification">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 1, <<"where">> => #{<<"status">> => 0}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                    {ok, #{<<"count">> := UnCount, <<"results">> := _}} ->
                        UnCount;
                    _ ->
                        0
                end,
            Carousel =
                case dgiot_parse:query_object(<<"Dict">>, #{<<"limit">> => 20, <<"where">> => #{<<"type">> => <<"57e4f8154e">>}}) of
                    {ok, #{<<"results">> := Dicts}} ->
                        lists:foldl(fun(X, Acc) ->
                            case X of
                                #{<<"data">> := #{<<"imgurl">> := Imgurl, <<"webUrl">> := WebUrl}} ->
                                    Acc ++ [#{<<"imgurl">> => Imgurl, <<"webUrl">> => WebUrl}];
                                _ ->
                                    Acc
                            end
                                    end, [], Dicts);
                    _ ->
                        []
                end,
            {ok, #{<<"deviceCount">> => Count, <<"devicelist">> => Results,
                <<"onlineCount">> => length(ONLINE), <<"onlinelist">> => ONLINE,
                <<"offlineCount">> => length(OFFLINE), <<"offlinelist">> => OFFLINE,
                <<"notificationCount">> => NotificationCount, <<"unPanalarmCount">> => UnPanalarmCount,
                <<"carousel">> => Carousel}};
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
                                <<"template_id">> => <<"">>,
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
                                    {Acc ++ [Param#{<<"value">> => <<>>}], Productname};
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

%% 告警列表
%% dgiot_parse:query_object(<<"Notification">>, #{<<"keys">> => [<<"count(*)">>],<<"where">> => #{<<"type">> => #{<<"$regex">> => <<"c1e44b39f0">>}}}).
get_notification(ProductId1, SessionToken, Order, Limit, Skip, Where) ->
    Where1 =
        case ProductId1 of
            <<"all">> ->
                Where;
            ProductId2 ->
                Where#{<<"type">> => #{<<"$regex">> => ProductId2}}
        end,
    case dgiot_parse:query_object(<<"Notification">>, #{<<"keys">> => [<<"count(*)">>], <<"order">> => Order, <<"limit">> => Limit, <<"skip">> => Skip, <<"where">> => Where1}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results}} ->
            NewResult =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"objectId">> := ObjectId, <<"type">> := Type, <<"public">> := Public, <<"status">> := Status, <<"content">> := Content, <<"process">> := Process, <<"createdAt">> := Createdat} ->
                            Alertstatus = maps:get(<<"alertstatus">>, Content, true),
                            DeviceId = maps:get(<<"_deviceid">>, Content, <<"">>),
                            Productid = maps:get(<<"_productid">>, Content, <<"">>),
                            {DeviceName, Devaddr, Address} =
                                case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                                    {ok, #{<<"name">> := DeviceName1, <<"devaddr">> := Devaddr1, <<"detail">> := Detail}} ->
                                        Address1 = maps:get(<<"address">>, Detail, <<"无位置"/utf8>>),
                                        {DeviceName1, Devaddr1, Address1};
                                    _ ->
                                        {<<"">>, <<"">>, <<"无位置"/utf8>>}
                                end,
                            Newdate = dgiot_datetime:format(dgiot_datetime:to_localtime(Createdat), <<"YY-MM-DD HH:NN:SS">>),
                            Result =
                                case binary:split(Type, <<$_>>, [global, trim]) of
                                    [Productid, <<"status">>] ->
                                        case dgiot_parse:get_object(<<"Product">>, Productid) of
                                            {ok, #{<<"name">> := ProductName}} ->
                                                #{<<"objectId">> => ObjectId, <<"dynamicform">> => [#{<<"设备编号"/utf8>> => Devaddr}, #{<<"设备地址"/utf8>> => Address}, #{<<"报警内容"/utf8>> => <<"设备"/utf8, DeviceName/binary, "离线"/utf8>>}, #{<<"离线时间"/utf8>> => Newdate}], <<"alertstatus">> => Alertstatus, <<"productname">> => ProductName, <<"devicename">> => DeviceName, <<"process">> => Process, <<"content">> => Content, <<"public">> => Public, <<"status">> => Status, <<"createdAt">> => Createdat};
                                            _ ->
                                                Acc
                                        end;
                                    [ProductId, AlertId] ->
                                        case dgiot_parse:get_object(<<"Product">>, ProductId) of
                                            {ok, #{<<"name">> := ProductName, <<"config">> := #{<<"parser">> := Parser}}} ->
                                                lists:foldl(fun(P, Par) ->
                                                    case P of
                                                        #{<<"uid">> := AlertId, <<"config">> := #{<<"formDesc">> := FormDesc}} ->
                                                            FormD =
                                                                maps:fold(fun(Key, Value1, Form) ->
                                                                    case maps:find(Key, Content) of
                                                                        {ok, Value} ->
                                                                            Label = maps:get(<<"label">>, Value1),
                                                                            Form ++ [#{Label => Value}];
                                                                        _ ->
                                                                            Label = maps:get(<<"label">>, Value1),
                                                                            Default = maps:get(<<"default">>, Value1, <<>>),
                                                                            Form ++ [#{Label => Default}]
                                                                    end
                                                                          end, [], FormDesc),
                                                            Par#{<<"dynamicform">> => FormD ++ [#{<<"报警时间"/utf8>> => Newdate}]};
                                                        _Oth ->
                                                            Par
                                                    end
                                                            end, #{<<"objectId">> => ObjectId, <<"alertstatus">> => Alertstatus, <<"productname">> => ProductName, <<"devicename">> => DeviceName, <<"process">> => Process, <<"public">> => Public, <<"status">> => Status, <<"createdAt">> => Newdate}, Parser);
                                            _Other ->
                                                Acc
                                        end;
                                    _Other1 ->
                                        Acc
                                end,
                            ?LOG(info, "Result ~p", [Result]),
                            Acc ++ [Result];
                        _Other2 ->
                            Acc
                    end
                            end, [], Results),
            {ok, #{<<"count">> => Count, <<"results">> => NewResult}};
        _ ->
            {error, <<"no device">>}
    end.
