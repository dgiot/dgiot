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

-module(dgiot_aliyun_auth).
-author("root").
-compile(nowarn_deprecated_function).
-include_lib("dgiot/include/logger.hrl").

-define(ALIYUN_VERSION, "2017-03-21").
-define(PUSH_KEY, "GE6T11xiXY").
-define(LIVE_KEY, "rzdZr4nvtc").
-define(PUSH_URL, "rtmp://push.iotn2n.com").
-define(LIVE_URL_RTMP, "rtmp://live.iotn2n.com").
-define(LIVE_URL_HTTP, "http://live.iotn2n.com").
-define(SignatureVersion, "1.0").
-define(AccessKeyId, <<"LTAI3jscIxezgvmt">>).
-define(AccessKeySecret, <<"WsVErdNZsfcX4PUaQZ4KkALT2Lc98o">>).
-define(DomainName, "http://vod.cn-shanghai.aliyuncs.com").
-define(UPLOAD_CALLBACK_URL, "http://25.40.204.194:8081").

-define(UPLOAD_HOST, "http://dgiotpump.oss-cn-shanghai.aliyuns.com").

%% API
-export([aliyun_upload/0
    , filepath_to_url/3
    , get_video_playauth/1
    , create_upload_image/0
    , url_generator/4
    , get_play_info/1
    , get_iso_8601/1
    , jwtlogin/1
    , get_categorys/0
    , get_category/2
    , get_ListAllCategoryConsole/0
    , getCookie/0
]).

-define(EXPIRE, 300).

aliyun_upload() ->
    AccessKeySecret = dgiot:get_env(aliyun_accessKeySecret),
    UPLOAD_CALLBACK_URL = dgiot:get_env(aliyun_uploadCallbackUrl),
    AccessKeyId = dgiot:get_env(aliyun_accessKeyId),
    UPLOAD_HOST = dgiot:get_env(aliyun_uploadHost),
    Expire_syncpoint = 1612345678,
    Expire = get_iso_8601(Expire_syncpoint),
    Policy_dict = [
        {<<"conditions">>, [[<<"starts-with">>, <<"$key">>, <<"">>]]},
        {<<"expiration">>, dgiot_utils:to_binary(Expire)}
    ],
    Policy = jsx:encode(Policy_dict, [{space, 1}]),
    Policy_encode = base64:encode(Policy),
    H = crypto:mac(sha, dgiot_utils:to_binary(AccessKeySecret), Policy_encode),
    Sign_result = base64:encode(H),
    Callback_dict = [
        {<<"callbackBodyType">>, <<"application/x-www-form-urlencoded">>},
        {<<"callbackBody">>, <<"filename=${object}&size=${size}&mimeType=${mimeType}&height=${imageInfo.height}&width=${imageInfo.width}">>},
        {<<"callbackUrl">>, dgiot_utils:to_binary(UPLOAD_CALLBACK_URL)}
    ],
    Callback_param = jsx:encode(Callback_dict, [{space, 1}]),
    Base64_callback_body = base64:encode(Callback_param),
    Token_dict = [
        {<<"accessid">>, dgiot_utils:to_binary(AccessKeyId)},
        {<<"host">>, dgiot_utils:to_binary(UPLOAD_HOST)},
        {<<"policy">>, Policy_encode},
        {<<"signature">>, Sign_result},
        {<<"expire">>, Expire_syncpoint},
        {<<"dir">>, <<"">>},
        {<<"callback">>, Base64_callback_body}
    ],
    maps:from_list(Token_dict).

get_iso_8601(Expire_syncpoint) ->
    dgiot_datetime:utc(Expire_syncpoint).

filepath_to_url(#{<<"bucket">> := Bucket, <<"end_point">> := EndPoint, <<"object_name">> := ObjectName}, <<"aliyun">>, Expire) ->
    AccessKeyId = dgiot:get_env(aliyun_accessKeyId),
    Sign = oss_signature("GET", Expire, Bucket, ObjectName),
    lists:concat(["https://", dgiot_utils:to_list(Bucket), ".", dgiot_utils:to_list(EndPoint), "/", dgiot_utils:to_list(ObjectName), "?",
        "Expires=", dgiot_utils:to_list(Expire),
        "&OSSAccessKeyId=", AccessKeyId,
        "&Signature=", Sign]);

filepath_to_url(_FilePath, <<"tencentyun">>, _Expire) ->
    throw({error, <<"not support tencentyun now">>});

filepath_to_url(_FilePath, _, _Expire) ->
    throw({error, <<"unknown file source">>}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%aliyun_api%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec url_generator(string(), string(), integer(), rtmp|push|m3u8|flv) -> binary().
url_generator(AppName, StreamName, EndTime, rtmp) ->
    real_url_generator(AppName, StreamName, EndTime, ?LIVE_URL_RTMP, ?LIVE_KEY);

url_generator(AppName, StreamName, EndTime, push) ->
    real_url_generator(AppName, StreamName, EndTime, ?PUSH_URL, ?PUSH_KEY);

url_generator(AppName, StreamName, EndTime, m3u8) ->
    real_url_generator(AppName, StreamName ++ ".m3u8", EndTime, ?LIVE_URL_HTTP, ?LIVE_KEY);

url_generator(AppName, StreamName, EndTime, flv) ->
    real_url_generator(AppName, StreamName ++ ".flv", EndTime, ?LIVE_URL_HTTP, ?LIVE_KEY).


-spec get_play_info(#{string() := string()}) -> {ok, list()} | {error, list()}.
get_play_info(Args = #{"VideoId" := _VideoId}) ->
    BaseArgs = maps:merge(base_args("GetPlayInfo"), Args),
    aliyun_api_request(BaseArgs).


-spec get_video_playauth(#{string() := string()}) -> {ok, list()} | {error, list()}.
get_video_playauth(Args = #{"VideoId" := _VideoId}) ->
    BaseArgs = maps:merge(base_args("GetVideoPlayAuth"), Args = #{"VideoId" => _VideoId}),
    aliyun_api_request(BaseArgs).

create_upload_image() ->
    BaseArgs = maps:merge(base_args("CreateUploadImage"), #{"ImageType" => "default"}),
    aliyun_api_request(BaseArgs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%aliyun_private%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

real_url_generator(AppName, StreamName, EndTime, Head, Key) ->
    Rand = "0",
    Uid = "0",
    Plaintxt1 = lists:concat(["/", AppName, "/", StreamName]),
    Plaintxt2 = lists:concat([EndTime, "-", Rand, "-", Uid, "-"]),
    ?LOG(info, "~p", [Plaintxt1 ++ "-" ++ Plaintxt2 ++ Key]),
    Live = crypto:hash(md5, Plaintxt1 ++ "-" ++ Plaintxt2 ++ Key),
    dgiot_utils:to_binary(Head ++ Plaintxt1 ++
        "?auth_key=" ++ Plaintxt2 ++ string:to_lower(dgiot_utils:to_list(dgiot_utils:binary_to_hex(Live)))).

aliyun_api_request(Args) ->
    List = lists:sort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, maps:to_list(Args)),
    Data = "GET&%2F&" ++ http_uri:encode(lists:concat(lists:join("&", [K ++ "=" ++ V || {K, V} <- List]))),
    Signature = http_uri:encode(dgiot_utils:to_list(base64:encode(crypto:hmac(sha, dgiot_utils:to_list(?AccessKeySecret) ++ "&", Data)))),
    Url = to_aliyun_url(Args#{"Signature" => Signature}),
    httpc:request(Url).


to_aliyun_url(Datas) ->
    ?DomainName ++ "?" ++ lists:concat(lists:join("&", [K ++ "=" ++ V || {K, V} <- maps:to_list(Datas)])).

uuid() ->
    {A, B, C} = emqx_guid:new(),
    dgiot_utils:to_list(A) ++ dgiot_utils:to_list(B) ++ dgiot_utils:to_list(C).

base_args(Action) ->
    #{
        "AccessKeyId" => dgiot_utils:to_list(?AccessKeyId),
        "Action" => Action,
        "Format" => "JSON",
        "SignatureMethod" => "HMAC-SHA1",
        "SignatureNonce" => uuid(),
        "SignatureVersion" => ?SignatureVersion,
        "TimeStamp" => http_uri:encode(dgiot_datetime:utc()),
        "Version" => ?ALIYUN_VERSION
    }.

oss_signature(VerB, Expire, Bucket, ObjectName) ->
    LineBreak = "\n",
    String = lists:concat([dgiot_utils:to_list(VerB), LineBreak, LineBreak, LineBreak, dgiot_utils:to_list(Expire), LineBreak, "/", dgiot_utils:to_list(Bucket), "/", dgiot_utils:to_list(ObjectName)]),
    http_uri:encode(dgiot_utils:to_list(base64:encode(crypto:hmac(sha, dgiot_utils:to_list(?AccessKeySecret), String)))).


jwtlogin(Idtoken) ->
    Path = code:priv_dir(dgiot_http),
    {ok, PublcPem} = file:read_file(Path ++ "/jwt/jwt_public_key_pkc8.pem"),
    Algorithm = dgiot_utils:to_atom(application:get_env(dgiot_http, jwt_algorithm, <<"rs256">>)),
    Md5Idtoken = dgiot_utils:to_md5(Idtoken),
    case dgiot_data:get({userinfo, Md5Idtoken}) of
        not_find ->
%        使用公钥解析Id_token
            case catch jwerl:verify(Idtoken, Algorithm, PublcPem) of
                {'EXIT', _Error} ->
                    {ok, #{<<"code">> => 500, <<"msg">> => <<"operation error">>}};
                {ok, #{<<"udAccountUuid">> := UdAccountUuid, username := Username} = TokenData} ->
                    Mobile = dgiot_utils:to_binary(maps:get(<<"mobile">>, TokenData, <<"">>)),
                    Email = dgiot_utils:to_binary(maps:get(email, TokenData, <<Mobile/binary, "@email.com">>)),
                    Name = dgiot_utils:to_binary(maps:get(name, TokenData, Username)),
                    UserBody = #{
                        <<"email">> => Email,
                        <<"emailVerified">> => true,
                        <<"nick">> => Name,
                        <<"password">> => UdAccountUuid,
                        <<"phone">> => Mobile,
                        <<"username">> => Username,
                        <<"tag">> => #{
                            <<"companyinfo">> => #{
                                <<"Copyright">> => <<"© 2017-2021 温岭水泵远程检测中心 Corporation, All Rights Reserved"/utf8>>,
                                <<"backgroundimage">> => <<"/dgiot_file/user/profile/Klht7ERlYn_companyinfo_backgroundimage.jpg?timestamp=1636974751417">>,
                                <<"logo">> => <<"/group1/default/20211019/18/33/4/微信图片_20210705103613.jpg"/utf8>>,
                                <<"name">> => <<"温岭水泵远程检测中心"/utf8>>,
                                <<"title">> => <<"欢迎登录温岭水泵远程检测中心"/utf8>>,
                                <<"_mimg">> => <<"/dgiot_file/user/profile/Klht7ERlYn_companyinfo__mimg.jpeg?timestamp=1635245663651">>,
                                <<"_pcimg">> => <<"/dgiot_file/user/profile/Klht7ERlYn_companyinfo__pcimg.jpeg?timestamp=1635245685140">>
                            },
                            <<"theme">> => #{
                                <<"columnStyle">> => <<"horizontal">>,
                                <<"fixedHeader">> => false,
                                <<"layout">> => <<"horizontal">>,
                                <<"pictureSwitch">> => false,
                                <<"showFullScreen">> => false,
                                <<"showLanguage">> => false,
                                <<"showNotice">> => false,
                                <<"showProgressBar">> => false,
                                <<"showRefresh">> => false,
                                <<"showSearch">> => false,
                                <<"showTabs">> => true,
                                <<"showTabsBarIcon">> => false,
                                <<"showTheme">> => true,
                                <<"showThemeSetting">> => false,
                                <<"tabsBarStyle">> => <<"smart">>,
                                <<"themeName">> => <<"white">>
                            },
                            <<"userinfo">> => #{
                                <<"avatar">> => <<"/dgiot_file/user/profile/Klht7ERlYn_userinfo_avatar.png?timestamp=1637914878741">>,
                                <<"phone">> => Mobile,
                                <<"sex">> => "男"
                            },
                            <<"jwt">> => TokenData}},
                    SessionToken = dgiot_parse_auth:get_token(<<230, 181, 153, 233, 135, 140, 229, 138, 158, 228, 186, 167, 228, 184, 154, 229, 164, 167, 232, 132, 145>>),
%                   用户匹配查找
                    case dgiot_parse:query_object(<<"_User">>, #{<<"where">> => #{<<"username">> => Username}}) of
                        {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
                            dgiot_parse_auth:create_user(UserBody#{<<"department">> => <<"459e01521c">>}, SessionToken);
                        {ok, #{<<"results">> := [#{<<"objectId">> := UserId, <<"tag">> := Tag} | _]}} ->
                            dgiot_parse:update_object(<<"_User">>, UserId, #{<<"tag">> => Tag#{<<"jwt">> => TokenData}})
                    end,
%                   验证账户登录获取用户信息
                    UserInfo =
                        case dgiot_parse_auth:login_by_account(Username, UdAccountUuid) of
                            {ok, #{<<"objectId">> := _UserId} = UserInfo1} ->
                                UserInfo1#{<<"code">> => 200, <<"username">> => Username, <<"state">> => TokenData, <<"msg">> => <<"operation success">>};
                            {error, _Msg} ->
                                UserInfo2 =
                                    case dgiot_parse:query_object(<<"_User">>, #{<<"where">> => #{<<"username">> => Username}}) of
                                        {ok, #{<<"results">> := Results1}} when length(Results1) == 0 ->
                                            UserInfo3 = UserBody#{<<"department">> => <<"459e01521c">>},
                                            dgiot_parse_auth:create_user(UserInfo3, SessionToken),
                                            UserInfo3;
                                        {ok, #{<<"results">> := [#{<<"objectId">> := UserId1, <<"tag">> := Tag1} = UserInfo1 | _]}} ->
                                            dgiot_parse:update_object(<<"_User">>, UserId1, #{<<"tag">> => Tag1#{<<"jwt">> => TokenData}}),
                                            UserInfo1
                                    end,
                                UserInfo2#{<<"code">> => 200, <<"username">> => Username, <<"state">> => TokenData, <<"msg">> => <<"operation success">>}
                        end,
                    dgiot_data:insert({userinfo, Md5Idtoken}, {UserInfo, Username, UdAccountUuid}),
                    {ok, UserInfo};
                _Error ->
                    {ok, #{<<"code">> => 500, <<"msg">> => <<"id_token invalid">>}}
            end;
        {UserInfo2, Username2, UdAccountUuid2} ->
            case dgiot_parse_auth:login_by_account(Username2, UdAccountUuid2) of
                {ok, #{<<"objectId">> := _UserId} = UserInfo3} ->
                    {ok, maps:merge(UserInfo2, UserInfo3)};
                {error, _Msg} ->
                    {ok, UserInfo2}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%aliyun_test%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%test_get_play_info() ->
%%    {ok, {{_, 200, "OK"}, _, _}} = get_play_info(#{"VideoId" => "bbf8adb6632e4655a98b4405b03b7c44"}).
%%
%%test_get_video_playauth() ->
%%    {ok, {{_, 200, "OK"}, _, _}} = get_video_playauth(#{"VideoId" => "bbf8adb6632e4655a98b4405b03b7c44"}).

get_category(Cookie, CategoryType) ->
    Url = "http://iot.console.aliyun.com/data/api.json",
    Params = dgiot_httpc:urlencode(jsx:encode(#{<<"RegionId">> => <<"cn-shanghai">>, <<"CategoryType">> => CategoryType})),
    Body = <<"product=iot&action=ListCategoryStdAbility&params=", Params/binary, "&region=cn-shanghai&sec_token=1AJt6fDbMKfeoZgEmT75y6&umid=Y20874b2d55043e407a92a7fcc6063d7c&collina=119%23Ml8rT4kfMxflWMMztssanuNz%2FCiLQxRASn3f2UVJKhtcbCumrD%2FM6caLhhPPlGEfSmQZSI4%2BJ3r%2BFxY3T7vKBMiEEN1nZ4Yw%2FEyIbghGMZA8R2VO8NHAloNSRSgFrtN8EUvZ40cy3eHPRJVeGLNLFHDpX4l%2B7gNyIn6NKLwpFCHUyhNS8hXKprey8XaWxseEJO9PhbWFx2PtYQfJ6ah17JHMlHQmUMofycaS8iGYc4mn1YlgVg6N437LTnFBHQTKSItqq6JQr9u3uSf8pW%2BK0tP5b1u64mTmKlujxG9G3oA8RJBONt8L9eN25SSUdAA89J2p4SMnQr3UROjONJkL8zsBBBlOTPr8fU%2BS4Q5hdoA8ZGEINN3Kz0ASRSSUdAr8fUVbz4bL%2F9q8RJVVNE3d9oAzNBqooNFdpJWS4lkLdDX8RJQ%2B8Sdw9dPg7SSe3AA89Fbv4lktILvLRq3%2BNt8G9eAzoPSedwbV9v2C4Q9GdeA8qVRVNN3a0n3mTPSe3AA89ALf4l5Oi6H1yUZONNFL9F%2BzRBq7doE8fmrI4l9LdeHYR2BONNAndFHXqmBh2VAYaUqlqC1rSyqOfoe7gGkjMu0heSNnT1yoTKAOcpOIO24gNHWaaXi1XGTUZt2TJ4TS8Qf%2Ft75GiukwzHo0Pk9KnkrH8mfKtJg2f4e5z7Wrhv%2FzYXbnmpQu0Tj0fZ3QK9vjm8Qg0tgY4iog6Ld8hZ6kZ1o5qCKrkgYs1lXcCdwuLjYQWeYmLQY2PBMy6GO7TBYvwN4itmVD0RyoV7dUNvVI8hCHr4rE4c%2FtNlNILgk5YXdxMu0tMC66Ajln8LBY8t5N%2FJ04%2FukbROwcIW2kS8075nxlFfHsWQtaHVUoAMcpSAHc%2BS%2BlClG7B6zKFAZ20uIBkH5CyT%2FZjyuOuSdtcrnWq6RGAm%2FHE%2F2N11SDN60scaCGMtaJ1MVN5fqJJzctkQErxy5dqRyIW4zTKZSSYGgLr1NM24h22uDi%2BevRbSvo19Ist%2BZon%2B3K%2BJJkwKoKGAQKmxOT0z0ox%2B%2FckD7bI3oWdBGmrlzHx9X28qPnLRyHuW%2FEZy2SQq0kLQHYlcEFPDZMfUld7zSmNMpVp2EEoTtrqt2EKqqqoPpujT55JV6kuymuRjgTpNm%3D">>,
    case httpc:request(post, {Url, [{"Cookie", binary_to_list(Cookie)}], "application/x-www-form-urlencoded; charset=UTF-8", Body}, [{timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_HTTPVersion, 200, _ReasonPhrase}, _Headers, Bin}} ->
            #{<<"data">> := #{<<"Data">> := #{<<"AbilityInfo">> := AbilityInfo}}} = jsx:decode(Bin, [{labels, binary}, return_maps]),
            {ok, AbilityInfo};
        {error, Reason} ->
            {error, Reason}
    end.

get_ListAllCategoryConsole() ->
    Url = "https://iot.console.aliyun.com/data/api.json?_action=ListAllCategoryConsole",
    {ok, Cookie} = dgiot_aliyun_auth:getCookie(),
    Body = <<>>,
    case httpc:request(post, {Url, [{"Cookie", binary_to_list(Cookie)}], "application/x-www-form-urlencoded; charset=UTF-8", Body}, [{timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_HTTPVersion, 200, _ReasonPhrase}, _Headers, Bin}} ->
            #{<<"data">> := #{<<"Data">> := #{<<"AbilityInfo">> := AbilityInfo}}} = jsx:decode(Bin, [{labels, binary}, return_maps]),
            {ok, AbilityInfo};
        {error, Reason} ->
            {error, Reason}
    end.

try_get_category(Cookie, Type) ->
    case get_category(Cookie, Type) of
        {ok, AbilityInfo} ->
            ?LOG(info, "~p:~p~n", [Type, AbilityInfo]),
            AbilityInfo;
        _ ->
            try_get_category(Cookie, Type)
    end.
get_categorys() ->
    {ok, Cookie} = getCookie(),
    dgiot_utils:format("~p", [Cookie]),
%%    Cookie = <<"cna=DkBtGsN+XnYCAbeeROWKLCR7; channel=bFSTcHN2%2FxDYyijD%2BCLV4vIbsjGrZmztkHbkOTlwi3XiByQoZantQxAb%2BJ2GoQuH%2B29TdmEmtrQ0HnRwogpg6w%3D%3D; aliyun_choice=CN; aliyun_lang=zh; _uab_collina=164489179900937299843432; _umdata=G14CFDB599B535EFC844DE670F61AE64E512C36; changelog_date=1624579200000; session-lead-visited/6049b3869191edede7ffbb6f=false; currentRegionId=cn-shanghai; iot_regionid=cn-shanghai; login_aliyunid="w4c****@qq.com"; login_aliyunid_csrf=_csrf_tk_1073347328016184; login_aliyunid_pk=1357932084858144; aliyun_country=CN; aliyun_site=CN; iot_instanceid=W3siMTg3OTUwMDk5ODMyOTIxMCI6ImlvdC0wNnowMGlpODZ1NzRsOGEifSx7IjEzNTc5MzIwODQ4NTgxNDQiOiIifV0=; serviceUnitCode=; tfstk=cIiFB3i9NHKEh-EWkkZyOoPgYBBNawHnEGyb-2BD5GIczpa3gsAPwRV9DRy5Q_4h.; l=eBEPbk1cg73PjQ5oBOfwhurza77tdIRfguPzaNbMiOCP94sW5zDCW60Y8m9XCnGVnsQDR3lrOrBaBcLSoy4EC85Hah5eMjVtndLh.; isg=BKSkOzNEmid0_e2xCeufrYWRdaKWPcin8WoDkr7FFm8yaUUz5knPNo-DKcHxsQD_">>
    {ok, #{<<"results">> := Datas}} = dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"type">> => <<"abilityInfo">>}}),
    A = lists:foldl(
        fun(#{<<"data">> := #{<<"CategoryType">> := Type}}, Acc) ->
            case get(Type) of
                undefined ->
                    AbilityInfo = try_get_category(Cookie, Type),
                    put(Type, true),
                    [#{
                        <<"type">> => Type,
                        <<"ACL">> => #{<<"*">> => #{
                            <<"read">> => true,
                            <<"write">> => false
                        }},
                        <<"data">> => #{
                            <<"Ability">> => AbilityInfo
                        }
                    } | Acc];
                _ ->
                    Acc
            end
        end, [], Datas),
    file:write_file("AbilityInfo.json", jsx:encode(A)).

%%
%% @description: 读取文件并返回
%%
getCookie() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/", dgiot_utils:to_list("cookie.txt")]),
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
            {error, Reason};
        {ok, Bin} ->
            {ok, Bin}
    end.

