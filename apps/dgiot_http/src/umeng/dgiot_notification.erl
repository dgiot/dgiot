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

-module(dgiot_notification).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_http.hrl").
-dgiot_data("ets").
-export([init_ets/0]).
%% API
-export([send_sms/2, send_sms/3, send_sms/5, send_sms/6, send_sms/7]).

-export([send_email/1, test_email/0]).

-export([send_verification_code/2, check_verification_code/2]).

-export([get_newbody/1]).

init_ets() ->
    dgiot_data:init(?NOTIFICATION).

% 验证类消息
send_verification_code(NationCode, Key) ->
    case catch dgiot_cache:get(Key) of
        Random when is_integer(Random) ->
            {error, unicode:characters_to_binary(<<"验证码未过期，查看验证码！"/utf8>>)};
        _ ->
            rand:seed(exs1024),
            Rand = 10000 + erlang:round(rand:uniform() * 10000),
            TTL = 3,
            case dgiot_notification:send_sms(NationCode, Key, [Rand, TTL]) of
                {ok, _Ext} ->
                    dgiot_cache:set(Key, Rand, TTL * 60),
                    {ok, #{<<"expire">> => TTL * 60}};
                {error, Err} ->
                    {error, Err}
            end
    end.

check_verification_code(Key, Code) ->
    case catch dgiot_cache:get(Key) of
        Code ->
            dgiot_cache:delete(Key),
            true;
        _ ->
            false
    end.



send_sms(Mobile, Params) ->
    send_sms("+86", Mobile, Params).
send_sms(NationCode, Mobile, Params) ->
    TplId = dgiot_utils:to_list(application:get_env(dgiot_http, tencent_sms_notification_templateId, <<"">>)),
    AppId = dgiot_utils:to_list(application:get_env(dgiot_http, tencent_sms_appid, <<"">>)),
    AppKey = dgiot_utils:to_list(application:get_env(dgiot_http, tencent_sms_appkey, <<"">>)),
    send_sms(NationCode, Mobile, Params, AppId, AppKey, TplId).
send_sms(Mobile, Params, AppId, AppKey, TplId) ->
    send_sms("+86", Mobile, Params, AppId, AppKey, TplId, <<>>).
send_sms(NationCode, Mobile, Params, AppId, AppKey, TplId) ->
    send_sms(NationCode, Mobile, Params, AppId, AppKey, TplId, <<>>).
send_sms(NationCode, Mobile, Params, AppId, AppKey, TplId, Ext) ->
    AppId_b =
        case is_binary(AppId) of
            true ->
                binary_to_list(AppId);
            _ ->
                AppId
        end,
    Random = dgiot_utils:to_list(1000 + rand:uniform(1000)),
    Url =
        case is_list(Mobile) of
            true ->
                "https://yun.tim.qq.com/v5/tlssmssvr/sendmultisms2?sdkappid=" ++ AppId_b ++ "&random=" ++ Random;
            false ->
                "https://yun.tim.qq.com/v5/tlssmssvr/sendsms?sdkappid=" ++ AppId_b ++ "&random=" ++ Random
        end,
    case re:run(NationCode, <<"\\+(\\d{1,3})">>, [{capture, all, binary}]) of % "+86" 自动转换二进制
        {match, [_, NationCode1]} ->

            {MobileStr, Tel_b} =
                case is_list(Mobile) of
                    true ->    %%短信多发
                        ListMobile = lists:foldl(fun(#{<<"mobile">> := Num}, Acc) -> Acc ++ [binary_to_list(Num)]
                                                 end, [], Mobile),
                        {string:join(ListMobile, ","), Mobile};
                    _ ->    %%短信单发
                        case is_binary(Mobile) of
                            true ->
                                {Mobile, #{<<"mobile">> => Mobile, <<"nationcode">> => NationCode1}};
                            _ ->
                                {Mobile, #{<<"mobile">> => unicode:characters_to_binary(Mobile), <<"nationcode">> => NationCode1}}
                        end
                end,
            Now = dgiot_datetime:nowstamp(),
            AppKey_b =
                case is_binary(AppKey) of
                    true ->
                        binary_to_list(AppKey);
                    _ ->
                        AppKey
                end,
            SigStr = io_lib:format("appkey=~s&random=~s&time=~s&mobile=~s", [AppKey_b, Random, integer_to_list(Now), MobileStr]),
            Sig_b = dgiot_utils:to_binary(string:to_lower(binary_to_list(<<<<Y>> || <<X:4>> <= crypto:hash(sha256, SigStr), Y <- integer_to_list(X, 16)>>))),
            FunParams =
                fun(X, Acc) ->
                    case is_binary(X) of
                        true ->
                            Acc ++ [X];
                        _ ->
                            Acc ++ [unicode:characters_to_binary(X)]
                    end
                end,
            Params_b = lists:foldl(FunParams, [], Params),
            case is_binary(TplId) of
                true ->
                    TplId_b = TplId;
                _ ->
                    TplId_b = unicode:characters_to_binary(TplId)
            end,
            Data = #{
                <<"tpl_id">> => TplId_b,
                <<"ext">> => Ext,
                <<"extend">> => <<>>,
                <<"params">> => Params_b,
                <<"sign">> => <<"dgiot"/utf8>>,
                <<"tel">> => Tel_b,
                <<"time">> => Now,
                <<"sig">> => Sig_b
            },
            Request = {Url, [], "application/json", jsx:encode(Data)},

            case catch httpc:request(post, Request, [], [{body_format, binary}]) of
                {ok, {{_HTTPVersion, 200, "OK"}, _Header, ResBody}} ->
                    case jsx:decode(ResBody, [{labels, binary}, return_maps]) of
                        #{<<"result">> := 0, <<"ext">> := Ext} ->
                            {ok, Ext};
                        #{<<"errmsg">> := ErrMsg, <<"result">> := Code} ->
                            ?LOG(error, "Send SMS ERROR: ~p->~ts, Request:~p~n", [list_to_binary(Url), unicode:characters_to_binary(ErrMsg), Data]),
                            {error, #{code => Code, error => ErrMsg}}
                    end;
                {Err, Reason} when Err == error; Err == 'EXIT' ->
                    ?LOG(error, "Send SMS ERROR: ~p, ~p, ~p~n", [Url, Data, Reason]),
                    {error, #{code => 1, error => list_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        _ ->
            {error, #{code => 1, error => <<"NationCode is illegality">>}}
    end.

test_email() ->
    Map = #{
        <<"from">> => <<"18257190166@163.com">>,
        <<"to">> => <<"463544084@qq.com">>,
        <<"subject">> => <<"测试邮件"/utf8>>,
        <<"fromdes">> => <<"徐 <18257190166@163.com>"/utf8>>,
        <<"todes">> => <<"唐 <463544084@qq.com>"/utf8>>,
        <<"data">> => <<"唐 中文测试 欢迎访问 https://github.com/dgiot "/utf8>>,
        <<"relay">> => <<"smtp.163.com">>,
        <<"username">> => <<"18257190166@163.com">>,
        <<"password">> => <<"ALRFYEVAFSITDXSX">>
    },
    send_email(Map).

%%-type options() :: [{ssl, boolean()} |
%%{tls, always | never | if_available} |
%%{tls_options, list()} | % ssl:option() / ssl:tls_client_option()
%%{sockopts, [gen_tcp:connect_option()]} |
%%{port, inet:port_number()} |
%%{timeout, timeout()} |
%%{relay, inet:ip_address() | inet:hostname()} |
%%{no_mx_lookups, boolean()} |
%%{auth, always | never | if_available} |
%%{hostname, string()} |
%%{retries, non_neg_integer()} |
%%{username, string()} |
%%{password, string()} |
%%{trace_fun, fun( (Fmt :: string(), Args :: [any()]) -> any() )}].

send_email(Email) ->
    From = maps:get(<<"from">>, Email, <<"dgiot@163.com">>),
    To = maps:get(<<"to">>, Email, <<"3333333@qq.com">>),
    ArrTo = binary:split(To, <<$,>>, [global, trim]),
    Subject = maps:get(<<"subject">>, Email, <<"测试邮件"/utf8>>),
    FromDes = maps:get(<<"fromdes">>, Email, <<"dgiot开源物联网 <dgiot@163.com>"/utf8>>),
    ToDes = maps:get(<<"todes">>, Email, <<"dgiot用户 <3333333@qq.com>"/utf8>>),
    Data = maps:get(<<"data">>, Email, <<"dgiot邮件 中文测试 欢迎访问 https://github.com/dgiot "/utf8>>),
    BodyBin = <<"Subject: ", Subject/binary, "\r\n", "From: ", FromDes/binary, "\r\n", "To:", ToDes/binary, "\r\n\r\n", Data/binary>>,
    Relay = maps:get(<<"relay">>, Email, <<"smtp.163.com">>),
    UserName = maps:get(<<"username">>, Email, <<"dgiot@163.com">>),
    PassWord = maps:get(<<"password">>, Email, <<"yourstmppassword">>),
    gen_smtp_client:send({From, ArrTo, BodyBin}, [{relay, Relay}, {username, UserName}, {password, PassWord}]).


get_newbody(#{<<"results">> := Results} = Map) ->
    NewResults =
        lists:foldl(fun(Notificat, Acc) ->
            Acc ++ [get_newbody(Notificat)]
                    end, [], Results),
    Map#{<<"results">> => NewResults};

get_newbody(#{<<"objectId">> := _Notificatid} = Map) ->
    Content = maps:get(<<"content">>, Map, #{}),
    ProductId = maps:get(<<"_productid">>, Content, <<>>),
    DeviceId = maps:get(<<"_deviceid">>, Content, <<>>),
    ViewId = maps:get(<<"_viewid">>, Content, <<>>),
    ProductName =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"name">> := Name}} ->
                Name;
            _ ->
                <<>>
        end,
    DevAddr =
        case dgiot_device:lookup(DeviceId) of
            {ok, #{<<"devaddr">> := Devaddr}} ->
                Devaddr;
            _ ->
                <<>>
        end,
    Map#{
        <<"productid">> => ProductId,
        <<"productname">> => ProductName,
        <<"deviceid">> => DeviceId,
        <<"devaddr">> => DevAddr,
        <<"viewid">> => ViewId
    };

get_newbody(Body) ->
    Body.
