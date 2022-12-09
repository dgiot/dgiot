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
-export([save_configuration/0]).

-export([send_sms/3, send_sms/4, send_sms/7, send_sms/8]).

-export([send_email/1, test_email/0]).

-export([send_verification_code/2, check_verification_code/2]).

-export([get_newbody/1, get_Mobile/3, get_Emails/3, get_users/2]).

init_ets() ->
    dgiot_data:init(?CONFIGURATION),
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

send_sms(Mobile, TplId, Params) ->
    send_sms("+86", Mobile, TplId, Params).

send_sms(NationCode, Mobile, TplId, Params) ->
    AppId = dgiot_utils:to_list(dgiot_data:get(?CONFIGURATION, sms_appid)),
    AppKey = dgiot_utils:to_list(dgiot_data:get(?CONFIGURATION, sms_appkey)),
    Sign = dgiot_data:get(?CONFIGURATION, sms_sign),
    send_sms(NationCode, Mobile, TplId, Params, AppId, AppKey, Sign).

send_sms(NationCode, Mobile, TplId, Params, AppId, AppKey, Sign) ->
    send_sms(NationCode, Mobile, TplId, Params, AppId, AppKey, Sign, <<>>).

send_sms(NationCode, Mobile, TplId, Params, AppId, AppKey, Sign, Ext) ->
    Random = dgiot_utils:to_list(1000 + rand:uniform(1000)),
    case re:run(NationCode, <<"\\+(\\d{1,3})">>, [{capture, all, binary}]) of % "+86" 自动转换二进制
        {match, [_, NationCode1]} ->
            {Url, MobileStr, Tel_b} =
                case is_list(Mobile) of
                    true ->
                        %%短信多发
                        MultipleUrl = "https://yun.tim.qq.com/v5/tlssmssvr/sendmultisms2?sdkappid=" ++ AppId ++ "&random=" ++ Random,
                        ListMobile =
                            lists:foldl(fun(#{<<"mobile">> := Num}, Acc) ->
                                Acc ++ [dgiot_utils:to_list(Num)]
                                        end, [], Mobile),
                        {MultipleUrl, string:join(ListMobile, ","), Mobile};
                    _ ->
                        %%短信单发
                        SingleUrl = "https://yun.tim.qq.com/v5/tlssmssvr/sendsms?sdkappid=" ++ AppId ++ "&random=" ++ Random,
                        {SingleUrl, Mobile, #{<<"mobile">> => unicode:characters_to_binary(Mobile), <<"nationcode">> => NationCode1}}
                end,
            Now = dgiot_datetime:nowstamp(),
            SigStr = "appkey=" ++ AppKey ++ "&random=" ++ Random ++ "&time=" ++ dgiot_utils:to_list(Now) ++ "&mobile=" ++ MobileStr,
            Sig = dgiot_utils:to_binary(string:to_lower(binary_to_list(<<<<Y>> || <<X:4>> <= crypto:hash(sha256, SigStr), Y <- integer_to_list(X, 16)>>))),
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
                <<"sign">> => Sign,
                <<"tel">> => Tel_b,
                <<"time">> => Now,
                <<"sig">> => Sig
            },
            Request = {Url, [], "application/json", jsx:encode(Data)},
            case catch httpc:request(post, Request, [], [{body_format, binary}]) of
                {ok, {{_HTTPVersion, 200, "OK"}, _Header, ResBody}} ->
                    case jsx:decode(ResBody, [{labels, binary}, return_maps]) of
                        #{<<"result">> := 0, <<"errmsg">> := <<"OK">>} ->
                            {ok, #{<<"code">> => 200, <<"msg">> => <<"send success">>}};
                        #{<<"errmsg">> := ErrMsg, <<"result">> := Code} ->
                            {ok, #{<<"code">> => Code, <<"error">> => ErrMsg}}
                    end;
                {Err, Reason} when Err == error; Err == 'EXIT' ->
                    {ok, #{<<"code">> => 1, <<"error">> => Reason}}
            end;
        _ ->
            {ok, #{<<"code">> => 1, <<"error">> => <<"NationCode is illegality">>}}
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
    From = dgiot_data:get(?CONFIGURATION, mail_username),
    UserName = dgiot_data:get(?CONFIGURATION, mail_username),
    PassWord = dgiot_data:get(?CONFIGURATION, mail_password),
    Relay = dgiot_data:get(?CONFIGURATION, mail_smtp),

    To = maps:get(<<"to">>, Email, <<"3333333@qq.com">>),
    ArrTo = binary:split(To, <<$,>>, [global, trim]),
    Subject = maps:get(<<"subject">>, Email, <<"测试邮件"/utf8>>),
    FromDes = maps:get(<<"fromdes">>, Email, <<"dgiot开源物联网 <dgiot@163.com>"/utf8>>),
    ToDes = maps:get(<<"todes">>, Email, <<"dgiot用户 <3333333@qq.com>"/utf8>>),
    Data = maps:get(<<"data">>, Email, <<"dgiot邮件 中文测试 欢迎访问 https://github.com/dgiot "/utf8>>),
    BodyBin = <<"Subject: ", Subject/binary, "\r\n", "From: ", FromDes/binary, "\r\n", "To:", ToDes/binary, "\r\n\r\n", Data/binary>>,

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

get_Mobile(_, _, NotifRoleid) when size(NotifRoleid) > 0 ->
    UserIds =
        lists:foldl(fun(Roleid, Acc) ->
            Acc ++ dgiot_parse_id:get_userids(Roleid)
                    end, [], binary:split(NotifRoleid, <<$,>>, [global, trim])),
    UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
    {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
    lists:foldl(fun(User, Acc1) ->
        Phone = maps:get(<<"phone">>, User, ""),
        case dgiot_utils:is_phone(Phone) of
            true ->
                Acc1 ++ [#{<<"mobile">> => Phone, <<"nationcode">> => <<"86">>}];
            _ ->
                Acc1
        end
                end, <<>>, Users);

get_Mobile(_, RoleId, _) when size(RoleId) > 0 ->
    UserIds = dgiot_parse_id:get_userids(RoleId),
    UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
    {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
    lists:foldl(fun(User, Acc1) ->
        Phone = maps:get(<<"phone">>, User, ""),
        case dgiot_utils:is_phone(Phone) of
            true ->
                Acc1 ++ [#{<<"mobile">> => Phone, <<"nationcode">> => <<"86">>}];
            _ ->
                Acc1
        end
                end, <<>>, Users);

get_Mobile(DeviceId, _, _) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := Acl}} ->
            lists:foldl(fun(X, Acc) ->
                BinX = atom_to_binary(X),
                case BinX of
                    <<"role:", Name/binary>> ->
                        case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                            <<"where">> => #{<<"name">> => Name}}) of
                            {ok, #{<<"results">> := [Role]}} ->
                                #{<<"objectId">> := RoleId1} = Role,
                                UserIds = dgiot_parse_id:get_userids(RoleId1),
                                UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
                                {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
                                lists:foldl(fun(User, Acc1) ->
                                    Phone = maps:get(<<"phone">>, User, ""),
                                    case dgiot_utils:is_phone(Phone) of
                                        true ->
                                            Acc1 ++ [#{<<"mobile">> => Phone, <<"nationcode">> => <<"86">>}];
                                        _ ->
                                            Acc1
                                    end
                                            end, Acc, Users);
                            _ ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
                        end, <<>>, Acl);
        _ ->
            <<>>
    end.

get_Emails(_, _, NotifRoleid) when size(NotifRoleid) > 0 ->
    UserIds =
        lists:foldl(fun(Roleid, Acc) ->
            Acc ++ dgiot_parse_id:get_userids(Roleid)
                    end, [], binary:split(<<"e562014215">>, <<$,>>, [global, trim])),
    UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
    {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
    lists:foldl(fun(User, Acc1) ->
        Email = maps:get(<<"email">>, User, ""),
        case dgiot_utils:is_email(Email) of
            true ->
                <<Acc1/binary, Email/binary, ",">>;
            _ ->
                Acc1
        end
                end, <<>>, Users);


get_Emails(_, RoleId, _) when size(RoleId) > 0 ->
    UserIds = dgiot_parse_id:get_userids(RoleId),
    UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
    {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
    lists:foldl(fun(User, Acc1) ->
        Email = maps:get(<<"email">>, User, ""),
        case dgiot_utils:is_email(Email) of
            true ->
                <<Acc1/binary, Email/binary, ",">>;
            _ ->
                Acc1
        end
                end, <<>>, Users);

get_Emails(DeviceId, _, _) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := Acl}} ->
            lists:foldl(fun(X, Acc) ->
                BinX = atom_to_binary(X),
                case BinX of
                    <<"role:", Name/binary>> ->
                        case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                            <<"where">> => #{<<"name">> => Name}}) of
                            {ok, #{<<"results">> := [Role]}} ->
                                #{<<"objectId">> := RoleId1} = Role,
                                UserIds = dgiot_parse_id:get_userids(RoleId1),
                                UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
                                {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
                                lists:foldl(fun(User, Acc1) ->
                                    Email = maps:get(<<"email">>, User, ""),
                                    case dgiot_utils:is_email(Email) of
                                        true ->
                                            <<Acc1/binary, Email/binary, ",">>;
                                        _ ->
                                            Acc1
                                    end
                                            end, Acc, Users);
                            _ ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
                        end, <<>>, Acl);
        _ ->
            <<>>
    end.


get_users(DeviceId, RoleId) ->
    case RoleId of
        <<>> ->
            case dgiot_device:lookup(DeviceId) of
                {ok, #{<<"acl">> := Acl}} ->
                    lists:foldl(fun(X, Acc) ->
                        BinX = atom_to_binary(X),
                        case BinX of
                            <<"role:", Name/binary>> ->
                                case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                                    <<"where">> => #{<<"name">> => Name}}) of
                                    {ok, #{<<"results">> := [Role]}} ->
                                        #{<<"objectId">> := RoleId1} = Role,
                                        UserIds = dgiot_parse_id:get_userids(RoleId1),
                                        UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
                                        {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
                                        Acc ++ Users;
                                    _ ->
                                        Acc
                                end;
                            _ ->
                                Acc
                        end
                                end, [], Acl);
                _ ->
                    []
            end;
        _ ->
            UserIds = dgiot_parse_id:get_userids(RoleId),
            UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
            {ok, #{<<"results">> := Users}} = dgiot_parse:query_object(<<"_User">>, UsersQuery),
            Users
    end.

save_configuration() ->
    DictId = dgiot_parse_id:get_dictid(<<"dgiotconfiguration">>, <<"configuration">>, <<"configuration">>, <<"dgiotconfiguration">>),
    case dgiot_parse:get_object(<<"Dict">>, DictId) of
        {ok, #{<<"data">> := Data}} ->
            Sms = maps:get(<<"sms">>, Data, #{}),
            Sms_appid = maps:get(<<"appid">>, Sms, <<"">>),
            Sms_appkey = maps:get(<<"appkey">>, Sms, <<"">>),
            Sms_sign = maps:get(<<"sign">>, Sms, <<"">>),
            dgiot_data:insert(?CONFIGURATION, sms_appid, Sms_appid),
            dgiot_data:insert(?CONFIGURATION, sms_appkey, Sms_appkey),
            dgiot_data:insert(?CONFIGURATION, sms_sign, Sms_sign),
            Mail = maps:get(<<"mail">>, Data, #{}),
            Mail_username = maps:get(<<"username">>, Mail, <<"">>),
            Mail_password = maps:get(<<"password">>, Mail, <<"">>),
            Mail_smtp = maps:get(<<"smtp">>, Mail, <<"">>),
            dgiot_data:insert(?CONFIGURATION, mail_username, Mail_username),
            dgiot_data:insert(?CONFIGURATION, mail_password, Mail_password),
            dgiot_data:insert(?CONFIGURATION, mail_smtp, Mail_smtp);
        _ ->
            pass
    end.
