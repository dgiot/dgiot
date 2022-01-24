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

-module(dgiot_umeng).
-author("jonhl").

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_http.hrl").
-export([
    send/2,
    send/3,
    test_broadcast/0,
    test_customizedcast/0,
    add_notification/3,
    save_devicestatus/2,
    save_notification/3,
    sendSubscribe/3
]).

test_broadcast() ->
    UserId = undefined,
    Type = <<"broadcast">>,
    Payload = #{
        <<"description">> => <<"description">>,
        <<"title">> => <<"title">>,
        <<"ticker">> => <<"ticker">>,
        <<"text">> => <<"text">>
    },
    send(UserId, Type, Payload).

test_customizedcast() ->
    %%    UserId = <<"QOGSAQMoX4">>, //杜力强
    UserId = <<"Zf94hIumlQ">>, %13313131319
    Payload = #{
        <<"description">> => <<"description">>,
        <<"title">> => <<"title">>,
        <<"ticker">> => <<"ticker">>,
        <<"text">> => <<"text">>
    },
    send(UserId, Payload).

send(UserId, Payload) ->
    send(UserId, <<"customizedcast">>, Payload).

send(UserId, Type, Payload) ->
    Message = get_msg(UserId, Type, Payload),
    case httpc:request(post, {get_url(Message), [], "application/json", Message}, [], []) of
        {ok, {_, _, Body}} ->
            case jsx:is_json(Body) of
                true ->
                    R = jsx:decode(Body, [{labels, binary}, return_maps]),
                    ?LOG(info, "~p", [R]),
                    R;
                false ->
                    ?LOG(info, "Body1 ~p ", [Body]),
                    ?LOG(info, "Body ~p ", [unicode:characters_to_list(Body)]),
                    Body
            end;
        {error, Reason} ->
            ?LOG(info, "Reason ~p", [Reason]),
            {error, Reason}
    end.

%%#{
%%<<"policy">> => #{
%%<<"expire_time">> => <<"2020-11-06 14:12:25">>
%%},
%%<<"description">> => <<"21312314">>,
%%<<"production_mode">> => true,
%%<<"appkey">> => <<"5f8bfc1780455950e4ad0482">>,
%%<<"payload">> => #{
%%<<"body">> => #{
%%<<"title">> => <<"测试推送">>,
%%<<"ticker">> => <<"测试推送">>,
%%<<"text">> => <<"测试推送内容1111">>,
%%<<"after_open">> => <<"go_app">>,
%%<<"play_vibrate">> => <<"false">>,
%%<<"play_lights">> => <<"false">>,
%%<<"play_sound">> => <<"true">>
%%},
%%<<"display_type">> => <<"notification">>
%%},
%%<<"mipush">> => true,
%%<<"mi_activity">> => <<"com.sinmahe.android.activity.StartActivity">>,
%%<<"type">> => <<"broadcast">>,
%%<<"timestamp">> => <<"1604388154901">>
%%},
get_msg(UserId, Type, Payload) ->
    AppKey = dgiot_utils:to_binary(dgiot:get_env(umeng_appkey)),
    Data = #{
        <<"policy">> => #{
            <<"expire_time">> => dgiot_datetime:format(dgiot_datetime:nowstamp() + 60 * 60 * 24 * 7, <<"YY-MM-DD HH:NN:SS">>)
        },
        <<"description">> => maps:get(<<"description">>, Payload, <<"description">>),
        <<"production_mode">> => true,
        <<"appkey">> => AppKey,
        <<"payload">> => #{
            <<"body">> => #{
                <<"title">> => maps:get(<<"title">>, Payload, <<"title">>),
                <<"ticker">> => maps:get(<<"ticker">>, Payload, <<"ticker">>),
                <<"text">> => maps:get(<<"text">>, Payload, <<"text">>),
                <<"after_open">> => <<"go_app">>,
                <<"play_vibrate">> => <<"false">>,
                <<"play_lights">> => <<"false">>,
                <<"play_sound">> => <<"true">>
            },
            <<"display_type">> => <<"notification">>
        },
        <<"alias_type">> => <<"objectId">>,
        <<"mipush">> => true,
        <<"mi_activity">> => <<"com.sinmahe.android.activity.StartActivity">>,
        <<"type">> => Type,
        <<"timestamp">> => dgiot_utils:to_binary(dgiot_datetime:nowstamp())
    },
    Notification = #{
        <<"userid">> => UserId,
        <<"sender">> => UserId,
        <<"public">> => true,
        <<"type">> => maps:get(<<"type">>, Payload, <<"title">>),
        <<"content">> => jsx:encode(Payload)
    },
    post_notification(Notification),
    NewData =
        case UserId of
            undefined -> Data;
            _ -> Data#{<<"alias">> => UserId}
        end,
    jsx:encode(NewData).

%% https://developer.umeng.com/docs/67966/detail/149296#h1--i-9
%% 签名验证方法
%% sign = md5('%s%s%s%s' % (method,url,post_body,app_master_secret)),
get_url(PostPayload) ->
    MasterKey = dgiot_utils:to_binary(dgiot:get_env(umeng_masterkey)),
    Uri = <<"http://msg.umeng.com/api/send">>,
    Sign = dgiot_license:to_md5(<<"POST", Uri/binary, PostPayload/binary, MasterKey/binary>>),
    dgiot_utils:to_list(Uri) ++ "?sign=" ++ dgiot_utils:to_list(Sign).

%%Notification = #{
%%<<"userid">> => <<"QOGSAQMoX4">>,
%%<<"sender">> => <<"QOGSAQMoX4">>,
%%<<"public">> => true,
%%<<"type">> => <<"notification">>,
%%<<"content">> => <<"hello">>
%%}
post_notification(Notification) ->
    UserId = maps:get(<<"userid">>, Notification, <<"x69mkAIpqA">>),
    dgiot_parse:create_object(<<"Notification">>, #{
        <<"ACL">> => #{
            UserId => #{
                <<"read">> => true,
                <<"write">> => true
            }
        },
        <<"content">> => maps:get(<<"content">>, Notification, <<"content">>),
        <<"public">> => maps:get(<<"public">>, Notification, true),
        <<"sender">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_User">>,
            <<"objectId">> => maps:get(<<"sender">>, Notification, UserId)
        },
        <<"type">> => maps:get(<<"type">>, Notification, <<"notification">>),
        <<"user">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_User">>,
            <<"objectId">> => UserId
        }
    }).

add_notification(<<"start_", Ruleid/binary>>, DevAddr, Payload) ->
    <<ProductId:10/binary, _/binary>> = Ruleid,
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_data:get(?NOTIFICATION, {DeviceId, Ruleid}) of
        {start, _Time} ->
            pass;
        _ ->
            save_notification(Ruleid, DevAddr, Payload#{<<"alertstatus">> => true})
    end,
    dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {start, dgiot_datetime:now_secs()});

add_notification(<<"stop_", Ruleid/binary>>, DevAddr, Payload) ->
    <<ProductId:10/binary, _/binary>> = Ruleid,
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_data:get(?NOTIFICATION, {DeviceId, Ruleid}) of
        {start, _Time} ->
            save_notification(Ruleid, DevAddr, Payload#{<<"alertstatus">> => false});
        _ ->
            pass
    end,
    dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {stop, dgiot_datetime:now_secs()});

add_notification(Ruleid, _DevAddr, _Payload) ->
    ?LOG(error, "Ruleid ~p", [Ruleid]),
    ok.

save_notification(Ruleid, DevAddr, Payload) ->
    case binary:split(Ruleid, <<$_>>, [global, trim]) of
        [ProductId, _] ->
            DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
            case dgiot_device:lookup(DeviceId) of
                {ok, #{<<"acl">> := Acl}} ->
                    Requests =
                        lists:foldl(fun(X, Acc) ->
                            BinX = atom_to_binary(X),
                            case BinX of
                                <<"role:", Name/binary>> ->
%%                                    RoleId = dgiot_parse:get_roleid(<<"dgiot">>),
                                    case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                                        <<"where">> => #{<<"name">> => Name}}) of
                                        {ok, #{<<"results">> := [Role]}} ->
                                            #{<<"objectId">> := RoleId} = Role,
                                            UserIds = dgiot_parse:get_userids(RoleId),
                                            lists:foldl(fun(UserId, Acc1) ->
                                                ObjectId = dgiot_parse:get_notificationid(Ruleid),
                                                Content = Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId},
                                                sendSubscribe(Ruleid, Content, UserId),
                                                Acc1 ++ [#{
                                                    <<"method">> => <<"POST">>,
                                                    <<"path">> => <<"/classes/Notification">>,
                                                    <<"body">> => #{
                                                        <<"objectId">> => ObjectId,
                                                        <<"ACL">> => #{
                                                            UserId => #{
                                                                <<"read">> => true,
                                                                <<"write">> => true
                                                            }
                                                        },
                                                        <<"content">> => Content,
                                                        <<"public">> => false,
                                                        <<"status">> => 0,
                                                        <<"sender">> => #{
                                                            <<"__type">> => <<"Pointer">>,
                                                            <<"className">> => <<"_User">>,
                                                            <<"objectId">> => <<"Klht7ERlYn">>
                                                        },
                                                        <<"process">> => <<"">>,
                                                        <<"type">> => Ruleid,
                                                        <<"user">> => #{
                                                            <<"__type">> => <<"Pointer">>,
                                                            <<"className">> => <<"_User">>,
                                                            <<"objectId">> => UserId
                                                        }
                                                    }
                                                }]
                                                        end, Acc, UserIds);
                                        _ ->
                                            Acc
                                    end;
                                <<"*">> ->
                                    ObjectId = dgiot_parse:get_notificationid(Ruleid),
                                    Acc ++ [#{
                                        <<"method">> => <<"POST">>,
                                        <<"path">> => <<"/classes/Notification">>,
                                        <<"body">> => #{
                                            <<"objectId">> => ObjectId,
                                            <<"ACL">> => #{
                                                <<"*">> => #{
                                                    <<"read">> => true,
                                                    <<"write">> => true
                                                }
                                            },
                                            <<"content">> => Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId},
                                            <<"public">> => false,
                                            <<"status">> => 0,
                                            <<"sender">> => #{
                                                <<"__type">> => <<"Pointer">>,
                                                <<"className">> => <<"_User">>,
                                                <<"objectId">> => <<"Klht7ERlYn">>
                                            },
                                            <<"type">> => Ruleid,
                                            <<"user">> => #{
                                                <<"__type">> => <<"Pointer">>,
                                                <<"className">> => <<"_User">>,
                                                <<"objectId">> => <<"">>
                                            }
                                        }
                                    }];
                                UserId ->
                                    ObjectId = dgiot_parse:get_notificationid(Ruleid),
                                    Content = Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId},
                                    sendSubscribe(Ruleid, Content, UserId),
                                    Acc ++ [#{
                                        <<"method">> => <<"POST">>,
                                        <<"path">> => <<"/classes/Notification">>,
                                        <<"body">> => #{
                                            <<"objectId">> => ObjectId,
                                            <<"ACL">> => #{
                                                UserId => #{
                                                    <<"read">> => true,
                                                    <<"write">> => true
                                                }
                                            },
                                            <<"content">> => Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId},
                                            <<"public">> => false,
                                            <<"status">> => 0,
                                            <<"sender">> => #{
                                                <<"__type">> => <<"Pointer">>,
                                                <<"className">> => <<"_User">>,
                                                <<"objectId">> => <<"Klht7ERlYn">>
                                            },
                                            <<"type">> => Ruleid,
                                            <<"user">> => #{
                                                <<"__type">> => <<"Pointer">>,
                                                <<"className">> => <<"_User">>,
                                                <<"objectId">> => UserId
                                            }
                                        }
                                    }]
                            end
                                    end, [], Acl),
                    dgiot_parse:batch(Requests);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

sendSubscribe(Type, Content, UserId) ->
    DeviceId = maps:get(<<"_deviceid">>, Content, <<"">>),
    {Devaddr, _Address} =
        case dgiot_parse:get_object(<<"Device">>, DeviceId) of
            {ok, #{<<"devaddr">> := Devaddr1, <<"detail">> := Detail}} ->
                Address1 = maps:get(<<"address">>, Detail, <<"无位置"/utf8>>),
                {Devaddr1, Address1};
            _ ->
                {<<"">>, <<"无位置"/utf8>>}
        end,
    Result =
        case binary:split(Type, <<$_>>, [global, trim]) of
            [ProductId, AlertId] ->
%%                Productname =
%%                    case dgiot_parse:get_object(<<"Product">>, ProductId) of
%%                        {ok, #{<<"name">> := Productname1}} ->
%%                            Productname1;
%%                        _ ->
%%                            <<" ">>
%%                    end,
                dgiot_datetime:now_secs(),
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"config">> := #{<<"parser">> := Parse}}} ->
                        lists:foldl(fun(P, Par) ->
                            case P of
                                #{<<"uid">> := AlertId, <<"config">> := #{<<"formDesc">> := FormDesc}} ->
                                    maps:fold(fun(Key, Value1, Form) ->
                                        case maps:find(Key, Content) of
                                            {ok, Value} ->
                                                BinValue = dgiot_utils:to_binary(Value),
                                                Form#{<<"thing15">> => #{<<"value">> => BinValue}};
                                            _ ->
                                                Default = maps:get(<<"default">>, Value1, <<>>),
                                                Form#{Key => #{<<"value">> => Default}}
                                        end
                                              end, #{<<"thing1">> => #{<<"value">> => Devaddr}, <<"date4">> => #{<<"value">> => dgiot_datetime:format("YYYY-MM-DD HH:NN:SS")}}, FormDesc);
                                _Oth ->
                                    Par
                            end
                                    end, #{}, Parse);
                    _Other ->
                        #{}
                end;
            _Other1 ->
                #{}
        end,
    dgiot_wechat:sendSubscribe(UserId, Result).

%% dgiot_umeng:save_devicestatus(<<"5adc65e32e">>, <<"OFFLINE">>).
save_devicestatus(DeviceId, Status) ->
    {_DeviceName, Address} =
        case dgiot_parse:get_object(<<"Device">>, DeviceId) of
            {ok, #{<<"name">> := DeviceName1, <<"detail">> := Detail}} ->
                Address1 = maps:get(<<"address">>, Detail, <<"无位置"/utf8>>),
                {DeviceName1, Address1};
            _ ->
                {<<" ">>, <<"无位置"/utf8>>}
        end,

    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"productid">> := ProductId}} ->
            Ruleid = <<ProductId/binary, "_status">>,
            Productname =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := Productname1}} ->
                        Productname1;
                    _ ->
                        <<" ">>
                end,
            Requests =
                lists:foldl(fun(X, Acc) ->
                    BinX = atom_to_binary(X),
                    case BinX of
                        <<"role:", Name/binary>> ->
                            case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                                <<"where">> => #{<<"name">> => Name}}) of
                                {ok, #{<<"results">> := [Role]}} ->
                                    #{<<"objectId">> := RoleId} = Role,
                                    UserIds = dgiot_parse:get_userids(RoleId),
                                    lists:foldl(fun(UserId, Acc1) ->
                                        ObjectId = dgiot_parse:get_notificationid(Ruleid),
                                        Content = #{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId, <<"status">> => Status},
                                        Result = #{<<"thing1">> => #{<<"value">> => Productname},
                                            <<"date4">> => #{<<"value">> => dgiot_datetime:format("YYYY-MM-DD HH:NN:SS")},
                                            <<"thing15">> => #{<<"value">> => <<"设备离线"/utf8>>},
                                            <<"thing5">> => #{<<"value">> => Address},
                                            <<"thing12">> => #{<<"value">> => <<Devaddr/binary, "离线"/utf8>>}},
                                        dgiot_wechat:sendSubscribe(UserId, Result),
                                        Acc1 ++ [#{
                                            <<"method">> => <<"POST">>,
                                            <<"path">> => <<"/classes/Notification">>,
                                            <<"body">> => #{
                                                <<"objectId">> => ObjectId,
                                                <<"ACL">> => #{
                                                    UserId => #{
                                                        <<"read">> => true,
                                                        <<"write">> => true
                                                    }
                                                },
                                                <<"content">> => Content,
                                                <<"public">> => false,
                                                <<"status">> => 0,
                                                <<"sender">> => #{
                                                    <<"__type">> => <<"Pointer">>,
                                                    <<"className">> => <<"_User">>,
                                                    <<"objectId">> => <<"Klht7ERlYn">>
                                                },
                                                <<"process">> => <<"">>,
                                                <<"type">> => Ruleid,
                                                <<"user">> => #{
                                                    <<"__type">> => <<"Pointer">>,
                                                    <<"className">> => <<"_User">>,
                                                    <<"objectId">> => UserId
                                                }
                                            }
                                        }]
                                                end, Acc, UserIds);
                                _ ->
                                    Acc
                            end;
                        <<"*">> ->
                            Acc;
                        UserId ->
                            ObjectId = dgiot_parse:get_notificationid(Ruleid),
                            Result = #{<<"thing1">> => #{<<"value">> => Productname},
                                <<"date4">> => #{<<"value">> => dgiot_datetime:format("YYYY-MM-DD HH:NN:SS")},
                                <<"thing15">> => #{<<"value">> => <<"设备离线"/utf8>>},
                                <<"thing5">> => #{<<"value">> => Address},
                                <<"thing12">> => #{<<"value">> => <<Devaddr/binary, "离线"/utf8>>}},
                            dgiot_wechat:sendSubscribe(UserId, Result),
                            Acc ++ [#{
                                <<"method">> => <<"POST">>,
                                <<"path">> => <<"/classes/Notification">>,
                                <<"body">> => #{
                                    <<"objectId">> => ObjectId,
                                    <<"ACL">> => #{
                                        UserId => #{
                                            <<"read">> => true,
                                            <<"write">> => true
                                        }
                                    },
                                    <<"content">> => #{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId, <<"status">> => Status},
                                    <<"public">> => false,
                                    <<"status">> => 0,
                                    <<"sender">> => #{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_User">>,
                                        <<"objectId">> => <<"Klht7ERlYn">>
                                    },
                                    <<"type">> => Ruleid,
                                    <<"user">> => #{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_User">>,
                                        <<"objectId">> => UserId
                                    }
                                }
                            }]
                    end
                            end, [], Acl),
            dgiot_parse:batch(Requests);
        _ ->
            pass
    end.
