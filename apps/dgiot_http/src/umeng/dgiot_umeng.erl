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
    sendSubscribe/3,
    create_maintenance/2,
    get_operations/0,
    send_message_to3D/3,
    triggeralarm/1
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

add_notification(<<"start_", Ruleid/binary>>, DeviceId, Payload) ->
    case dgiot_data:get(?NOTIFICATION, {DeviceId, Ruleid}) of
        {start, _Time} ->
            pass;
        _ ->
            io:format("~s ~p Msg= ~p ~p.~n", [?FILE, ?LINE,DeviceId, Payload]),
            save_notification(Ruleid, DeviceId, Payload#{<<"alertstatus">> => true})

    end,
    dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {start, dgiot_datetime:now_secs()});

add_notification(<<"stop_", Ruleid/binary>>, DeviceId, Payload) ->
    case dgiot_data:get(?NOTIFICATION, {DeviceId, Ruleid}) of
        {start, _Time} ->
            save_notification(Ruleid, DeviceId, Payload#{<<"alertstatus">> => false});
        _ ->
            pass
    end,
    dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {stop, dgiot_datetime:now_secs()});

add_notification(Ruleid, _DevAddr, _Payload) ->
    ?LOG(error, "Ruleid ~p", [Ruleid]),
    ok.

save_notification(Ruleid, DeviceId, Payload) ->
    case binary:split(Ruleid, <<$_>>, [global, trim]) of
        [ProductId, ViewId] ->
            case dgiot_device:lookup(DeviceId) of
                {ok, #{<<"acl">> := Acl}} ->
                    Requests =
                        lists:foldl(fun(X, Acc) ->
                            BinX = atom_to_binary(X),
                            case BinX of
                                <<"role:", Name/binary>> ->
%%                                    RoleId = dgiot_parse_id:get_roleid(<<"dgiot">>),
                                    case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                                        <<"where">> => #{<<"name">> => Name}}) of
                                        {ok, #{<<"results">> := [Role]}} ->
                                            #{<<"objectId">> := RoleId} = Role,
                                            UserIds = dgiot_parse_id:get_userids(RoleId),
                                            lists:foldl(fun(UserId, Acc1) ->
                                                ObjectId = dgiot_parse_id:get_notificationid(Ruleid),
                                                Content = Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId, <<"_viewid">> => ViewId},
%%                                                sendSubscribe(Ruleid, Content, UserId),
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
                                    ObjectId = dgiot_parse_id:get_notificationid(Ruleid),
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
                                            <<"content">> => Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId, <<"_viewid">> => ViewId},
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
                                    ObjectId = dgiot_parse_id:get_notificationid(Ruleid),
                                    Content = Payload#{<<"_deviceid">> => DeviceId, <<"_productid">> => ProductId, <<"_viewid">> => ViewId},
%%                                    sendSubscribe(Ruleid, Content, UserId),
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
                                            <<"content">> => Content,
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
                                    UserIds = dgiot_parse_id:get_userids(RoleId),
                                    lists:foldl(fun(UserId, Acc1) ->
                                        ObjectId = dgiot_parse_id:get_notificationid(Ruleid),
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
                            ObjectId = dgiot_parse_id:get_notificationid(Ruleid),
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

%%SELECT payload, payload.1.value as value, clientid, 'e636739559' as productid FROM "profile/e636739559/#" WHERE value = '02000000000000001A00000000250222'
send_message_to3D(ProductId, DevAddr, Payload) ->
    Warn = maps:get(<<"9">>, Payload, #{}),
    {Lev, Type} =
        case maps:get(<<"value">>, Warn, <<"一级"/utf8>>) of
            <<"一级"/utf8>> ->
                {<<"一级"/utf8>>, <<"故障工单"/utf8>>};
            _ ->

                {<<"二级"/utf8>>, <<"告警工单"/utf8>>}

        end,
    io:format("~s ~p Payload = ~p.~n", [?FILE, ?LINE, Payload]),
    Deviceid = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    DeviceName =
        case dgiot_parse:get_object(<<"Device">>, Deviceid) of
            {ok, #{<<"name">> := Name}} ->
                Name;
            _ ->
                <<"电表_001"/utf8>>
        end,
    ProductName =
        case dgiot_parse:get_object(<<"Product">>, ProductId) of
            {ok, #{<<"name">> := Name1}} ->
                Name1;
            _ ->
                <<"电表"/utf8>>
        end,
    Topic = <<"/devWar/up">>,
    <<Number:10/binary, _/binary>> = dgiot_utils:random(),
    Timestamp = dgiot_datetime:format(dgiot_datetime:to_localtime(dgiot_datetime:now_secs()), <<"YY-MM-DD HH:NN:SS">>),
    Data = #{
        <<"id">> => Number,
        <<"deviceid">> => Deviceid,
        <<"devicename">> => DeviceName,
        <<"status">> => <<"拉闸"/utf8>>,
        <<"content">> => <<"电表拉闸断电"/utf8>>,
        <<"time">> => Timestamp,
        <<"level">> => Lev,
        <<"data">> => #{
            <<"id">> => Number,
            <<"deviceid">> => Deviceid,
            <<"devicename">> => DeviceName,
            <<"productid">> => ProductId,
            <<"productname">> => ProductName,
            <<"type">> => Type,
            <<"description">> => <<"电表拉闸断电"/utf8>>
        }
    },
    case Payload of
%%        拉闸 <<"02000000000000001A00000000250222">>
        #{<<"1">> := #{<<"value">> := <<"02000000000000001A", _/binary>>}} ->
            dgiot_mqtt:publish(Deviceid, Topic, jsx:encode(Data));
        #{<<"4">> := #{<<"value">> := <<"1A">>}} ->
            dgiot_mqtt:publish(Deviceid, Topic, jsx:encode(Data));
        _ ->
            pass
    end.

create_maintenance(Info, SessionToken) ->
    <<Number:10/binary, _/binary>> = dgiot_utils:random(),
    Timestamp = dgiot_datetime:format(dgiot_datetime:to_localtime(dgiot_datetime:now_secs()), <<"YY-MM-DD HH:NN:SS">>),
    {Username, UserId, UserPhone} =
        case dgiot_auth:get_session(SessionToken) of
            #{<<"nick">> := Nick, <<"objectId">> := UserId1, <<"phone">> := UserPhone1} ->
                {Nick, UserId1, UserPhone1};
            _ ->
                {<<"">>, <<"">>, <<"">>}
        end,
    DeviceId = maps:get(<<"deviceid">>, Info, <<"8d7bdaff69">>),
    Acl =
        case dgiot_device:lookup(DeviceId) of
            {ok, #{<<"acl">> := Acl1}} ->
                NewAcl1 =
                    lists:foldl(fun(X, Acc) ->
                        Acc#{atom_to_binary(X) => #{<<"read">> => true, <<"write">> => true}}
                                end, #{}, Acl1),
                NewAcl1;
            _ ->
                #{<<"role:开发者"/utf8>> => #{<<"read">> => true, <<"write">> => true}}
        end,
    Body = #{
        <<"number">> => maps:get(<<"id">>, Info, Number),
        <<"type">> => maps:get(<<"type">>, Info, <<"故障工单"/utf8>>),
        <<"status">> => 1,
        <<"ACL">> => Acl#{<<"role:", UserId/binary>> => #{<<"read">> => true, <<"write">> => true}},
        <<"info">> => Info#{
            <<"step1">> => #{},
            <<"step2">> => #{},
            <<"step3">> => #{},
            <<"step4">> => #{},
            <<"timeline">> => [
                #{
                    <<"timestamp">> => Timestamp,
                    <<"h4">> => <<"生成工单"/utf8>>,
                    <<"p">> => <<Username/binary, " 新建工单"/utf8>>
                },
                #{
                    <<"timestamp">> => Timestamp,
                    <<"h4">> => <<"生成工单"/utf8>>,
                    <<"p">> => <<Username/binary, " 分配给 "/utf8, Username/binary>>
                }
            ],
            <<"createdname">> => <<"管理员"/utf8>>,
            <<"receiveuseid">> => UserId,
            <<"receiveusername">> => Username,
            <<"receiveuserphone">> => UserPhone
        },
        <<"device">> => #{
            <<"objectId">> => DeviceId,
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"Device">>
        }
    },
    case dgiot_parse:create_object(<<"Maintenance">>, Body,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, Result} ->
            send_gdmessage(DeviceId, #{
                <<"character_string9">> => #{<<"value">> => maps:get(<<"id">>, Info, Number)},
                <<"thing2">> => #{<<"value">> => maps:get(<<"devicename">>, Info, <<"电表_001"/utf8>>)},
                <<"thing3">> => #{<<"value">> => maps:get(<<"type">>, Info, <<"故障工单"/utf8>>)},
                <<"thing12">> => #{<<"value">> => maps:get(<<"description">>, Info, <<"电表拉闸断电"/utf8>>)},
                <<"date5">> => #{<<"value">> => Timestamp}
            }),
            {ok, Result};
        Other ->
            Other
    end.


send_gdmessage(DeviceId, Data) ->
    Result = #{<<"data">> => Data,
        <<"lang">> => <<"zh_CN">>,
        <<"miniprogramstate">> => <<"formal">>,
        <<"page">> => <<"pages/work/work">>,
        <<"templateid">> => <<"wFe8o5L65ZwoQQyzEpR1hhCbvlXHN4ehtFSd_BT_6ZY">>},
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := Acl}} ->
            lists:map(fun(X) ->
                BinX = atom_to_binary(X),
                case BinX of
                    <<"role:", Name/binary>> ->
                        case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                            <<"where">> => #{<<"name">> => Name}}) of
                            {ok, #{<<"results">> := [Role]}} ->
                                #{<<"objectId">> := RoleId} = Role,
                                UserIds = dgiot_parse_id:get_userids(RoleId),
                                lists:map(fun(UserId) ->
                                    dgiot_wechat:sendSubscribe_test(UserId, Result)
                                          end, UserIds);
                            _ ->
                                pass
                        end;
                    <<"*">> ->
                        pass;
                    UserId ->
                        dgiot_wechat:sendSubscribe_test(UserId, Result)
                end
                      end, Acl);
        _ ->
            pass
    end.

%% 运维管理
get_operations() ->
    #{
        <<"巡检"/utf8>> => #{
            <<"巡检状态"/utf8>> => <<"当日已巡检"/utf8>>, %% 当日未巡检，巡检中
            <<"巡检时长"/utf8>> => <<"1小时"/utf8>>,
            <<"当月巡检次数"/utf8>> => <<"15次"/utf8>>
        },
        <<"报修"/utf8>> => #{
            <<"报修设备"/utf8>> => <<"1号空调"/utf8>>,
            <<"报修地点"/utf8>> => <<"201空调机组间"/utf8>>,
            <<"报修时间"/utf8>> => <<"2022/2/3"/utf8>>,
            <<"报修状态"/utf8>> => <<"未完成"/utf8>>
        },
        <<"维保"/utf8>> => #{
            <<"维保设备"/utf8>> => <<"1号电梯"/utf8>>,
            <<"维保时间"/utf8>> => <<"2022/2/1"/utf8>>,
            <<"维保周期"/utf8>> => <<"6个月"/utf8>>,
            <<"年维保频率"/utf8>> => <<"2次"/utf8>>
        },
        <<"资产"/utf8>> => #{
            <<"园区面积"/utf8>> => <<"26W平方米"/utf8>>,
            <<"建筑面积"/utf8>> => <<"8W平方米"/utf8>>,
            <<"固定资产"/utf8>> => <<"未知"/utf8>>,
            <<"流动资产"/utf8>> => <<"未知"/utf8>>
        },
        <<"空间管理"/utf8>> => #{
            <<"竞技场状态"/utf8>> => <<"未开启"/utf8>>,
            <<"观众席上座率"/utf8>> => <<"0%"/utf8>>,
            <<"VIP上座率"/utf8>> => <<"0%"/utf8>>,
            <<"商业区出租率"/utf8>> => <<"75%"/utf8>>
        }
    }.

triggeralarm(DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"name">> := DeviceName, <<"product">> := #{<<"objectId">> := ProductId}, <<"detail">> := Detail}} ->
            {ProductName, Lev, Type, Content} =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := <<"检票闸机"/utf8>>}} ->
                        Desc = maps:get(<<"desc">>, Detail, <<"通过人数超过阈值，请到现场处理"/utf8>>),
                        {<<"检票闸机"/utf8>>, <<"二级"/utf8>>, <<"告警工单"/utf8>>, <<DeviceName/binary, Desc/binary>>};
                    {ok, #{<<"name">> := <<"车闸机"/utf8>>}} ->
                        Desc = maps:get(<<"desc">>, Detail, <<"当前车闸机无法自动抬起，请检查该设备"/utf8>>),
                        {<<"车闸机"/utf8>>, <<"二级"/utf8>>, <<"告警工单"/utf8>>, <<DeviceName/binary, Desc/binary>>};
                    {ok, #{<<"name">> := Name1}} ->
                        Desc = maps:get(<<"desc">>, Detail, <<"发生火灾，请赶往A区查看"/utf8>>),
                        {Name1, <<"一级"/utf8>>, <<"报警工单"/utf8>>, <<DeviceName/binary, Desc/binary>>}
                end,
            <<Number:10/binary, _/binary>> = dgiot_utils:random(),
            Timestamp = dgiot_datetime:format(dgiot_datetime:to_localtime(dgiot_datetime:now_secs()), <<"YY-MM-DD HH:NN:SS">>),
            Data = #{
                <<"id">> => Number,
                <<"deviceid">> => DeviceId,
                <<"devicename">> => DeviceName,
                <<"status">> => <<"在线"/utf8>>,
                <<"content">> => Content,
                <<"time">> => Timestamp,
                <<"level">> => Lev,
                <<"data">> => #{
                    <<"id">> => Number,
                    <<"deviceid">> => DeviceId,
                    <<"devicename">> => DeviceName,
                    <<"productid">> => ProductId,
                    <<"productname">> => ProductName,
                    <<"type">> => Type,
                    <<"description">> => Content
                }
            },
            Topic = <<"/devWar/up">>,
            dgiot_mqtt:publish(DeviceId, <<"bridge/", Topic/binary>>, jsx:encode(Data)),
            dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Data));
        _ ->
            pass
    end.










