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
    manual_recovery/1,
    save_notification/4,
    update_notification/2,
    create_maintenance/2,
    send_maintenance/1,
    send_other/1,
    get_operations/0,
    send_message_to3D/3,
    triggeralarm/2,
    send_msg/1,
    sendSubscribe/1,
    send_dashboard/1,
    get_defultmessage/1,
    replace_miniparam/10
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
        <<"content">> => dgiot_json:encode(Payload)
    },
    post_notification(Notification),
    NewData =
        case UserId of
            undefined -> Data;
            _ -> Data#{<<"alias">> => UserId}
        end,
    dgiot_json:encode(NewData).

%% https://developer.umeng.com/docs/67966/detail/149296#h1--i-9
%% 签名验证方法
%% sign = md5('%s%s%s%s' % (method,url,post_body,app_master_secret)),
get_url(PostPayload) ->
    MasterKey = dgiot_utils:to_binary(dgiot:get_env(umeng_masterkey)),
    Uri = <<"http://msg.umeng.com/api/send">>,
    Sign = dgiot_utils:to_md5(<<"POST", Uri/binary, PostPayload/binary, MasterKey/binary>>),
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
    Now = dgiot_datetime:now_secs(),
    case dgiot_data:get(dgiot_notification, {DeviceId, Ruleid}) of
        {start, Time, _} when (Now - Time) < 3600 ->
            pass;
        _ ->
            NotificationId = dgiot_parse_id:get_notificationid(Ruleid),
            dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {start, dgiot_datetime:now_secs(), NotificationId}),
            Content = save_notification(Ruleid, DeviceId, Payload, NotificationId),
            dgiot_umeng:send_dashboard(Content),
            dgiot_umeng:send_other(Content),
            dgiot_umeng:send_maintenance(Content#{<<"notificationid">> => NotificationId}),
            dgiot_umeng:send_msg(Content),
            dgiot_umeng:sendSubscribe(Content)
    end;

add_notification(<<"stop_", Ruleid/binary>>, DeviceId, #{<<"dgiot_alarmvalue">> := Value}) ->
    case dgiot_data:get(?NOTIFICATION, {DeviceId, Ruleid}) of
        {start, _Time, NotificationId} ->
            dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {stop, dgiot_datetime:now_secs(), <<>>}),
            Content = update_notification(NotificationId, #{<<"restored_value">> => Value}),
            dgiot_umeng:send_msg(Content),
%%            io:format("~s ~p Content1 = ~p.~n", [?FILE, ?LINE, Content]),
            dgiot_umeng:sendSubscribe(Content);
        _ ->
            pass
    end;

add_notification(Ruleid, _DevAddr, _Payload) ->
    ?LOG(error, "Ruleid ~p", [Ruleid]),
    ok.

%% 手动恢复
manual_recovery(ObjectId) ->
    case dgiot_parse:get_object(<<"Notification">>, ObjectId) of
        {ok, #{<<"content">> := #{<<"_deviceid">> := DeviceId}, <<"type">> := Ruleid}} ->
            dgiot_data:insert(?NOTIFICATION, {DeviceId, Ruleid}, {stop, dgiot_datetime:now_secs(), <<>>});
        _ ->
            pass
    end.

save_notification(Ruleid, DeviceId, Payload, NotificationId) ->
    Alarm_createdAt = dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"),
    case binary:split(Ruleid, <<$_>>, [global, trim]) of
        [ProductId, ViewId] ->
            case dgiot_device:lookup(DeviceId) of
                {ok, #{<<"acl">> := Acls, <<"devaddr">> := Devaddr}} ->
                    Acl =
                        lists:foldl(fun(X, Acc) ->
                            Acc#{
                                dgiot_utils:to_binary(X) => #{
                                    <<"read">> => true,
                                    <<"write">> => true
                                }}
                                    end, #{}, Acls),
                    {Alarm_message, RoleId} =
                        case get_defultmessage(ViewId) of
                            #{<<"alarm_message">> := Message, <<"roleid">> := Id} ->
                                {Message, Id};
                            _ ->
                                {<<>>, <<>>}
                        end,
                    Content = Payload#{<<"alarm_createdAt">> => Alarm_createdAt, <<"alarm_message">> => Alarm_message, <<"roleid">> => RoleId, <<"_deviceid">> => DeviceId, <<"_productid">> => ProductId, <<"_viewid">> => ViewId},
                    dgiot_parse:create_object(<<"Notification">>, #{
                        <<"objectId">> => NotificationId,
                        <<"ACL">> => Acl,
                        <<"content">> => Content,
                        <<"public">> => false,
                        <<"status">> => 0,
                        <<"process">> => <<"">>,
                        <<"type">> => Ruleid,
                        <<"device">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Device">>, <<"objectId">> => DeviceId}
                    }),
                    Content#{<<"send_alarm_status">> => <<"start">>, <<"devaddr">> => Devaddr};
                _ ->
                    #{}
            end;
        _ ->
            #{}
    end.

get_defultmessage(ViewId) ->
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"data">> := #{<<"body">> := [#{<<"body">> := Body} | _]}}} ->
            lists:foldl(fun
                            (#{<<"name">> := <<"alarm_message">>, <<"value">> := Value}, Acc) ->
                                Acc#{<<"alarm_message">> => Value};
                            (#{<<"name">> := <<"roleid">>, <<"value">> := Value}, Acc) ->
                                Acc#{<<"roleid">> => Value};
                            (_, Acc) ->
                                Acc
                        end, #{<<"alarm_message">> => <<>>, <<"roleid">> => <<>>}, Body);
        _ ->
            #{}
    end.

%% 0 未确认 1 误报 2 手动恢复 3 自动恢复
update_notification(NotificationId, Payload) ->
    case dgiot_parse:get_object(<<"Notification">>, NotificationId) of
        {ok, #{<<"content">> := Content}} ->
            NewContent = maps:merge(Content, Payload),
            dgiot_parse:update_object(<<"Notification">>, NotificationId, #{
                <<"public">> => true,
                <<"status">> => 3,
                <<"content">> => NewContent,
                <<"process">> => <<"自动恢复"/utf8>>
            }),
            NewContent#{<<"send_alarm_status">> => <<"stop">>};
        _ ->
            #{}
    end.

%%SELECT payload, payload.1.value as value, clientid, 'e636739559' as productid FROM "profile/e636739559/#" WHERE value = '02000000000000001A00000000250222'
send_message_to3D(ProductId, DevAddr, Payload) ->
    Warn = maps:get(9, Payload, #{}),
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
        #{1 := #{<<"value">> := <<"02000000000000001A", _/binary>>}} ->
            dgiot_mqtt:publish(Deviceid, Topic, dgiot_json:encode(Data));
        #{4 := #{<<"value">> := <<"1A">>}} ->
            dgiot_mqtt:publish(Deviceid, Topic, dgiot_json:encode(Data));
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

triggeralarm(DeviceId, Content) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"name">> := DeviceName, <<"product">> := #{<<"objectId">> := ProductId}}} ->
            {ProductName, Lev, Type} =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := <<"检票闸机"/utf8>>}} ->
                        {<<"检票闸机"/utf8>>, <<"二级"/utf8>>, <<"告警工单"/utf8>>};
                    {ok, #{<<"name">> := <<"车闸机"/utf8>>}} ->
                        {<<"车闸机"/utf8>>, <<"二级"/utf8>>, <<"告警工单"/utf8>>};
                    {ok, #{<<"name">> := Name1}} ->
                        {Name1, <<"一级"/utf8>>, <<"报警工单"/utf8>>}
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
            dgiot_mqtt:publish(DeviceId, <<"bridge/", Topic/binary>>, dgiot_json:encode(Data)),
            dgiot_mqtt:publish(DeviceId, Topic, dgiot_json:encode(Data));
        _ ->
            pass
    end.


%% 触发
send_msg(#{<<"send_alarm_status">> := <<"start">>, <<"_productid">> := ProductId, <<"_viewid">> := ViewId} = NotifContent) ->
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"meta">> := Content}} ->
            ProductName =
                case dgiot_product:lookup_prod(ProductId) of
                    {ok, #{<<"name">> := Name}} ->
                        Name;
                    _ ->
                        <<>>
                end,
            maps:fold(fun(K, V, _Acc) ->
                send_mode(#{K => V}, ProductName, NotifContent, <<"触发"/utf8>>)
                      end, #{}, Content);
        _ ->
            pass
    end;

%% 恢复
send_msg(#{<<"send_alarm_status">> := <<"stop">>, <<"_productid">> := ProductId, <<"_viewid">> := ViewId} = NotifContent) ->
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"meta">> := Content}} ->
            ProductName =
                case dgiot_product:lookup_prod(ProductId) of
                    {ok, #{<<"name">> := Name}} ->
                        Name;
                    _ ->
                        <<>>
                end,
            maps:fold(fun(K, V, _Acc) ->
                send_mode(#{K => V}, ProductName, NotifContent, <<"恢复"/utf8>>)
                      end, #{}, Content);
        _ ->
            pass
    end;

send_msg(_) ->
    pass.

%% 触发 小程序通知
sendSubscribe(#{<<"send_alarm_status">> := <<"start">>, <<"roleid">> := NotifRoleid, <<"alarm_createdAt">> := Alarm_createdAt,
    <<"alarm_message">> := Alarm_message, <<"_deviceid">> := DeviceId, <<"_productid">> := ProductId, <<"_viewid">> := ViewId, <<"dgiot_alarmkey">> := Alarmkey, <<"dgiot_alarmvalue">> := Alarmvalue}) ->
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"meta">> := #{<<"minipg">> := #{<<"issend">> := <<"true">>, <<"params">> := Params, <<"tplid">> := TplId, <<"roleid">> := RoleId} = Minipg}}} ->
            ProductName =
                case dgiot_product:lookup_prod(ProductId) of
                    {ok, #{<<"name">> := Name}} ->
                        Name;
                    _ ->
                        <<>>
                end,
            Device =
                case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                    {ok, Result} ->
                        Result;
                    _ ->
                        #{}
                end,
            NewAlarm_message = maps:get(<<"alarm_message">>, Minipg, Alarm_message),
            Page = maps:get(<<"page">>, Minipg, <<"pages/home/home">>),
            Data =
                maps:fold(fun(Key, Value, Acc) ->
                    dgiot_umeng:replace_miniparam(Acc, Key, Value, Alarm_createdAt, Alarmkey, Alarmvalue, Device, ProductId, ProductName, NewAlarm_message)
                          end, #{}, Params),
%%            io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, Data]),
            lists:foldl(fun(#{<<"objectId">> := UserId}, Rcc) ->
                Rcc ++ [dgiot_wechat:sendSubscribe(UserId, TplId, Data, Page)]
                        end, [], dgiot_notification:get_users(DeviceId, RoleId, NotifRoleid));
        _O ->
%%            io:format("~s ~p _O = ~p.~n", [?FILE, ?LINE, _O]),
            pass
    end;

%% 恢复 小程序通知
%%sendSubscribe(#{<<"send_alarm_status">> := <<"stop">>, <<"alarm_createdAt">> := Alarm_createdAt, <<"_deviceid">> := DeviceId, <<"_productid">> := ProductId, <<"dgiot_alarmkey">> := Key, <<"dgiot_alarmvalue">> := Value}) ->
%%
%%    pass;

%% 小程序订阅
sendSubscribe(_O) ->
%%    io:format("~s ~p _O = ~p.~n", [?FILE, ?LINE, _O]),
    pass.

%% 推送前端弹框
send_dashboard(#{<<"_deviceid">> := DeviceId, <<"dgiot_alarmvalue">> := Alarmvalue} = Args) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"ACL">> := ACL, <<"name">> := DeviceName, <<"product">> := #{<<"objectId">> := ProductId}}} ->
            ProductName =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := Name1}} ->
                        Name1;
                    _ ->
                        <<>>
                end,
            <<Number:10/binary, _/binary>> = dgiot_utils:random(),
            Timestamp = dgiot_datetime:format(dgiot_datetime:to_localtime(dgiot_datetime:now_secs()), <<"YY-MM-DD HH:NN:SS">>),
%%            BinAlarmvalue = dgiot_utils:to_binary(Alarmvalue),
            Data = #{
                <<"id">> => Number,
                <<"deviceid">> => DeviceId,
                <<"devicename">> => DeviceName,
                <<"productid">> => ProductId,
                <<"productname">> => ProductName,
                <<"value">> => Alarmvalue,
                <<"time">> => Timestamp,
                <<"level">> => 1,
                <<"type">> => <<"warn">>,
                <<"description">> => maps:get(<<"description">>, Args, <<>>)
            },
            RoleIds = dgiot_role:get_roleids(maps:keys(ACL)),
            lists:foldl(fun(RoleId, _) ->
                Pubtopic = <<"$dg/user/dashboard/notification/", RoleId/binary, "/report">>,
                dgiot_mqtt:publish(DeviceId, Pubtopic, dgiot_json:encode(Data))
                        end, {}, RoleIds);
        _ ->
            pass
    end;

send_dashboard(_O) ->
    pass.

%% 推送第三方
send_other(#{<<"send_alarm_status">> := <<"start">>, <<"_deviceid">> := DeviceId, <<"_viewid">> := ViewId, <<"dgiot_alarmvalue">> := Alarmvalue}) ->
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"meta">> := #{<<"otherpush">> := #{<<"ispush">> := <<"true">>, <<"type">> := <<"mqtt">>, <<"topic">> := Topic, <<"level">> := Level, <<"description">> := Description}}}} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"name">> := DeviceName, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                    ProductName =
                        case dgiot_parse:get_object(<<"Product">>, ProductId) of
                            {ok, #{<<"name">> := Name1}} ->
                                Name1;
                            _ ->
                                <<>>
                        end,
                    <<Number:10/binary, _/binary>> = dgiot_utils:random(),
                    Timestamp = dgiot_datetime:format(dgiot_datetime:to_localtime(dgiot_datetime:now_secs()), <<"YY-MM-DD HH:NN:SS">>),
                    BinAlarmvalue = dgiot_utils:to_binary(Alarmvalue),
                    Data = #{
                        <<"id">> => Number,
                        <<"deviceid">> => DeviceId,
                        <<"devicename">> => DeviceName,
                        <<"status">> => <<"在线"/utf8>>,
                        <<"content">> => Description,
                        <<"time">> => Timestamp,
                        <<"level">> => Level,
                        <<"data">> => #{
                            <<"id">> => Number,
                            <<"deviceid">> => DeviceId,
                            <<"devicename">> => DeviceName,
                            <<"productid">> => ProductId,
                            <<"productname">> => ProductName,
                            <<"value">> => Alarmvalue,
                            <<"type">> => <<"warn">>,
                            <<"description">> => <<Description/binary, "；触发值："/utf8, BinAlarmvalue/binary>>
                        }
                    },
                    dgiot_mqtt:publish(DeviceId, <<"bridge/", Topic/binary>>, dgiot_json:encode(Data)),
                    dgiot_mqtt:publish(DeviceId, Topic, dgiot_json:encode(Data));
                _ ->
                    pass
            end;
        _O ->
            pass
    end;

send_other(_O) ->
    pass.

%% 触发 告警工单创建
send_maintenance(#{<<"send_alarm_status">> := <<"start">>, <<"notificationid">> := NotificationId, <<"_deviceid">> := DeviceId, <<"devaddr">> := Devaddr, <<"_viewid">> := ViewId, <<"_productid">> := ProductId}) ->
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"meta">> := #{<<"workorder">> := #{<<"iscreat">> := <<"true">>, <<"type">> := Type, <<"cause">> := Cause}}}} ->
            Now = dgiot_datetime:now_secs(),
            Num = dgiot_datetime:format(dgiot_datetime:to_localtime(Now), <<"YYMMDDHHNNSS">>),
            Timestamp = dgiot_datetime:format(dgiot_datetime:to_localtime(Now), <<"YY-MM-DD HH:NN:SS">>),
            ACL =
                case dgiot_device:lookup(DeviceId) of
                    {ok, #{<<"acl">> := Acls, <<"devaddr">> := Devaddr}} ->
                        lists:foldl(fun(X, Acc) ->
                            Acc#{
                                dgiot_utils:to_binary(X) => #{
                                    <<"read">> => true,
                                    <<"write">> => true
                                }}
                                    end, #{}, Acls);
                    _ ->
                        #{<<"role:开发者"/utf8>> => #{<<"read">> => true, <<"write">> => true}}
                end,

            Body = #{
                <<"number">> => <<"WX", Num/binary>>,
                <<"type">> => Type,
                <<"status">> => 0,
                <<"devaddr">> => Devaddr,
                <<"ACL">> => ACL,
                <<"info">> => #{
                    <<"createdtime">> => dgiot_datetime:now_ms(),
                    <<"phone">> => <<"">>,
                    <<"video">> => <<"">>,
                    <<"spinner">> => <<"">>,
                    <<"devaddr">> => Devaddr,
                    <<"fault_cause">> => Cause,
                    <<"timeline">> => [
                        #{
                            <<"timestamp">> => Timestamp,
                            <<"h4">> => <<"生成工单"/utf8>>,
                            <<"p">> => <<"自动创建工单"/utf8>>
                        }
                    ]
                },
                <<"product">> => #{
                    <<"objectId">> => ProductId,
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>
                },
                <<"device">> => #{
                    <<"objectId">> => DeviceId,
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Device">>
                },
                <<"notification">> => #{
                    <<"objectId">> => NotificationId,
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Notification">>
                }
            },
            dgiot_parse:create_object(<<"Maintenance">>, Body);
        _O ->
%%            io:format("~s ~p _O = ~p.~n", [?FILE, ?LINE, _O]),
            pass
    end;

send_maintenance(_O) ->
    pass.

%%  产品名称：%PRODUCTNAME%
%%  部门名称：%ROLENAME%
%%  设备名称：%DEVICENAME%
%%  设备地址：%DEVICEADDR%
%%  日期：%DATE%
%%  时间：%DATETIME%
%%  用户名称：%USERNAME%
%%  报警时间：%TRIGGERTIME%
%%  变量名称：%DATAPOINTNAME%
%%  当前值：%NOWVALUE%
%%  触发描述：%TRIGGERDESCRIPTION%
replace_param(Param, ProductName, ProductId, DeviceId, Key, Value, Alarm_createdAt, TriggerdeScription, Alarm_message) ->
    Date = dgiot_datetime:format("YYYY-MM-DD"),
    DateTime = dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"),
    {DeviceName, DeviceAddr, OldACL, UserToken} =
        case dgiot_parse:get_object(<<"Device">>, DeviceId) of
            {ok, #{<<"name">> := Name, <<"devaddr">> := Devaddr, <<"ACL">> := Acl} = Device} ->
                Content = maps:get(<<"content">>, Device, #{}),
                Person = maps:get(<<"person">>, Content, #{}),
                SessionToken = maps:get(<<"sessiontoken">>, Person, <<>>),
                {Name, Devaddr, Acl, SessionToken};
            _ ->
                {<<"--">>, <<"--">>, #{}, <<"--">>}
        end,
    DeviceAcl = #{
        <<"ACL">> => OldACL,
        <<"objectId">> => DeviceId
    },
    NewACL = dgiot_role:get_acls(DeviceAcl),
    DeviceRoleName =
        lists:foldl(fun(X, _Acc) ->
            BinX = atom_to_binary(X),
            case BinX of
                <<"role:", NewName/binary>> ->
                    NewName;
                _ ->
                    <<"admin">>
            end
                    end, <<>>, NewACL),
    Username =
        case dgiot_auth:get_session(UserToken) of
            #{<<"nick">> := Nick} ->
                Nick;
            _ ->
                <<"admin">>
        end,
    DataPointname =
        case dgiot_data:get({product, <<ProductId/binary, Key/binary>>}) of
            not_find ->
                <<"--">>;
            {ThingName, _, _} ->
                ThingName
        end,
    Str1 = re:replace(Param, "%PRODUCTNAME%", ProductName, [global, {return, list}]),
    Str2 = re:replace(Str1, "%ROLENAME%", DeviceRoleName, [global, {return, list}]),
    Str3 = re:replace(Str2, "%DEVICENAME%", DeviceName, [global, {return, list}]),
    Str4 = re:replace(Str3, "%DEVICEADDR%", DeviceAddr, [global, {return, list}]),
    Str5 = re:replace(Str4, "%DATE%", Date, [global, {return, list}]),
    Str6 = re:replace(Str5, "%DATETIME%", DateTime, [global, {return, list}]),
    Str7 = re:replace(Str6, "%USERNAME%", Username, [global, {return, list}]),
    Str8 = re:replace(Str7, "%TRIGGERTIME%", Alarm_createdAt, [global, {return, list}]),
    Str9 = re:replace(Str8, "%DATAPOINTNAME%", DataPointname, [global, {return, list}]),
    Str10 = re:replace(Str9, "%TRIGGERDESCRIPTION%", TriggerdeScription, [global, {return, list}]),
    Str11 = re:replace(Str10, "%TRIGGERCONTENT%", Alarm_message, [global, {return, list}]),
    re:replace(Str11, "%NOWVALUE%", dgiot_utils:to_list(Value), [global, {return, binary}]).


%%  产品名称：%PRODUCTNAME%
replace_miniparam(Acc, <<"%PRODUCTNAME%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, _Device, _ProductId, ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => ProductName}};

%%  部门名称：%ROLENAME%
replace_miniparam(Acc, <<"%ROLENAME%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, Device, _ProductId, _ProductName, _TriggerdeScription) ->
    NewACL = dgiot_role:get_acls(Device),
    DeviceRoleName =
        lists:foldl(fun(X, _Acc) ->
            BinX = atom_to_binary(X),
            case BinX of
                <<"role:", NewName/binary>> ->
                    NewName;
                _ ->
                    <<"admin">>
            end
                    end, <<>>, NewACL),
    Acc#{Value => #{<<"value">> => DeviceRoleName}};

%%  设备名称：%DEVICENAME%
replace_miniparam(Acc, <<"%DEVICENAME%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, Device, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => maps:get(<<"name">>, Device, <<>>)}};

%%  设备编号：%DEVICEADDR%
replace_miniparam(Acc, <<"%DEVICEADDR%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, Device, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => maps:get(<<"devaddr">>, Device, <<>>)}};

%%  设备位置：%DEVICELOCATION%
replace_miniparam(Acc, <<"%DEVICELOCATION%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, Device, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => maps:get(<<"address">>, Device, <<"无位置"/utf8>>)}};

%%  日期：%DATE%
replace_miniparam(Acc, <<"%DATE%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, _DeviceId, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => dgiot_datetime:format("YYYY-MM-DD")}};

%%  时间：%DATETIME%
replace_miniparam(Acc, <<"%DATETIME%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, _DeviceId, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => dgiot_datetime:format("YYYY-MM-DD HH:NN:SS")}};

%%  用户名称：%USERNAME%
replace_miniparam(Acc, <<"%USERNAME%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, Device, _ProductId, _ProductName, _TriggerdeScription) ->
    Content = maps:get(<<"content">>, Device, #{}),
    Person = maps:get(<<"person">>, Content, #{}),
    SessionToken = maps:get(<<"sessiontoken">>, Person, <<>>),
    Username =
        case dgiot_auth:get_session(SessionToken) of
            #{<<"nick">> := Nick} ->
                Nick;
            _ ->
                <<"admin">>
        end,
    Acc#{Value => #{<<"value">> => Username}};

%%  报警时间：%TRIGGERTIME%
replace_miniparam(Acc, <<"%TRIGGERTIME%">>, Value, Alarm_createdAt, _Alarmkey, _Alarmvalue, _DeviceId, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => Alarm_createdAt}};

%%  变量名称：%DATAPOINTNAME%
replace_miniparam(Acc, <<"%DATAPOINTNAME%">>, Value, _Alarm_createdAt, Alarmkey, _Alarmvalue, _DeviceId, ProductId, _ProductName, _TriggerdeScription) ->
    DataPointname =
        case dgiot_data:get({product, <<ProductId/binary, Alarmkey/binary>>}) of
            not_find ->
                <<"--">>;
            {ThingName, _, _} ->
                ThingName
        end,
    Acc#{Value => #{<<"value">> => DataPointname}};

%%  当前值：%NOWVALUE%
replace_miniparam(Acc, <<"%NOWVALUE%">>, Value, _Alarm_createdAt, _Alarmkey, Alarmvalue, _DeviceId, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => Alarmvalue}};

%%  触发描述：%TRIGGERDESCRIPTION%
replace_miniparam(Acc, <<"%TRIGGERDESCRIPTION%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, _DeviceId, _ProductId, _ProductName, TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => TriggerdeScription}};

%%  内容：%TRIGGERCONTENT%
replace_miniparam(Acc, <<"%TRIGGERCONTENT%">>, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, _DeviceId, _ProductId, _ProductName, TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => TriggerdeScription}};

replace_miniparam(Acc, _, Value, _Alarm_createdAt, _Alarmkey, _Alarmvalue, _DeviceId, _ProductId, _ProductName, _TriggerdeScription) ->
    Acc#{Value => #{<<"value">> => <<"无"/utf8>>}}.

send_mode(#{<<"sms">> := #{<<"issend">> := <<"true">>, <<"params">> := Params, <<"tplid">> := TplId, <<"roleid">> := RoleId} = Sms},
    ProductName,
    #{<<"roleid">> := NotifRoleid, <<"alarm_createdAt">> := Alarm_createdAt, <<"alarm_message">> := Alarm_message, <<"_deviceid">> := DeviceId, <<"_productid">> := ProductId, <<"dgiot_alarmkey">> := Key, <<"dgiot_alarmvalue">> := Value},
    TriggerdeScription) when is_list(Params) ->
    NewParams =
        lists:foldl(fun(Param, Acc) ->
            Acc ++ [replace_param(Param, ProductName, ProductId, DeviceId, Key, Value, Alarm_createdAt, TriggerdeScription, Alarm_message)]
                    end, [], Params),
    Mobile =
        case maps:find(<<"users">>, Sms) of
            {ok, Users} ->
                UsersQuery = #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => Users}}},
                case dgiot_parse:query_object(<<"_User">>, UsersQuery) of
                    {ok, #{<<"results">> := Row}} ->
                        lists:foldl(fun(User, Acc) ->
                            Phone = maps:get(<<"phone">>, User, ""),
                            case dgiot_utils:is_phone(Phone) of
                                true ->
                                    Acc ++ [#{<<"mobile">> => Phone, <<"nationcode">> => <<"86">>}];
                                _ ->
                                    Acc
                            end
                                    end, [], Row);
                    _ ->
                        dgiot_notification:get_Mobile(DeviceId, RoleId, NotifRoleid)
                end;
            _ ->
                dgiot_notification:get_Mobile(DeviceId, RoleId, NotifRoleid)
        end,
    dgiot_notification:send_sms(Mobile, TplId, NewParams);

send_mode(#{<<"email">> := #{<<"issend">> := <<"true">>, <<"params">> := Params} = Email},
    ProductName,
    #{<<"roleid">> := NotifRoleid, <<"alarm_createdAt">> := Alarm_createdAt, <<"alarm_message">> := Alarm_message, <<"_deviceid">> := DeviceId, <<"_productid">> := ProductId, <<"dgiot_alarmkey">> := Key, <<"dgiot_alarmvalue">> := Value},
    TriggerdeScription) when is_binary(Params) ->
    Subject = maps:get(<<"subject">>, Email, <<>>),
    Todes = maps:get(<<"todes">>, Email, <<>>),
    Fromdes = maps:get(<<"fromdes">>, Email, <<>>),
    RoleId = maps:get(<<"roleid">>, Email, <<>>),
    Emails = dgiot_notification:get_Emails(DeviceId, RoleId, NotifRoleid),

    Data = #{
        <<"to">> => Emails,
        <<"fromdes">> => replace_param(Fromdes, ProductName, ProductId, DeviceId, Key, Value, Alarm_createdAt, TriggerdeScription, Alarm_message),
        <<"todes">> => replace_param(Todes, ProductName, ProductId, DeviceId, Key, Value, Alarm_createdAt, TriggerdeScription, Alarm_message),
        <<"subject">> => replace_param(Subject, ProductName, ProductId, DeviceId, Key, Value, Alarm_createdAt, TriggerdeScription, Alarm_message),
        <<"data">> => replace_param(Params, ProductName, ProductId, DeviceId, Key, Value, Alarm_createdAt, TriggerdeScription, Alarm_message)
    },
    dgiot_notification:send_email(Data);

send_mode(_, _ProductName, _, _) ->
    pass.
