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

-module(dgiot_parse_id).
-author("kenneth").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    get_objectid/2,
    get_categoryid/2,
    get_channelid/3,
    get_deviceid/2,
    get_userid/1,
    get_dictid/4,
    get_viewid/4,
    get_shapeid/2,
    get_instructid/3,
    get_roleid/1,
    get_ruleid/1,
    get_menuid/1,
    get_productid/3,
    get_maintenanceid/2,
    get_articleid/2,
    get_loglevelid/2,
    get_sessionId/1,
    get_userids/1,
    get_roleids/1,
    get_notificationid/1,
    get_evidenceId/2,
    get_devicelogid/2,
    get_notificationid/2,
    get_filesId/2,
    get_masterDataId/1,
    get_metaDataId/1,
    get_gitid/2,
    get_orderid/3
]).


get_gitid(ObjectId, Ts) ->
    <<GitId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Git", ObjectId/binary, Ts:32>>),
    GitId.

get_categoryid(Level, Name) ->
    <<CategoryId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Category", Level/binary, Name/binary>>),
    CategoryId.


get_shapeid(DeviceId, Identifier) ->
    BinDeviceId = dgiot_utils:to_binary(DeviceId),
    BinIdentifier = dgiot_utils:to_binary(Identifier),
    <<ShapeId:10/binary, _/binary>> = dgiot_utils:to_md5(<<BinDeviceId/binary, BinIdentifier/binary, "dgiottopo">>),
    ShapeId.

get_dictid(Key, Type, Class, Title) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse_id:get_objectid(<<"Dict">>, #{<<"key">> => Key, <<"type">> => Type, <<"class">> => Class, <<"title">> => Title}),
    DeviceId.

get_viewid(Key, Type, Class, Title) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse_id:get_objectid(<<"View">>, #{<<"key">> => Key, <<"type">> => Type, <<"class">> => Class, <<"title">> => Title}),
    DeviceId.

get_channelid(Type, CType, Name) ->
    #{<<"objectId">> := ChannelID} =
        dgiot_parse_id:get_objectid(<<"Channel">>, #{<<"name">> => Name, <<"type">> => Type, <<"cType">> => CType}),
    ChannelID.

get_deviceid(ProductId, DevAddr) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse_id:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
    DeviceId.


get_orderid(Device, Schedule, Type) ->
    <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Order", Device/binary, Schedule/binary, Type/binary>>),
    DId.

get_userid(UserName) ->
    #{<<"objectId">> := UserId} =
        dgiot_parse_id:get_objectid(<<"_User">>, #{<<"username">> => UserName}),
    UserId.

get_devicelogid(DeviceId, DevAddr) ->
    #{<<"objectId">> := DevicelogId} =
        dgiot_parse_id:get_objectid(<<"Devicelog">>, #{<<"device">> => #{<<"objectId">> => DeviceId}, <<"devaddr">> => DevAddr}),
    DevicelogId.

get_notificationid(DeviceId, Type) ->
    #{<<"objectId">> := NotificationId} =
        dgiot_parse_id:get_objectid(<<"Notification">>, #{<<"device">> => DeviceId, <<"type">> => Type}),
    NotificationId.

get_filesId(Path, Name) ->
    #{<<"objectId">> := FilesId} =
        dgiot_parse_id:get_objectid(<<"Files">>, #{<<"path">> => Path, <<"name">> => Name}),
    FilesId.

get_instructid(DeviceId, Pn, Di) ->
    <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Instruct", DeviceId/binary, Pn/binary, Di/binary>>),
    DId.

get_roleid(Name) ->
    <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"_Role", Name/binary>>),
    DId.

get_ruleid(Name) ->
    <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Permission", Name/binary>>),
    DId.

get_menuid(Name) ->
    <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Menu", Name/binary>>),
    DId.

get_notificationid(Type) ->
    UUID = dgiot_utils:guid(),
    <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Notification", Type/binary, UUID/binary>>),
    DId.

get_productid(Categoryid, DevType, Name) ->
    <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Product", Categoryid/binary, DevType/binary, Name/binary>>),
    Pid.

get_maintenanceid(Deviceid, Number) ->
    <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Maintenance", Deviceid/binary, Number/binary>>),
    Pid.

get_articleid(ProjectId, Timestamp) ->
    <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Article", ProjectId/binary, Timestamp/binary>>),
    Pid.

get_loglevelid(Name, Type) ->
    <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"LogLevel", Name/binary, Type/binary>>),
    Pid.

get_sessionId(SessionToken) ->
    <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"_Session", SessionToken/binary>>),
    Pid.

get_evidenceId(Ukey, TimeStamp) ->
    <<EId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Evidence", Ukey/binary, TimeStamp/binary>>),
    EId.

get_masterDataId(Name) ->
    <<EId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"MasterData", Name/binary>>),
    EId.

get_metaDataId(Name) ->
    <<EId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"MetaData", Name/binary>>),
    EId.

get_objectid(Class, Map) ->
    case Class of
        <<"post_classes_session">> ->
            get_objectid(<<"Session">>, Map);
        <<"Session">> ->
            SessionToken = maps:get(<<"sessionToken">>, Map, <<"">>),
            <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"_Session", SessionToken/binary>>),
            Map#{
                <<"objectId">> => Pid
            };
        <<"post_classes_article">> ->
            get_objectid(<<"Article">>, Map);
        <<"Article">> ->
            Timestamp = maps:get(<<"timestamp">>, Map, <<"">>),
            ProjectId = maps:get(<<"projectId">>, Map, <<"">>),
            <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Article", ProjectId/binary, Timestamp/binary>>),
            Map#{
                <<"objectId">> => Pid
            };
        <<"post_classes_maintenance">> ->
            get_objectid(<<"Maintenance">>, Map);
        <<"Maintenance">> ->
            #{<<"objectId">> := Deviceid} = maps:get(<<"device">>, Map, #{<<"objectId">> => <<"">>}),
            Number = maps:get(<<"number">>, Map, <<"">>),
            <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Maintenance", Deviceid/binary, Number/binary>>),
            Map#{
                <<"objectId">> => Pid
            };
        <<"post_classes_product">> ->
            get_objectid(<<"Product">>, Map);
        <<"Product">> ->
            DevType = maps:get(<<"devType">>, Map, <<"">>),
            Category = maps:get(<<"category">>, Map, <<"">>),
            Categoryid = maps:get(<<"objectId">>, Category, <<"">>),
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Product", Categoryid/binary, DevType/binary, Name/binary>>),
            Map#{
                <<"objectId">> => Pid
            };
        <<"post_classes_producttemplet">> ->
            get_objectid(<<"ProductTemplet">>, Map);
        <<"ProductTemplet">> ->
            Category = maps:get(<<"category">>, Map, <<"">>),
            Categoryid = maps:get(<<"objectId">>, Category, <<"">>),
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"ProductTemplet", Categoryid/binary, Name/binary>>),
            Map#{
                <<"objectId">> => Pid
            };
        <<"post_classes_category">> ->
            get_objectid(<<"Category">>, Map);
        <<"Category">> ->
            Level = dgiot_utils:to_binary(maps:get(<<"level">>, Map, 1)),
            Name = maps:get(<<"name">>, Map, <<"">>),
            Map#{
                <<"objectId">> => get_categoryid(Level, Name)
            };
        <<"Git">> ->
            Ts = maps:get(<<"ts">>, Map, dgiot_datetime:now_ms()),
            Id = maps:get(<<"id">>, Map, <<"">>),
            Map#{
                <<"objectId">> => get_gitid(Id, Ts)
            };
        <<"post_classes_device">> ->
            get_objectid(<<"Device">>, Map);
        <<"post_classes_masterData">> ->
            get_objectid(<<"MasterData">>, Map);
        <<"post_classes_metaData">> ->
            get_objectid(<<"MetaData">>, Map);
        <<"Device">> ->
            Product = case maps:get(<<"product">>, Map) of
                          #{<<"objectId">> := ProductId} ->
                              dgiot_utils:to_binary(ProductId);
                          ProductId1 ->
                              dgiot_utils:to_binary(ProductId1)
                      end,
            DevAddr = dgiot_utils:to_binary(maps:get(<<"devaddr">>, Map, <<>>)),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Device", Product/binary, DevAddr/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"MetaData">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"MetaData", Name/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"MasterData">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"MasterData", Name/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"post_classes_devicelog">> ->
            get_objectid(<<"Devicelog">>, Map);
        <<"Devicelog">> ->
            Device =
                case maps:get(<<"device">>, Map, #{}) of
                    #{<<"objectId">> := DeviceId} ->
                        DeviceId;
                    _ ->
                        dgiot_utils:to_binary(dgiot_datetime:now_microsecs())
                end,
            DevAddr = maps:get(<<"devaddr">>, Map, <<"">>),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Devicelog", Device/binary, DevAddr/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"post_classes_notification">> ->
            get_objectid(<<"Notification">>, Map);
        <<"Notification">> ->
            UUID = dgiot_utils:guid(),
            Type = maps:get(<<"type">>, Map, <<"">>),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Notification", Type/binary, UUID/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"post_classes_loglevel">> ->
            get_objectid(<<"LogLevel">>, Map);
        <<"LogLevel">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            Type = maps:get(<<"type">>, Map, <<"">>),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"LogLevel", Name/binary, Type/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"post_classes_evidence">> ->
            get_objectid(<<"Evidence">>, Map);
        <<"Evidence">> ->
            Ukey = maps:get(<<"ukey">>, Map, <<"">>),
            TimeStamp = dgiot_utils:to_binary(maps:get(<<"timestamp">>, Map, <<"">>)),
            <<EId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Evidence", Ukey/binary, TimeStamp/binary>>),
            Map#{
                <<"objectId">> => EId
            };
        <<"post_classes_channel">> ->
            get_objectid(<<"Channel">>, Map);
        <<"Channel">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            Config = maps:get(<<"config">>, Map, #{}),
            DefultName =
                case maps:find(<<"defultname">>, Config) of
                    {ok, Defultname} when byte_size(Defultname) > 0 ->
                        Defultname;
                    _ ->
                        Name
                end,
            Type = maps:get(<<"type">>, Map, <<"">>),
            CType = maps:get(<<"cType">>, Map, <<"">>),
            <<CId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Channel", Type/binary, CType/binary, DefultName/binary>>),
            Map#{
                <<"name">> => DefultName,
                <<"objectId">> => CId
            };
        <<"post_classes_dict">> ->
            get_objectid(<<"Dict">>, Map);
        <<"Dict">> ->
            Key = maps:get(<<"key">>, Map, <<"">>),
            Type = maps:get(<<"type">>, Map, <<"">>),
            Class1 = maps:get(<<"class">>, Map, <<"">>),
            Title = maps:get(<<"title">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Dict", Class1/binary, Key/binary, Type/binary, Title/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_view">> ->
            get_objectid(<<"View">>, Map);
        <<"View">> ->
            Key = maps:get(<<"key">>, Map, <<"">>),
            Type = maps:get(<<"type">>, Map, <<"">>),
            Class2 = maps:get(<<"class">>, Map, <<"">>),
            Title = maps:get(<<"title">>, Map, <<"">>),
            <<VId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"View", Class2/binary, Key/binary, Type/binary, Title/binary>>),
            Map#{
                <<"objectId">> => VId
            };
        <<"post_classes_instruct">> ->
            get_objectid(<<"Instruct">>, Map);
        <<"Instruct">> ->
            #{<<"objectId">> := DeviceId} = maps:get(<<"device">>, Map, #{<<"objectId">> => <<"">>}),
            Pn = maps:get(<<"pn">>, Map, <<"">>),
            Di = maps:get(<<"di">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Instruct", DeviceId/binary, Pn/binary, Di/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_menu">> ->
            get_objectid(<<"Menu">>, Map);
        <<"Menu">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Menu", Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_permission">> ->
            get_objectid(<<"Permission">>, Map);
        <<"Permission">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Permission", Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"_Role">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"_Role", Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"_User">> ->
            Name = maps:get(<<"username">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"_User", Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"shift">> ->
            Date = maps:get(<<"date">>, Map, <<"">>),
            Device = maps:get(<<"device">>, Map, <<"">>),
            Shift = maps:get(<<"shift">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Shift", Date/binary, Device/binary, Shift/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"Files">> ->
            Path = maps:get(<<"path">>, Map, <<"">>),
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Files", Path/binary, Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"Order">> ->
            #{<<"objectId">> := Device} = maps:get(<<"device">>, Map, #{<<"objectId">> => <<"">>}),
            #{<<"objectId">> := Schedule} = maps:get(<<"schedule">>, Map, #{<<"objectId">> => <<"">>}),
            Type = maps:get(<<"type">>, Map, <<>>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Order", Device/binary, Schedule/binary, Type/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        _ ->
            Map
    end.

get_userids(Roleid) ->
    case dgiot_data:get(?ROLE_USER_ETS, Roleid) of
        not_find ->
            [];
        UserIds when length(UserIds) > 0 ->
            UserIds;
        _ ->
            []
    end.


get_roleids(Userid) ->
    case dgiot_data:get(user_role_ets, Userid) of
        not_find ->
            [];
        RoleIds when length(RoleIds) > 0 ->
            RoleIds;
        _ ->
            []
    end.
