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

-module(dgiot_mqtt_acl).

%% ACL Callbacks
-export([check_acl/5, description/0, check_device_acl/3]).

check_acl(ClientInfo, PubSub, <<"$dg/", _/binary>> = Topic, _NoMatchAction, _Params) ->
    _Username = maps:get(username, ClientInfo, undefined),
    case do_check(ClientInfo, PubSub, Topic) of
        allow ->
            {stop, allow};
        deny ->
            {stop, deny};
        _ ->
            ok
    end;

check_acl(_ClientInfo, _PubSub, _Topic, _NoMatchAction, _Params) ->
    ok.

description() -> "Acl with Dlink".

%%--------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%% "$dg/thing/productid/devaddr/#"
do_check(#{clientid := DeviceAddr, username := ProductID} = _ClientInfo, publish, <<"$dg/thing/", ProductID:10/binary, "/", DeviceInfo/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p _ClientInfo ~p~n", [?FILE, ?LINE, _Topic, _ClientInfo]),
    check_device_addr(DeviceInfo, DeviceAddr);

do_check(#{clientid := <<ProductID:10/binary, "_", DeviceAddr/binary>>, username := ProductID} = _ClientInfo, publish, <<"$dg/thing/", ProductID:10/binary, "/", DeviceInfo/binary>> = _Topic) ->
    check_device_addr(DeviceInfo, DeviceAddr);

%% "$dg/thing/deviceid/#"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, publish, <<"$dg/thing/", DeviceId:10/binary, "/",  _Rest/binary>> = _Topic)
    when Token =/= undefined ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case check_device_acl(Token, DeviceId, UserId) of
        ok ->
            allow;
        _ ->
            deny
    end;

%%"$dg/device/productid/devaddr/#"
do_check(#{clientid := <<ProductID:10/binary, "_", DeviceAddr/binary>>, username := ProductID} = _ClientInfo, subscribe, <<"$dg/device/", ProductID:10/binary, "/", DeviceInfo/binary>> = _Topic) ->
    check_device_addr(DeviceInfo, DeviceAddr);

do_check(#{clientid := DeviceAddr, username := ProductID} = _ClientInfo, subscribe, <<"$dg/device/", ProductID:10/binary,"/", DeviceInfo/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p _ClientInfo ~p~n", [?FILE, ?LINE, _Topic, _ClientInfo]),
    check_device_addr(DeviceInfo, DeviceAddr);

%% 用户订阅 "$dg/user/deviceid/#"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, subscribe, <<"$dg/user/", DeviceID:10/binary, "/", _Rest/binary>> = _Topic)
    when UserId =/= undefined ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    %% 此时的ClientID为 Token
    case check_device_acl(Token, DeviceID, UserId) of
        ok ->
            allow;
        _ ->
            deny
    end;

%% 告警上报 "$dg/alarm/{productId}/{deviceId}/properties/report"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, publish, <<"$dg/alarm/", _ProductID:10/binary,"/",DeviceId:10/binary, "/",  _Rest/binary>>  = _Topic)
    when Token =/= undefined ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case check_device_acl(Token, DeviceId, UserId) of
        ok ->
            allow;
        _ ->
            deny
    end;

%% "$dg/channel/{channelId}/{productId}/{deviceId}"
do_check(#{clientid := Token} = _ClientInfo, subscribe, <<"$dg/user/channel/", DeviceInfo/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    [ChannelId | _] = binary:split(DeviceInfo, <<"/">>, [global]),
    case dgiot_parse:get_object(<<"Channel">>, ChannelId, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
        {ok, _} ->
            allow;
        _ ->
            deny
    end;

%% $dg/dashboard/{dashboardId}/{productId}/{deviceId}
do_check(#{clientid := Token} = _ClientInfo, subscribe, <<"$dg/user/dashboard/", DashboardId:10/binary, "/", _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case dgiot_parse:get_object(<<"View">>, DashboardId, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
        {ok, _} ->
            allow;
        _ ->
            deny
    end;

%% "$dg/trace/{DeviceId}/{Topic}"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, subscribe, <<"$dg/trace/", DeviceId:10/binary, "/",  _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    check_device_acl(Token, DeviceId, UserId);


do_check(_ClientInfo, _PubSub, _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    deny.

check_device_addr(DeviceInfo, DeviceAddr) ->
    case binary:split(DeviceInfo, <<"/">>, [global]) of
        [DeviceAddr | _] ->
            allow;
        _ ->
            deny
    end.

check_device_acl(Token, DeviceID, UserId) ->
    case dgiot_auth:get_session(Token) of
        #{<<"objectId">> := UserId, <<"roles">> := Roles} ->
            UserAcl = dgiot_role:get_aclNames(Roles),
            DeviceAcl = maps:keys(dgiot_device:get_acl(DeviceID)),
            case DeviceAcl -- UserAcl of
                DeviceAcl ->
                    deny;
                _ ->
                    ok
            end;
        _ -> deny
    end.
