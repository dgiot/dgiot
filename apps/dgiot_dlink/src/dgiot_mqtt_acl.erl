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
-export([
    check_acl/5
    , description/0
]).

check_acl(ClientInfo, PubSub, <<"$dg/", _/binary>> = Topic, _NoMatchAction, _Params) ->
    io:format("~s ~p Topic: ~p _NoMatchAction ~p ~n", [?FILE, ?LINE, Topic, _NoMatchAction]),
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

%% 用户订阅 "$dg/user/deviceid/#"
do_check(#{clientid := ClientID, username := Username} = _ClientInfo, subscribe, <<"$dg/user/", DeviceInfo/binary>> = Topic)
    when ClientID =/= undefined ->
    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, Topic]),
    [DeviceID | _] = binary:split(DeviceInfo, <<"/">>),
    %% 此时的ClientID为 Token
    case check_device_acl(ClientID, DeviceID, Username) of
        ok ->
            allow;
        _ ->
            deny
    end;

%%"$dg/device/productid/devaddr/#"
do_check(#{clientid := ClientID} = _ClientInfo, subscribe, <<"$dg/device/", DeviceInfo/binary>> = Topic) ->
    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, Topic]),
    [ProuctID, Devaddr | _] = binary:split(DeviceInfo, <<"/">>, [global]),
    DeviceID = dgiot_parse:get_deviceid(ProuctID, Devaddr),
    case ClientID == DeviceID of
        true ->
            allow;
        _ ->
            deny
    end;

%%"$dg/thing/deviceid/#"
%%"$dg/thing/productid/devaddr/#"
do_check(#{clientid := ClientID, username := UserId} = _ClientInfo, publish, <<"$dg/thing/", DeviceInfo/binary>> = Topic)
    when ClientID =/= undefined ->
    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, Topic]),
    [Id, Devaddr | _] = binary:split(DeviceInfo, <<"/">>, [global]),
    %% 先判断clientid为Token
    case check_device_acl(ClientID, Id, UserId) of
        ok ->
            allow;
        _ ->
            DeviceID = dgiot_parse:get_deviceid(Id, Devaddr),
            case ClientID == DeviceID of
                true ->
                    allow;
                _ ->
                    deny
            end
    end;

do_check(_ClientInfo, _PubSub, Topic) ->
    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, Topic]),
    deny.

check_device_acl(Token, DeviceID, UserId) ->
    case dgiot_auth:get_session(Token) of
        #{<<"objectId">> := UserId, <<"ACL">> := Acl} ->
            case dgiot_device:get_acl(DeviceID) of
                Acl ->
                    ok;
                _ ->
                    deny
            end;
        _ -> deny
    end.
