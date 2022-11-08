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
do_check(#{peerhost := PeerHost}, _PubSub, _Topic)  when PeerHost == {127,0,0,1}  ->
%%    io:format("~s ~p ClientId: ~p _Topic ~p ~n", [?FILE, ?LINE, ClientId, _Topic]),
    allow;

do_check(#{username := <<"dgiot">>, clientid := ClientId}, _PubSub, _Topic) ->
%%    io:format("~s ~p ClientId: ~p _Topic ~p ~n", [?FILE, ?LINE, ClientId, _Topic]),
    SuperPwd = dgiot_utils:to_binary(dgiot:get_env(dgiot_dlink, super_pwd, <<"">>)),
    case SuperPwd of
        ClientId ->
            allow;
        _ ->
            deny
    end;

%% <<"$dg/thing/uniapp/r:46d3eaec6dfeafc9d719899eae858cb7_uniapp/report">>
do_check(#{clientid := <<Token:34/binary, _Type/binary>>, username := _UserId} = _ClientInfo, publish, <<"$dg/thing/uniapp/", SessionToken:34/binary, "/", _Rest/binary>> = _Topic) ->
    %% io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case Token of
        SessionToken ->
            allow;
        _ ->
            deny
    end;

%% "$dg/thing/productid/devaddr/#"
do_check(#{clientid := DeviceAddr, username := ProductID} = _ClientInfo, publish, <<"$dg/thing/", ProductID:10/binary, "/", DeviceInfo/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p _ClientInfo ~p~n", [?FILE, ?LINE, _Topic, _ClientInfo]),
    check_device_addr(DeviceInfo, DeviceAddr);

do_check(#{clientid := <<ProductID:10/binary, "_", DeviceAddr/binary>>, username := ProductID} = _ClientInfo, publish, <<"$dg/thing/", ProductID:10/binary, "/", DeviceInfo/binary>> = _Topic) ->
    check_device_addr(DeviceInfo, DeviceAddr);

%% "$dg/thing/deviceid/#"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, publish, <<"$dg/thing/", DeviceId:10/binary, "/", _Rest/binary>> = _Topic)
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

do_check(#{clientid := DeviceAddr, username := ProductID} = _ClientInfo, subscribe, <<"$dg/device/", ProductID:10/binary, "/", DeviceInfo/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p _ClientInfo ~p~n", [?FILE, ?LINE, _Topic, _ClientInfo]),
    check_device_addr(DeviceInfo, DeviceAddr);


%% "$dg/user/uniapp/{SessionToken}/report"
do_check(#{clientid := <<Token:34/binary, _Type/binary>>, username := _UserId} = _ClientInfo, subscribe, <<"$dg/user/uniapp/", SessionToken:34/binary, "/", _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case Token of
        SessionToken ->
            allow;
        _ ->
            deny
    end;

%% 告警上报 "$dg/user/alarm/{productId}/{deviceId}/properties/report"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, publish, <<"$dg/user/alarm/", _ProductID:10/binary, "/", DeviceId:10/binary, "/", _Rest/binary>> = _Topic)
    when Token =/= undefined ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case check_device_acl(Token, DeviceId, UserId) of
        ok ->
            allow;
        _ ->
            deny
    end;

%% "$dg/user/channel/{channelId}/{productId}/{deviceId}"
do_check(#{clientid := Token} = _ClientInfo, subscribe, <<"$dg/user/channel/", DeviceInfo/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    [ChannelId | _] = binary:split(DeviceInfo, <<"/">>, [global]),
    case dgiot_parse:get_object(<<"Channel">>, ChannelId, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
        {ok, _} ->
            allow;
        _ ->
            deny
    end;

%% $dg/user/dashboard/{dashboardId}/{productId}/{deviceId}
do_check(#{clientid := Token} = _ClientInfo, subscribe, <<"$dg/user/dashboard/", DashboardId:10/binary, "/", _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case dgiot_parse:get_object(<<"View">>, DashboardId, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
        {ok, _} ->
            allow;
        _ ->
            deny
    end;

%% "$dg/user/topo/{SessionToken}/#"
do_check(#{clientid := <<Token:34/binary, _Type/binary>>, username := _UserId} = _ClientInfo, subscribe, <<"$dg/user/topo/", SessionToken:34/binary, "/", _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    case Token of
        SessionToken ->
            allow;
        _ ->
            deny
    end;

%% "$dg/user/trace/{DeviceId}/{Topic}"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, subscribe, <<"$dg/user/trace/", DeviceId:10/binary, "/", _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    check_device_acl(Token, DeviceId, UserId);

%% "$dg/user/trace/{DeviceId}/{Topic}"
do_check(#{clientid := Token, username := UserId} = _ClientInfo, subscribe, <<"$dg/user/trajectory/", DeviceId:10/binary, "/", _Rest/binary>> = _Topic) ->
%%    io:format("~s ~p Topic: ~p~n", [?FILE, ?LINE, _Topic]),
    check_device_acl(Token, DeviceId, UserId);

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

do_check(_ClientInfo, _PubSub, _Topic) ->
%%    io:format("~s ~p _PubSub: ~p~n", [?FILE, ?LINE, _PubSub]),
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
