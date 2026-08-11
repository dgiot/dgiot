<<<<<<< HEAD
%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_device_permission).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    get_acl/1,
    get_acl_by_role/1,
    get_roleids/1,
    get_readonly_acl/1,
    get_appname/1
]).

%% @doc 获取设备ACL - 通过设备ID
-spec get_acl(DeviceId :: binary()) -> map().
get_acl(DeviceId) when is_binary(DeviceId) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := Acls}} ->
            lists:foldl(fun(Acl, Acc) ->
                maps:merge(get_acl_by_role(Acl), Acc)
                        end, #{}, Acls);
        _ ->
            #{<<"role:root">> => #{
                <<"read">> => true,
                <<"write">> => true}
            }
    end.

%% @doc 获取设备ACL - 通过角色名
-spec get_acl_by_role(Acl :: atom()) -> map().
get_acl_by_role(Acl) when is_atom(Acl) ->
    ACL = dgiot_utils:to_binary(Acl),
    #{ACL => #{
        <<"read">> => true,
        <<"write">> => true}
    }.

%% @doc 获取设备角色ID列表
-spec get_roleids(DeviceId :: binary()) -> list().
get_roleids(DeviceId) ->
    Acl = get_acl(DeviceId),
    io:format("~p", [Acl]),
    Keys = maps:keys(Acl),
    lists:foldl(
    fun
        (<<"role:", Name/binary>>, Acc) -> Acc ++ [dgiot_parse_id:get_roleid(Name)];
        (<<"*">>, Acc) -> Acc 
    end, [], Keys).

%% @doc 获取设备只读ACL
-spec get_readonly_acl(DeviceId :: binary()) -> map().
get_readonly_acl(DeviceId) ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"acl">> := AclList}} ->
            lists:foldl(
                fun(Role, Acc) ->
                    Acc#{Role => #{
                        <<"read">> => true,
                        <<"write">> => false}}
                end, #{}, AclList);
        _ ->
            #{}
    end.

%% @doc 获取设备应用名称
-spec get_appname(DeviceId :: binary()) -> binary().
get_appname(DeviceId) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := [Acl | _]}} ->
            BinAcl = atom_to_binary(Acl),
            case BinAcl of
                <<"role:", Name/binary>> ->
                    Name;
                _ ->
                    <<"admin">>
            end;
        _ ->
            <<"admin">>
    end.
=======
-module(dgiot_device_permission).
-export([get_acl/1, get_acl_by_role/1, get_appname/1,
         get_readonly_acl/1, get_roleids/1]).
get_acl(_) -> [].
get_acl_by_role(_) -> [].
get_appname(_) -> <<>>.
get_readonly_acl(_) -> [].
get_roleids(_) -> [].
>>>>>>> origin/dgaiot-plugins
