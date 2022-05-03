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

-module(dgiot_device_static).
-define(CRLF, "\r\n").
-author("jonhliu").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([inc/1, inc/2, dec/1, dec/2, val/1, val/2, get_count/1, get_count/2]).

get_count(Header) ->
    Token =
        case proplists:get_value("X-Parse-Session-Token", Header) of
            undefined ->
                proplists:get_value(<<"X-Parse-Session-Token">>, Header);
            Token1 ->
                Token1
        end,
    RoleIds =
        case dgiot_auth:get_session(Token) of
            #{<<"roles">> := Roles} ->
                maps:keys(Roles);
            _ ->
                []
        end,
    get_count(RoleIds, roleid).

%% dgiot_parse_cache:get_count(<<"Product">>, [<<"role:root">>],acl).
%% dgiot_parse_cache:get_count(<<"Device">>, [<<"role:root">>],acl).
%% dgiot_parse_cache:get_count(<<"ProductTemplet">>, [<<"role:root">>],acl).
get_count(Acls, acl) ->
    ChildAcls =
        lists:foldl(
            fun
                (AclName, Acc) ->
                    Acc ++ dgiot_role:get_childacl(AclName)
            end, [], Acls),
    loop_count(dgiot_utils:unique_1(ChildAcls));

get_count(RoleId, roleid) ->
    ChildRoleIds = dgiot_role:get_childrole(dgiot_utils:to_binary(RoleId)),
    Acls = lists:foldl(fun(ChildRoleId, Acc) ->
        Acc ++ [dgiot_role:get_alcname(ChildRoleId)]
                       end, [dgiot_role:get_alcname(RoleId)], ChildRoleIds),
    loop_count(Acls).

loop_count(QueryAcls) ->
    AtomQueryAcls = dgiot_role:get_acls(QueryAcls),
    Fun =
        fun
            ({_, _, ['Device', Acls| _] = V}) ->
                case AtomQueryAcls -- Acls of
                    AtomQueryAcls ->
                        pass;
                    _ ->
                        dgiot_device:inc(V)
                end;
            (_) ->
                pass
        end,
    dgiot_mnesia:search(Fun, #{<<"skip">> => 0, <<"limit">> => 1000000}).

inc(Class) ->
    inc(Class, all).
inc(Class, Type) ->
    AtomClass = dgiot_utils:to_atom(Class),
    AtomType = dgiot_utils:to_atom(Type),
    case erlang:get({self(), AtomClass, AtomType}) of
        undefined ->
            erlang:put({self(), AtomClass, AtomType}, 1);
        Count ->
            NewCount = Count + 1,
            erlang:put({self(), AtomClass, AtomType}, NewCount)
    end.

dec(Class) ->
    dec(Class, all).
dec(Class, Type) ->
    AtomClass = dgiot_utils:to_atom(Class),
    AtomType = dgiot_utils:to_atom(Type),
    case erlang:get({self(), AtomClass, AtomType}) of
        undefined ->
            erlang:put({self(), AtomClass, AtomType}, 0);
        0 ->
            pass;
        Count ->
            NewCount = Count - 1,
            erlang:put({self(), AtomClass, AtomType}, NewCount)
    end.

val(Class) ->
    val(Class, all).
val(Class, Type) ->
    AtomClass = dgiot_utils:to_atom(Class),
    AtomType = dgiot_utils:to_atom(Type),
    case erlang:get({self(), AtomClass, AtomType}) of
        undefined ->
            0;
        Count ->
            Count
    end.