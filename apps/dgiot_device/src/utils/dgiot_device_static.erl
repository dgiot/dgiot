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
-export([stats/2, val/2, val/3, get_count/1, get_count/3]).

get_count(Header) ->
    Token =
        case proplists:get_value("X-Parse-Session-Token", Header) of
            undefined ->
                proplists:get_value(<<"X-Parse-Session-Token">>, Header);
            Token1 ->
                Token1
        end,
    RoleIds =
        case dgiot_auth:get_session(dgiot_utils:to_binary(Token)) of
            #{<<"roles">> := Roles} ->
                maps:keys(Roles);
            _ ->
                []
        end,
    Key = dgiot_utils:random(),
    get_count(RoleIds, roleid, Key),
    Key.

%% dgiot_device_static:get_count([<<"role:root">>],acl, ddd).
get_count(Acls, acl, Key) ->
    ChildAcls =
        lists:foldl(
            fun
                (AclName, Acc) ->
                    Acc ++ dgiot_role:get_childacl(AclName)
            end, [], Acls),
    loop_count(dgiot_utils:unique_1(ChildAcls), Key);

get_count(RoleId, roleid, Key) ->
    ChildRoleIds = dgiot_role:get_childrole(dgiot_utils:to_binary(RoleId)),
    Acls = lists:foldl(fun(ChildRoleId, Acc) ->
        Acc ++ [dgiot_role:get_alcname(ChildRoleId)]
                       end, [dgiot_role:get_alcname(RoleId)], ChildRoleIds),
    loop_count(Acls, Key).

loop_count(QueryAcls, Key) ->
    AtomQueryAcls = dgiot_role:get_acls(QueryAcls),
    Fun =
        fun
            ({_, _, ['Device', Acls | _] = V}) ->
%%                io:format("~s ~p  AtomQueryAcls ~p~n", [?FILE, ?LINE, AtomQueryAcls]),
%%                io:format("~s ~p  Acls ~p~n", [?FILE, ?LINE, Acls]),
                case AtomQueryAcls -- Acls of
                    AtomQueryAcls -> pass;
                    _ -> add(V, Key)
                end;
            (_) -> pass
        end,
    dgiot_mnesia:search(Fun, #{<<"skip">> => 0, <<"limit">> => 1000000}).

%%['Device', Acl, Status, Now, IsEnable, dgiot_utils:to_atom(ProductId), Devaddr, DeviceSecret, Node]
add(['Device', _Acls, Status, _Time, IsEnable, ProductId | _] = _V, Key) ->
    inc(<<"Device">>, Key),
    inc(<<"Device">>, ProductId, Key),
    inc("Device_" ++ dgiot_utils:to_list(IsEnable), Key),
    inc("Device_" ++ dgiot_utils:to_list(IsEnable), ProductId, Key),
    case Status of
        true ->
            inc(<<"Device_Online">>, ProductId, Key),
            inc(<<"Device_Online">>, Key);
        false ->
            inc(<<"Device_Offline">>, ProductId, Key),
            inc(<<"Device_Offline">>, Key)
    end;

add(_V, _Key) ->
%%    io:format("~s ~p ~p ~n",[?FILE, ?LINE,_V]),
    pass.

inc(Class, Key) ->
    inc(Class, all, Key).
inc(Class, Type, Key) ->
    BinClass = dgiot_utils:to_binary(Class),
    BinType = dgiot_utils:to_binary(Type),
    case erlang:get({Key, BinClass, BinType}) of
        undefined ->
            erlang:put({Key, BinClass, BinType}, 1),
            1;
        Count ->
            NewCount = Count + 1,
            erlang:put({Key, BinClass, BinType}, NewCount),
%%            io:format("~s ~p ~p ~p NewCount ~p~n", [?FILE, ?LINE, BinClass, BinType, NewCount]),
            NewCount
    end.

val(Class, Key) ->
    val(Class, all, Key).
val(Class, Type, Key) ->
    BinClass = dgiot_utils:to_binary(Class),
    BinType = dgiot_utils:to_binary(Type),
    case erlang:get({Key, BinClass, BinType}) of
        undefined ->
            0;
        Count ->
%%            io:format("~s ~p ~p ~p Count ~p ~n", [?FILE, ?LINE, BinClass,  BinType, Count]),
            Count
    end.

stats(#{<<"results">> := Results} = Map, Key) ->
    NewResults =
        lists:foldl(
            fun
                (Product, Acc) ->
                    Acc ++ [stats(Product, Key)]
            end, [], Results),
    Map#{<<"results">> => NewResults};

stats(#{<<"objectId">> := ProductId} = Map, Key) ->
    Map#{
        <<"device_counts">> => val(<<"Device">>, ProductId, Key),
        <<"online_counts">> => val(<<"Device_Online">>, ProductId, Key),
        <<"offline_counts">> => val(<<"Device_Offline">>, ProductId, Key),
        <<"poweron_counts">> => val(<<"Device_true">>, ProductId, Key),
        <<"poweroff_counts">> => val(<<"Device_false">>, ProductId, Key)
    };

stats(Body, _Key) ->
    Body.