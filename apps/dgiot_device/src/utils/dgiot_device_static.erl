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
-export([get_counter/1, get_pie/1, get_realdata/1]).

get_counter({Token, <<"product_counter">>}) ->
    Query = #{<<"count">> => <<"objectId">>,
        <<"keys">> => [<<"objectId">>], <<"where">> => #{}, <<"limit">> => 1},
    case dgiot_parse:query_object(<<"Product">>, Query, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            {ok, #{<<"lable">> => <<"产品数量"/utf8>>, <<"value">> => Count}};
        _ ->
            pass
    end;

get_counter({Token, <<"device_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device">>, Key),
    {ok, #{<<"lable">> => <<"设备数量"/utf8>>, <<"value">> => Count}};

get_counter({Token, <<"device_online_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device_Online">>, Key),
    {ok, #{<<"lable">> => <<"在线设备"/utf8>>, <<"value">> => Count}};

get_counter({Token, <<"device_offline_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device_Offline">>, Key),
    {ok, #{<<"lable">> => <<"离线设备"/utf8>>, <<"value">> => Count}};

get_counter({Token, <<"device_poweron_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device_true">>, Key),
    {ok, #{<<"lable">> => <<"开机设备"/utf8>>, <<"value">> => Count}};

get_counter({Token, <<"device_poweroff_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device_false">>, Key),
    {ok, #{<<"lable">> => <<"关机设备"/utf8>>, <<"value">> => Count}};

get_counter({Token, <<"device_peace_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device_peace">>, Key),
    {ok, #{<<"lable">> => <<"平时设备"/utf8>>, <<"value">> => Count}};

get_counter({Token, <<"device_war_counter">>}) ->
    Key = get_count(Token),
    Count = val(<<"Device_war">>, Key),
    {ok, #{<<"lable">> => <<"战时设备"/utf8>>, <<"value">> => Count}};

get_counter({_Token, _}) ->
    pass.

get_pie({Token, <<"device_online_offline">>}) ->
    Key = get_count(Token),
    Device_Online = val(<<"Device_Online">>, Key),
    Device_Offline = val(<<"Device_Offline">>, Key),
    Payload = #{
        <<"columns">> => [<<"名称"/utf8>>, <<"数量"/utf8>>],
        <<"rows">> => [
            #{<<"名称"/utf8>> => <<"在线数"/utf8>>, <<"数量"/utf8>> => Device_Online},
            #{<<"名称"/utf8>> => <<"离线数"/utf8>>, <<"数量"/utf8>> => Device_Offline}
        ]
    },
    {ok, Payload};


get_pie({Token, <<"device_poweron_poweroff">>}) ->
    Key = get_count(Token),
    PowerOnCount = val(<<"Device_true">>, Key),
    PowerOffCount = val(<<"Device_false">>, Key),
    Payload = #{
        <<"columns">> => [<<"名称"/utf8>>, <<"数量"/utf8>>],
        <<"rows">> => [
            #{<<"名称"/utf8>> => <<"开机数"/utf8>>, <<"数量"/utf8>> => PowerOnCount},
            #{<<"名称"/utf8>> => <<"关机数"/utf8>>, <<"数量"/utf8>> => PowerOffCount}
        ]
    },
    {ok, Payload};
get_pie({_Token, _}) ->
    pass.


get_realdata({Token, NodeId}) ->
    Len = size(NodeId) - 16,
    case NodeId of
        <<DeviceId:10/binary, "_", Identifier:Len/binary, "_text">> ->
            case dgiot_product_tdengine:get_channel(Token) of
                {error, Error} ->
                    {error, Error};
                {ok, Channel} ->
                    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                        {ok, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                            case dgiot_device_card:get_device_card(Channel, ProductId, DeviceId, #{<<"keys">> => [Identifier]}) of
                                {ok, #{<<"data">> := [#{<<"identifier">> := Identifier} = Data | _]}} ->
                                    {ok, #{<<"lable">> => NodeId, <<"data">> => Data}};
                                _ ->
                                    pass
                            end;
                        _ ->
                            pass
                    end
            end;
        _ ->
            pass
    end.

get_count(Token) ->
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
                case AtomQueryAcls -- Acls of
                    AtomQueryAcls -> pass;
                    _ -> add(V, Key)
                end;
            (_) -> pass
        end,
    dgiot_mnesia:search(Fun, #{}).

%%['Device', Acl, Status, Now, IsEnable, dgiot_utils:to_atom(ProductId), Devaddr, DeviceSecret, Node]
add(['Device', _Acls, Status, State, _Time, IsEnable, ProductId | _] = _V, Key) ->
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
    end,
    case State of
        1 ->
            inc(<<"Device_war">>, ProductId, Key),
            inc(<<"Device_war">>, Key);
        _ ->
            inc(<<"Device_peace">>, ProductId, Key),
            inc(<<"Device_peace">>, Key)
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
