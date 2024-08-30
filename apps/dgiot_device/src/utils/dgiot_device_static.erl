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
-export([get_counter/1, get_pie/1, get_realdata/1, query_realdata/2, get_labels/2]).

get_counter({Token, <<"product_counter">>}) ->
    Query = #{<<"count">> => <<"objectId">>,
        <<"keys">> => [<<"objectId">>], <<"where">> => #{}, <<"limit">> => 1},
    case dgiot_parsex:query_object(<<"Product">>, Query, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
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

query_realdata(Channel, Sql) ->
    dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end).

get_labels(ProductId, Results) ->
    Props = dgiot_product:get_props(ProductId),
    lists:foldl(fun
                    (#{<<"devaddr">> := Devaddr} = X, Acc) when Devaddr =/= null ->
                        DeviceId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_binary(Devaddr)),
                        maps:fold(
                            fun
                                (Identifier, Value, Lcc) ->
                                    case maps:find(Identifier, Props) of
                                        {ok, #{<<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := Typea} = DataType}} ->
                                            Specs = maps:get(<<"specs">>, DataType, #{}),
                                            Unit = maps:get(<<"unit">>, Specs, <<"">>),
                                            {Color, _, _} = dgiot_device:get_color(DeviceId, <<"quantity">>),
                                            NewV = dgiot_product_tdengine:check_field(Typea, Value, #{<<"datatype">> => DataType, <<"specs">> => Specs, <<"deviceid">> => DeviceId}),
                                            Lcc ++ [#{<<"label">> => <<DeviceId/binary, "_", Identifier/binary, "_text">>, <<"number">> => NewV, <<"unit">> => Unit, <<"color">> => Color, <<"screen_deviceid">> => DeviceId, <<"funcId">> => Identifier}];
                                        _ ->
                                            Lcc
                                    end
                            end, Acc, X);
                    (_, Acc) ->
                        Acc
                end, [], Results).

%% INSERT INTO _24b9b4bc50._77c57f6860 using _24b9b4bc50._19c923fd82 TAGS("322000_AL_usb7") VALUES(now,15,30,29,26.9,54.7);
%% select last(devaddr) as devaddr, last(process_duration) as process_duration FROM  _24b9b4bc50._5392ccb3d7 group by devaddr;
%% select last(devaddr) as devaddr, sum(process_duration) as process_duration FROM  _24b9b4bc50._5392ccb3d7 where quantity = 2 group by devaddr;
get_realdata({Token, Realdatas}) when is_map(Realdatas) ->
    Payload =
        maps:fold(
            fun
                (ProductId, #{<<"keys">> := Keys, <<"deviceids">> := _Deviceids}, Acc) ->
                    case dgiot_product_tdengine:get_channel(Token) of
                        {error, Error} ->
                            {error, Error};
                        {ok, Channel} ->
                            {_, Newkeys} = dgiot_product_tdengine:get_keys(ProductId, <<"_", ProductId/binary>>, <<"last">>, Keys),
                            DB = dgiot_tdengine:get_database(Channel, ProductId),
                            Sql1 = <<"select last(devaddr) as devaddr,", Newkeys/binary, " FROM ", DB/binary, "_", ProductId/binary, " group by devaddr;">>,
%%                            io:format("Channel = ~p.~n Sql = ~p.~n", [Channel, Sql1]),
                            case dgiot_device_static:query_realdata(Channel, Sql1) of
                                {ok, #{<<"code">> := 0, <<"results">> := Results1}} ->
                                    Acc ++ dgiot_device_static:get_labels(ProductId, Results1);
                                _ ->
                                    Acc
                            end
                    end;
                (_, _, Acc) ->
                    Acc
            end, [], Realdatas),
    Base64 = base64:encode(dgiot_json:encode(Payload)),
    Pubtopic = <<"$dg/user/topo/", Token/binary, "/allrealdata/report">>,
%%    io:format("~s ~p Pubtopic ~p Base64 ~p ~n", [?FILE, ?LINE, Pubtopic, Base64]),
    dgiot_mqtt:publish(self(), Pubtopic, Base64);

get_realdata(_) ->
    pass.

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

get_count([], roleid, _Key) ->
    pass;
get_count([RoleId | Roles1] = RoleIds, roleid, Key) when is_list(RoleIds) ->
    get_count(RoleId, roleid, Key),
    get_count(dgiot_utils:unique_1(Roles1), roleid, Key);
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
