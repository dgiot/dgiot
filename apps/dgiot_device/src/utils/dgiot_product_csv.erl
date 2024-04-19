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

-module(dgiot_product_csv).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

-export([read_csv/4, get_products/1, create_product/5, create_device/3, post_thing/3, get_CategoryId/1, get_channelAcl/1]).
-export([get_max_addrs/1]).

%%  dgiot_product_csv:read_csv(<<"8cff09f988">>, <<"modbustcp">>).
%%  dgiot_utils:save_csv_ets(<<"/dgiot_file/product/csv/modbustcp.csv">>)
read_csv(ChannelId, FilePath, Is_refresh, Is_shard) ->
    FileName = dgiot_csv:save_csv_ets(?MODULE, FilePath),
    spawn(fun() ->
        Productmap = dgiot_product_csv:get_products(FileName),
        TdChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
        {Devicemap, ProductIds} = dgiot_product_csv:create_product(ChannelId, FileName, Productmap, TdChannelId, Is_shard),
        dgiot_product_csv:create_device(FileName, Devicemap, ProductIds),
        timer:sleep(1000),
        dgiot_product_csv:post_thing(FileName, ProductIds, Is_refresh)
%%        timer:sleep(1000),
%%        dgiot_bridge:control_channel(TdChannelId, <<"update">>, <<>>)
          end),
    get_max_addrs(FileName).

%%  ets:match(ruodian,{'_', ['$1', '_', <<"D6101">> | '_']}).
get_products(FileName) ->
    AtomName = dgiot_utils:to_atom(FileName),
    Products = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', ['$1' | '_']}))),
    lists:foldl(fun(ProductName, Acc) ->
        Devices = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', [ProductName, '_', '_', '$1' | '_']}))),
        Acc#{ProductName => Devices}
                end, #{}, Products).

create_product(ChannelId, FileName, Productmap, TdChannelId, Is_shard) ->
    AtomName = dgiot_utils:to_atom(FileName),
    maps:fold(fun(ProductName, DeviceNames, {Acc, Acc2}) ->
        Types = ets:match(AtomName, {'_', [ProductName, '$1', '$2' | '_']}),
        case Types of
            [[DevType, CategoryName | _] | _] ->
                CategoryId = dgiot_product_csv:get_CategoryId(CategoryName),
                ProductId = dgiot_parse_id:get_productid(CategoryId, DevType, ProductName),
                Result =
                    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
                        {ok, #{<<"objectId">> := ProductId}} ->
                            {ok, ProductId};
                        _ ->
                            Acl = dgiot_product_csv:get_channelAcl(ChannelId),
                            ProductBody = #{
                                <<"name">> => ProductName,
                                <<"devType">> => DevType,
                                <<"category">> => #{<<"objectId">> => CategoryId, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
                                <<"desc">> => DevType,
                                <<"config">> => #{<<"interval">> => -1},
                                <<"channel">> => #{<<"type">> => 1, <<"tdchannel">> => TdChannelId, <<"taskchannel">> => <<"fa3fad91f8">>, <<"otherchannel">> => [ChannelId]},
                                <<"thing">> => #{},
                                <<"ACL">> => Acl,
                                <<"nodeType">> => 0,
                                <<"productSecret">> => dgiot_utils:random()
                            },
                            dgiot_product:create_product(ProductBody)
                    end,
                case Result of
                    {ok, ProductId} ->
%%                        dgiot_data:insert(AtomName, ProductId, ProductName),
                        dgiot_data:insert({shard_storage, ProductId}, Is_shard),
                        Devicemap =
                            lists:foldl(fun(DeviceName1, Acc1) ->
                                Acc1#{DeviceName1 => ProductId}
                                        end, Acc, DeviceNames),
                        {Devicemap, Acc2#{ProductId => ProductName}};
                    _ ->
                        {Acc, Acc2}
                end;
            _ ->
                {Acc, Acc2}
        end
              end, {#{}, #{}}, Productmap).

create_device(FileName, Devicemap, ProductIds) ->
    AtomName = dgiot_utils:to_atom(FileName),
    maps:fold(fun(DeviceName, ProductId, _Acc) ->
        ProductName = maps:get(ProductId, ProductIds, '_'),
        Devaddrs = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', [ProductName, '_', '_', DeviceName, '$1' | '_']}))),
        NewAcl =
            case dgiot_parsex:get_object(<<"Product">>, ProductId) of
                {ok, #{<<"ACL">> := Acl}} ->
                    Acl;
                _ ->
                    #{<<"role:开发者"/utf8>> => #{<<"read">> => true, <<"write">> => true}}
            end,
        lists:foldl(fun(Devaddr, _Acc1) ->
            dgiot_device:create_device(#{
                <<"status">> => <<"ONLINE">>,
                <<"name">> => DeviceName,
                <<"devaddr">> => Devaddr,
                <<"brand">> => <<"MODBUSTCP"/utf8>>,
                <<"devModel">> => <<"DGIOT_GROUP">>,
                <<"product">> => ProductId,
                <<"basedata">> => #{},
                <<"address">> => DeviceName,
                <<"ACL">> => NewAcl})
                    end, #{}, Devaddrs)
              end, #{}, Devicemap).

post_thing(FileName, ProductIds, Is_refresh) ->
    AtomName = dgiot_utils:to_atom(FileName),
    maps:fold(fun(ProductId, ProductName, _) ->
        Things = ets:match(AtomName, {'_', [ProductName, '_', '_', '_', '_', '$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11', '$12' | '_']}),
        NewProperties = post_properties(Things, AtomName, ProductId, ProductName),
        case dgiot_parsex:get_object(<<"Product">>, ProductId) of
            {ok, #{<<"thing">> := Thing}} when Is_refresh ->
                OldProperties =
                    lists:foldl(fun(#{<<"identifier">> := Identifier} = X, Acc) ->
                        Acc#{Identifier => X}
                                end, #{}, maps:get(<<"properties">>, Thing, [])),
                Properties =
                    maps:fold(fun(_, Prop, Acc) ->
                        Acc ++ [Prop]
                              end, [], dgiot_map:merge(OldProperties, NewProperties)),
                dgiot_parsex:update_object(<<"Product">>, ProductId, #{<<"thing">> => Thing#{<<"properties">> => Properties}});
            _ ->
                pass
        end
              end, [], ProductIds).

get_CategoryId(CategoryName) ->
    case dgiot_parsex:query_object(<<"Category">>, #{<<"limit">> => 1, <<"where">> => #{<<"name">> => CategoryName}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := CategoryId} | _]}} ->
            CategoryId;
        _ ->
            Body = #{
                <<"name">> => CategoryName,
                <<"order">> => 0,
                <<"ACL">> => #{
                    <<"*">> => #{<<"read">> => true},
                    <<"role:admin">> => #{
                        <<"read">> => true, <<"write">> => true
                    }
                },
                <<"parent">> => #{<<"objectId">> => <<"a60a85475a">>, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
                <<"level">> => 1
            },
            case dgiot_parsex:create_object(<<"Category">>, Body) of
                {ok, #{<<"objectId">> := ObjectId}} ->
                    ObjectId;
                _ ->
                    <<"3b77f833e5">>
            end
    end.

get_channelAcl(ChannelId) ->
    case dgiot_bridge:get_acl(ChannelId) of
        {ok, Acl} ->
            Acl;
        _ ->
            #{
                <<"role:admin">> => #{
                    <<"read">> => true, <<"write">> => true
                }
            }
    end.

post_properties(Things, AtomName, ProductId, ProductName) ->
    lists:foldl(fun([Devicetype, Identifier, Name, _Address, _Bytes, AccessMode, Min_Max, Unit, Type, _Operatetype, _Originaltype, Specs | _], Acc) ->
        Acc#{
            to_lower(Identifier) =>
            #{
                <<"name">> => Name,
                <<"index">> => 0,
                <<"isstorage">> => false,
                <<"isshow">> => true,
                <<"dataForm">> => #{
                    <<"address">> => <<"0">>,
                    <<"rate">> => 1,
                    <<"order">> => 0,
                    <<"round">> => <<"all">>,
                    <<"offset">> => 0,
                    <<"control">> => <<"%{d}">>,
                    <<"iscount">> => <<"0">>,
                    <<"protocol">> => <<"DLINK">>,
                    <<"strategy">> => <<"主动上报"/utf8>>,
                    <<"collection">> => <<"%{s}">>,
                    <<"countround">> => <<"all">>,
                    <<"countstrategy">> => 3,
                    <<"countcollection">> => <<"%{s}">>
                },
                <<"dataType">> => get_dataType(to_lower(Type), Min_Max, Unit, Specs),
                <<"required">> => true,
                <<"accessMode">> => get_accessmode(AccessMode),
                <<"dataSource">> => get_dataSource(AtomName, ProductId, ProductName, Identifier),
                <<"devicetype">> => Devicetype,
                <<"identifier">> => to_lower(Identifier),
                <<"moduleType">> => <<"properties">>,
                <<"isaccumulate">> => false
            }
        }
                end, #{}, Things).

get_dataType(<<"float">>, Min_Max, Unit, _) ->
    {Min, Max} = get_min_max(Min_Max),
    #{
        <<"das">> => [],
        <<"type">> => <<"float">>,
        <<"specs">> => #{
            <<"min">> => Min,
            <<"max">> => Max,
            <<"step">> => 0,
            <<"unit">> => get_unit(Unit),
            <<"precision">> => 3
        }
    };

get_dataType(<<"enum">>, _, _, Specs) ->
    Newspecs = get_specs(Specs),
    #{
        <<"das">> => [],
        <<"type">> => <<"enum">>,
        <<"specs">> => Newspecs
    };

get_dataType(Type, Min_Max, Unit, _) ->
    {Min, Max} = get_min_max(Min_Max),
    #{
        <<"das">> => [],
        <<"type">> => Type,
        <<"specs">> => #{
            <<"min">> => Min,
            <<"max">> => Max,
            <<"step">> => 0,
            <<"unit">> => get_unit(Unit),
            <<"precision">> => 3
        }
    }.

get_specs(Specs) ->
    case binary:split(Specs, <<$;>>, [global, trim]) of
        List when length(List) > 0 ->
            lists:foldl(fun(Map, Acc) ->
                case binary:split(Map, <<$:>>, [global, trim]) of
                    [Key, Value] ->
                        Acc#{Key => Value};
                    _ ->
                        Acc
                end
                        end, #{}, List);
        _ ->
            #{}
    end.

get_dataSource(AtomName, ProductId, ProductName, Identifier) ->
    Things = ets:match(AtomName, {'_', [ProductName, '_', '_', '_', '_', '_', Identifier, '_', '$1', '$2' | '_']}),
    Dis =
        lists:foldl(fun([Address, Bytes | _], Acc) ->
            dgiot_data:insert(AtomName, {addr, Address}, ProductId),
            Acc ++ [#{<<"key">> => Address, <<"data">> => Bytes}]
                    end, [], Things),
    #{<<"dis">> => Dis,
        <<"_dlinkindex">> => "1"}.

get_max_addrs(FileName) ->
    AtomName = dgiot_utils:to_atom(FileName),
    Address = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', ['_', '_', '_', '_', '_', '_', '_', '_', '$1' | '_']}))),
    Address1 = [dgiot_utils:to_int(dgiot_utils:to_float(X)) || X <- Address],
%%    dgiot_data:insert(AtomName, adds, Address1),
    {FileName, lists:min(Address1), lists:max(Address1)}.

to_lower(Value) ->
    Str1 = re:replace(Value, <<"\\.">>, <<"_">>, [global, {return, list}]),
    list_to_binary(string:to_lower(Str1)).

get_accessmode(<<229, 143, 170, 232, 175, 187>>) ->
    <<"r">>;

get_accessmode(_AccessMode) ->
    <<"rw">>.

get_min_max(Min_Max) ->
    case binary:split(Min_Max, <<$->>, [global, trim]) of
        [<<>>, Min, Max] ->
            {-dgiot_utils:to_int(Min), dgiot_utils:to_int(Max)};
        [Min, Max] ->
            {dgiot_utils:to_int(Min), dgiot_utils:to_int(Max)};
        _ ->
            {-65535, 65535}
    end.

%%get_operatetype(Operatetype) ->
%%    case Operatetype of
%%        <<"01">> -> <<"readCoils">>;
%%        <<"02">> -> <<"readInputs">>;
%%        <<"03">> -> <<"readHregs">>;
%%        <<"04">> -> <<"readIregs">>;
%%        <<"05">> -> <<"writeCoil">>;
%%        <<"06">> -> <<"writeHreg">>;
%%        <<"0f">> -> <<"writeCoils">>;
%%        <<"10">> -> <<"writeHregs">>;
%%        _ -> <<"readHregs">>
%%    end.

get_unit(<<"null">>) ->
    <<"">>;

get_unit(Unit) ->
    Unit.
