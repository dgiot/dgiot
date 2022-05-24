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

-export([read_csv/2, read_from_csv/1, get_products/1, create_product/3, create_device/3, post_thing/2, get_CategoryId/1, get_channelAcl/1]).
-export([get_max_addrs/1]).

%%  dgiot_product_csv:read_csv(<<"b28ee0e3c4">>, <<"modbustcp">>).
read_csv(ChannelId, FileName) ->
    read_from_csv(FileName),
    Productmap = dgiot_product_csv:get_products(FileName),
    {Devicemap, ProductIds} = dgiot_product_csv:create_product(ChannelId, FileName, Productmap),
    dgiot_product_csv:create_device(FileName, Devicemap, ProductIds),
    dgiot_product_csv:post_thing(FileName, ProductIds),
    get_max_addrs(FileName).

%% dgiot_product_csv:read_from_csv(<<"modbustcp">>)
read_from_csv(FileName) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/csv/"]),
    Name = dgiot_utils:to_list(FileName),
    AtomName = dgiot_utils:to_atom(FileName),
    dgiot_data:init(AtomName),
    NewName =
        case filename:extension(Name) of
            [] ->
                Name ++ ".csv";
            _ ->
                Name
        end,
    Path = Dir ++ NewName,
    put(count, -1),
    Fun = fun(X) ->
        Count = get(count),
        case Count > 0 of
            true ->
                dgiot_data:insert(AtomName, Count, X ++ [0]);
            _ ->
                pass
        end,
        put(count, Count + 1)
          end,
    dgiot_utils:read_from_csv(Path, Fun).

%%  ets:match(ruodian,{'_', ['$1', '_', <<"D6101">> | '_']}).
get_products(FileName) ->
    AtomName = dgiot_utils:to_atom(FileName),
    Products = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', ['$1' | '_']}))),
    lists:foldl(fun(ProductName, Acc) ->
        Devices = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', [ProductName, '_', '_', '$1' | '_']}))),
        Acc#{ProductName => Devices}
                end, #{}, Products).

create_product(ChannelId, FileName, Productmap) ->
    AtomName = dgiot_utils:to_atom(FileName),
    maps:fold(fun(ProductName, [DeviceName | _] = DeviceNames, {Acc, Acc2}) ->
        Types = ets:match(AtomName, {'_', [ProductName, '$1', '$2', DeviceName | '_']}),
        case Types of
            [[DevType, CategoryName | _] | _] ->
                Acl = dgiot_product_csv:get_channelAcl(ChannelId),
                CategoryId = dgiot_product_csv:get_CategoryId(CategoryName),
                ProductBody = #{
                    <<"name">> => ProductName,
                    <<"devType">> => DevType,
                    <<"category">> => #{<<"objectId">> => CategoryId, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
                    <<"desc">> => DevType,
                    <<"config">> => #{},
                    <<"channel">> => #{<<"type">> => 1, <<"tdchannel">> => <<"24b9b4bc50">>, <<"otherchannel">> => [ChannelId]},
                    <<"thing">> => #{},
                    <<"ACL">> => Acl,
                    <<"nodeType">> => 0,
                    <<"productSecret">> => dgiot_utils:random()
                },
                Result = dgiot_product:create_product(ProductBody),
                case Result of
                    {ok, ProductId} ->
%%                        dgiot_data:insert(AtomName, ProductId, ProductName),
                        Devicemap =
                            lists:foldl(fun(DeviceName1, Acc1) ->
                                Acc1#{DeviceName1 => ProductId}
                                        end, Acc, DeviceNames),
                        {Devicemap, Acc2#{ProductId => {DeviceName, ProductName}}};
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
        {_, ProductName} = maps:get(ProductId, ProductIds, {'_', '_'}),
        Devaddrs = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', [ProductName, '_', '_', DeviceName, '$1' | '_']}))),
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
                <<"ACL">> => #{<<"role:admin">> => #{<<"read">> => true, <<"write">> => true}}})
                    end, #{}, Devaddrs)
              end, #{}, Devicemap).

post_thing(FileName, Productids) ->
    AtomName = dgiot_utils:to_atom(FileName),
    maps:fold(fun(ProductId, {DeviceName, ProductName}, _Acc) ->
        Things = ets:match(AtomName, {'_', [ProductName, '_', '_', DeviceName, '_', '$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11' | '_']}),
        Properties = get_properties(Things, AtomName, ProductId, ProductName),
        dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"thing">> => #{<<"properties">> => Properties}})
              end, [], Productids).

get_CategoryId(CategoryName) ->
    case dgiot_parse:query_object(<<"Category">>, #{<<"limit">> => 1, <<"where">> => #{<<"name">> => CategoryName}}) of
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
            case dgiot_parse:create_object(<<"Category">>, Body) of
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

get_properties(Things, AtomName, ProductId, ProductName) ->
    lists:foldl(fun([Devicetype, Identifier, Name, _Address, _Bytes, AccessMode, Min_Max, Unit, Type, _Operatetype, _Originaltype | _], Propertie) ->
        {Min, Max} = get_min_max(Min_Max),
        Propertie ++ [#{
            <<"name">> => Name,
            <<"index">> => 0,
            <<"isshow">> => true,
            <<"dataForm">> => #{
                <<"rate">> => 1,
                <<"order">> => 0,
                <<"round">> => <<"all">>,
                <<"offset">> => 0,
                <<"control">> => <<"%d">>,
                <<"iscount">> => <<"0">>,
                <<"protocol">> => <<"DLINK">>,
                <<"strategy">> => <<"主动上报"/utf8>>,
                <<"collection">> => <<"%s">>,
                <<"countround">> => <<"all">>,
                <<"countstrategy">> => 20,
                <<"countcollection">> => <<"%s">>
            },
            <<"dataType">> => #{
                <<"das">> => [],
                <<"type">> => to_lower(Type),
                <<"specs">> => #{
                    <<"min">> => Min,
                    <<"max">> => Max,
                    <<"step">> => 0,
                    <<"unit">> => get_unit(Unit),
                    <<"precision">> => 3
                }
            },
            <<"required">> => true,
            <<"accessMode">> => get_accessmode(AccessMode),
            <<"dataSource">> => get_dataSource(AtomName, ProductId, ProductName, Name),
            <<"devicetype">> => Devicetype,
            <<"identifier">> => to_lower(Identifier),
            <<"moduleType">> => <<"properties">>,
            <<"isaccumulate">> => false
        }]
                end, [], Things).

get_dataSource(AtomName, ProductId, ProductName, ThingName) ->
    Things = ets:match(AtomName, {'_', [ProductName, '_', '_', '_', '_', '_', '_', ThingName, '$1', '$2' | '_']}),
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
    Address1 = [dgiot_utils:to_int(X) || X <- Address],
%%    dgiot_data:insert(AtomName, adds, Address1),
    {lists:min(Address1), lists:max(Address1)}.

to_lower(Value) ->
    list_to_binary(string:to_lower(binary_to_list(Value))).

get_accessmode(<<229, 143, 170, 232, 175, 187>>) ->
    <<"r">>;

get_accessmode(_AccessMode) ->
    <<"rw">>.

get_min_max(Min_Max) ->
    case binary:split(Min_Max, <<$->>, [global, trim]) of
        [Min, Max] ->
            {dgiot_utils:to_int(Min), dgiot_utils:to_int(Max)};
        _ ->
            {0, 999}
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
