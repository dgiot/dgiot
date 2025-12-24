%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_plc).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

-export([read_csv/2, create_product/3, post_thing/2, get_CategoryId/1, get_channelAcl/1]).

%%  dgiot_plc:read_csv(<<"27e85ddfa6">>, <<"/dgiot_file/product/csv/xiechuanji1.csv">>),
%%  dgiot_plc:read_csv(<<"08d5b38bd5">>, <<"/dgiot_file/product/csv/xiechuanji2.csv">>),
%%  dgiot_plc:read_csv(<<"41c8c541ed">>, <<"/dgiot_file/product/csv/xiechuanji3.csv">>),
%%  dgiot_plc:read_csv(<<"d90f785bec">>, <<"/dgiot_file/product/csv/xiechuanji4.csv">>),
%%  dgiot_plc:read_csv(<<"51a1266860">>, <<"/dgiot_file/product/csv/xiechuanji5.csv">>),
%%
%%  dgiot_plc:read_csv(<<"891b52b091">>, <<"/dgiot_file/product/csv/zhuangchuanji1.csv">>),
%%  dgiot_plc:read_csv(<<"0559466325">>, <<"/dgiot_file/product/csv/zhuangchuanji2.csv">>),
%%  dgiot_plc:read_csv(<<"6ef9099747">>, <<"/dgiot_file/product/csv/zhuangchuanji3.csv">>).
%%
%%  dgiot_plc:read_csv(<<"5cfbbcd875">>, <<"/dgiot_file/product/csv/doulunji1.csv">>),
%%  dgiot_plc:read_csv(<<"b0d7fa4d8b">>, <<"/dgiot_file/product/csv/doulunji2.csv">>),
%%  dgiot_plc:read_csv(<<"1767edd6b9">>, <<"/dgiot_file/product/csv/doulunji3.csv">>).
%%  dgiot_plc:read_csv(<<"5ca25a524e">>, <<"/dgiot_file/product/csv/dapengpenqiang1.csv">>).
read_csv(ChannelId, FilePath) ->
    FileName = dgiot_csv:save_csv_ets(?MODULE, FilePath),
    TdChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
    ProductIds = dgiot_plc:create_product(ChannelId, FileName, TdChannelId),
    timer:sleep(1000),
    dgiot_plc:post_thing(FileName, ProductIds),
    timer:sleep(1000),
    dgiot_bridge:control_channel(#{<<"id">> => TdChannelId, <<"action">> => <<"disable">>}, <<>>),
    dgiot_bridge:control_channel(#{<<"id">> => TdChannelId, <<"action">> => <<"enable">>}, <<>>).

%%  ets:match(ruodian,{'_', ['$1', '_', <<"D6101">> | '_']}).
create_product(ChannelId, FileName, TdChannelId) ->
    AtomName = dgiot_utils:to_atom(FileName),
    Products = dgiot_utils:unique_1(lists:flatten(ets:match(AtomName, {'_', ['$1' | '_']}))),
    lists:foldl(fun(ProductName, Acc) ->
        Types = ets:match(AtomName, {'_', [ProductName, '$1', '$2' | '_']}),
        case Types of
            [[DevType, CategoryName | _] | _] ->
                CategoryId = dgiot_plc:get_CategoryId(CategoryName),
                ProductId = dgiot_parse_id:get_productid(CategoryId, DevType, ProductName),
                Result =
                    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
                        {ok, #{<<"objectId">> := ProductId}} ->
                            {ok, ProductId};
                        _ ->
                            Acl = dgiot_plc:get_channelAcl(ChannelId),
                            ProductBody = #{
                                <<"name">> => ProductName,
                                <<"devType">> => DevType,
                                <<"category">> => #{<<"objectId">> => CategoryId, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
                                <<"desc">> => DevType,
                                <<"config">> => #{},
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
                        Acc#{ProductId => ProductName};
                    _ ->
                        Acc
                end;
            _ ->
                Acc
        end
                end, #{}, Products).

post_thing(FileName, ProductIds) ->
    AtomName = dgiot_utils:to_atom(FileName),
    maps:fold(fun(ProductId, ProductName, _Acc) ->
        Things = ets:match(AtomName, {'$1', [ProductName, '_', '_', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11' | '_']}),
        NewProperties = post_properties(Things),
        case dgiot_parsex:get_object(<<"Product">>, ProductId) of
            {ok, #{<<"thing">> := Thing}} ->
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

post_properties(Things) ->
    lists:foldl(fun([Index, Devicetype, Name, Identifier, Address, Originaltype, AccessMode, Min_Max, Unit, Type, Specs | _], Acc) ->
        Acc#{
            to_lower(Identifier) => #{
                <<"name">> => Name,
                <<"index">> => Index,
                <<"isstorage">> => true,
                <<"isshow">> => true,
                <<"dataForm">> => #{
                    <<"address">> => <<"0">>,
                    <<"rate">> => 1,
                    <<"order">> => Index,
                    <<"round">> => <<"all">>,
                    <<"offset">> => 0,
                    <<"control">> => <<"%{d}">>,
                    <<"iscount">> => <<"0">>,
                    <<"protocol">> => <<"S7">>,
                    <<"strategy">> => <<"1">>,
                    <<"collection">> => <<"%{s}">>,
                    <<"countround">> => <<"all">>,
                    <<"countstrategy">> => 3,
                    <<"countcollection">> => <<"%{s}">>
                },
                <<"dataType">> => get_dataType(to_lower(Type), Min_Max, Unit, Specs),
                <<"required">> => true,
                <<"accessMode">> => get_accessmode(AccessMode),
                <<"dataSource">> => #{
                    <<"_dlinkindex">> => <<"">>,
                    <<"address">> => Address,
                    <<"originaltype">> => Originaltype
                },
                <<"devicetype">> => Devicetype,
                <<"identifier">> => to_lower(Identifier),
                <<"moduleType">> => <<"properties">>,
                <<"isaccumulate">> => false
            }}
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

get_unit(<<"null">>) ->
    <<"">>;

get_unit(Unit) ->
    Unit.
