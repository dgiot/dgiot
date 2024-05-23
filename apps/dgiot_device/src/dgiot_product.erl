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

-module(dgiot_product).
-author("jonliu").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_data("ets").
-export([init_ets/0, load_all_cache/1, local/1, save/1, put/1, get/1, delete/1, save_prod/2, lookup_prod/1]).
-export([parse_frame/3, to_frame/2]).
-export([create_product/1, create_product/2, add_product_relation/2, delete_product_relation/1]).
-export([get_prop/1, get_props/1, get_props/2, get_unit/1, update_properties/2, update_properties/0]).
-export([update_topics/0, update_product_filed/1]).
-export([save_devicetype/1, get_devicetype/1, get_device_thing/2, get_productSecret/1]).
-export([save_/1, get_keys/1, get_control/1, save_control/1, get_interval/1, save_device_thingtype/1, get_product_identifier/2]).

init_ets() ->
    dgiot_data:init(?DGIOT_PRODUCT, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
    dgiot_data:init(?DGIOT_PRODUCT_IDENTIFIE, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
    dgiot_data:init(?DGIOT_CHANNEL_SESSION, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
    dgiot_data:init(?DEVICE_DEVICE_COLOR, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
    dgiot_data:init(?DEVICE_PROFILE, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]).

load_all_cache({Skip}) ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"limit">> => 1000, <<"skip">> => Skip}) of
        {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
            load_end;
        {ok, #{<<"results">> := Products}} ->
            lists:map(fun(Product) ->
                dgiot_product:save(Product)
                      end, Products),
            {next, Skip + 1000};
        _ ->
            {next, Skip}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_prod(ProductId, #{<<"thing">> := _thing} = Product) ->
    dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product),
    {ok, Product};

save_prod(_ProductId, _Product) ->
    pass.

local(ProductId) ->
    case dgiot_data:lookup(?DGIOT_PRODUCT, ProductId) of
        {ok, Product} ->
            {ok, Product};
        {error, not_find} ->
            {error, not_find}
    end.

lookup_prod(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, ProductId) of
        not_find ->
            not_find;
        Value ->
            {ok, Value}
    end.

save(Product) ->
    Product1 = format_product(Product),
    #{<<"productId">> := ProductId} = Product1,
    dgiot_data:delete(?DGIOT_PRODUCT, ProductId),
    dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product1),
    save_(ProductId),
    save_productSecret(ProductId),
    dgiot_product_channel:save_channel(ProductId),
    dgiot_product_channel:save_tdchannel(ProductId),
    dgiot_product_channel:save_taskchannel(ProductId),
%%    dgiot_product_enum:save_product_enum(ProductId),
    {ok, Product1}.

put(Product) ->
    ProductId = maps:get(<<"objectId">>, Product),
    case lookup_prod(ProductId) of
        {ok, OldProduct} ->
            NewProduct = maps:merge(OldProduct, Product),
            save(NewProduct);
        _ ->
            pass
    end.

delete(ProductId) ->
    dgiot_data:delete(?DGIOT_PRODUCT, ProductId).

get(ProductId) ->
    Keys = [<<"ACL">>, <<"name">>, <<"devType">>, <<"status">>, <<"content">>, <<"profile">>, <<"nodeType">>, <<"dynamicReg">>, <<"topics">>, <<"productSecret">>],
    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
        {ok, Product} ->
            {ok, maps:with(Keys, Product)};
        {error, Reason} ->
            {error, Reason}
    end.

save_productSecret(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"productSecret">> := ProductSecret}} ->
            dgiot_data:insert({productSecret, ProductId}, ProductSecret);
        _ ->
            pass
    end.

get_productSecret(ProductId) ->
    dgiot_data:get({productSecret, ProductId}).

%% 保存配置下发控制字段
save_control(ProductId) ->
    Keys =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(
                    fun
                        (#{<<"identifier">> := Identifier, <<"profile">> := Profile}, Acc) ->
                            Acc#{Identifier => Profile};
                        (_, Acc) ->
                            Acc
                    end, #{}, Props);

            _Error ->
                []
        end,
    dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, profile_control}, Keys).

get_control(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, profile_control}) of
        not_find ->
            #{};
        Keys ->
            Keys
    end.


%% 设备类型
save_devicetype(ProductId) ->
    DeviceTypes =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(
                    fun
                        (#{<<"devicetype">> := DeviceType}, Acc) ->
                            Acc ++ [DeviceType];
                        (_, Acc) ->
                            Acc
                    end, [], Props);
            _Error ->
                []
        end,
    dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, devicetype}, dgiot_utils:unique_2(DeviceTypes)).

get_devicetype(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, devicetype}) of
        not_find ->
            [];
        DeviceTypes ->
            DeviceTypes
    end.


%% 设备类型
save_device_thingtype(ProductId, DeviceType, NewMap) ->
    case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}) of
        not_find ->
            dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}, NewMap);
        Map ->
            dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}, dgiot_map:merge(Map, NewMap))
    end.

save_device_thingtype(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:map(
                fun
                    (#{<<"devicetype">> := DeviceType, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := Type}}) ->
                        case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}) of
                            not_find ->
                                dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}, #{Identifier => Type});
                            Map ->
                                dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}, Map#{Identifier => Type})
                        end;
                    (_) ->
                        pass
                end, Props);

        _Error ->
            []
    end.

%% 物模型标识符
delete_product_identifier(ProductId) ->
    Fun =
        fun
            ({Key, _}) ->
                case Key of
                    {ProductId, _, identifie} ->
                        dgiot_data:delete(?DGIOT_PRODUCT_IDENTIFIE, Key);
                    _ ->
                        pass
                end;
            (_) ->
                pass
        end,
    dgiot_data:loop(?DGIOT_PRODUCT_IDENTIFIE, Fun).

get_product_identifier(ProductId, Identifie) ->
    case dgiot_data:get(?DGIOT_PRODUCT_IDENTIFIE, {ProductId, Identifie, identifie}) of
        not_find ->
            not_find;
        Prop ->
            Prop
    end.

get_device_thing(ProductId, DeviceType) ->
    case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, device_thing, DeviceType}) of
        not_find ->
            not_find;
        Thingtypes ->
            Thingtypes
    end.

update_properties(ProductId, Product) ->
%%    io:format("~s ~p ProductId = ~p.~n", [?FILE, ?LINE, ProductId]),
    PropertiesTpl = dgiot_dlink:get_json(<<"properties_tpl">>),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props} = Thing}} ->
            NewProperties = lists:foldl(
                fun
                    (Property, Acc) ->
                        X = dgiot_map:merge(PropertiesTpl, Property),
                        Acc ++ [X]
                end, [], Props),
            NewThing = Thing#{
                <<"properties">> => NewProperties
            },
            dgiot_parsex:update_object(<<"Product">>, ProductId, #{<<"thing">> => NewThing}),
            dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product#{<<"thing">> => NewThing});
        _Error ->
            []
    end.

update_properties() ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"skip">> => 0}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, _Acc) ->
                case X of
                    #{<<"objectId">> := ProductId} ->
                        update_properties(ProductId, X);
                    _ ->
                        pass
                end
                        end, #{}, Results);
        _ ->
            pass
    end.

%% 更新topics
update_topics() ->
    {ok, #{<<"fields">> := Fields}} = dgiot_parsex:get_schemas(<<"Product">>),
    #{<<"type">> := Type} = maps:get(<<"topics">>, Fields),
    case Type of
        <<"Object">> ->
%% 判断目前topics 是那种类型，如果是object类型，就不更新
%% 删除原有topics字段
            case dgiot_parsex:del_filed_schemas(<<"topics">>, <<"Product">>) of
                {ok, _} ->
                    %%  新增topics字段
                    dgiot_parsex:create_schemas(<<"Product">>, #{<<"topics">> => []});
                Err ->
                    io:format("~s ~p ~p ~n", [?FILE, ?LINE, Err])
            end;
        _ -> pass
    end.

%% 产品字段新增
update_product_filed(_Filed) ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"skip">> => 0}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, _Acc) ->
                case X of
                    #{<<"objectId">> := ProductId} ->
                        update_properties(ProductId, X);
                    _ ->
                        pass
                end
                        end, #{}, Results);
        _ ->
            pass
    end.

save_(ProductId) ->
    {Keys, Control, DeviceTypes} =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props} = Thing}} ->
                delete_product_identifier(ProductId),
                Tags = maps:get(<<"tags">>, Thing, []),
                lists:foldl(
                    fun
                        (#{<<"devicetype">> := DeviceType, <<"identifier">> := Identifier, <<"isstorage">> := true, <<"profile">> := Profile, <<"dataType">> := #{<<"type">> := Type}} = Prop, {Acc, Ccc, Dcc}) ->
                            dgiot_data:insert(?DGIOT_PRODUCT_IDENTIFIE, {ProductId, Identifier, identifie}, Prop),
                            save_device_thingtype(ProductId, DeviceType, #{Identifier => Type}),
                            {Acc ++ [Identifier], Ccc#{Identifier => Profile}, Dcc ++ [DeviceType]};
                        (#{<<"devicetype">> := DeviceType, <<"identifier">> := Identifier, <<"profile">> := Profile, <<"dataType">> := #{<<"type">> := Type}} = Prop, {Acc, Ccc, Dcc}) ->
                            dgiot_data:insert(?DGIOT_PRODUCT_IDENTIFIE, {ProductId, Identifier, identifie}, Prop),
                            save_device_thingtype(ProductId, DeviceType, #{Identifier => Type}),
                            {Acc, Ccc#{Identifier => Profile}, Dcc ++ [DeviceType]};
                        (#{<<"devicetype">> := DeviceType, <<"identifier">> := Identifier, <<"isstorage">> := true, <<"dataType">> := #{<<"type">> := Type}} = Prop, {Acc, Ccc, Dcc}) ->
                            dgiot_data:insert(?DGIOT_PRODUCT_IDENTIFIE, {ProductId, Identifier, identifie}, Prop),
                            save_device_thingtype(ProductId, DeviceType, #{Identifier => Type}),
                            {Acc ++ [Identifier], Ccc, Dcc ++ [DeviceType]};
                        (_, {Acc, Ccc, Dcc}) ->
                            {Acc, Ccc, Dcc}
                    end, {[], #{}, []}, Props ++ Tags);
            _Error ->
                {[], #{}, []}
        end,
    dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, keys}, Keys),
    dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, profile_control}, Control),
    dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, devicetype}, dgiot_utils:unique_2(DeviceTypes)).

get_keys(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, keys}) of
        not_find ->
            [];
        Keys ->
            Keys
    end.

get_interval(ProductId) ->
    case lookup_prod(ProductId) of
        {ok, #{<<"config">> := #{<<"interval">> := Interval}}} ->
            dgiot_utils:to_int(Interval);
        _ ->
            -1
    end.

%% 解码器
parse_frame(ProductId, Bin, Opts) ->
    apply(binary_to_atom(ProductId, utf8), parse_frame, [Bin, Opts]).

to_frame(ProductId, Msg) ->
    apply(binary_to_atom(ProductId, utf8), to_frame, [Msg]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
format_product(#{<<"objectId">> := ProductId} = Product) ->
    Thing = maps:get(<<"thing">>, Product, #{}),
    Props = maps:get(<<"properties">>, Thing, []),
    Keys = [<<"ACL">>, <<"name">>, <<"config">>, <<"devType">>, <<"status">>, <<"channel">>, <<"content">>, <<"profile">>, <<"nodeType">>, <<"dynamicReg">>, <<"topics">>, <<"productSecret">>],
    Map = maps:with(Keys, Product),
    Map#{
        <<"productId">> => ProductId,
        <<"topics">> => maps:get(<<"topics">>, Product, []),
        <<"thing">> => Thing#{
            <<"properties">> => Props
        }
    }.

create_product(#{<<"name">> := ProductName, <<"devType">> := DevType, <<"category">> := #{
    <<"objectId">> := CategoryId, <<"__type">> := <<"Pointer">>, <<"className">> := <<"Category">>}} = Product, SessionToken) ->
    ProductId = dgiot_parse_id:get_productid(CategoryId, DevType, ProductName),
    case dgiot_parsex:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            dgiot_parsex:update_object(<<"Product">>, ObjectId, Product,
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        _ ->
            ACL = maps:get(<<"ACL">>, Product, #{}),
            case dgiot_auth:get_session(SessionToken) of
                #{<<"roles">> := Roles} = _User ->
                    [#{<<"name">> := Role} | _] = maps:values(Roles),
                    CreateProductArgs = Product#{
                        <<"ACL">> => ACL#{
                            <<"role:", Role/binary>> => #{
                                <<"read">> => true,
                                <<"write">> => true
                            }
                        },
                        <<"productSecret">> => dgiot_utils:random()},
                    dgiot_parsex:create_object(<<"Product">>,
                        CreateProductArgs, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                Err ->
                    {400, Err}
            end
    end.

create_product(#{<<"name">> := ProductName, <<"devType">> := DevType, <<"category">> := #{
    <<"objectId">> := CategoryId, <<"__type">> := <<"Pointer">>, <<"className">> := <<"Category">>}} = Product) ->
    ProductId = dgiot_parse_id:get_productid(CategoryId, DevType, ProductName),
    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            dgiot_parsex:update_object(<<"Product">>, ObjectId, Product),
            {ok, ObjectId};
        _ ->
            case dgiot_parsex:create_object(<<"Product">>, Product) of
                {ok, #{<<"objectId">> := ObjectId}} ->
                    {ok, ObjectId};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

add_product_relation(ChannelIds, ProductId) ->
    Map =
        #{<<"product">> =>
        #{
            <<"__op">> => <<"AddRelation">>,
            <<"objects">> => [
                #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => ProductId
                }
            ]
        }
        },
    lists:map(fun
                  (ChannelId) when size(ChannelId) > 0 ->
                      dgiot_parsex:update_object(<<"Channel">>, ChannelId, Map);
                  (_) ->
                      pass
              end, ChannelIds).

delete_product_relation(ProductId) ->
    Map =
        #{<<"product">> => #{
            <<"__op">> => <<"RemoveRelation">>,
            <<"objects">> => [
                #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => ProductId
                }
            ]}
        },
    case dgiot_parsex:query_object(<<"Channel">>, #{<<"where">> => #{<<"product">> => #{
        <<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => ProductId}}, <<"limit">> => 20}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun(#{<<"objectId">> := ChannelId}, _Acc) ->
                dgiot_parsex:update_object(<<"Channel">>, ChannelId, Map)
                        end, [], Results);
        _ ->
            []
    end.

get_prop(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"name">> := Name, <<"isshow">> := true} ->
                        Acc#{Identifier => Name};
                    _ -> Acc
                end
                        end, #{}, Props);
        _ ->
            #{}
    end.


get_unit(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"name">> := Name, <<"isshow">> := true, <<"dataType">> := #{<<"specs">> := #{<<"unit">> := Unit}}} ->
                        Acc#{Name => Unit};
                    _ -> Acc
                end
                        end, #{}, Props);
        _ ->
            #{}
    end.

get_props(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"isshow">> := true} ->
                        Acc#{Identifier => X};
                    _ -> Acc
                end
                        end, #{}, Props);
        _ ->
            #{}
    end.

get_props(ProductId, <<"*">>) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(Prop, Acc) ->
                case Prop of
                    #{<<"isshow">> := true} ->
                        Acc ++ [Prop];
                    _ ->
                        Acc
                end
                        end, [], Props);
        _ ->
            []
    end;

get_props(ProductId, Keys) when Keys == undefined; Keys == <<>> ->
    get_props(ProductId, <<"*">>);

get_props(ProductId, Keys) ->
    List =
        case is_list(Keys) of
            true -> Keys;
            false -> re:split(Keys, <<",">>)
        end,
    lists:foldl(fun(Identifier, Acc) ->
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(fun(Prop, Acc1) ->
                    case Prop of
                        #{<<"identifier">> := Identifier, <<"isshow">> := true} ->
                            Acc1 ++ [Prop];
                        _ ->
                            Acc1
                    end
                            end, Acc, Props);
            _ ->
                Acc
        end
                end, [], List).
