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
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-dgiot_data("ets").
-export([init_ets/0, load_cache/0, local/1, save/1, put/1, get/1, delete/1, save_prod/2, lookup_prod/1]).
-export([parse_frame/3, to_frame/2]).
-export([create_product/1, create_product/2, add_product_relation/2, delete_product_relation/1]).
-export([get_prop/1, get_props/1, get_props/2, get_unit/1, do_td_message/1, update_properties/2, update_properties/0]).
-export([update_topics/0, update_product_filed/1]).
-export([save_keys/1, get_keys/1, get_control/1, save_control/1, save_channel/1, get_channel/1]).
-type(result() :: any()).   %% todo 目前只做参数检查，不做结果检查

init_ets() ->
    dgiot_data:init(?DGIOT_PRODUCT),
    dgiot_data:init(?DEVICE_PROFILE).

load_cache() ->
    Success = fun(Page) ->
        lists:map(fun(Product) ->
%%            dgiot_mnesia:insert(ObjectId, ['Product', dgiot_role:get_acls(Product), ProductSecret]),
            dgiot_product:save(Product)
                  end, Page)
              end,
    Query = #{
        <<"where">> => #{}
    },
    dgiot_parse_loader:start(<<"Product">>, Query, 0, 100, 1000000, Success).

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
    dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product1),
    save_keys(ProductId),
    save_control(ProductId),
    save_channel(ProductId),
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
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, Product} ->
            {ok, maps:with(Keys, Product)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec save_channel(binary()) -> result().
save_channel(ProductId) ->
    case lookup_prod(ProductId) of
        {ok, #{<<"channel">> := #{<<"otherchannel">> := [Channel|_]}}} ->
            dgiot_data:insert({channel_product, binary_to_atom(Channel)}, ProductId);
        {ok, #{<<"channel">> := #{<<"otherchannel">> := Channel}}} ->
            dgiot_data:insert({channel_product, binary_to_atom(Channel)}, ProductId);
        _ ->
            pass
    end.

-spec get_channel(binary() | atom()) -> result().
get_channel(ChannelId) when is_binary(ChannelId) ->
    get_channel(binary_to_atom(ChannelId));
get_channel(ChannelId) ->
    dgiot_data:get({channel_product, ChannelId}).

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
            dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"thing">> => NewThing}),
            dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product#{<<"thing">> => NewThing});
        _Error ->
            []
    end.

update_properties() ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
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
    {ok, #{<<"fields">> := Fields}} = dgiot_parse:get_schemas(<<"Product">>),
    #{<<"type">> := Type} = maps:get(<<"topics">>, Fields),
    case Type of
        <<"Object">> ->
%% 判断目前topics 是那种类型，如果是object类型，就不更新
%% 删除原有topics字段
            case dgiot_parse:del_filed_schemas(<<"topics">>, <<"Product">>) of
                {ok, _} ->
                    %%  新增topics字段
                    dgiot_parse:create_schemas(<<"Product">>, #{<<"topics">> => []});
                Err ->
                    io:format("~s ~p ~p ~n", [?FILE, ?LINE, Err])
            end;
        _ -> pass
    end.

%% 产品字段新增
update_product_filed(_Filed) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
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

save_keys(ProductId) ->
    Keys =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(
                    fun
                        (#{<<"identifier">> := Identifier, <<"isshow">> := true}, Acc) ->
                            Acc ++ [Identifier];
                        (_, Acc) ->
                            Acc
                    end, [], Props);

            _Error ->
                []
        end,
    dgiot_data:insert(?DGIOT_PRODUCT, {ProductId, keys}, Keys).

get_keys(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, {ProductId, keys}) of
        not_find ->
            [];
        Keys ->
            Keys
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
    Keys = [<<"ACL">>, <<"name">>, <<"devType">>, <<"status">>, <<"channel">>, <<"content">>, <<"profile">>, <<"nodeType">>, <<"dynamicReg">>, <<"topics">>, <<"productSecret">>],
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
    case dgiot_parse:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            dgiot_parse:update_object(<<"Product">>, ObjectId, Product,
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
                    dgiot_parse:create_object(<<"Product">>,
                        CreateProductArgs, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                Err ->
                    {400, Err}
            end
    end.

create_product(#{<<"name">> := ProductName, <<"devType">> := DevType, <<"category">> := #{
    <<"objectId">> := CategoryId, <<"__type">> := <<"Pointer">>, <<"className">> := <<"Category">>}} = Product) ->
    ProductId = dgiot_parse_id:get_productid(CategoryId, DevType, ProductName),
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            dgiot_parse:update_object(<<"Product">>, ObjectId, Product),
            {ok, ObjectId};
        _ ->
            case dgiot_parse:create_object(<<"Product">>, Product) of
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
                      dgiot_parse:update_object(<<"Channel">>, ChannelId, Map);
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
    case dgiot_parse:query_object(<<"Channel">>, #{<<"where">> => #{<<"product">> => #{
        <<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => ProductId}}, <<"limit">> => 20}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun(#{<<"objectId">> := ChannelId}, _Acc) ->
                dgiot_parse:update_object(<<"Channel">>, ChannelId, Map)
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
                    #{<<"name">> := Name, <<"dataType">> := #{<<"specs">> := #{<<"unit">> := Unit}}} ->
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
            Props;
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
                        #{<<"identifier">> := Identifier} ->
                            Acc1 ++ [Prop];
                        _ ->
                            Acc1
                    end
                            end, Acc, Props);
            _ ->
                Acc
        end
                end, [], List).

%% 发消息通知td 重载超级表、字段
do_td_message(ProfuctId) ->
    ChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
    dgiot_channelx:do_message(ChannelId, {sync_product, <<"Product">>, ProfuctId}).
