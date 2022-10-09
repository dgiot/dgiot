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

-module(dgiot_product_knova).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post/1, get_konva/3, get_konva_thing/2, edit_konva/2, get_konva_view/4, get_Product_konva/0, save_Product_konva/1]).

%%dgiot_product_knova:post(<<"d0cb711d3d">>).
post(ProductId) ->
    dgiot_parse:create_object(<<"View">>, #{
        <<"title">> => ProductId,
        <<"key">> => ProductId,
        <<"type">> => <<"topo">>,
        <<"class">> => <<"Product">>,
        <<"data">> => dgiot_utils:get_JsonFile(?MODULE, <<"knova.json">>)
    }).

get_konva(ProductId, DeviceId, Payload) ->
    Topo =
        case dgiot_data:get({toponotext, ProductId}) of
            not_find -> [];
            Topo1 -> Topo1
        end,
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Type =
                case dgiot_data:get({shapetype, dgiot_parse_id:get_shapeid(ProductId, K)}) of
                    not_find ->
                        <<"text">>;
                    Type1 ->
                        Type1
                end,
            BinV = get_konva_value(ProductId, K, V),
            Unit = get_konva_unit(ProductId, K),
            Acc ++ [#{<<"id">> => dgiot_parse_id:get_shapeid(DeviceId, <<ProductId/binary, "_", K/binary, "_text">>), <<"text">> => <<BinV/binary, " ", Unit/binary>>, <<"type">> => Type}]
                  end, Topo, Payload),
    base64:encode(jsx:encode(#{<<"konva">> => Shape})).

get_konva_thing(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid,
        <<"viewid">> := ViewId
    } = Arg,
    NewViewId =
        case ViewId of
            undefined ->
                dgiot_parse_id:get_viewid(ProductId, <<"topo">>, <<"Product">>, ProductId);
            _ ->
                ViewId
        end,
    Children =
        case dgiot_parse:get_object(<<"View">>, NewViewId) of
            {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children2}}}}} when length(Children2) > 0 ->
                Children2;
            _ ->
                []
        end,

    case length(Children) > 0 of
        true ->
            case dgiot_parse:get_object(<<"Product">>, ProductId) of
                {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
                    put({self(), shapeids}, []),
                    get_children(<<"web">>, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
                    Shapids = get({self(), shapeids}),
                    Nobound =
                        lists:foldl(fun(Prop, Acc) ->
                            Identifier = maps:get(<<"identifier">>, Prop),
                            NewIdentifier = <<ProductId/binary, "_", Identifier/binary, "_text">>,
                            case lists:member(NewIdentifier, Shapids) of
                                false ->
                                    Acc ++ [Prop];
                                true ->
                                    Acc
                            end
                                    end, [], Properties),
                    KonvaThing =
                        lists:foldl(fun(Prop, Acc) ->
                            Identifier = maps:get(<<"identifier">>, Prop),
                            case Shapeid of
                                Identifier ->
                                    Prop;
                                _ ->
                                    Acc
                            end
                                    end, #{}, Properties),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => #{<<"nobound">> => Nobound, <<"konvathing">> => KonvaThing}}};
                _ ->
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => #{<<"nobound">> => #{}, <<"konvathing">> => #{}}}}
            end;
        _ ->
            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => #{<<"nobound">> => #{}, <<"konvathing">> => #{}}}}
    end.

get_konva_view(View, Devaddr, ProductId, Type) ->
    #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva}} = View,
    case Devaddr of
        undefined ->
            get_children(Type, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
            List = get_wechat(),
            case Type of
                <<"wechat">> ->
                    List;
                _ ->
                    Konva#{<<"Stage">> => Stage}
            end;
        _ ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
            case dgiot_device_tdengine:get_device(ProductId, Devaddr, #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1}) of
                {ok, #{<<"results">> := [Result | _]}} ->
                    put({self(), td}, Result);
                _ ->
                    put({self(), td}, #{})
            end,
            NewChildren1 = get_children(Type, ProductId, Children, DeviceId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
            List = get_wechat(),
            case Type of
                <<"wechat">> ->
                    List;
                _ ->
                    Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}
            end
    end.


get_wechat() ->
    case get(wechat) of
        undefined ->
            [];
        List ->
            List
    end.

edit_konva(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid,
        <<"identifier">> := Identifier,
        <<"name">> := Name
    } = Arg,
    NewViewId =
        case maps:find(<<"viewid">>, Arg) of
            error ->
                dgiot_parse_id:get_viewid(ProductId, <<"topo">>, <<"Product">>, ProductId);
            {ok, ViewId} ->
                ViewId
        end,
    case dgiot_parse:get_object(<<"View">>, NewViewId) of
        {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children2} = Stage} = Konva} = Data}} when length(Children2) > 0 ->
            put({self(), shapeids}, []),
            NewChildren = get_children(<<"web">>, ProductId, Children2, ProductId, ProductId, Shapeid, Identifier, Name),
            NewData = Data#{<<"konva">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren}}},
            case dgiot_parse:update_object(<<"View">>, NewViewId, #{<<"data">> => NewData}) of
                {ok, Message} ->
                    {ok, #{<<"code">> => 200, <<"message">> => Message}};
                {error, Message} ->
                    {ok, Message};
                _ ->
                    {ok, #{<<"code">> => 500, <<"message">> => <<"error">>}}
            end;
        _ ->
            {ok, #{<<"code">> => 101, <<"message">> => <<ProductId/binary, " not found">>}}
    end.

get_children(Type, ProductId, Children, DeviceId, KonvatId, Shapeid, Identifier, Name) ->
    lists:foldl(fun(X, Acc) ->
        #{<<"attrs">> := Attrs, <<"className">> := ClassName} = X,
        X1 = get_attrs(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name, X),
        X2 =
            case maps:find(<<"children">>, X1) of
                error ->
                    X1;
                {ok, SubChildren} ->
                    X1#{<<"children">> => get_children(Type, ProductId, SubChildren, DeviceId, KonvatId, Shapeid, Identifier, Name)}
            end,
        Acc ++ [X2]
                end, [], Children).

get_attrs(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name, X) ->
    case ClassName of
        <<"Layer">> ->
            X;
        <<"Group">> ->
            X;
        <<"Label">> ->
            X;
        <<"Text">> ->
            get_text(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name, X);
        _ ->
            X
    end.

get_text(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name, X) ->
    case maps:find(<<"id">>, Attrs) of
        {ok, Id} ->
            case ProductId of
                KonvatId ->
                    case Id of
                        Shapeid ->
                            NewAttrs = Attrs#{<<"id">> => <<ProductId/binary, "_", Identifier/binary, "_text">>, <<"text">> => Name},
                            save(Type, NewAttrs),
                            X#{<<"attrs">> => NewAttrs};
                        _ ->
                            save(Type, Attrs),
                            X
                    end;
                DeviceId ->
                    case get({self(), shapeids}) of
                        undefined ->
                            put({self(), shapeids}, [Id]);
                        List ->
                            put({self(), shapeids}, List ++ [Id])
                    end,
                    dgiot_data:insert({shapetype, dgiot_parse_id:get_shapeid(ProductId, Id)}, ClassName),
                    save(Type, Attrs),
                    X#{<<"attrs">> => Attrs};
                _ ->
                    Len = size(Id) - 16,
                    Identifier1 =
                        case Id of
                            <<_:10/binary, "_", Identifier2:Len/binary, "_text">> ->
                                Identifier2;
                            _ ->
                                <<"">>
                        end,
                    Result = get({self(), td}),
                    Unit = get_konva_unit(ProductId, Identifier1),
                    Text =
                        case maps:find(Identifier1, Result) of
                            error ->
                                Text2 = maps:get(<<"text">>, Attrs, <<"">>),
                                case dgiot_data:get({toponotext, ProductId}) of
                                    not_find ->
                                        dgiot_data:insert({toponotext, ProductId}, [#{<<"id">> => dgiot_parse_id:get_shapeid(DeviceId, Id), <<"text">> => <<Text2/binary, " ", Unit/binary>>, <<"type">> => Type}]);
                                    Topo ->
                                        New_Topo = dgiot_utils:unique_2(Topo ++ [#{<<"id">> => dgiot_parse_id:get_shapeid(DeviceId, Id), <<"text">> => <<Text2/binary, " ", Unit/binary>>, <<"type">> => Type}]),
                                        dgiot_data:insert({toponotext, ProductId}, New_Topo)
                                end,
                                Text2;
                            {ok, null} ->
                                <<"--">>;
                            {ok, Text1} ->
                                get_konva_value(ProductId, Identifier1, Text1)
                        end,
                    NewAttrs = Attrs#{<<"id">> => dgiot_parse_id:get_shapeid(DeviceId, Id), <<"text">> => <<Text/binary, " ", Unit/binary>>, <<"draggable">> => false},
                    save(Type, NewAttrs),
                    X#{<<"attrs">> => NewAttrs}
            end;
        error ->
            X
    end.

save(Type, Attrs) ->
    AttrType = maps:get(<<"type">>, Attrs, <<"image">>),
    NewAttrs = Attrs#{<<"type">> => AttrType},
    case Type of
        <<"wechat">> ->
            case get(wechat) of
                undefined ->
                    put(wechat, [NewAttrs]);
                List ->
                    put(wechat, List ++ [NewAttrs])
            end;
        _ -> pass
    end.

get_konva_unit(ProductId, Id) ->
    case dgiot_data:get({product, <<ProductId/binary, Id/binary>>}) of
        not_find ->
            <<>>;
        {_, _, Unit1} ->
            Unit1
    end.

get_konva_value(ProductId, K, V) ->
    Props = dgiot_product:get_props(ProductId),
    case maps:find(K, Props) of
        error ->
            dgiot_utils:to_binary(V);
        {ok, #{<<"dataType">> := #{<<"type">> := Type} = DataType}} ->
            Specs = maps:get(<<"specs">>, DataType, #{}),
            case Type of
                Type1 when Type1 == <<"enum">>; Type1 == <<"bool">> ->
                    Value = maps:get(dgiot_utils:to_binary(V), Specs, V),
                    dgiot_utils:to_binary(Value);
                Type3 when Type3 == <<"geopoint">> ->
                    Addr =
                        case dgiot_data:get({topogps, dgiot_parse_id:get_shapeid(ProductId, K)}) of
                            not_find ->
                                dgiot_utils:to_binary(V);
                            Gpsaddr ->
                                Gpsaddr
                        end,
                    dgiot_utils:to_binary(Addr);
                Type4 when Type4 == <<"float">>; Type4 == <<"double">> ->
                    Precision = maps:get(<<"precision">>, Specs, 3),
                    dgiot_utils:to_binary(dgiot_utils:to_float(V, Precision));
                _ ->
                    dgiot_utils:to_binary(V)
            end;
        _ ->
            dgiot_utils:to_binary(V)
    end.

get_Product_konva() ->
    Success = fun(Page) ->
        lists:map(fun(#{<<"objectId">> := ProductId}) ->
            save_Product_konva(ProductId)
                  end, Page)
              end,
    Query = #{
        <<"where">> => #{}
    },
    dgiot_parse_loader:start(<<"Product">>, Query, 0, 100, 1000000, Success).

save_Product_konva(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
            lists:map(fun(P) ->
                DataType = maps:get(<<"dataType">>, P, #{}),
                Type = maps:get(<<"type">>, DataType, <<"">>),
                Specs = maps:get(<<"specs">>, DataType, #{}),
                Unit = maps:get(<<"unit">>, Specs, <<"">>),
                Identifier = maps:get(<<"identifier">>, P, <<"">>),
                Name = maps:get(<<"name">>, P, <<"">>),
                dgiot_data:insert({product, <<ProductId/binary, Identifier/binary>>}, {Name, Type, Unit}),
                dgiot_data:insert({thing, <<ProductId/binary, Identifier/binary>>}, P)
                      end, Properties),
            case dgiot_parse:query_object(<<"View">>, #{<<"limit">> => 1, <<"where">> => #{<<"key">> => ProductId, <<"type">> => <<"topo">>, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Views}} when length(Views) > 0 ->
                    lists:foldl(fun(View, _Acc1) ->
                        #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}} = View,
                        get_children(<<"web">>, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>)
                                end, #{}, Views);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

%%update_konva(PdocuctId) ->
%%    Amis = dgiot_utils:get_JsonFile(?MODULE, <<"knova.json">>),
%%    Keys = dgiot_product:get_keys(PdocuctId)
%%.
