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
-module(dgiot_topo).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([docroot/0, get_topo/2, send_topo/3, get_Product/0, get_name/3, put_topo/2, get_konva_thing/2, edit_konva/2, push/4]).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".


%%{ok,#{<<"results">> =>
%%[#{<<"battery_voltage">> => 11.7,<<"charge_current">> => 0,
%%<<"core_temperature">> => 37,
%%<<"createdat">> => <<"2021-06-07 18:49:42.061">>,
%%<<"day_electricity">> => 0.11,<<"dump_energy">> => 75.0,
%%<<"i_out">> => 0.0,<<"outside_temperature">> => 25,
%%<<"system_state">> => <<"0">>,<<"total_power">> => 2.1,
%%<<"v_out">> => 0.0,<<"v_solarpanel">> => 0.3}]}}


get_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId, <<"devaddr">> := Devaddr} = Arg,
    Type = maps:get(<<"type">>, Arg, <<"web">>),
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva}}} when length(Children) > 0 ->
            case Devaddr of
                undefined ->
                    NewChildren1 = get_children(Type, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
                    List = get_wechat(),
                    case Type of
                        <<"wechat">> ->
                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => List}};
                        _ ->
                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}}
                    end;
                _ ->
                    DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
                    case dgiot_tdengine:get_device(ProductId, Devaddr, #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1}) of
                        {ok, #{<<"results">> := [Result | _]}} ->
                            put({self(), td}, Result);
                        _ ->
                            put({self(), td}, #{})
                    end,
                    NewChildren1 = get_children(Type, ProductId, Children, DeviceId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
                    List = get_wechat(),
                    case Type of
                        <<"wechat">> ->
                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => List}};
                        _ ->
                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}}
                    end
            end;
        _ ->
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
    end.

get_konva_thing(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid
    } = Arg,
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}, <<"thing">> := #{<<"properties">> := Properties}}} ->
            put({self(), shapeids}, []),
            get_children(<<"web">>, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
            Shapids = get({self(), shapeids}),
            Nobound =
                lists:foldl(fun(Prop, Acc) ->
                    Identifier = maps:get(<<"identifier">>, Prop),
                    case lists:member(Identifier, Shapids) of
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
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
    end.

edit_konva(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid,
        <<"identifier">> := Identifier,
        <<"name">> := Name
    } = Arg,
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva} = Config}} ->
            put({self(), shapeids}, []),
            NewChildren = get_children(<<"web">>, ProductId, Children, ProductId, ProductId, Shapeid, Identifier, Name),
            NewConfig = Config#{<<"konva">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren}}},
            case dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"config">> => NewConfig}) of
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
        X1 =
            case maps:find(<<"children">>, X) of
                error ->
                    X#{<<"attrs">> => get_attrs(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name)};
                {ok, SubChildren} ->
                    X#{<<"attrs">> => get_attrs(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name),
                        <<"children">> => get_children(Type, ProductId, SubChildren, DeviceId, KonvatId, Shapeid, Identifier, Name)}
            end,
        Acc ++ [X1]
                end, [], Children).

get_attrs(Type, ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name) ->
    case ClassName of
        <<"Layer">> ->
            Attrs;
        <<"Group">> ->
            Attrs;
        _ ->
            case maps:find(<<"id">>, Attrs) of
                error ->
                    Attrs;
                {ok, Id} ->
                    case ProductId of
                        KonvatId ->
                            case Id of
                                Shapeid ->
                                    NewAttrs = Attrs#{<<"id">> => Identifier, <<"text">> => Name},
                                    save(Type, NewAttrs),
                                    NewAttrs;
                                _ ->
                                    save(Type, Attrs),
                                    Attrs
                            end;
                        DeviceId ->
                            case get({self(), shapeids}) of
                                undefined ->
                                    put({self(), shapeids}, [Id]);
                                List ->
                                    put({self(), shapeids}, List ++ [Id])
                            end,
                            dgiot_data:insert({shapetype, dgiot_parse:get_shapeid(ProductId, Id)}, ClassName),
                            save(Type, Attrs),
                            Attrs;
                        _ ->
                            Id = maps:get(<<"id">>, Attrs),
                            Result = get({self(), td}),
                            Text =
                                case maps:find(Id, Result) of
                                    error ->
                                        <<"0">>;
                                    {ok, Text1} ->
                                        dgiot_utils:to_binary(Text1)
                                end,
                            Unit = get_unit(ProductId, Id),
                            NewAttrs = Attrs#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, Id), <<"text">> => <<Text/binary, " ", Unit/binary>>, <<"draggable">> => false},
                            save(Type, NewAttrs),
                            NewAttrs
                    end
            end
    end.

save(Type, Attrs) ->
    AttrType = maps:get(<<"type">>,Attrs,<<"image">>),
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

get_wechat() ->
    case get(wechat) of
        undefined ->
            [];
        List ->
            List
    end.

get_unit(ProductId, Id) ->
    case dgiot_data:get({product, <<ProductId/binary, Id/binary>>}) of
        not_find ->
            <<>>;
        {_, _, Unit1} ->
            Unit1
    end.

put_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr,
        <<"base64">> := Base64
    } = Arg,
    DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
    Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64),
    {ok, <<"Success">>}.

get_name(ProductId, K, V) ->
    case dgiot_data:get({product, <<ProductId/binary, K/binary>>}) of
        not_find ->
            V;
        {Name, Type, Unit} when Type =:= <<"float">> orelse Type =:= <<"double">> ->
            NewV = dgiot_utils:to_binary(dgiot_utils:to_float(V, 3)),
            <<Name/binary, ": ", NewV/binary, " ", Unit/binary>>;
        {Name, _Type, Unit} ->
            <<Name/binary, ":", V/binary, " ", Unit/binary>>
    end.

get_Product() ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, _Acc) ->
                case X of
                    #{<<"objectId">> := ProductId, <<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}, <<"thing">> := #{<<"properties">> := Properties}} ->
                        lists:map(fun(P) ->
                            DataType = maps:get(<<"dataType">>, P),
                            Type = maps:get(<<"type">>, DataType),
                            Specs = maps:get(<<"specs">>, DataType),
                            Unit = maps:get(<<"unit">>, Specs, <<"">>),
                            Identifier = maps:get(<<"identifier">>, P),
                            Name = maps:get(<<"name">>, P),
                            dgiot_data:insert({product, <<ProductId/binary, Identifier/binary>>}, {Name, Type, Unit}),
                            dgiot_data:insert({thing, <<ProductId/binary, Identifier/binary>>}, P)
                                  end, Properties),
                        get_children(<<"web">>, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>);
                    _ ->
                        pass
                end
                        end, [], Results);
        _ ->
            pass
    end.

send_topo(ProductId, DeviceId, Payload) ->
    Base64 = get_optshape(ProductId, DeviceId, Payload),
    Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64).


get_optshape(ProductId, DeviceId, Payload) ->
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Type =
                case dgiot_data:get({shapetype, dgiot_parse:get_shapeid(ProductId, K)}) of
                    not_find ->
                        <<"text">>;
                    Type1 ->
                        Type1
                end,
            Unit = get_unit(ProductId, K),
            BinV = dgiot_utils:to_binary(V),
            Acc ++ [#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, K), <<"text">> => <<BinV/binary, " ", Unit/binary>>, <<"type">> => Type}]
                  end, [], Payload),
    base64:encode(jsx:encode(#{<<"konva">> => Shape})).

push(ProductId, Devaddr, DeviceId, Payload) ->
    Base64 = get_optshape(ProductId, DeviceId, Payload),
    Url = dgiot_data:get(topourl),
    Url1 = dgiot_utils:to_list(Url),
    Data = #{<<"productid">> => ProductId, <<"devaddr">> => Devaddr, <<"base64">> => Base64},
    Data1 = dgiot_utils:to_list(jsx:encode(Data)),
    httpc:request(post, {Url1, [], "application/json", Data1}, [], []).

