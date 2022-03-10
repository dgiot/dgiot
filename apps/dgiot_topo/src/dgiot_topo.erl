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

-export([docroot/0, get_topo/2, send_topo/3, send_realtimedata/3, get_Product/0, get_name/3, put_topo/2, get_konva_thing/2, edit_konva/2, push/4, get_gpsaddr/1]).

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


%%get_topo(Arg, _Context) ->
%%    #{<<"productid">> := ProductId, <<"devaddr">> := Devaddr} = Arg,
%%    Type = maps:get(<<"type">>, Arg, <<"web">>),
%%    case dgiot_parse:get_object(<<"Product">>, ProductId) of
%%        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva}}} when length(Children) > 0 ->
%%            case Devaddr of
%%                undefined ->
%%                    NewChildren1 = get_children(Type, ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
%%                    List = get_wechat(),
%%                    case Type of
%%                        <<"wechat">> ->
%%                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => List}};
%%                        _ ->
%%                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}}
%%                    end;
%%                _ ->
%%                    DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
%%                    case dgiot_tdengine:get_device(ProductId, Devaddr, #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1}) of
%%                        {ok, #{<<"results">> := [Result | _]}} ->
%%                            put({self(), td}, Result);
%%                        _ ->
%%                            put({self(), td}, #{})
%%                    end,
%%                    NewChildren1 = get_children(Type, ProductId, Children, DeviceId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
%%                    List = get_wechat(),
%%                    case Type of
%%                        <<"wechat">> ->
%%                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => List}};
%%                        _ ->
%%                            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}}
%%                    end
%%            end;
%%        _ ->
%%            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
%%    end.

get_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId, <<"devaddr">> := Devaddr, <<"viewid">> := ViewId} = Arg,
    Type = maps:get(<<"type">>, Arg, <<"web">>),
    case ViewId of
        undefined ->
            case dgiot_parse:query_object(<<"View">>, #{<<"limit">> => 1, <<"where">> => #{<<"key">> => ProductId, <<"type">> => <<"topo">>, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Views}} when length(Views) > 0 ->
                    NewStage =
                        lists:foldl(fun(View, Acc) ->
                            case View of
                                #{<<"objectId">> := ViewId1, <<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}} when length(Children) > 0 ->
                                    NewView = get_view(View, Devaddr, ProductId, Type),
                                    NewView#{<<"viewid">> => ViewId1};
                                #{<<"objectId">> := ViewId2} ->
                                    Acc#{<<"viewid">> => ViewId2};
                                _ ->
                                    Acc
                            end
                                    end, #{}, Views),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => NewStage}};
                _ ->
                    {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
            end;
        _ ->
            case dgiot_parse:get_object(<<"View">>, ViewId) of
                {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}} = View} when length(Children) > 0 ->
                    NewView = get_view(View, Devaddr, ProductId, Type),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => NewView#{<<"viewid">> => ViewId}}};
                _ ->
                    {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
            end
    end.

get_view(View, Devaddr, ProductId, Type) ->
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
                    List;
                _ ->
                    Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}
            end
    end.

get_konva_thing(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid,
        <<"viewid">> := ViewId
    } = Arg,
    Children =
        case ViewId of
            undefined ->
                case dgiot_parse:query_object(<<"View">>, #{<<"limit">> => 1, <<"where">> => #{<<"key">> => ProductId, <<"type">> => <<"topo">>, <<"class">> => <<"Product">>}}) of
                    {ok, #{<<"results">> := Views}} when length(Views) > 0 ->
                        [#{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children1}}}} | _] = Views,
                        Children1;
                    _ ->
                        []
                end;
            _ ->
                case dgiot_parse:get_object(<<"View">>, ViewId) of
                    {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children2}}}}} when length(Children2) > 0 ->
                        Children2;
                    _ ->
                        []
                end
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


edit_konva(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid,
        <<"identifier">> := Identifier,
        <<"name">> := Name
    } = Arg,
    case maps:find(<<"viewid">>, Arg) of
        error ->
            case dgiot_parse:query_object(<<"View">>, #{<<"limit">> => 1, <<"where">> => #{<<"key">> => ProductId, <<"type">> => <<"topo">>, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Views}} when length(Views) > 0 ->
                    [#{<<"objectId">> := ViewId1, <<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children1} = Stage} = Konva} = Data} | _] = Views,
                    put({self(), shapeids}, []),
                    NewChildren = get_children(<<"web">>, ProductId, Children1, ProductId, ProductId, Shapeid, Identifier, Name),
                    NewData = Data#{<<"konva">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren}}},
                    case dgiot_parse:update_object(<<"View">>, ViewId1, #{<<"data">> => NewData}) of
                        {ok, Message} ->
                            {ok, #{<<"code">> => 200, <<"message">> => Message}};
                        {error, Message} ->
                            {ok, Message};
                        _ ->
                            {ok, #{<<"code">> => 500, <<"message">> => <<"error">>}}
                    end;
                _ ->
                    {ok, #{<<"code">> => 101, <<"message">> => <<ProductId/binary, " not found">>}}
            end;
        {ok, ViewId} ->
            case dgiot_parse:get_object(<<"View">>, ViewId) of
                {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children2} = Stage} = Konva} = Data}} when length(Children2) > 0 ->
                    put({self(), shapeids}, []),
                    NewChildren = get_children(<<"web">>, ProductId, Children2, ProductId, ProductId, Shapeid, Identifier, Name),
                    NewData = Data#{<<"konva">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren}}},
                    case dgiot_parse:update_object(<<"View">>, ViewId, #{<<"data">> => NewData}) of
                        {ok, Message} ->
                            {ok, #{<<"code">> => 200, <<"message">> => Message}};
                        {error, Message} ->
                            {ok, Message};
                        _ ->
                            {ok, #{<<"code">> => 500, <<"message">> => <<"error">>}}
                    end;
                _ ->
                    {ok, #{<<"code">> => 101, <<"message">> => <<ProductId/binary, " not found">>}}
            end
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
            case maps:find(<<"id">>, Attrs) of
                error ->
                    X;
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
                            dgiot_data:insert({shapetype, dgiot_parse:get_shapeid(ProductId, Id)}, ClassName),
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
                            Unit = get_unit(ProductId, Identifier1),
                            Text =
                                case maps:find(Identifier1, Result) of
                                    error ->
                                        Text2 = maps:get(<<"text">>, Attrs, <<"">>),
                                        case dgiot_data:get({toponotext, ProductId}) of
                                            not_find ->
                                                dgiot_data:insert({toponotext, ProductId}, [#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, Id), <<"text">> => <<Text2/binary, " ", Unit/binary>>, <<"type">> => Type}]);
                                            Topo ->
                                                New_Topo = dgiot_utils:unique_2(Topo ++ [#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, Id), <<"text">> => <<Text2/binary, " ", Unit/binary>>, <<"type">> => Type}]),
                                                dgiot_data:insert({toponotext, ProductId}, New_Topo)
                                        end,
                                        Text2;
                                    {ok, Text1} ->
                                        get_value(ProductId, Identifier1, Text1)
                                end,
                            NewAttrs = Attrs#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, Id), <<"text">> => Text, <<"draggable">> => false},
                            save(Type, NewAttrs),
                            X#{<<"attrs">> => NewAttrs}
                    end
            end;
        _ ->
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
    Pubtopic = <<"$dg/konva/", DeviceId/binary, "/properties/report">>,
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
                    #{<<"objectId">> := ProductId, <<"thing">> := #{<<"properties">> := Properties}} ->
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
                end
                        end, [], Results);
        _ ->
            pass
    end.

send_topo(ProductId, DeviceId, Payload) ->
    Base64 = get_optshape(ProductId, DeviceId, Payload),
    Pubtopic = <<"$dg/konva/", DeviceId/binary, "/properties/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64).

get_optshape(ProductId, DeviceId, Payload) ->
    Topo =
        case dgiot_data:get({toponotext, ProductId}) of
            not_find -> [];
            Topo1 -> Topo1
        end,
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Type =
                case dgiot_data:get({shapetype, dgiot_parse:get_shapeid(ProductId, K)}) of
                    not_find ->
                        <<"text">>;
                    Type1 ->
                        Type1
                end,
            BinV = get_value(ProductId, K, V),
            Unit = get_unit(ProductId, K),
            Acc ++ [#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, <<ProductId/binary, "_", K/binary, "_text">>), <<"text">> => <<BinV/binary, " ", Unit/binary>>, <<"type">> => Type}]
                  end, Topo, Payload),
    base64:encode(jsx:encode(#{<<"konva">> => Shape})).

get_value(ProductId, K, V) ->
    Props = dgiot_tdengine_handler:get_props(ProductId),
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
                        case dgiot_data:get({topogps, dgiot_parse:get_shapeid(ProductId, K)}) of
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

push(ProductId, Devaddr, DeviceId, Payload) ->
    Base64 = get_optshape(ProductId, DeviceId, Payload),
    Url = dgiot_data:get(topourl),
    Url1 = dgiot_utils:to_list(Url),
    Data = #{<<"productid">> => ProductId, <<"devaddr">> => Devaddr, <<"base64">> => Base64},
    Data1 = dgiot_utils:to_list(jsx:encode(Data)),
    httpc:request(post, {Url1, [], "application/json", Data1}, [], []).


get_gpsaddr(V) ->
    BinV = dgiot_utils:to_binary(V),
    case binary:split(BinV, <<$_>>, [global, trim]) of
        [Longitude, Latitude] ->
            case dgiot_gps:get_baidu_addr(Longitude, Latitude) of
                #{<<"baiduaddr">> := #{<<"formatted_address">> := FormattedAddress}} ->
                    FormattedAddress;
                _ ->
                    <<"[", BinV/binary, "]经纬度解析错误"/utf8>>
            end;
        _ ->
            <<"无GPS信息"/utf8>>
    end.

send_realtimedata(ProductId, DeviceId, Payload) ->
    Base64 = get_realtimedata(ProductId, DeviceId, Payload),
    Pubtopic = <<"$dg/user/", DeviceId/binary, "/properties/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64).

get_realtimedata(ProductId, DeviceId, Payload) ->
    Maps = get_prop(ProductId),
    Props = get_props(ProductId),
    Data =
        maps:fold(fun(K, V, Acc) ->
            Time = dgiot_datetime:now_secs(),
            NewTime = dgiot_tdengine_handler:get_time(dgiot_utils:to_binary(Time), <<"111">>),
            case maps:find(K, Maps) of
                error ->
                    Acc;
                {ok, Name} ->
                    {Type, NewV, Unit, Ico, Devicetype} =
                        case maps:find(K, Props) of
                            error ->
                                {V, <<"">>, <<"">>, <<"others">>};
                            {ok, #{<<"dataType">> := #{<<"type">> := Typea} = DataType} = Prop} ->
                                Devicetype1 = maps:get(<<"devicetype">>, Prop, <<"others">>),
                                Specs = maps:get(<<"specs">>, DataType, #{}),
                                case Typea of
                                    Type1 when Type1 == <<"enum">>; Type1 == <<"bool">> ->
                                        Value = maps:get(dgiot_utils:to_binary(V), Specs, V),
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        {Type1, Value, <<"">>, Ico1, Devicetype1};
                                    Type2 when Type2 == <<"struct">> ->
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        {Type2, V, <<"">>, Ico1, Devicetype1};
                                    Type3 when Type3 == <<"geopoint">> ->
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        BinV = dgiot_utils:to_binary(V),
                                        Addr =
                                            case binary:split(BinV, <<$_>>, [global, trim]) of
                                                [Longitude, Latitude] ->
                                                    case dgiot_gps:get_baidu_addr(Longitude, Latitude) of
                                                        #{<<"baiduaddr">> := #{<<"formatted_address">> := FormattedAddress}} ->
                                                            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                                                                {ok, #{<<"location">> := #{<<"__type">> := <<"GeoPoint">>, <<"longitude">> := Longitude, <<"latitude">> := Latitude}}} ->
                                                                    pass;
                                                                {ok, #{<<"detail">> := Detail}} ->
                                                                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{
                                                                        <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => Longitude, <<"latitude">> => Latitude},
                                                                        <<"detail">> => Detail#{<<"address">> => FormattedAddress}});
                                                                _ ->
                                                                    pass
                                                            end,
                                                            FormattedAddress;
                                                        _ ->
                                                            <<"[", BinV/binary, "]经纬度解析错误"/utf8>>
                                                    end;
                                                _ ->
                                                    <<"无GPS信息"/utf8>>
                                            end,
                                        {Type3, Addr, <<"">>, Ico1, Devicetype1};
                                    Type4 when Type4 == <<"float">>; Type4 == <<"double">> ->
                                        Unit1 = maps:get(<<"unit">>, Specs, <<"">>),
                                        Precision = maps:get(<<"precision">>, Specs, 3),
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        {Type4, dgiot_utils:to_float(V, Precision), Unit1, Ico1, Devicetype1};
                                    Type5 when Type5 == <<"image">> ->
                                        AppName = dgiot_device:get_appname(DeviceId),
                                        Url = dgiot_device:get_url(AppName),
                                        Unit1 = maps:get(<<"unit">>, Specs, <<"">>),
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        Imagevalue = maps:get(<<"imagevalue">>, DataType, <<"">>),
                                        BinV = dgiot_utils:to_binary(V),
                                        {Type5, <<Url/binary, "/dgiot_file/", DeviceId/binary, "/", BinV/binary, ".", Imagevalue/binary>>, Unit1, Ico1, Devicetype1};
                                    Type6 when Type6 == <<"date">> ->
                                        V1 =
                                            case V of
                                                <<"1970-01-01 08:00:00.000">> ->
                                                    <<"--">>;
                                                _ ->
                                                    V
                                            end,
                                        Unit1 = maps:get(<<"unit">>, Specs, <<"">>),
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        {Type6, V1, Unit1, Ico1, Devicetype1};
                                    _ ->
                                        Unit1 = maps:get(<<"unit">>, Specs, <<"">>),
                                        Ico1 = maps:get(<<"ico">>, Prop, <<"">>),
                                        {Typea, V, Unit1, Ico1, Devicetype1}
                                end;
                            _ ->
                                {<<"others">>, V, <<"">>, <<"">>, <<"others">>}
                        end,
                    Acc ++ [#{<<"name">> => Name, <<"type">> => Type, <<"number">> => NewV, <<"time">> => NewTime, <<"unit">> => Unit, <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}]
            end
                  end, [], Payload),
    base64:encode(jsx:encode(#{<<"data">> => Data})).

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
