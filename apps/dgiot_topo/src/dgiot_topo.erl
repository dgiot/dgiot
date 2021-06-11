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
-module(dgiot_topo).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([start_http/0, docroot/0, get_topo/2, send_topo/3, get_product/0, get_name/3, put_topo/2]).

start_http() ->
    Port = application:get_env(?MODULE, port, 6081),
    DocRoot = docroot(),
    dgiot_http_server:start_http(?MODULE, Port, DocRoot).


docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".



get_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr
    } = Arg,
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva}}} when length(Children) > 0 ->
            case Devaddr of
                undefined ->
                    NewChildren1 = get_children(ProductId, Children, ProductId),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}};
                _ ->
%%                    device
                    DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
                    NewChildren1 = get_children(ProductId, Children, DeviceId),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}}
            end;
        _ ->
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}

    end.

get_children(ProductId, Children, DeviceId) ->
    lists:foldl(fun(X, Acc) ->
        #{<<"attrs">> := Attrs, <<"className">> := ClassName} = X,
        X1 =
            case maps:find(<<"children">>, X) of
                error ->
                    X#{<<"attrs">> => get_attrs(ProductId, ClassName, Attrs, DeviceId)};
                {ok, SubChildren} ->
                    X#{<<"attrs">> => get_attrs(ProductId, ClassName, Attrs, DeviceId),
                        <<"children">> => get_children(ProductId, SubChildren, DeviceId)}
            end,
        Acc ++ [X1]
                end, [], Children).

get_attrs(ProductId, ClassName, Attrs, DeviceId) ->
    case ClassName of
        <<"Layer">> ->
            Attrs;
        <<"Group">> ->
            Attrs;
        _ ->
            case ProductId of
                DeviceId ->
                    dgiot_data:insert({shapetype, dgiot_parse:get_shapeid(ProductId, maps:get(<<"id">>, Attrs))}, ClassName),
%%                    Attrs#{<<"text">> => Text};
                    Attrs;
                _ ->
%%                    Text = get_name(ProductId, maps:get(<<"id">>, Attrs), dgiot_utils:to_binary(maps:get(<<"text">>, Attrs))),
%%                    Attrs#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, maps:get(<<"id">>, Attrs)), <<"text">> => Text}
                    Attrs#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, maps:get(<<"id">>, Attrs))}
            end
    end.

%% #{<<"Arcel">>=> 1,<<"Flow">> => 1.2} => ShapeId = md5(<<DeviceId/binary,"Arcel">>)
%%{
%%"konva":{
%%    [
%%                {
%%                "id":[shapeid],
%%                "text":"16",
%%                "type":"text",
%%                },
%%                {
%%                "id":[shapeid],
%%                "text":"16",
%%                "type":"Image",
%%                }
%%        ]
%%    }
%%}  dgiot_data:get({product, <<"16cf2bf9f7energy">>})
%% dgiot_topo:send_topo(<<"9b5c1a3ed5">>, <<"001">>, #{<<"Acrel">> => 10,<<"current">> => 20,<<"current">> => 30}).
send_topo(ProductId, Devaddr, Payload) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Text = get_name(ProductId, K, dgiot_utils:to_binary(V)),
            Type =
                case dgiot_data:get({shapetype, dgiot_parse:get_shapeid(ProductId, K)}) of
                    not_find ->
                        <<"text">>;
                    Type1 ->
                        Type1
                end,
            Acc ++ [#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, K), <<"text">> => Text, <<"type">> => Type}]
                  end, [], Payload),
    Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
    Base64 = base64:encode(jsx:encode(#{<<"konva">> => Shape})),
    dgiot_mqtt:publish(self(), Pubtopic, Base64).


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
        {Name, Type, Unit} when Type =:= <<"float">> ->
            NewV = dgiot_utils:to_binary(dgiot_utils:to_float(V, 3)),
            <<Name/binary, ": ", NewV/binary, " ", Unit/binary>>;
        {Name, _Type, Unit} ->
            %todo 物模型配置错误，临时规避一下
            NewV = dgiot_utils:to_binary(dgiot_utils:to_float(V / 1.0, 3)),
            <<Name/binary, ":", NewV/binary, " ", Unit/binary>>
    end.

get_product() ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, _Acc) ->
                case X of
                    #{<<"objectId">> := ProductId, <<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}, <<"thing">> := #{<<"properties">> := Properties}} ->
                        lists:map(fun(P) ->
%%                            "dataType": {
%%                                "type": "float",
%%                                "specs": {
%%                                    "max": 500,
%%                                    "min": -500,
%%                                    "step": 0.1,
%%                                    "unit": "MPa"
%%                                }
%%                            },
                            DataType = maps:get(<<"dataType">>, P),
                            Type = maps:get(<<"type">>, DataType),
                            Specs = maps:get(<<"specs">>, DataType),
                            Unit = maps:get(<<"unit">>, Specs, <<"">>),
                            Identifier = maps:get(<<"identifier">>, P),
                            Name = maps:get(<<"name">>, P),
                            dgiot_data:insert({product, <<ProductId/binary, Identifier/binary>>}, {Name, Type, Unit})
                                  end, Properties),
                        get_children(ProductId, Children, ProductId);
                    _ ->
                        pass
                end
                        end, [], Results);
        _ ->
            pass
    end.

