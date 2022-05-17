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

-export([docroot/0, get_topo/2, send_konva/3, send_realtime_card/3, get_name/3, put_topo/2, push/4]).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".

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
                                    NewView = dgiot_product_knova:get_konva_view(View, Devaddr, ProductId, Type),
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
                    NewView = dgiot_product_knova:get_konva_view(View, Devaddr, ProductId, Type),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => NewView#{<<"viewid">> => ViewId}}};
                _ ->
                    {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
            end
    end.

put_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr,
        <<"base64">> := Base64
    } = Arg,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
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

%% 发送实时卡片数据
send_realtime_card(ProductId, DeviceId, Payload) ->
    Data = dgiot_device_card:get_card(ProductId, [Payload], DeviceId, #{}),
    Pubtopic = <<"$dg/user/", DeviceId/binary, "/realtimecard/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, base64:encode(jsx:encode(#{<<"data">> => Data}))).

%% 发送实时组态数据
send_konva(ProductId, DeviceId, Payload) ->
    Base64 = dgiot_product_knova:get_konva(ProductId, DeviceId, Payload),
    Pubtopic = <<"$dg/user/", DeviceId/binary, "/konva/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64).


push(ProductId, Devaddr, DeviceId, Payload) ->
    Base64 = dgiot_product_knova:get_konva(ProductId, DeviceId, Payload),
    Url = dgiot_data:get(topourl),
    Url1 = dgiot_utils:to_list(Url),
    Data = #{<<"productid">> => ProductId, <<"devaddr">> => Devaddr, <<"base64">> => Base64},
    Data1 = dgiot_utils:to_list(jsx:encode(Data)),
    httpc:request(post, {Url1, [], "application/json", Data1}, [], []).

