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

-export([docroot/0, get_topo/2, send_konva/3, send_realtime_card/3, get_name/3, put_topo/2, push/4, send_topo/2, get_que/1]).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".

get_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId, <<"devaddr">> := Devaddr, <<"viewid">> := ViewId} = Arg,
    Type = maps:get(<<"type">>, Arg, <<"web">>),
    NewViewId =
        case ViewId of
            undefined ->
                dgiot_parse_id:get_viewid(ProductId, <<"topo">>, <<"Product">>, ProductId);
            _ ->
                ViewId
        end,
    case dgiot_parse:get_object(<<"View">>, NewViewId) of
        {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}} = View} when length(Children) > 0 ->
            NewView = dgiot_product_knova:get_konva_view(View, Devaddr, ProductId, Type),
            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => NewView#{<<"viewid">> => NewViewId}}};
        _ ->
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
    end.

put_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr,
        <<"base64">> := Base64
    } = Arg,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
    Pubtopic = <<"$dg/user/konva/", DeviceId/binary, "/report">>,
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
    Pubtopic = <<"$dg/user/realtimecard/", DeviceId/binary, "/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, base64:encode(jsx:encode(#{<<"data">> => Data}))).

%% 发送实时组态数据
send_konva(ProductId, DeviceId, Payload) ->
    Base64 = dgiot_product_knova:get_konva(ProductId, DeviceId, Payload),
    Pubtopic = <<"$dg/user/konva/", DeviceId/binary, "/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64).

push(ProductId, Devaddr, DeviceId, Payload) ->
    Base64 = dgiot_product_knova:get_konva(ProductId, DeviceId, Payload),
    Url = dgiot_data:get(topourl),
    Url1 = dgiot_utils:to_list(Url),
    Data = #{<<"productid">> => ProductId, <<"devaddr">> => Devaddr, <<"base64">> => Base64},
    Data1 = dgiot_utils:to_list(jsx:encode(Data)),
    httpc:request(post, {Url1, [], "application/json", Data1}, [], []).

send_topo({NodeType, NodeId}, Token) ->
%%    io:format("NodeType ~p NodeId ~p Token ~p ~n", [NodeType, NodeId,Token]),
    case dgiot_hook:run_hook({topo, NodeType, NodeId}, {Token, NodeId}) of
        {ok, [{ok, Payload}]} ->
            Base64 = base64:encode(jsx:encode(Payload)),
            Pubtopic = <<"$dg/user/topo/", Token/binary, "/", NodeType/binary, "/", NodeId/binary, "/report">>,
%%            io:format("~s ~p Pubtopic ~p Base64 ~p ~n", [?FILE,?LINE, Pubtopic, Base64]),
            dgiot_mqtt:publish(self(), Pubtopic, Base64);
        _ ->
            pass
    end.

get_que(DashboardId) ->
    case dgiot_parse:get_object(<<"View">>, DashboardId) of
        {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := Stage}}}} ->
            Rects = dgiot_product_knova:get_nodes(Stage, [<<"Rect">>]),
            maps:fold(
                fun
                    (NodeId, #{<<"name">> := <<"vuecomponent">>, <<"source">> := <<"mqtt">>, <<"type">> := NodeType}, Acc) ->
                        Acc ++ [{NodeType, NodeId}];
                    (_, _, Acc) ->
                        Acc
                end, [], Rects);
        _ ->
            []
    end.
