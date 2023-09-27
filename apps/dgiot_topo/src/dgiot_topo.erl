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

-export([docroot/0, get_topo/2, send_konva/3, send_realtime_card/3, get_name/3, put_topo/2, push/4, send_topo/2, get_que/1, get_konva/1, send_amisdata/1]).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".

get_topo(Arg, #{<<"sessionToken">> := SessionToken} = _Context) ->
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
            NewView = dgiot_product_knova:get_konva_view(View, Devaddr, ProductId, Type, SessionToken),
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
            NewV = dgiot_utils:to_float(V, 3),
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
    case dgiot_hook:run_hook({'topo', NodeType}, {Token, NodeId}) of
        {ok, [{ok, Payload}]} ->
            Base64 = base64:encode(jsx:encode(Payload)),
            Pubtopic = <<"$dg/user/topo/", Token/binary, "/", NodeType/binary, "/", NodeId/binary, "/report">>,
%%            io:format("~s ~p Pubtopic ~p Base64 ~p ~n", [?FILE,?LINE, Pubtopic, Base64]),
            dgiot_mqtt:publish(self(), Pubtopic, Base64);
        _ ->
            pass
    end.

%% dgiot_topo:send_amisdata(#{<<"dashboardId">> => <<"599080d6d5">>, <<"sessionToken">> => <<"r:93a396474043d106b40557ff3ff6a3cf">>})
send_amisdata(#{<<"dashboardId">> := DashboardId, <<"sessionToken">> := Token}) ->
    {ViewIds, AmisDatas} =
        case dgiot_parse:get_object(<<"View">>, DashboardId) of
            {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := Stage}}}} ->
                Rects = dgiot_product_knova:get_nodes(Stage, [<<"Rect">>, <<"Image">>, <<"Text">>, <<"Group">>]),
                maps:fold(
                    fun
                        (_, #{<<"source">> := <<"mqtt">>, <<"type">> := <<"amisview">>, <<"amisid">> := Viewid} = AmisData, {Idd, Acc}) ->
                            {Idd ++ [Viewid], Acc ++ [AmisData]};
                        (_, _, {Idd, Acc}) ->
                            {Idd, Acc}
                    end, {[], []}, Rects);
            _ ->
                {[], []}
        end,
    ViewDatas =
        case dgiot_parse:query_object(<<"View">>, #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => ViewIds}}}) of
            {ok, #{<<"results">> := Views}} ->
                lists:foldl(fun(#{<<"objectId">> := ViewId, <<"data">> := Data}, Acc) ->
                    Acc#{ViewId => Data}
                            end, #{}, Views);
            _ ->
                #{}
        end,
    Payload =
        lists:foldl(fun(#{<<"amisid">> := ViewId} = AmisData, Acc) ->
            case maps:find(ViewId, ViewDatas) of
                {ok, ViewData} ->
                    Screen_productid = maps:get(<<"screen_productid">>, AmisData, <<>>),
                    Screen_deviceid = maps:get(<<"screen_deviceid">>, AmisData, maps:get(<<"terminalId">>, AmisData, <<>>)),
                    Rep = re:replace(jsx:encode(ViewData), <<"{{screen_productid}}">>, Screen_productid, [global, {return, binary}]),
                    NewJson = re:replace(Rep, <<"{{screen_deviceid}}">>, Screen_deviceid, [global, {return, binary}]),
                    Acc ++ [AmisData#{<<"viewData">> => jsx:decode(NewJson)}];
                _ ->
                    Acc
            end
                    end, [], AmisDatas),
    Base64 = base64:encode(jsx:encode(Payload)),
    Pubtopic = <<"$dg/user/topo/", Token/binary, "/amisdata/report">>,
    dgiot_mqtt:publish(self(), Pubtopic, Base64).

get_que(DashboardId) ->
    case dgiot_parse:get_object(<<"View">>, DashboardId) of
        {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := Stage}}}} ->
            Rects = dgiot_product_knova:get_nodes(Stage, [<<"Rect">>, <<"Image">>, <<"Text">>]),
            Realdatas =
                maps:fold(
                    fun
                        (NodeId, #{<<"source">> := <<"mqtt">>, <<"type">> := <<"realdata">>, <<"screen_productid">> := ProductId}, Acc) ->
                            Len = size(NodeId) - 16,
                            case NodeId of
                                <<DeviceId:10/binary, "_", Identifier:Len/binary, "_text">> ->
                                    List = maps:get(ProductId, Acc, #{}),
                                    Keys = maps:get(<<"keys">>, List, []),
                                    DeviceIds = maps:get(<<"deviceids">>, List, []),
                                    Acc#{ProductId => List#{<<"keys">> => lists:umerge(Keys, [Identifier]), <<"deviceids">> => lists:umerge(DeviceIds, [DeviceId])}};
                                _ ->
                                    Acc
                            end;
                        (_, _, Acc) ->
                            Acc
                    end, #{}, Rects),
            maps:fold(
                fun
                    (_, #{<<"source">> := <<"mqtt">>, <<"type">> := <<"realdata">>}, Acc) ->
                        Acc;
                    (NodeId, #{<<"source">> := <<"mqtt">>, <<"type">> := NodeType}, Acc) ->
                        Acc ++ [{NodeType, NodeId}];
                    (_, _, Acc) ->
                        Acc
                end, [{<<"realdata">>, Realdatas}], Rects);
        _ ->
            []
    end.

get_konva(#{<<"Stage">> := #{<<"children">> := [#{<<"children">> := LayerChildren} = Layer | _]} = Stage} = Konva) ->
    NewSubChildren =
        lists:foldl(
            fun
                (#{<<"className">> := <<"Label">>, <<"children">> := LabelChild} = X, Acc) ->
                    NewLabelChild = get_labelchild(LabelChild),
                    Acc ++ [X#{<<"children">> => NewLabelChild}];
                (X, Acc) ->
                    Acc ++ [X]
            end, [], LayerChildren),
    Konva#{<<"Stage">> => Stage#{<<"children">> => [Layer#{<<"children">> => NewSubChildren}]}};
get_konva(Konva) ->
    Konva.

get_labelchild(LabelChild) when length(LabelChild) > 1 ->
    case lists:nth(1, LabelChild) of
        #{<<"className">> := <<"Text">>, <<"attrs">> := #{<<"id">> := Id}} = TextChild ->
            case lists:nth(2, LabelChild) of
                #{<<"className">> := <<"Tag">>, <<"attrs">> := TagAttrs} = TagChild ->
                    [TagChild#{<<"attrs">> := TagAttrs#{<<"id">> => <<Id/binary, "_tag">>}}, TextChild];
                _ ->
                    LabelChild
            end;
        #{<<"className">> := <<"Tag">>, <<"attrs">> := TagAttrs} = TagChild ->
            case lists:nth(2, LabelChild) of
                #{<<"className">> := <<"Text">>, <<"attrs">> := #{<<"id">> := Id}} = TextChild ->
                    [TagChild#{<<"attrs">> := TagAttrs#{<<"id">> => <<Id/binary, "_tag">>}}, TextChild];
                _ ->
                    LabelChild
            end;
        _ ->
            LabelChild
    end;

get_labelchild(LabelChild) ->
    LabelChild.
