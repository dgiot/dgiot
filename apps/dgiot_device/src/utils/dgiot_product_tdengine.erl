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

-module(dgiot_product_tdengine).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([get_product/2, get_products/2, get_keys/3, check_field/3, test_product/0]).
-export([get_channel/1, do_channel/3, get_product_data/4]).

test_product() ->
    ProductId = <<"0765bee775">>,
    Query = #{
        <<"keys">> => [<<"last_row(createdat)">>],
        <<"group">> => <<"devaddr">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"createdat">> => #{
                <<"$gte">> => <<"now - 10d">>
            }
        }
    },
    get_product(ProductId, Query).

get_product(ProductId, Query) ->
    case dgiot_data:get({ProductId, ?TYPE}) of
        not_find -> [];
        ChannelId ->
            TableName = ?Table(ProductId),
            case dgiot_tdengine:query_object(ChannelId, TableName, Query#{<<"db">> => ProductId}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_products(ProductId, ChannelId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"nodeType">> := 2}} ->
            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
            case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"product">> => ProductId}}) of
                {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
                    [#{<<"objectId">> := Devid} | _] = Results,
                    case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1000, <<"keys">> => [<<"product">>], <<"where">> => #{<<"parentId">> => Devid}}) of
                        {ok, #{<<"results">> := R}} ->
                            lists:foldl(fun(#{<<"product">> := Product}, Acc) ->
                                #{<<"objectId">> := SubProductId} = Product,
%%                                ?LOG(info, "SubProductId ~p ChannelId ~p", [SubProductId, ChannelId]),
                                dgiot_data:insert({SubProductId, ?TYPE}, ChannelId),
                                Acc ++ [SubProductId]
                                        end, [ProductId], R);
                        _ -> [ProductId]
                    end;
                _ -> [ProductId]
            end;
        _ ->
            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
            [ProductId]
    end.

do_channel(ProductId, Session, Fun) ->
    Body = #{
        <<"keys">> => <<"objectId">>,
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"cType">> => ?TYPE,
            <<"isEnable">> => true,
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"Product">>,
                <<"objectId">> => ProductId
            }
        }
    },
    case dgiot_parse:query_object(<<"Channel">>, Body, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, #{<<"results">> := []}} ->
            {404, <<"not find channel">>};
        {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId}]}} ->
            Fun(ChannelId);
        {error, Reason} ->
            {error, Reason}
    end.

get_channel(Session) ->
    Body = #{
        <<"keys">> => <<"objectId">>,
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"cType">> => <<"TD">>,
            <<"isEnable">> => true
        }
    },
    case dgiot_parse:query_object(<<"Channel">>, Body, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, #{<<"results">> := []}} ->
            {error, <<"not find channel">>};
        {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId}]}} ->
            {ok, ChannelId};
        {error, Reason} ->
            {error, Reason}
    end.

get_keys(ProductId, Function, <<"*">>) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} when length(Props) > 0 ->
            lists:foldl(fun(X, {Names, Acc}) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"name">> := Name, <<"isstorage">> := true, <<"isshow">> := true} ->
                        {Names ++ [Name], <<Acc/binary, ", ", Function/binary, "(", Identifier/binary, ") ", Identifier/binary>>};
                    _ ->
                        {Names, Acc}
                end
                        end, {[], <<Function/binary, "(createdat) createdat">>}, Props);
        _Other ->
            ?LOG(debug, "~p _Other ~p", [ProductId, _Other]),
            {[], <<"*">>}
    end;

get_keys(ProductId, Function, Keys) when Keys == undefined; Keys == <<>> ->
    get_keys(ProductId, Function, <<"*">>);

get_keys(ProductId, Function, Keys) ->
    List =
        case is_list(Keys) of
            true -> Keys;
            false -> re:split(Keys, <<",">>)
        end,
    Maps = get_prop(ProductId),
    lists:foldl(fun(X, {Names, Acc}) ->
        case maps:find(X, Maps) of
            error ->
                {Names, Acc};
            Name ->
                {Names ++ [Name], <<Acc/binary, ", ", Function/binary, "(", X/binary, ") ", X/binary>>}
        end
                end, {[], <<Function/binary, "(createdat) createdat">>}, List).

check_field(Typea, V, #{<<"specs">> := Specs}) when Typea == <<"enum">>; Typea == <<"bool">> ->
    maps:get(dgiot_utils:to_binary(V), Specs, V);

check_field(<<"struct">>, V, _) ->
    V;

check_field(<<"geopoint">>, V, #{<<"deviceid">> := DeviceId}) ->
    BinV = dgiot_utils:to_binary(V),
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
    end;

check_field(Typea, V, #{<<"specs">> := Specs}) when Typea == <<"float">>; Typea == <<"double">> ->
    Precision = maps:get(<<"precision">>, Specs, 3),
    dgiot_utils:to_float(V, Precision);

check_field(<<"image">>, V, #{<<"datatype">> := DataType, <<"deviceid">> := DeviceId}) ->
    AppName = dgiot_device:get_appname(DeviceId),
    Url = dgiot_device:get_url(AppName),
    Imagevalue = maps:get(<<"imagevalue">>, DataType, <<"">>),
    BinV = dgiot_utils:to_binary(V),
    <<Url/binary, "/dgiot_file/", DeviceId/binary, "/", BinV/binary, ".", Imagevalue/binary>>;

check_field(<<"date">>, V, _) ->
    case V of
        <<"1970-01-01 08:00:00.000">> ->
            <<"--">>;
        _ ->
            dgiot_utils:to_binary(V)
    end;

check_field(_Typea, V, _) ->
    dgiot_utils:to_binary(V).

get_prop(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"name">> := Name} ->
                        Acc#{Identifier => Name};
                    _ -> Acc
                end
                        end, #{}, Props);
        _ ->
            []
    end.


get_product_data(Channel, ProductId, DeviceId, Args) ->
    Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
    ?LOG(info, "Channel ~p Args ~p", [Channel, Args]),
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            Keys =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        <<"count(*)">> -> Acc ++ [<<"count(*)">>];
                        <<"*">> -> Acc ++ [<<"values">>];
                        _ -> Acc ++ [<<"values.", X/binary>>]
                    end
                            end, [], binary:split(maps:get(<<"keys">>, Args, <<"count(*)">>), <<$,>>, [global, trim])),
            Query2 = Query#{<<"where">> => #{<<"product">> => ProductId,
                <<"device">> => DeviceId}, <<"keys">> => Keys},
            ?LOG(info, "Query2 ~p", [Query2]),
            case dgiot_parse:query_object(<<"Timescale">>, Query2) of
                {ok, #{<<"results">> := Results} = All} when length(Results) > 0 ->
                    Data = lists:foldl(fun(X, Acc) ->
                        Acc ++ [maps:get(<<"values">>, X)]
                                       end, [], Results),
                    {ok, All#{<<"results">> => Data}};
                {ok, #{<<"results">> := Result}} ->
                    {error, Result};
                Error ->
                    Error
            end;
        _ ->
            Where = maps:get(<<"where">>, Args),
            Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
            TableName = ?Table(DeviceId),
            case dgiot_tdengine:query_object(Channel, TableName, Query#{
                <<"db">> => ProductId,
                <<"where">> => Where
            }) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {400, Reason}
            end
    end.
