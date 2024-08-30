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

-export([get_product/2, get_products/2, get_keys/4, check_field/3, test_product/0]).
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
    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"nodeType">> := 2}} ->
            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
            case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"product">> => ProductId}}) of
                {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
                    [#{<<"objectId">> := Devid} | _] = Results,
                    case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 1000, <<"keys">> => [<<"product">>], <<"where">> => #{<<"parentId">> => Devid}}) of
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
    case dgiot_parsex:query_object(<<"Channel">>, Body, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
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
    case dgiot_data:get(?DGIOT_CHANNEL_SESSION, Session) of
        not_find ->
            case dgiot_parsex:query_object(<<"Channel">>, Body, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
                {ok, #{<<"results">> := []}} ->
                    {error, <<"not find channel">>};
                {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId}]}} ->
                    dgiot_data:insert(?DGIOT_CHANNEL_SESSION, Session, ChannelId),
                    {ok, ChannelId};
                {error, Reason} ->
                    {error, Reason}
            end;
        ChannelId ->
            {ok, ChannelId}
    end.

spell_sql(_ProductId, _Function, [], {Names, Acc}) ->
    {Names, Acc};
spell_sql(ProductId, Function, [Key | Keys], {Names, Acc}) ->
    case dgiot_product:get_product_identifier(ProductId, Key) of
        #{<<"identifier">> := Identifier, <<"name">> := Name, <<"isstorage">> := Isstorage} when Isstorage > 0 ->
            {NewNames, NewAcc} =
                case Acc of
                    <<>> ->
                        {Names ++ [Name], <<Acc/binary, Function/binary, "(", Identifier/binary, ") ", Identifier/binary>>};
                    _ ->
                        {Names ++ [Name], <<Acc/binary, ", ", Function/binary, "(", Identifier/binary, ") ", Identifier/binary>>}
                end,
            spell_sql(ProductId, Function, Keys, {NewNames, NewAcc});
        _ ->
            {Names, Acc}
    end.

get_keys(ProductId, TableName, Function, <<"*">>) ->
    case dgiot_product:get_keys(ProductId) of
        Keys when length(Keys) > 0 ->
            spell_sql(ProductId, Function, Keys, {[], get_defult(TableName, Function)});
        _ ->
            {[], <<"*">>}
    end;

get_keys(ProductId, TableName, Function, Keys) when Keys == undefined; Keys == <<>> ->
    get_keys(ProductId, TableName, Function, <<"*">>);

get_keys(ProductId, TableName, Function, Keys) ->
    List =
        case is_list(Keys) of
            true -> Keys;
            false -> re:split(Keys, <<",">>)
        end,
    case dgiot_product:get_keys(ProductId) of
        TdKeys when length(TdKeys) > 0 ->
            lists:foldl(fun(Key, {Names, Acc}) ->
                case TdKeys -- [Key] of
                    TdKeys ->
                        {Names, Acc};
                    _ ->
                        case dgiot_product:get_product_identifier(ProductId, Key) of
                            #{<<"identifier">> := Identifier, <<"name">> := Name, <<"isstorage">> := Isstorage} when Isstorage > 0 ->
                                case Acc of
                                    <<>> ->
                                        {Names ++ [Name], <<Acc/binary, Function/binary, "(", Identifier/binary, ") ", Identifier/binary>>};
                                    _ ->
                                        {Names ++ [Name], <<Acc/binary, ", ", Function/binary, "(", Identifier/binary, ") ", Identifier/binary>>}
                                end;
                            _ ->
                                {Names, Acc}
                        end
                end
                        end, {[], get_defult(TableName, Function)}, List);
        _ ->
            {[], <<"*">>}
    end.

%%    秒转换为分钟
check_field(_, V, #{<<"specs">> := #{<<"type">> := <<"minutes">>}}) ->
    dgiot_utils:to_float(V / 60, 2);

%%    秒转换为小时
check_field(_, V, #{<<"specs">> := #{<<"type">> := <<"hour">>}}) ->
    dgiot_utils:to_float(V / (60 * 60), 2);

%%    秒转换为天
check_field(_, V, #{<<"specs">> := #{<<"type">> := <<"day">>}}) ->
    dgiot_utils:to_float(V / (60 * 60 * 24), 2);

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
                    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
                        {ok, #{<<"location">> := #{<<"__type">> := <<"GeoPoint">>, <<"longitude">> := Longitude, <<"latitude">> := Latitude}}} ->
                            pass;
                        {ok, #{<<"detail">> := Detail}} ->
                            dgiot_parsex:update_object(<<"Device">>, DeviceId, #{
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

get_product_data(Channel, ProductId, DeviceId, Args) ->
    Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
    ?LOG(info, "Channel ~p Args ~p", [Channel, Args]),
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            pass;
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

get_defult(TableName, <<"first">>) ->
    <<"first(", TableName/binary, ".createdat) createdat">>;
get_defult(TableName, <<"last">>) ->
    <<"last(", TableName/binary, ".createdat) createdat">>;
get_defult(_TableName, <<"count">>) ->
    <<"">>;
get_defult(_TableName, <<"avg">>) ->
    <<"">>;
get_defult(_TableName, <<"sum">>) ->
    <<"">>;
get_defult(_TableName, <<"stddev">>) ->
    <<"">>;
get_defult(TableName, _) ->
    <<"(", TableName/binary, ".createdat) createdat">>.
