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

-module(dgiot_product_hook).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post/2, put/3, delete/3]).

post('befor', BeforeData) ->
    case BeforeData of
        #{<<"objectId">> := ProductId, <<"channel">> := Channel} ->
            io:format("~s ~p  Channel = ~p.~n", [?FILE, ?LINE, Channel]),
            TdchannelId = maps:get(<<"tdchannel">>, Channel, <<"">>),
            TaskchannelId = maps:get(<<"taskchannel">>, Channel, <<"">>),
            Otherchannel = maps:get(<<"otherchannel">>, Channel, []),
            dgiot_product:add_product_relation(Otherchannel ++ [TdchannelId] ++ [TaskchannelId], ProductId);
        _ ->
            pass
    end;

post('after', AfterData) ->
    case AfterData of
        #{<<"producttemplet">> := #{<<"className">> := <<"ProductTemplet">>, <<"objectId">> := ProducttempletId, <<"__type">> := <<"Pointer">>}, <<"objectId">> := ProductId} ->
            io:format("~s ~p  ProductId = ~p.~n", [?FILE, ?LINE, ProductId]),
            case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"key">> => ProducttempletId, <<"class">> => <<"ProductTemplet">>}}) of
                {ok, #{<<"results">> := Dicts}} when length(Dicts) > 0 ->
                    dgiot_product_dict:post_batch(Dicts, ProductId);
                _ ->
                    pass
            end,
            case dgiot_parse:query_object(<<"View">>, #{<<"where">> => #{<<"key">> => ProducttempletId, <<"class">> => <<"ProductTemplet">>}}) of
                {ok, #{<<"results">> := Views}} when length(Views) > 0 ->
                    dgiot_product_view:post_batch(Views, ProductId);
                _ ->
                    dgiot_product_konva:post(ProductId)
            end;
        _ ->
            pass
    end.

put('befor', BeforeData, ProductId) ->
    case BeforeData of
        #{<<"channel">> := Channel} ->
            dgiot_product:delete_product_relation(ProductId),
            TdchannelId = maps:get(<<"tdchannel">>, Channel, <<"">>),
            TaskchannelId = maps:get(<<"taskchannel">>, Channel, <<"">>),
            Otherchannel = maps:get(<<"otherchannel">>, Channel, []),
            dgiot_product:add_product_relation(Otherchannel ++ [TdchannelId] ++ [TaskchannelId], ProductId);
        _ ->
            pass
    end;

put('after', _AfterData, _ProductId) ->
    %%    todo
    pass.


delete('befor', _BeforeData, _ProductId) ->
%%    todo
    pass;

delete('after', AfterData, ProductId) ->
    case AfterData of
        #{<<"objectId">> := ProductId} ->
            case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"key">> => ProductId, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Dicts}} ->
                    dgiot_product_dict:delete_batch(Dicts);
                _ ->
                    pass
            end,
            case dgiot_parse:query_object(<<"View">>, #{<<"where">> => #{<<"key">> => ProductId, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Views}} ->
                    dgiot_product_view:delete_batch(Views);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.
