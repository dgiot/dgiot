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
-author("johnliu").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post/2, put/2, delete/2]).

post('before', _QueryData) ->
    pass;

post('after', #{<<"objectId">> := ProductId, <<"channel">> := Channel} = QueryData) ->
    TdchannelId = maps:get(<<"tdchannel">>, Channel, <<"">>),
    TaskchannelId = maps:get(<<"taskchannel">>, Channel, <<"">>),
    Otherchannel = maps:get(<<"otherchannel">>, Channel, <<"">>),
    dgiot_product:add_product_relation([Otherchannel] ++ [TdchannelId] ++ [TaskchannelId], ProductId),
    post('after', maps:without([<<"channel">>], QueryData));

post('after', #{<<"objectId">> := ProductId, <<"producttemplet">> := #{<<"objectId">> := ProducttempletId}} = _QueryData) ->
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
            dgiot_product_knova:post(ProductId),
            dgiot_product_amis:post(ProductId)
    end;

post(_, _) ->
    pass.

put('before', _QueryData) ->
    pass;

put('after', #{<<"channel">> := Channel, <<"objectId">> := ProductId}) ->
    dgiot_product:delete_product_relation(ProductId),
    TdchannelId = maps:get(<<"tdchannel">>, Channel, <<"">>),
    TaskchannelId = maps:get(<<"taskchannel">>, Channel, <<"">>),
    Otherchannel = maps:get(<<"otherchannel">>, Channel, <<"">>),
    dgiot_product:add_product_relation([Otherchannel] ++ [TdchannelId] ++ [TaskchannelId], ProductId);

put(_, _) ->
    pass.

delete('before', _ProductId) ->
%%    todo
    pass;

delete('after', ProductId) ->
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

delete(_, _) ->
    pass.

