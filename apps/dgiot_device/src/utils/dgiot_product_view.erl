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

-module(dgiot_product_view).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post_batch/2, delete_batch/1, post/1]).

post_batch(Views, ProductId) ->
    ViewRequests =
        lists:foldl(fun(View, Acc) ->
            NewDict = maps:without([<<"createdAt">>, <<"objectId">>, <<"updatedAt">>], View),
            Type = maps:get(<<"type">>, View, <<"">>),
            Title = maps:get(<<"title">>, View, <<"">>),
            ViewId = dgiot_parse_id:get_viewid(ProductId, Type, <<"Product">>, Title),
            Acc ++ [#{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"/classes/View">>,
                <<"body">> => NewDict#{
                    <<"objectId">> => ViewId,
                    <<"key">> => ProductId,
                    <<"class">> => <<"Product">>}
            }]
                    end, [], Views),
    dgiot_parse:batch(ViewRequests).

delete_batch(Views) ->
    ViewRequests =
        lists:foldl(fun(#{<<"objectId">> := ViewId}, Acc) ->
            Acc ++ [#{
                <<"method">> => <<"DELETE">>,
                <<"path">> => <<"/classes/View/", ViewId/binary>>,
                <<"body">> => #{}
            }]
                    end, [], Views),
    dgiot_parse:batch(ViewRequests).

%%dgiot_product_view:post(<<"d0cb711d3d">>).
post(ProductId) ->
    dgiot_parse:create_object(<<"View">>, #{
        <<"title">> => ProductId,
        <<"key">> => ProductId,
        <<"type">> => <<"topo">>,
        <<"class">> => <<"Product">>,
        <<"data">> =>  dgiot_utils:get_JsonFile(?MODULE,<<"knonva.json">>)
    }).
