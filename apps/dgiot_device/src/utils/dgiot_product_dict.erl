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

-module(dgiot_product_dict).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post_batch/2, delete_batch/1, post/1]).

post_batch(Dicts, ProductId) ->
    DictRequests =
        lists:foldl(fun(Dict, Acc) ->
            NewDict = maps:without([<<"createdAt">>, <<"objectId">>, <<"updatedAt">>], Dict),
            Type = maps:get(<<"type">>, Dict, <<"">>),
            Title = maps:get(<<"title">>, Dict, <<"">>),
            DictId = dgiot_parse_id:get_dictid(ProductId, Type, <<"Product">>, Title),
            Acc ++ [#{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"/classes/Dict">>,
                <<"body">> => NewDict#{
                    <<"objectId">> => DictId,
                    <<"key">> => ProductId,
                    <<"class">> => <<"Product">>}
            }]
                    end, [], Dicts),
    dgiot_parse:batch(DictRequests).

delete_batch(Dicts) ->
    DictRequests =
        lists:foldl(fun(#{<<"objectId">> := DictId}, Acc) ->
            Acc ++ [#{
                <<"method">> => <<"DELETE">>,
                <<"path">> => <<"/classes/Dict/", DictId/binary>>,
                <<"body">> => #{}
            }]
                    end, [], Dicts),
    dgiot_parse:batch(DictRequests).

post(ProductId) ->
    NewConfig = #{
        <<"konva">> => #{
            <<"Stage">> => #{
                <<"attrs">> => #{
                    <<"width">> => <<"1200">>,
                    <<"height">> => <<"700">>},
                <<"className">> => <<"Stage">>,
                <<"children">> => [#{
                    <<"attrs">> => #{
                        <<"id">> => <<"Layer_Thing">>},
                    <<"className">> => <<"Layer">>,
                    <<"children">> => [#{
                        <<"attrs">> => #{
                            <<"id">> => <<"bg">>,
                            <<"type">> => <<"bg-image">>,
                            <<"width">> => <<"1200">>,
                            <<"height">> => <<"700">>,
                            <<"src">> => <<"//img7.ddove.com/upload/20181127/134600237598.jpg?timestamp=1635422987361">>},
                        <<"className">> => <<"Image">>}]}]}}},
    dgiot_parse:create_object(<<"View">>, #{
        <<"title">> => ProductId,
        <<"key">> => ProductId,
        <<"type">> => <<"topo">>,
        <<"class">> => <<"Product">>,
        <<"data">> => NewConfig
    }).
