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

-module(dgiot_product_amis).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post/1]).

%%dgiot_product_amis:post(<<"d0cb711d3d">>).
post(ProductId) ->
    dgiot_parse:create_object(<<"View">>, #{
        <<"title">> => ProductId,
        <<"key">> => ProductId,
        <<"type">> => <<"amis">>,
        <<"class">> => <<"Product">>,
        <<"data">> => dgiot_utils:get_JsonFile(?MODULE,<<"amis.json">>)
    }).
