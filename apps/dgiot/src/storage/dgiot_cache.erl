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


-module(dgiot_cache).
-author("johnliu").
%% API
-export([set/3, get/1, delete/1]).


set(Key, Value, TTLInSeconds) ->
    dgiot_cache_worker:set(Key, Value, TTLInSeconds).

get(Key) ->
    dgiot_cache_worker:get(Key).

delete(Key) ->
    dgiot_cache_worker:delete(Key).
