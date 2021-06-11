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

-module(dgiot_types).

-include("dgiot.hrl").
-include("dgiot_mnesia.hrl").
-include("types.hrl").

-export_type([
    attrs/0
    , infos/0
    , stats/0
]).

-export_type([oom_policy/0]).

-export_type([
    key/0
    , value/0
    , mnesia/0
    ]).

-type(attrs() :: #{atom() => term()}).
-type(infos() :: #{atom() => term()}).
-type(stats() :: [{atom(), term()}]).

-type(oom_policy() :: #{message_queue_len => non_neg_integer(),
max_heap_size => non_neg_integer()
}).

-type(key() :: binary() | list() | tuple()).
-type(value() :: node() | {binary(), node()} | {map(), node()} | {list(), node()}).
-type(mnesia() :: #mnesia{}).
