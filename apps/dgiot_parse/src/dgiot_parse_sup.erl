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

-module(dgiot_parse_sup).
-behaviour(supervisor).
-include("dgiot_parse.hrl").
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, transient, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        ?CHILD(dgiot_parse_loader_sup, supervisor, []),
        dgiot_channelx:spec(parse_channelx)
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.


