%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_rule_engine_sup).

-behaviour(supervisor).

-include_lib("emqx_rule_engine/include/rule_engine.hrl").

-export([start_link/0]).

-export([start_locker/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Registry = #{id => dgiot_rule_registry,
                 start => {dgiot_rule_registry, start_link, []},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker,
                 modules => [dgiot_rule_registry]},
    {ok, {{one_for_one, 10, 10}, [Registry]}}.

start_locker() ->
    Locker = #{id => dgiot_rule_locker,
               start => {dgiot_rule_locker, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [dgiot_rule_locker]},
    supervisor:start_child(?MODULE, Locker).
