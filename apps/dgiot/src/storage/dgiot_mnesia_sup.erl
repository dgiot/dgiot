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

-module(dgiot_mnesia_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Router helper
    Helper = #{id       => helper,
               start    => {dgiot_mnesia_helper, start_link, []},
               restart  => permanent,
               shutdown => 5000,
               type     => worker,
               modules  => [dgiot_mnesia_helper]},

    ok = persistent_term:put(dgiot_mnesia_lock_type,
                             dgiot:get_env(mnesia_lock_type, key)
                            ),

    %% Mnesia pool
    MnesiaPool = dgiot_pool_sup:spec([mnesia_pool, hash,
                                     {dgiot_mnesia, start_link, []}]),
    {ok, {{one_for_all, 0, 1}, [Helper, MnesiaPool]}}.

