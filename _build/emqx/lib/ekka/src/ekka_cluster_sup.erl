%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(ekka_cluster_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% API
-export([start_child/2, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(M, Args) ->
    supervisor:start_child(?MODULE, child_spec(M, Args)).

child_spec(M, Args) ->
    {M, {M, start_link, Args}, permanent, 5000, worker, [M]}.

stop_child(M) ->
    case supervisor:terminate_child(?MODULE, M) of
        ok -> supervisor:delete_child(?MODULE, M);
        {error, not_found} -> ok;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    Childs = case ekka:env(cluster_discovery) of
                 {ok, {mcast, Options}} ->
                     Mcast = #{id       => ekka_cluster_mcast,
                               start    => {ekka_cluster_mcast, start_link, [Options]},
                               restart  => permanent,
                               shutdown => 5000,
                               type     => worker,
                               modules  => [ekka_cluster_mcast]
                              },
                     [Mcast];
                 _Other -> []
             end,
    {ok, {{one_for_one, 10, 100}, Childs}}.

