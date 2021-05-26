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

-module(dgiot_sup).

-author("johnliu").
-include("dgiot.hrl").

-behaviour(supervisor).

-include("types.hrl").

-export([start_link/0
    , start_child/1
    , start_child/2
    , start_child/3
    , stop_child/1
]).

-export([init/1]).

-type(startchild_ret() :: {ok, supervisor:child()}
| {ok, supervisor:child(), term()}
| {error, term()}).

-define(SUP, ?MODULE).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(start_link() -> startlink_ret()).
start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

-spec(start_child(supervisor:child_spec()) -> startchild_ret()).
start_child(ChildSpec) when is_map(ChildSpec) ->
    supervisor:start_child(?SUP, ChildSpec).

-spec(start_child(module(), worker | supervisor) -> startchild_ret()).
start_child(Mod, Type) ->
    start_child(child_spec(Mod, Type, [])).


-spec(start_child(module(), worker | supervisor, list()) -> startchild_ret()).
start_child(Mod, Type, Args) ->
    start_child(child_spec(Mod, Type, Args)).

-spec(stop_child(supervisor:child_id()) -> ok | {error, term()}).
stop_child(ChildId) ->
    case supervisor:terminate_child(?SUP, ChildId) of
        ok -> supervisor:delete_child(?SUP, ChildId);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    KernelSup = child_spec(dgiot_kernel_sup, supervisor, []),
    MnesiaSup = child_spec(dgiot_mnesia_sup, supervisor, []),
    CMSup = child_spec(dgiot_cm_sup, supervisor, []),
    Childs = [KernelSup]
        ++ [MnesiaSup]
        ++ [CMSup],
    SupFlags = #{strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    {ok, {SupFlags, Childs}}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

child_spec(Mod, supervisor, Args) ->
    #{id => Mod,
        start => {Mod, start_link, Args},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [Mod]
    };

child_spec(Mod, worker, Args) ->
    #{id => Mod,
        start => {Mod, start_link, Args},
        restart => permanent,
        shutdown => 15000,
        type => worker,
        modules => [Mod]
    }.
