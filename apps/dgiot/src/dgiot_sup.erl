%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-behaviour(supervisor).

-include("types.hrl").
-include("dgiot.hrl").

-logger_header("[Supervisor]").

-export([start_link/0
    , start_child/1
    , start_child/2
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
    start_child(child_spec(Mod, Type)).

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
    Childs = get_child_supervisor() ,
    io:format(" ~p ~n", [Childs]),
    SupFlags = #{strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    {ok, {SupFlags, Childs}}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

child_spec(Mod, supervisor) ->
    #{id => Mod,
        start => {Mod, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [Mod]
    };

child_spec(Mod, worker) ->
    #{id => Mod,
        start => {Mod, start_link, []},
        restart => permanent,
        shutdown => 15000,
        type => worker,
        modules => [Mod]
    }.

get_child_supervisor() ->
    Specs = lists:foldl(fun({_Name, Mod, [Order | _]}, Acc) ->
        case lists:member({behaviour, [supervisor]}, Mod:module_info(attributes)) of
            true ->
                Acc ++ [{Order, child_spec(Mod, supervisor)}];
            _ ->
                case lists:member({behaviour, [worker]}, Mod:module_info(attributes)) of
                    true -> Acc ++ [{Order, child_spec(Mod, supervisor)}];
                    false -> Acc
                end
        end
                        end, [], lists:sort(ekka_boot:all_module_attributes(dgiot_supervisor))),
    lists:foldl(fun({_,Spec}, Acc1) ->
        Acc1 ++ [Spec]
                end, [], lists:keysort(1, Specs)).


