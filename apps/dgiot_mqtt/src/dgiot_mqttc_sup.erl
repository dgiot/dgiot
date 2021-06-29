%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 John liu <34489690@qq.com>.
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

%% @doc shuwa_zeta supervisor
-module(shuwa_mqttc_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_sup/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, transient, 5000, Type, [I]}).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link(Name, Mod) ->
    supervisor:start_link({local, Name}, ?MODULE, [Mod]).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([Mod]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        ?CHILD(Mod, worker, [])
    ]}}.


start_sup(Name, Mod) ->
    case whereis(Name) of
        undefined ->
            Args = [Name, Mod],
            supervisor:start_child(shuwa_devcenter_sup, {Name, {shuwa_mqttc_sup, start_link, Args}, permanent, 5000, supervisor, [Name]});
        Pid ->
            {ok, Pid}
    end.


