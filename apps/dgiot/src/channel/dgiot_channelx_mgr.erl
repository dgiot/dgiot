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


-module(dgiot_channelx_mgr).
-author("johnliu").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------


start_link(Server) ->
    supervisor:start_link({local, Server}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    AChild = {dgiot_channelx_sup, {dgiot_channelx_sup, start_link, []}, transient, 2000, supervisor, [dgiot_channelx_sup]},
    {ok, {{simple_one_for_one, 1000, 3600}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


