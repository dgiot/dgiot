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

-module(task_loader).
-author("johnliu").
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").

-behaviour(gen_server).
%% API
-export([start_link/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).


-define(PAGE_SIZE, 100).
-define(PAGE_INDEX, 1).
-define(MAX, 0).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  erlang:send_after(100, self(), load),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({deliver, _Topic, Msg}, State) ->
  _Payload = binary_to_term(dgiot_mqtt:get_payload(Msg)),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
