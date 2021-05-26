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

-module(dgiot_cm_locker).

-include("dgiot.hrl").
-include("types.hrl").

-export([start_link/0]).

-export([ trans/2
        , trans/3
        , lock/1
        , lock/2
        , unlock/1
        ]).

-spec(start_link() -> startlink_ret()).
start_link() ->
    ekka_locker:start_link(?MODULE).

-spec(trans(dgiot_types:clientid(), fun(([node()]) -> any())) -> any()).
trans(ClientId, Fun) ->
    trans(ClientId, Fun, undefined).

-spec(trans(maybe(dgiot_types:clientid()),
            fun(([node()])-> any()), ekka_locker:piggyback()) -> any()).
trans(undefined, Fun, _Piggyback) ->
    Fun([]);
trans(ClientId, Fun, Piggyback) ->
    case lock(ClientId, Piggyback) of
        {true, Nodes} ->
            try Fun(Nodes) after unlock(ClientId) end;
        {false, _Nodes} ->
            {error, client_id_unavailable}
    end.

-spec(lock(dgiot_types:clientid()) -> ekka_locker:lock_result()).
lock(ClientId) ->
    ekka_locker:acquire(?MODULE, ClientId, strategy()).

-spec(lock(dgiot_types:clientid(), ekka_locker:piggyback()) -> ekka_locker:lock_result()).
lock(ClientId, Piggyback) ->
    ekka_locker:acquire(?MODULE, ClientId, strategy(), Piggyback).

-spec(unlock(dgiot_types:clientid()) -> {boolean(), [node()]}).
unlock(ClientId) ->
    ekka_locker:release(?MODULE, ClientId, strategy()).

-spec(strategy() -> local | leader | quorum | all).
strategy() ->
    dgiot:get_env(session_locking_strategy, quorum).

