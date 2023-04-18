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

-module(dgiot_grpc_client).

-export([login/1, login/2, logout/1, send/2]).

login(ClinetId) ->
    login(ClinetId, "http://127.0.0.1:30051").

login(ClinetId, SvrAddr) ->
    {ok, _} = grpc_client_sup:create_channel_pool(ClinetId, SvrAddr, #{}).

logout(ClinetId) ->
    _ = grpc_client_sup:stop_channel_pool(ClinetId).

send(ClinetId, Map) when is_map(Map) ->
    case dgiot_dlink_client:payload(#{name => base64:encode(jsx:encode(Map))}, #{channel => ClinetId}) of
        {ok, #{message := ReMessage}, _} ->
            {ok, ReMessage};
        _ ->
            error
    end;

send(_, _) ->
    pass.
