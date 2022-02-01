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

-module(dgiot_dlink_test).

-compile(export_all).
-compile(nowarn_export_all).

start() ->
    Services = #{protos => [dgiot_dlink_pb],
        services => #{'dgiot.Dlink' => dgiot_dlink_server}
    },
    {ok, _} = grpc:start_server(server, 30051, Services, []).

stop() ->
    _ = grpc:stop_server(server).

login() ->
    SvrAddr =  "http://127.0.0.1:30051",
    {ok, _} = grpc_client_sup:create_channel_pool(channel, SvrAddr, #{}).

logout() ->
    _ = grpc_client_sup:stop_channel_pool(channel).

send() ->
    dgiot_dlink_client:say_hello(#{name => <<"Xiao Ming">>}, #{channel => channel}).

