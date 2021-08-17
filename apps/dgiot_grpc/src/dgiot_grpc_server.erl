%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_grpc Protocol
-module(dgiot_grpc_server).
-include("dgiot_grpc.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
]).

-behavior(routeguide_route_guide_bhvr).

-compile(export_all).
-compile(nowarn_export_all).

%%--------------------------------------------------------------------
%% Callbacks

get_feature(Request, _Md) ->
    ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Request]),
    {ok, #{}, _Md}.

list_features(Stream, _Md) ->
    {eos, [Request], NStream} = grpc_stream:recv(Stream),
    ?LOG(info,"~p: ~0p~n", [?FUNCTION_NAME, Request]),

    grpc_stream:reply(Stream, [#{name => "City1", location => #{latitude => 1, longitude => 1}}]),
    grpc_stream:reply(Stream, [#{name => "City2", location => #{latitude => 2, longitude => 2}}]),
    grpc_stream:reply(Stream, [#{name => "City3", location => #{latitude => 3, longitude => 3}}]),
    {ok, NStream}.

record_route(Stream, _Md) ->
    LoopRecv = fun _Lp(St, Acc) ->
        case grpc_stream:recv(St) of
            {more, Reqs, NSt} ->
                ?LOG(info,"~p: ~0p~n", [?FUNCTION_NAME, Reqs]),

                _Lp(NSt, Acc ++ Reqs);
            {eos, Reqs, NSt} ->
                ?LOG(info,"~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                {NSt, Acc ++ Reqs}
        end
               end,
    {NStream, Points} = LoopRecv(Stream, []),
    grpc_stream:reply(NStream, #{point_count => length(Points)}),
    {ok, NStream}.

route_chat(Stream, _Md) ->
    grpc_stream:reply(Stream, [#{name => "City1", location => #{latitude => 1, longitude => 1}}]),
    grpc_stream:reply(Stream, [#{name => "City2", location => #{latitude => 2, longitude => 2}}]),
    LoopRecv = fun _Lp(St) ->
        case grpc_stream:recv(St) of
            {more, Reqs, NSt} ->
                ?LOG(info,"~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                _Lp(NSt);
            {eos, Reqs, NSt} ->
                ?LOG(info,"~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                NSt
        end
               end,
    NStream = LoopRecv(Stream),
    grpc_stream:reply(NStream, [#{name => "City3", location => #{latitude => 3, longitude => 3}}]),
    {ok, NStream}.
