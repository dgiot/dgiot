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

-module(dgiot_app).

-emqx_plugin(?MODULE).

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

-define(APP, dgiot).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_Type, _Args) ->
    print_banner(),
    {ok, Sup} = dgiot_sup:start_link(),
    register(dgiot, self()),
    print_vsn(),
    {ok, Sup}.

-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Print Banner
%%--------------------------------------------------------------------

print_banner() ->
    io:format("Starting ~s on node ~s~n", [?APP, node()]).

print_vsn() ->
    {ok, Descr} = application:get_key(description),
    {ok, Vsn} = application:get_key(vsn),
    io:format("~s ~s is running now!~n", [Descr, Vsn]).

%%--------------------------------------------------------------------
%% Autocluster
%%--------------------------------------------------------------------

