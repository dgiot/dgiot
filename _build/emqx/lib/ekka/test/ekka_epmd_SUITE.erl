%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(ekka_epmd_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

t_start_link(_) ->
    ignore = ekka_epmd:start_link().

t_register_node(_) ->
    {ok, X} = ekka_epmd:register_node('n@127.0.0.1', 4370),
    ?assert(is_integer(X) and (1 =< X) and (X =< 3)).

t_port_please(_) ->
    ?assertEqual({port, 4370, 5}, ekka_epmd:port_please('n@127.0.0.1', {127,0,0,1})),
    ?assertEqual({port, 4370, 5}, ekka_epmd:port_please('n0@127.0.0.1', {127,0,0,1})),
    ?assertEqual({port, 4371, 5}, ekka_epmd:port_please('n1@127.0.0.1', {127,0,0,1})),
    ?assertEqual({port, 4372, 5}, ekka_epmd:port_please('n2@127.0.0.1', {127,0,0,1})),
    ?assertEqual({port, 4470, 5}, ekka_epmd:port_please('n100@127.0.0.1', {127,0,0,1})).

t_names(_) ->
    ?assertEqual({error, address}, ekka_epmd:names()).

