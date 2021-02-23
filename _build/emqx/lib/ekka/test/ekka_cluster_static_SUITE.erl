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

-module(ekka_cluster_static_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

t_discover(_) ->
    Options = [{seeds, ['ekka@127.0.0.1']}],
    {ok, ['ekka@127.0.0.1']} = ekka_cluster_static:discover(Options).

t_lock(_) ->
    ignore = ekka_cluster_static:lock([]).

t_unlock(_) ->
    ignore = ekka_cluster_static:unlock([]).

t_register(_) ->
    ignore = ekka_cluster_static:register([]).

t_unregister(_) ->
    ignore = ekka_cluster_static:unregister([]).

