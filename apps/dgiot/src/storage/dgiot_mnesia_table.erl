%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_mnesia_table).
-author("dgiot").
-export([create_table/2, copy_table/2, running_nodes/0]).

-define(HAS_EKKA, code:is_loaded(ekka_mnesia) =:= {file, _} orelse
                   code:ensure_loaded(ekka_mnesia) =:= {module, ekka_mnesia}).

ensure_mnesia_started() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        _ ->
            ok = mnesia:start()
    end.

create_table(Name, Opts) ->
    ensure_mnesia_started(),
    try ekka_mnesia:create_table(Name, Opts)
    catch _:_ -> mnesia:create_table(Name, Opts)
    end.

copy_table(Name, Type) ->
    try ekka_mnesia:copy_table(Name, Type)
    catch _:_ -> {atomic, ok}
    end.

running_nodes() ->
    try ekka_mnesia:running_nodes()
    catch _:_ -> mnesia:system_info(running_db_nodes)
    end.
