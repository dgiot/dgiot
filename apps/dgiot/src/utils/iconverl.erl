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

-module(iconverl).
-author("johnliu").
-on_load(load_nif/0).

-export([open/2, conv/2, conv/3]).

-opaque cd() :: binary().
-export_type([cd/0]).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec open(string(), string()) -> cd().
open(_To, _From) ->
    erlang:nif_error(not_loaded).

-spec conv(cd(), binary()) -> {ok, binary()} | {error, atom()}.
conv(_CD, _Binary) ->
    erlang:nif_error(not_loaded).

-spec conv(string(), string(), binary()) -> {ok, binary()} | {error, atom()}.
conv(To, From, Binary) ->
    conv(open(To, From), Binary).

%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------

load_nif() ->
    case os:type() of
        {win32, _} ->
            ok;
        _ ->
            erlang:load_nif(filename:join(code:priv_dir(dgiot), atom_to_list(?MODULE)), 0)
    end.