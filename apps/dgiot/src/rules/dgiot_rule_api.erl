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

-module(dgiot_rule_api).
-author("dgiot").

-export([
    show_rule/2,
    create_rule/2,
    update_rule/2,
    delete_rule/2,
    list_actions/2,
    show_action/2,
    list_resources/2,
    show_resource/2,
    create_resource/2,
    delete_resource/2
]).

show_rule(Params, _Opts) ->
    local_rule:show(Params).

create_rule(Params, _Opts) ->
    local_rule:create(Params).

update_rule(Params, _Opts) ->
    local_rule:update(Params).

delete_rule(Params, _Opts) ->
    local_rule:delete(Params).

list_actions(_Params, _Opts) ->
    {ok, []}.

show_action(_Params, _Opts) ->
    {error, not_found}.

list_resources(_Params, _Opts) ->
    {ok, []}.

show_resource(_Params, _Opts) ->
    {error, not_found}.

create_resource(Params, _Opts) ->
    local_rule:create_resource(Params).

delete_resource(Params, _Opts) ->
    local_rule:delete_resource(Params).
