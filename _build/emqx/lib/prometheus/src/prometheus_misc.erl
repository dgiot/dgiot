%% @hidden
%% behaviour_modules original Copyright message
%% all other code is under MIT
%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(prometheus_misc).

-export([behaviour_modules/1]).

%%====================================================================
%% Public API
%%====================================================================

-spec behaviour_modules(Behaviour :: atom()) -> [atom()] | [].
behaviour_modules(Behaviour) ->
  [Module || {Module, Behaviours} <-
               all_module_attributes(behaviour),
             lists:member(Behaviour, Behaviours)].

%%====================================================================
%% Private Parts
%%====================================================================

all_module_attributes(Name) ->
  Targets =
    lists:usort(
      lists:append(
        [[{App, Module} || Module <- Modules] ||
          {App, _, _}   <- application:loaded_applications(),
          {ok, Modules} <- [application:get_key(App, modules)]])),
  lists:foldl(
    fun ({_App, Module}, Acc) ->
        case lists:append([Atts || {N, Atts} <- module_attributes(Module),
                                   N =:= Name]) of
          []   -> Acc;
          Atts -> [{Module, Atts} | Acc]
        end
    end, [], Targets).

module_attributes(Module) ->
  case catch Module:module_info(attributes) of
    {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
      [];
    {'EXIT', _Reason} ->
      %% return empty list too
      [];
    V ->
      V
  end.
