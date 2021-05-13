%% -------------------------------------------------------------------
%%
%% cuttlefish_advanced: handles merging of advanced configs
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(cuttlefish_advanced).

-export([overlay/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% @doc this function overlays the values in proplist 'AdvancedConfig'
%% on top of 'GeneratedConfig'
overlay(GeneratedConfig, AdvancedConfig) ->
    lists:foldl(
        fun({ApplicationName, ApplicationConfig}, OuterAcc) ->
            GeneratedApplicationConfig = proplists:get_value(ApplicationName, GeneratedConfig, []),
            Updated = lists:foldl(
                fun({ConfigElementName, ConfigElement}, Acc) ->
                    cuttlefish_util:replace_proplist_value(ConfigElementName, ConfigElement, Acc)
                end,
                GeneratedApplicationConfig,
                ApplicationConfig),
            cuttlefish_util:replace_proplist_value(ApplicationName, Updated, OuterAcc)
        end,
        GeneratedConfig,
        AdvancedConfig).


-ifdef(TEST).

overlay_test() ->
    GeneratedConfig = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1', [{"blah", "blah"}, {"blarg", "blarg"}]}]}
    ],

    AdvancedConfig = [
        {app3, [{'setting3.1', i_dont_care}]},
        {app4, [{'some_unschemad_thing', 'like_a_penguin'}]}
    ],

    Expected = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1', i_dont_care}]},
        {app4, [{'some_unschemad_thing', 'like_a_penguin'}]}
    ],
    NewConfig = overlay(GeneratedConfig, AdvancedConfig),

    ?assertEqual(Expected, NewConfig),

    ok.

-endif.
