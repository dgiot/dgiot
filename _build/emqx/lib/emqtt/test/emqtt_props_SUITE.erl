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

-module(emqtt_props_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("emqtt.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("emqx_ct_helpers/include/emqx_ct.hrl").

all() -> emqx_ct:all(?MODULE).

t_id(_) ->
    foreach_prop(
      fun({Id, Prop}) ->
              ?assertEqual(Id, emqtt_props:id(element(1, Prop)))
      end),
    ?catch_error({bad_property, 'Bad-Property'}, emqtt_props:id('Bad-Property')).

t_name(_) ->
    foreach_prop(
      fun({Id, Prop}) ->
              ?assertEqual(emqtt_props:name(Id), element(1, Prop))
      end),
    ?catch_error({unsupported_property, 16#FF}, emqtt_props:name(16#FF)).

t_filter(_) ->
    PubProps = #{'Payload-Format-Indicator' => 6,
                 'Message-Expiry-Interval' => 300,
                 'Session-Expiry-Interval' => 300,
                 'User-Property' => {<<"username">>, <<"test">>}
                },
    ?assertEqual(#{'Payload-Format-Indicator' => 6,
                   'Message-Expiry-Interval' => 300,
                   'User-Property' => {<<"username">>, <<"test">>}
                  },
                 emqtt_props:filter(?PUBLISH, PubProps)),

    BadProps = #{'Unknown-Property' => 10},
    ?catch_error({bad_property,'Unknown-Property'},
                 emqtt_props:filter(?PUBLISH, BadProps)).

t_validate(_) ->
    ConnProps = #{'Payload-Format-Indicator' => 1,
                  'Server-Keep-Alive' => 20,
                  'Session-Expiry-Interval' => 1,
                  'Subscription-Identifier' => 1,
                  'Correlation-Data' => <<"test">>,
                  'Maximum-Packet-Size' => 255,
                  'User-Property' => [{<<"username">>, <<"test">>}]
                 },
    ok = emqtt_props:validate(ConnProps),

    BadProps = #{'Unknown-Property' => 10},
    ?catch_error({bad_property,'Unknown-Property'},
                 emqtt_props:validate(BadProps)).

foreach_prop(Fun) ->
    lists:foreach(Fun, maps:to_list(emqtt_props:all())).

