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

-module(emqx_mgmt_api_alarms).

-include("emqx_mgmt.hrl").

-include_lib("emqx/include/emqx.hrl").

-rest_api(#{name   => list_all_alarms,
            method => 'GET',
            path   => "/alarms/present",
            func   => list_present,
            descr  => "List all alarms"}).

-rest_api(#{name   => list_node_alarms,
            method => 'GET',
            path   => "/alarms/present/:atom:node",
            func   => list_present,
            descr  => "List alarms of a node"}).

-rest_api(#{name   => list_all_alarm_history,
            method => 'GET',
            path   => "/alarms/history",
            func   => list_history,
            descr  => "List all alarm history"}).

-rest_api(#{name   => list_node_alarm_history,
            method => 'GET',
            path   => "/alarms/history/:atom:node",
            func   => list_history,
            descr  => "List alarm history of a node"}).

-export([ list_present/2
        , list_history/2
        ]).

list_present(Bindings, _Params) when map_size(Bindings) == 0 ->
    {ok, #{code => ?SUCCESS,
           data => [#{node => Node, alarms => format(Alarms)} || {Node, Alarms} <- emqx_mgmt:get_alarms(present)]}};

list_present(#{node := Node}, _Params) ->
    {ok, #{code => ?SUCCESS,
           data => format(emqx_mgmt:get_alarms(Node, present))}}.

list_history(Bindings, _Params) when map_size(Bindings) == 0 ->
    {ok, #{code => ?SUCCESS,
           data => [#{node => Node, alarms => format(Alarms)} || {Node, Alarms} <- emqx_mgmt:get_alarms(history)]}};

list_history(#{node := Node}, _Params) ->
    {ok, #{code => ?SUCCESS,
           data => format(emqx_mgmt:get_alarms(Node, history))}}.

format(Alarms) when is_list(Alarms) ->
    [format(Alarm) || Alarm <- Alarms];

format({AlarmId, #alarm{severity  = Severity, 
                        title     = Title,
                        summary   = Summary, 
                        timestamp = Ts}}) ->
    #{id   => maybe_to_binary(AlarmId),
      desc => #{severity  => Severity,
                title     => iolist_to_binary(Title),
                summary   => iolist_to_binary(Summary),
                timestamp => iolist_to_binary(emqx_mgmt_util:strftime(Ts))}};
format({AlarmId, AlarmDesc}) ->
    #{id   => maybe_to_binary(AlarmId),
      desc => maybe_to_binary(AlarmDesc)};
format({AlarmId, #alarm{severity  = Severity, 
                        title     = Title,
                        summary   = Summary, 
                        timestamp = Ts}, ClearAt}) ->
    #{id       => maybe_to_binary(AlarmId),
      desc     => #{severity  => Severity,
                    title     => iolist_to_binary(Title),
                    summary   => iolist_to_binary(Summary),
                    timestamp => iolist_to_binary(emqx_mgmt_util:strftime(Ts))},
      clear_at => iolist_to_binary(emqx_mgmt_util:strftime(ClearAt))};
format({AlarmId, AlarmDesc, ClearAt}) ->
    #{id       => maybe_to_binary(AlarmId),
      desc     => maybe_to_binary(AlarmDesc),
      clear_at => iolist_to_binary(emqx_mgmt_util:strftime(ClearAt))}.

maybe_to_binary(Data) when is_binary(Data) ->
    Data;
maybe_to_binary(Data) ->
    iolist_to_binary(io_lib:format("~p", [Data])).
