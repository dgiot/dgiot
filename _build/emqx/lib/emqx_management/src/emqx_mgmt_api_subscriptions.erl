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

-module(emqx_mgmt_api_subscriptions).

-include_lib("emqx/include/emqx.hrl").

-import(minirest, [return/1]).

-rest_api(#{name   => list_subscriptions,
            method => 'GET',
            path   => "/subscriptions/",
            func   => list,
            descr  => "A list of subscriptions in the cluster"}).

-rest_api(#{name   => list_node_subscriptions,
            method => 'GET',
            path   => "/nodes/:atom:node/subscriptions/",
            func   => list,
            descr  => "A list of subscriptions on a node"}).

-rest_api(#{name   => lookup_client_subscriptions,
            method => 'GET',
            path   => "/subscriptions/:bin:clientid",
            func   => lookup,
            descr  => "A list of subscriptions of a client"}).

-rest_api(#{name   => lookup_client_subscriptions_with_node,
            method => 'GET',
            path   => "/nodes/:atom:node/subscriptions/:bin:clientid",
            func   => lookup,
            descr  => "A list of subscriptions of a client on the node"}).

-export([ list/2
        , lookup/2
        ]).

list(Bindings, Params) when map_size(Bindings) == 0 ->
    %%TODO: across nodes?
    list(#{node => node()}, Params);

list(#{node := Node}, Params) when Node =:= node() ->
    return({ok, emqx_mgmt_api:paginate(emqx_suboption, Params, fun format/1)});

list(#{node := Node} = Bindings, Params) ->
    case rpc:call(Node, ?MODULE, list, [Bindings, Params]) of
        {badrpc, Reason} -> return({error, Reason});
        Res -> Res
    end.

lookup(#{node := Node, clientid := ClientId}, _Params) ->
    case ets:lookup(emqx_subid, http_uri:decode(ClientId)) of
        [] ->
            return({ok, []});
        [{_, Pid}] ->
            return({ok, format(emqx_mgmt:lookup_subscriptions(Node, Pid))})
    end;

lookup(#{clientid := ClientId}, _Params) ->
    case ets:lookup(emqx_subid, http_uri:decode(ClientId)) of
        [] ->
            return({ok, []});
        [{_, Pid}] ->
            return({ok, format(emqx_mgmt:lookup_subscriptions(Pid))})
    end.

format(Items) when is_list(Items) ->
    [format(Item) || Item <- Items];

format({{Subscriber, Topic}, Options}) ->
    format({Subscriber, Topic, Options});

format({_Subscriber, Topic, Options = #{share := Group}}) ->
    QoS = maps:get(qos, Options),
    #{node => node(), topic => filename:join([<<"$share">>, Group, Topic]), clientid => maps:get(subid, Options), qos => QoS};
format({_Subscriber, Topic, Options}) ->
    QoS = maps:get(qos, Options),
    #{node => node(), topic => Topic, clientid => maps:get(subid, Options), qos => QoS}.
