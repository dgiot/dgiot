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

-module(emqx_mgmt_api_clients).

-include("emqx_mgmt.hrl").

-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("emqx/include/emqx.hrl").

-import(minirest, [ return/0
                  , return/1
                  ]).

-rest_api(#{name   => list_clients,
            method => 'GET',
            path   => "/clients/",
            func   => list,
            descr  => "A list of clients on current node"}).

-rest_api(#{name   => list_node_clients,
            method => 'GET',
            path   => "nodes/:atom:node/clients/",
            func   => list,
            descr  => "A list of clients on specified node"}).

-rest_api(#{name   => lookup_client,
            method => 'GET',
            path   => "/clients/:bin:clientid",
            func   => lookup,
            descr  => "Lookup a client in the cluster"}).

-rest_api(#{name   => lookup_node_client,
            method => 'GET',
            path   => "nodes/:atom:node/clients/:bin:clientid",
            func   => lookup,
            descr  => "Lookup a client on the node"}).

-rest_api(#{name   => lookup_client_via_username,
            method => 'GET',
            path   => "/clients/username/:bin:username",
            func   => lookup,
            descr  => "Lookup a client via username in the cluster"
           }).

-rest_api(#{name   => lookup_node_client_via_username,
            method => 'GET',
            path   => "/nodes/:atom:node/clients/username/:bin:username",
            func   => lookup,
            descr  => "Lookup a client via username on the node "
           }).

-rest_api(#{name   => kickout_client,
            method => 'DELETE',
            path   => "/clients/:bin:clientid",
            func   => kickout,
            descr  => "Kick out the client in the cluster"}).

-rest_api(#{name   => clean_acl_cache,
            method => 'DELETE',
            path   => "/clients/:bin:clientid/acl_cache",
            func   => clean_acl_cache,
            descr  => "Clear the ACL cache of a specified client in the cluster"}).

-rest_api(#{name   => list_acl_cache,
            method => 'GET',
            path   => "/clients/:bin:clientid/acl_cache",
            func   => list_acl_cache,
            descr  => "List the ACL cache of a specified client in the cluster"}).

-import(emqx_mgmt_util, [ ntoa/1
                        , strftime/1
                        ]).

-export([ list/2
        , lookup/2
        , kickout/2
        , clean_acl_cache/2
        , list_acl_cache/2
        ]).

list(Bindings, Params) when map_size(Bindings) == 0 ->
    list(#{node => node()}, Params);

list(#{node := Node}, Params) when Node =:= node() ->
    return({ok, emqx_mgmt_api:paginate(emqx_channel, Params, fun format/1)});

list(Bindings = #{node := Node}, Params) ->
    case rpc:call(Node, ?MODULE, list, [Bindings, Params]) of
        {badrpc, Reason} -> return({error, ?ERROR1, Reason});
        Res -> Res
    end.

lookup(#{node := Node, clientid := ClientId}, _Params) ->
    return({ok, emqx_mgmt:lookup_client(Node, {clientid, http_uri:decode(ClientId)}, fun format/1)});

lookup(#{clientid := ClientId}, _Params) ->
    return({ok, emqx_mgmt:lookup_client({clientid, http_uri:decode(ClientId)}, fun format/1)});

lookup(#{node := Node, username := Username}, _Params) ->
    return({ok, emqx_mgmt:lookup_client(Node, {username, http_uri:decode(Username)}, fun format/1)});

lookup(#{username := Username}, _Params) ->
    return({ok, emqx_mgmt:lookup_client({username, http_uri:decode(Username)}, fun format/1)}).

kickout(#{clientid := ClientId}, _Params) ->
    case emqx_mgmt:kickout_client(http_uri:decode(ClientId)) of
        ok -> return();
        {error, not_found} -> return({error, ?ERROR12, not_found});
        {error, Reason} -> return({error, ?ERROR1, Reason})
    end.

clean_acl_cache(#{clientid := ClientId}, _Params) ->
    case emqx_mgmt:clean_acl_cache(http_uri:decode(ClientId)) of
        ok -> return();
        {error, not_found} -> return({error, ?ERROR12, not_found});
        {error, Reason} -> return({error, ?ERROR1, Reason})
    end.

list_acl_cache(#{clientid := ClientId}, _Params) ->
    case emqx_mgmt:list_acl_cache(http_uri:decode(ClientId)) of
        {error, not_found} -> return({error, ?ERROR12, not_found});
        {error, Reason} -> return({error, ?ERROR1, Reason});
        Caches -> return({ok, [format_acl_cache(Cache) || Cache <- Caches]})
    end.

format([]) ->
    [];
format(Items) when is_list(Items) ->
    [format(Item) || Item <- Items];
format(Key) when is_tuple(Key) ->
    format(emqx_mgmt:item(client, Key));

format(Data) when is_map(Data)->
    {IpAddr, Port} = maps:get(peername, Data),
    ConnectedAt = maps:get(connected_at, Data),
    CreatedAt = maps:get(created_at, Data),
    Data1 = maps:without([peername], Data),
    maps:merge(Data1#{node         => node(),
                      ip_address   => iolist_to_binary(ntoa(IpAddr)),
                      port         => Port,
                      connected_at => iolist_to_binary(strftime(ConnectedAt)),
                      created_at   => iolist_to_binary(strftime(CreatedAt))},
               case maps:get(disconnected_at, Data, undefined) of
                   undefined -> #{};
                   DisconnectedAt -> #{disconnected_at => iolist_to_binary(strftime(DisconnectedAt))}
               end).

format_acl_cache({{PubSub, Topic}, {AclResult, Timestamp}}) ->
    #{access => PubSub,
      topic => Topic,
      result => AclResult,
      updated_time => Timestamp}.
