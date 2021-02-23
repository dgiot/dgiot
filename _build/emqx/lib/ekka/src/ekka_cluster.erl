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

%% @doc Cluster via Mnesia database.
-module(ekka_cluster).

-export([info/0, info/1]).

%% Cluster API
-export([ join/1
        , leave/0
        , force_leave/1
        , status/1
        ]).

%% RPC call for Cluster Management
-export([ prepare/1
        , heal/1
        , reboot/0
        ]).

-type(info_key() :: running_nodes | stopped_nodes).

-type(infos() :: #{running_nodes := list(node()),
                   stopped_nodes := list(node())
                  }).

-export_type([info_key/0, infos/0]).

-spec(info(atom()) -> list(node())).
info(Key) -> maps:get(Key, info()).

-spec(info() -> infos()).
info() -> ekka_mnesia:cluster_info().

%% @doc Cluster status of the node.
status(Node) -> ekka_mnesia:cluster_status(Node).

%% @doc Join the cluster
-spec(join(node()) -> ok | ignore | {error, term()}).
join(Node) when Node =:= node() ->
    ignore;
join(Node) when is_atom(Node) ->
    case {ekka_mnesia:is_node_in_cluster(Node), ekka_node:is_running(Node, ekka)} of
        {false, true} ->
            prepare(join), ok = ekka_mnesia:join_cluster(Node), reboot();
        {false, false} ->
            {error, {node_down, Node}};
        {true, _} ->
            {error, {already_in_cluster, Node}}
    end.

%% @doc Leave from the cluster.
-spec(leave() -> ok | {error, any()}).
leave() ->
    case ekka_mnesia:running_nodes() -- [node()] of
        [_|_] ->
            prepare(leave), ok = ekka_mnesia:leave_cluster(), reboot();
        [] ->
            {error, node_not_in_cluster}
    end.

%% @doc Force a node leave from cluster.
-spec(force_leave(node()) -> ok | ignore | {error, term()}).
force_leave(Node) when Node =:= node() ->
    ignore;
force_leave(Node) ->
    case ekka_mnesia:is_node_in_cluster(Node)
         andalso rpc:call(Node, ?MODULE, prepare, [leave]) of
        ok ->
            case ekka_mnesia:remove_from_cluster(Node) of
                ok    -> rpc:call(Node, ?MODULE, reboot, []);
                Error -> Error
            end;
        false ->
            {error, node_not_in_cluster};
        {badrpc, nodedown} ->
            ekka_membership:announce({force_leave, Node}),
            ekka_mnesia:remove_from_cluster(Node);
        {badrpc, Reason} ->
            {error, Reason}
    end.

%% @doc Heal partitions
-spec(heal(shutdown | reboot) -> ok | {error, term()}).
heal(shutdown) ->
    prepare(heal), ekka_mnesia:ensure_stopped();
heal(reboot) ->
    ekka_mnesia:ensure_started(), reboot().

%% @doc Prepare to join or leave the cluster.
-spec(prepare(join | leave) -> ok | {error, term()}).
prepare(Action) ->
    ekka_membership:announce(Action),
    case ekka:callback(prepare) of
        {ok, Prepare} -> Prepare(Action);
        undefined     -> application:stop(ekka)
    end.

%% @doc Reboot after join or leave cluster.
-spec(reboot() -> ok | {error, term()}).
reboot() ->
    case ekka:callback(reboot) of
        {ok, Reboot} -> Reboot();
        undefined    -> ekka:start()
    end.

