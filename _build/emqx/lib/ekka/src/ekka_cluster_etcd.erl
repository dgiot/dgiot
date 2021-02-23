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

-module(ekka_cluster_etcd).

-behaviour(ekka_cluster_strategy).

-export([ discover/1
        , lock/1
        , unlock/1
        , register/1
        , unregister/1
        ]).

%% TTL callback
-export([etcd_set_node_key/1]).

-define(LOG(Level, Format, Args), logger:Level("Ekka(etcd): " ++ Format, Args)).

%%--------------------------------------------------------------------
%% ekka_cluster_strategy callbacks
%%--------------------------------------------------------------------

discover(Options) ->
    case etcd_get_nodes_key(Options) of
        {ok, Response} ->
            {ok, extract_nodes(Response)};
        {error, {404, _}} ->
            case ensure_nodes_path(Options) of
                {ok, _} -> discover(Options);
                Error   -> Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

lock(Options) ->
    lock(Options, 10).

lock(_Options, 0) ->
    {error, failed};

lock(Options, Retries) ->
    case etcd_set_lock_key(Options) of
        {ok, _Response} -> ok;
        {error, {412, _}} ->
            timer:sleep(1000),
            lock(Options, Retries -1);
        {error, Reason} ->
            {error, Reason}
    end.

unlock(Options) ->
    case etcd_del_lock_key(Options) of
        {ok, _Response} -> ok;
        {error, Reason} ->
            {error, Reason}
    end.

register(Options) ->
    case etcd_set_node_key(Options) of
        {ok, _Response} ->
            ensure_node_ttl(Options);
        {error, Reason} ->
            {error, Reason}
    end.

unregister(Options) ->
    ok = ekka_cluster_sup:stop_child(ekka_node_ttl),
    case etcd_del_node_key(Options) of
        {ok, _Response} -> ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

extract_nodes([]) ->
    [];
extract_nodes(Response) ->
    [extract_node(V) || V <- maps:get(<<"nodes">>, maps:get(<<"node">>, Response), [])].

ensure_node_ttl(Options) ->
    Ttl = proplists:get_value(node_ttl, Options),
    MFA = {?MODULE, etcd_set_node_key, [Options]},
    case ekka_cluster_sup:start_child(ekka_node_ttl, [Ttl, MFA]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Err = {error, _} -> Err
    end.

extract_node(V) ->
    list_to_atom(binary_to_list(lists:last(binary:split(maps:get(<<"key">>, V), <<"/">>, [global])))).

ensure_nodes_path(Options) ->
    etcd_set(server(Options), nodes_path(Options), [{dir, true}], ssl_options(Options)).

etcd_get_nodes_key(Options) ->
    etcd_get(server(Options), nodes_path(Options), [{recursive, true}], ssl_options(Options)).

etcd_set_node_key(Options) ->
    Ttl = config(node_ttl, Options) div 1000,
    etcd_set(server(Options), node_path(Options), [{ttl, Ttl}], ssl_options(Options)).

etcd_del_node_key(Options) ->
    etcd_del(server(Options), node_path(Options), [], ssl_options(Options)).

etcd_set_lock_key(Options) ->
    Values = [{ttl, 30}, {'prevExist', false}, {value, node()}],
    etcd_set(server(Options), lock_path(Options), Values, ssl_options(Options)).

etcd_del_lock_key(Options) ->
    Values = [{'prevExist', true}, {'prevValue', node()}],
    etcd_del(server(Options), lock_path(Options), Values, ssl_options(Options)).

server(Options) ->
    config(server, Options).

ssl_options(Options) ->
    case proplists:get_value(ssl_options, Options, []) of
        [] -> [];
        SSLOptions -> [{ssl, SSLOptions}]
    end.

config(Key, Options) ->
    proplists:get_value(Key, Options).

etcd_get(Servers, Key, Params, HttpOpts) ->
    ekka_httpc:get(rand_addr(Servers), Key, Params, HttpOpts).

etcd_set(Servers, Key, Params, HttpOpts) ->
    ekka_httpc:put(rand_addr(Servers), Key, Params, HttpOpts).

etcd_del(Servers, Key, Params, HttpOpts) ->
    ekka_httpc:delete(rand_addr(Servers), Key, Params, HttpOpts).

nodes_path(Options) ->
    with_prefix(config(prefix, Options), "/nodes").

node_path(Options) ->
    with_prefix(config(prefix, Options), "/nodes/" ++ atom_to_list(node())).

lock_path(Options) ->
    with_prefix(config(prefix, Options), "/lock").

with_prefix(Prefix, Path) ->
    Cluster = atom_to_list(ekka:env(cluster_name, ekka)),
    lists:concat(["v2/keys/", Prefix, "/", Cluster, Path]).

rand_addr([Addr]) ->
    Addr;
rand_addr(AddrList) ->
    lists:nth(rand:uniform(length(AddrList)), AddrList).

