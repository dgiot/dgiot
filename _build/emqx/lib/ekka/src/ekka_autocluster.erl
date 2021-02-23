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

-module(ekka_autocluster).

-include("ekka.hrl").

-export([enabled/0 , run/1, unregister_node/0]).

-export([acquire_lock/1, release_lock/1]).

-define(LOG(Level, Format, Args),
        logger:Level("Ekka(AutoCluster): " ++ Format, Args)).

-spec(enabled() -> boolean()).
enabled() ->
    case ekka:env(cluster_discovery) of
        {ok, {manual, _}} -> false;
        {ok, _Strategy}   -> true;
        undefined         -> false
    end.

-spec(run(atom()) -> any()).
run(App) ->
    case acquire_lock(App) of
        ok ->
            spawn(fun() ->
                      group_leader(whereis(init), self()),
                      wait_application_ready(App, 10),
                      try
                          discover_and_join()
                      catch
                          _:Error:Stacktrace ->
                              ?LOG(error, "Discover error: ~p~n~p", [Error, Stacktrace])
                      after
                          release_lock(App)
                      end,
                      maybe_run_again(App)
                  end);
        failed -> ignore
    end.

wait_application_ready(_App, 0) ->
    timeout;
wait_application_ready(App, Retries) ->
    case ekka_node:is_running(App) of
        true  -> ok;
        false -> timer:sleep(1000),
                 wait_application_ready(App, Retries - 1)
    end.

maybe_run_again(App) ->
    %% Check if the node joined cluster?
    case ekka_mnesia:is_node_in_cluster() of
        true  -> ok;
        false -> timer:sleep(5000),
                 run(App)
    end.

-spec(discover_and_join() -> any()).
discover_and_join() ->
    with_strategy(
      fun(Mod, Options) ->
        case Mod:lock(Options) of
            ok ->
                discover_and_join(Mod, Options),
                log_error("Unlock", Mod:unlock(Options));
            ignore ->
                timer:sleep(rand:uniform(3000)),
                discover_and_join(Mod, Options);
            {error, Reason} ->
                ?LOG(error, "AutoCluster stopped for lock error: ~p", [Reason])
        end
      end).

-spec(unregister_node() -> ok).
unregister_node() ->
    with_strategy(
      fun(Mod, Options) ->
          log_error("Unregister", Mod:unregister(Options))
      end).

-spec(acquire_lock(atom()) -> ok | failed).
acquire_lock(App) ->
    case application:get_env(App, autocluster_lock) of
        undefined ->
            application:set_env(App, autocluster_lock, true);
        {ok, _} -> failed
    end.

-spec(release_lock(atom()) -> ok).
release_lock(App) ->
    application:unset_env(App, autocluster_lock).

with_strategy(Fun) ->
    case ekka:env(cluster_discovery) of
        {ok, {manual, _}} ->
            ignore;
        {ok, {Strategy, Options}} ->
            Fun(strategy_module(Strategy), Options);
        undefined ->
            ignore
    end.

strategy_module(Strategy) ->
    case code:is_loaded(Strategy) of
        {file, _} -> Strategy; %% Provider?
        false     -> list_to_atom("ekka_cluster_" ++  atom_to_list(Strategy))
    end.

discover_and_join(Mod, Options) ->
    case Mod:discover(Options) of
        {ok, Nodes} ->
            maybe_join([N || N <- Nodes, ekka_node:is_aliving(N)]),
            log_error("Register", Mod:register(Options));
        {error, Reason} ->
            ?LOG(error, "Discovery error: ~p", [Reason])
    end.

maybe_join([]) ->
    ignore;
maybe_join(Nodes) ->
    case ekka_mnesia:is_node_in_cluster() of
        true  -> ignore;
        false -> join_with(find_oldest_node(Nodes))
    end.

join_with(false) ->
    ignore;
join_with(Node) when Node =:= node() ->
    ignore;
join_with(Node) ->
    ekka_cluster:join(Node).

find_oldest_node([Node]) ->
    Node;
find_oldest_node(Nodes) ->
    case rpc:multicall(Nodes, ekka_membership, local_member, []) of
        {ResL, []} ->
            case [M || M <- ResL, is_record(M, member)] of
                [] -> ?LOG(error, "Bad members found on nodes ~p: ~p", [Nodes, ResL]),
                      false;
                Members ->
                    (ekka_membership:oldest(Members))#member.node
            end;
        {ResL, BadNodes} ->
            ?LOG(error, "Bad nodes found: ~p, ResL: ", [BadNodes, ResL]), false
   end.

log_error(Format, {error, Reason}) ->
    ?LOG(error, Format ++ " error: ~p", [Reason]);
log_error(_Format, _Ok) -> ok.

