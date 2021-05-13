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

-module(esockd_dtls_acceptor_sup).

-behaviour(supervisor).

-export([start_link/5]).

-export([ start_acceptor/2
        , count_acceptors/1
        ]).

-export([init/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(start_link(pid(), esockd:sock_fun(), [esockd:sock_fun()], fun(), fun()) -> {ok, pid()}).
start_link(ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun) ->
    supervisor:start_link(?MODULE, [ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]).

%% @doc Start a acceptor
-spec(start_acceptor(pid(), ssl:sslsocket()) -> {ok, pid()} | {error, term()}).
start_acceptor(Sup, LSock) ->
    supervisor:start_child(Sup, [LSock]).

%% @doc Count acceptors.
-spec(count_acceptors(Sup :: pid()) -> pos_integer()).
count_acceptors(Sup) ->
    proplists:get_value(active, supervisor:count_children(Sup), 0).

init([ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 100,
                 period => 3600
                },
    Acceptor = #{id => acceptor,
                 start => {esockd_dtls_acceptor, start_link,
                           [ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]},
                 restart => transient,
                 shutdown => 1000,
                 type => worker,
                 modules => [esockd_dtls_acceptor]
                },
    {ok, {SupFlags, [Acceptor]}}.

