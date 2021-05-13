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

-module(ekka_dist).

-export([listen/1,
         select/1,
         accept/1,
         accept_connection/5,
         setup/5,
         close/1,
         childspecs/0]).

-export([port/1]).

-define(DEFAULT_PORT, 4370).
-define(MAX_PORT_LIMIT, 60000).

listen(Name) ->
    %% Here we figure out what port we want to listen on.
    Port = port(Name),

    %% Set both "min" and "max" variables, to force the port number to
    %% this one.
    ok = application:set_env(kernel, inet_dist_listen_min, Port),
    ok = application:set_env(kernel, inet_dist_listen_max, Port),

    %% Finally run the real function!
    with_module(fun(M) -> M:listen(Name) end).

select(Node) ->
    with_module(fun(M) -> M:select(Node) end).

accept(Listen) ->
    with_module(fun(M) -> M:accept(Listen) end).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    with_module(fun(M) ->
                    M:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime)
                end).

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    with_module(fun(M) ->
                    M:setup(Node, Type, MyNode, LongOrShortNames, SetupTime)
                end).

close(Listen) ->
    with_module(fun(M) -> M:close(Listen) end).

childspecs() ->
    with_module(fun(M) -> M:childspecs() end).

with_module(Fun) ->
    Proto = application:get_env(ekka, proto_dist, inet_tcp),
    Fun(list_to_existing_atom(atom_to_list(Proto) ++ "_dist")).

%% @doc Figure out dist port from node's name.
-spec(port(node() | string()) -> inet:port_number()).
port(Name) when is_atom(Name) ->
    port(atom_to_list(Name));
port(Name) when is_list(Name) ->
    %% Figure out the base port.  If not specified using the
    %% inet_dist_base_port kernel environment variable, default to
    %% 4370, one above the epmd port.
    BasePort = application:get_env(kernel, inet_dist_base_port, ?DEFAULT_PORT),

    %% Now, figure out our "offset" on top of the base port.  The
    %% offset is the integer just to the left of the @ sign in our node
    %% name.  If there is no such number, the offset is 0.
    %%
    %% Also handle the case when no hostname was specified.
    BasePort + offset(Name).

%% @doc Figure out the offset by node's name
offset(NodeName) ->
    ShortName = re:replace(NodeName, "@.*$", ""),
    case re:run(ShortName, "[0-9]+$", [{capture, first, list}]) of
        nomatch ->
            0;
        {match, [OffsetAsString]} ->
            (list_to_integer(OffsetAsString) rem ?MAX_PORT_LIMIT)
    end.
