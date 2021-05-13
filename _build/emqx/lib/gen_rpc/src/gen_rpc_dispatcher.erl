%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Dispatcher is a serialization trick to prevent starting up
%%% multiple children for one connection

-module(gen_rpc_dispatcher).

%%% Behaviour
-behaviour(gen_server).

%%% Include the HUT library
-include("logger.hrl").
%%% Include this library's name macro
-include("app.hrl").
%%% Include helpful guard macros
-include("guards.hrl").
%%% Include helpful guard macros
-include("types.hrl").

%%% Supervisor functions
-export([start_link/0, stop/0]).

%%% Server functions
-export([start_client/1]).

%%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%%% ===================================================
%%% Public API
%%% ===================================================
-spec start_link() -> gen_server:startlink_ret().
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE, normal, infinity).

-spec start_client(node_or_tuple()) -> {ok, pid()} | {error, any()}.
start_client(NodeOrTuple) when ?is_node_or_tuple(NodeOrTuple) ->
    gen_server:call(?MODULE, {start_client,NodeOrTuple}, infinity).

%%% ===================================================
%%% Behaviour callbacks
%%% ===================================================
init([]) ->
    ?log(info, "event=start"),
    {ok, undefined}.

%% Simply launch a connection to a node through the appropriate
%% supervisor. This is a serialization interface so that
handle_call({start_client, NodeOrTuple}, _Caller, undefined) ->
    PidName = gen_rpc_helper:make_process_name("client", NodeOrTuple),
    Reply = case erlang:whereis(PidName) of
        undefined ->
            ?log(debug, "message=start_client event=starting_client_server target=\"~p\"", [NodeOrTuple]),
            gen_rpc_client_sup:start_child(NodeOrTuple);
        Pid ->
            ?log(debug, "message=start_client event=node_already_started target=\"~p\"", [NodeOrTuple]),
            {ok, Pid}
    end,
    {reply, Reply, undefined};

%% Catch-all for calls - die if we get a message we don't expect
handle_call(Msg, _Caller, undefined) ->
    ?log(error, "event=uknown_call_received message=\"~p\" action=stopping", [Msg]),
    {stop, {unknown_call, Msg}, undefined}.

%% Catch-all for casts - die if we get a message we don't expect
handle_cast(Msg, undefined) ->
    ?log(error, "event=uknown_cast_received message=\"~p\" action=stopping", [Msg]),
    {stop, {unknown_cast, Msg}, undefined}.

%% Catch-all for info - our protocol is strict so die!
handle_info(Msg, undefined) ->
    ?log(error, "event=uknown_message_received message=\"~p\" action=stopping", [Msg]),
    {stop, {unknown_info, Msg}, undefined}.

code_change(_OldVersion, undefined, _Extra) ->
    {ok, undefined}.

terminate(_Reason, undefined) ->
    ok.
