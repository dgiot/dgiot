%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_app).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(application).

%%% Include this library's name macro
-include("app.hrl").

%%% Application callbacks
-export([start/2, stop/1]).

%%% Development start/stop functions
-export([start/0, stop/0]).

%%% ===================================================
%%% Application callbacks
%%% ===================================================
-spec start(application:start_type(), term()) -> {error,any()} | {ok,pid()} | {ok,pid(),any()}.
start(_StartType, _StartArgs) ->
    gen_rpc_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%% ===================================================
%%% Development functions
%%% ===================================================
-spec start() -> ok | {error, term()}.
start() ->
    application:start(?APP).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?APP).
