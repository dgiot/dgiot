%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(minirest).

-export([ start_http/3
        , start_https/3
        , stop_http/1
        ]).

-export([handler/1]).

-export([ return/0
        , return/1
        ]).

%% Cowboy callback
-export([init/2]).

-define(SUCCESS, 0).

-type(option() :: {authorization, fun()}).
-type(handler() :: {string(), mfa()} | {string(), mfa(), list(option())}).

-export_type([ option/0
             , handler/0
             ]).

%%------------------------------------------------------------------------------
%% Start/Stop Http
%%------------------------------------------------------------------------------

-spec(start_http(atom(), list(), list()) -> {ok, pid()}).
start_http(ServerName, Options, Handlers) ->
    Dispatch = cowboy_router:compile([{'_', handlers(Handlers)}]),
    {ok, _} = cowboy:start_clear(ServerName, Options, #{env => #{dispatch => Dispatch}}),
    io:format("Start ~s listener on ~p successfully.~n", [ServerName, get_port(Options)]).

-spec(start_https(atom(), list(), list()) -> {ok, pid()}).
start_https(ServerName, Options, Handlers) ->
    Dispatch = cowboy_router:compile([{'_', handlers(Handlers)}]),
    {ok, _} = cowboy:start_tls(ServerName, Options, #{env => #{dispatch => Dispatch}}),
    io:format("Start ~s listener on ~p successfully.~n", [ServerName, get_port(Options)]).

-spec(stop_http(atom()) -> ok).
stop_http(ServerName) ->
    cowboy:stop_listener(ServerName).

get_port(#{socket_opts := SocketOpts}) ->
    proplists:get_value(port, SocketOpts, 18083).

map({Prefix, MFArgs}) ->
    map({Prefix, MFArgs, []});
map({Prefix, MFArgs, Options}) ->
    #{prefix => Prefix, mfargs => MFArgs, options => maps:from_list(Options)}.

handlers(Handlers) ->
    lists:map(fun
        ({Prefix, minirest, HHs}) -> {Prefix, minirest, [map(HH) || HH <- HHs]};
        (Handler) -> Handler
    end, Handlers).

%%------------------------------------------------------------------------------
%% Handler helper
%%------------------------------------------------------------------------------

-spec(handler(minirest_handler:config()) -> handler()).
handler(Config) -> minirest_handler:init(Config).

%%------------------------------------------------------------------------------
%% Cowboy callbacks
%%------------------------------------------------------------------------------

init(Req, Opts) ->
    Req1 = handle_request(Req, Opts),
    {ok, Req1, Opts}.

%% Callback
handle_request(Req, Handlers) ->
    case match_handler(binary_to_list(cowboy_req:path(Req)), Handlers) of
        {ok, Path, Handler} ->
            try
                apply_handler(Req, Path, Handler)
            catch _:Error:Stacktrace ->
                internal_error(Req, Error, Stacktrace)
            end;
        not_found ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Not found.">>, Req)
    end.

match_handler(_Path, []) ->
    not_found;
match_handler(Path, [Handler = #{prefix := Prefix} | Handlers]) ->
    case string:prefix(Path, Prefix) of
        nomatch -> match_handler(Path, Handlers);
        RelPath -> {ok, add_slash(RelPath), Handler}
    end.

add_slash("/" ++ _ = Path) -> Path;
add_slash(Path) -> "/" ++ Path.

apply_handler(Req, Path, #{mfargs := MFArgs, options := #{authorization := AuthFun}}) ->
    case AuthFun(Req) of
        true  -> apply_handler(Req, Path, MFArgs);
        false ->
            cowboy_req:reply(401, #{<<"WWW-Authenticate">> => <<"Basic Realm=\"minirest-server\"">>},
                             <<"UNAUTHORIZED">>, Req)
    end;

apply_handler(Req, Path, #{mfargs := MFArgs}) ->
    apply_handler(Req, Path, MFArgs);

apply_handler(Req, Path, {M, F, Args}) ->
    erlang:apply(M, F, [Path, Req | Args]).

internal_error(Req, Error, Stacktrace) ->
    error_logger:error_msg("~s ~s error: ~p, stacktrace:~n~p",
                           [cowboy_req:method(Req), cowboy_req:path(Req), Error, Stacktrace]),
    cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain">>}, <<"Internal Error">>, Req).

%%------------------------------------------------------------------------------
%% Return
%%------------------------------------------------------------------------------

return() ->
    {ok, [{code, ?SUCCESS}]}.

return(ok) ->
    {ok, [{code, ?SUCCESS}]};
return({ok, #{data := Data, meta := Meta}}) ->
    {ok, [{code, ?SUCCESS},
          {data, Data},
          {meta, Meta}]};
return({ok, Data}) ->
    {ok, [{code, ?SUCCESS},
          {data, Data}]};
return({ok, Code, Message}) when is_integer(Code) ->
    {ok, [{code,    Code},
          {message, format_msg(Message)}]};
return({ok, Data, Meta}) ->
    {ok, [{code, ?SUCCESS},
          {data, Data},
          {meta, Meta}]};
return({error, Message}) ->
    {ok, [{message, format_msg(Message)}]};
return({error, Code, Message}) ->
    {ok, [{code,    Code},
          {message, format_msg(Message)}]}.

format_msg(Message) when is_atom(Message) ->
    Message;

format_msg(Message) when is_tuple(Message) ->
    iolist_to_binary(io_lib:format("~p", [Message])).