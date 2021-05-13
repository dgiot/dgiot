%%% Copyright (c) 2020, JianBo He <heeejianbo@gmail.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the copyright holders nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

%%%
%%% @doc ErlPort Java interface
%%% @author JianBo He<heeejianbo@gmail.com>
%%% @copyright 2020 JianBo He <heeejianbo@gmail.com>
%%%

-module(java).

-author('JianBo He<heeejianbo@gmail.com>').

-export([
    start/0,
    start/1,
    start/2,
    start_link/0,
    start_link/1,
    start_link/2,
    stop/1,
    call/4,
    call/5,
    cast/2
    ]).

-include("java.hrl").

-type server_name() :: {local, Name::atom()}
    | {global, GlobalName::term()}
    | {via, Module::atom(), ViaName::term()}.

%%
%% @equiv start([])
%%

-spec start() ->
    {ok, pid()} | {error, Reason::term()}.

start() ->
    start([]).

%%
%% @doc Start Java instance
%%

-spec start(java_options:options() | server_name()) ->
    {ok, pid()} | {error, Reason::term()}.

start(Options) when is_list(Options) ->
    start(start, pid, Options);
start(Name) ->
    start(start, Name, []).

%%
%% @doc Start named Java instance
%%

-spec start(Name::server_name(), Options::java_options:options()) ->
    {ok, pid()} | {error, Reason::term()}.

start(Name, Options) when is_list(Options) ->
    start(start, Name, Options).

%%
%% @equiv start_link([])
%%

-spec start_link() ->
    {ok, pid()} | {error, Reason::term()}.

start_link() ->
    start_link([]).

%%
%% @doc Start linked Java instance
%%

-spec start_link(java_options:options() | server_name()) ->
    {ok, pid()} | {error, Reason::term()}.

start_link(Options) when is_list(Options) ->
    start(start_link, pid, Options);
start_link(Name) ->
    start(start_link, Name, []).

%%
%% @doc Start named and linked Java instance
%%

-spec start_link(Name::server_name(), Options::java_options:options()) ->
    {ok, pid()} | {error, Reason::term()}.

start_link(Name, Options) when is_list(Options) ->
    start(start_link, Name, Options).

%%
%% @doc Stop Java instance
%%

-spec stop(Instance::erlport:server_instance()) -> ok.

stop(Pid) ->
    erlport:stop(Pid).

%%
%% @equiv call(Instance, Module, Function, Args, [])
%%

-spec call(Instance::erlport:server_instance(), Module::atom(),
        Function::atom(), Args::list()) ->
    Result::term().

call(Instance, Module, Function, Args) ->
    call(Instance, Module, Function, Args, []).

%%
%% @doc Call Java function with arguments and return result
%%

-spec call(Instance::erlport:server_instance(), Module::atom(),
        Function::atom(), Args::list(),
        Options::erlport:call_options()) ->
    Result::term().

call(Pid, Module, Function, Args, Options) ->
    erlport:call(Pid, Module, Function, Args, Options).

%%
%% @doc Send message to the external process
%%

-spec cast(Instance::erlport:server_instance(), Message::term()) -> ok.

cast(Pid, Message) ->
    erlport:cast(Pid, Message).

%%%============================================================================
%%% Utility functions
%%%============================================================================

start(Function, Name, OptionsList) when is_list(OptionsList) ->
    case java_options:parse(OptionsList) of
        {ok, Options=#java_options{start_timeout=Timeout}} ->
            Init = init_factory(Options),
            case Name of
                pid ->
                    gen_server:Function(erlport, Init, [{timeout, Timeout}]);
                Name ->
                    gen_server:Function(Name, erlport, Init,
                        [{timeout, Timeout}])
            end;
        Error={error, _} ->
            Error
    end.

init_factory(#java_options{java=Java, java_path=JavaPath, use_stdio=UseStdio, packet=Packet,
        compressed=Compressed, port_options=PortOptions,
        call_timeout=Timeout, buffer_size=BufferSize}) ->
    fun () ->
        Path = lists:concat([Java,
            % Binary STDIO
            %%" -u",
            " -cp ", JavaPath,
            " erlport.CLI",
            " --packet=", Packet,
            " --", UseStdio,
            " --compressed=", Compressed,
            " --buffer_size=", BufferSize]),
        try open_port({spawn, Path}, PortOptions) of
            Port ->
                {ok, #state{port=Port, timeout=Timeout, compressed=Compressed}}
        catch
            error:Error ->
                {stop, {open_port_error, Error}}
        end
    end.
