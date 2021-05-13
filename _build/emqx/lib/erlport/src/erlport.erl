%%% Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
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
%%% @doc ErlPort interface
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2015 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(erlport).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-behaviour(gen_server).

-export([
    stop/1,
    call/5,
    cast/2
    ]).

%% Behaviour callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-include("erlport.hrl").

-type server_instance() :: pid()
    | atom()
    | {Name::atom(), Node::atom()}
    | {global, GlobalName::term()}
    | {via, Module::atom(), ViaName::term()}.

-type call_option() :: {timeout, pos_integer() | infinity}
    | async.
-type call_options() :: [call_option()].

-export_type([server_instance/0, call_options/0]).

%%
%% @doc Stop port protocol
%%

-spec stop(Instance::server_instance()) -> ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%
%% @doc Call remote function with arguments and options and return result
%%

-spec call(Instance::server_instance(), Module::atom(), Function::atom(),
    Args::list(), Options::call_options()) -> Result::term().

call(Pid, Module, Function, Args, Options) when is_atom(Module)
        andalso is_atom(Function) andalso is_list(Args)
        andalso is_list(Options) ->
    ok = check_call_options(Options),
    Request = {call, Module, Function, Args, Options},
    case proplists:get_value(async, Options, false) of
        false ->
            % TODO: Timeout?
            call(Pid, Request, infinity);
        _ ->
            gen_server:cast(Pid, Request)
    end.

%%
%% @doc Send message to the external process
%%

-spec cast(Instance::server_instance(), Message::term()) -> ok.

cast(Pid, Message) ->
    gen_server:cast(Pid, {message, Message}).

%%%
%%% Behaviour callbacks
%%%

%%
%% @doc Process initialization callback
%% @hidden
%%
init(Fun) when is_function(Fun, 0) ->
    process_flag(trap_exit, true),
    Fun().

%%
%% @doc Synchronous event handler
%% @hidden
%%
handle_call(Call={call, _M, _F, _A, Options}, From, State=#state{})
        when is_list(Options) ->
    send_request(Call, From, Options, State);
handle_call(Request, From, State) ->
    Error = {unknown_call, ?MODULE, Request, From},
    {reply, Error, State}.

%%
%% @doc Asynchronous event handler
%% @hidden
%%
handle_cast(Call={message, _Message}, State) ->
    send_request(Call, unknown, [], State);
handle_cast(Call={call, _M, _F, _A, Options}, State)
        when is_list(Options) ->
    send_request(Call, unknown, Options, State);
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Event, State) ->
    {noreply, State}.

%%
%% @doc Messages handler
%% @hidden
%%
handle_info({Port, {data, Data}}, State=#state{port=Port}) ->
    handle_port_data(Data, State);
handle_info({'EXIT', Port, Reason}, State=#state{port=Port}) ->
    {stop, {port_closed, Reason}, State};
handle_info({'EXIT', Pid, {Id, Result}}, State=#state{calls=Calls}) ->
    case orddict:find(Id, Calls) of
        {ok, {Pid, Timer}} ->
            Calls2 = orddict:erase(Id, Calls),
            erlport_utils:stop_timer(Timer),
            handle_call_result(Id, Result, State#state{calls=Calls2});
        error ->
            {noreply, State}
    end;
handle_info({erlport_timeout, {in, Id}}, State=#state{calls=Calls}) ->
    case orddict:find(Id, Calls) of
        {ok, {Pid, _Timer}} ->
            Calls2 = orddict:erase(Id, Calls),
            true = exit(Pid, timeout),
            {noreply, State#state{calls=Calls2}};
        error ->
            {noreply, State}
    end;
handle_info({erlport_timeout, {out, From}}, State) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, State};
handle_info({erlport_timeout, out}, State) ->
    {stop, timeout, State};
handle_info({Port, closed}, State=#state{port=Port}) ->
    {stop, port_closed, State};
handle_info({Port, {exit_status, Code}}, State=#state{port=Port}) ->
    % We can get this message even if 'exit_status' option wasn't set for the
    % port
    {stop, {port_closed, {code, Code}}, State};
handle_info(Message, State) ->
    % Send all other messages to the port
    send_request({message, Message}, unknown, [], State).

%%
%% @doc Server is about to terminate
%% @hidden
%%
terminate(Reason, #state{sent=Sent}) ->
    Error = get_termination_error(Reason),
    Factory = send_error_factory(Error),
    lists:foreach(Factory, [V || {_K, V} <- orddict:to_list(Sent)]).

%%
%% @doc Code change handler
%% @hidden
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

%%
%% @doc Return error to send to all connected clients on the server termination
%%
get_termination_error(normal) ->
    {error, stopped};
get_termination_error(Reason) ->
    {error, Reason}.

%%
%% @doc Return factory to send error response to the client if possible
%%
send_error_factory(Error) ->
    fun
        ({unknown, _Timer}) ->
            ok;
        ({From, _Timer}) ->
            gen_server:reply(From, Error)
    end.

%%
%% @doc Call the server and return call result
%%
call(Pid, Request, Timeout) ->
    case gen_server:call(Pid, Request, Timeout) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            erlang:error(Error)
    end.

%%
%% @doc Handle incoming 'print' request
%%
print(Data, State) ->
    ok = io:put_chars(Data),
    {noreply, State}.

%%
%% @doc Spawn a process to handle incoming call request
%%
spawn_call(Id, Module, Function, Args) ->
    spawn_link(fun () ->
        exit({Id, call_mfa(Module, Function, Args)})
        end).

%%
%% @doc Check options for the call request
%%
check_call_options([{timeout, Timeout}=Value | Tail]) ->
    case erlport_options:timeout(Timeout) of
        {ok, _} ->
            check_call_options(Tail);
        error ->
            erlang:error({erlport_option_error, Value})
    end;
check_call_options([async | Tail]) ->
    check_call_options(Tail);
check_call_options([Invalid | _]) ->
    erlang:error({erlport_invalid_option, Invalid});
check_call_options([]) ->
    ok.

%%
%% @doc Handle incoming 'cast' request
%%
send(Pid, Message, State) ->
    try Pid ! Message of
        _ ->
            {noreply, State}
    catch
        ErrType:Error ->
            {stop, {send_error, {ErrType, Error}}, State}
    end.

%%
%% @doc Handle incoming data
%%
handle_port_data(Data, State) ->
    try binary_to_term(Data) of
        Message ->
            handle_message(Message, State)
    catch
        error:badarg ->
            {stop, {invalid_protocol_data, Data}, State}
    end.

%%
%% @doc Handle incoming reply message
%%
handle_message({'r', Id, Result}, State) ->
    erlport_utils:handle_response({ok, Result}, Id, State);
handle_message({'e', Id, Error}, State) ->
    erlport_utils:handle_response({error, Error}, Id, State);
handle_message({'e', Error}, State) ->
    {stop, {message_handler_error, Error}, State};
handle_message(Request, State) ->
    handle_incoming_message(Request, State).

%%
%% @doc Handle incoming message
%%
handle_incoming_message({'C', Id, Module, Function, Args, Context}, State)
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    incoming_call(Id, Module, Function, Args, Context, State);
handle_incoming_message({'M', Pid, Message}, State) ->
    send(Pid, Message, State);
handle_incoming_message({'P', StdoutData}, State) ->
    print(StdoutData, State);
handle_incoming_message(Request, State) ->
    {stop, {invalid_protocol_message, Request}, State}.

%%
%% @doc Handle incoming call result
%%
handle_call_result(Id, Result, State=#state{port=Port,
        compressed=Compressed}) ->
    Data = erlport_utils:encode_term(format_call_result(Id, Result),
        Compressed),
    case erlport_utils:send_data(Port, Data) of
        ok ->
            {noreply, State};
        error ->
            {stop, port_closed, State}
    end.

%%
%% @doc Format incoming call result
%%
format_call_result(Id, {ok, Response}) ->
    {'r', Id, erlport_utils:prepare_term(Response)};
format_call_result(Id, {error, Error}) ->
    {'e', Id, erlport_utils:prepare_term(Error)};
format_call_result(Id, Error) ->
    {'e', Id, {erlang, undefined, erlport_utils:prepare_term(Error), []}}.

%%
%% @doc Send or queue outgoing call request
%%
send_request(Request, From, Options, State=#state{timeout=DefaultTimeout}) ->
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            send_request2(Request, From, Timeout, State);
        error ->
            Error = {error, {invalid_option, {timeout, Timeout}}},
            case From of
                unknown ->
                    {stop, Error, State};
                _ ->
                    {reply, Error, State}
            end
    end.

send_request2({call, Module, Function, Args, _Options}, From, Timeout,
        State=#state{compressed=Compressed})
        when is_atom(Module) andalso is_atom(Function) andalso is_list(Args) ->
    Id = next_message_id(State),
    Data = erlport_utils:encode_term({'C', Id, Module, Function,
        erlport_utils:prepare_list(Args)}, Compressed),
    erlport_utils:send_request(From, Data, Id, State, Timeout);
send_request2({message, Message}, From, Timeout, State=#state{
        compressed=Compressed}) ->
    Data = erlport_utils:encode_term({'M',
        erlport_utils:prepare_term(Message)}, Compressed),
    erlport_utils:send_request(From, Data, undefined, State, Timeout).

%%
%% @doc Generate next message ID
%%
next_message_id(#state{sent=[]}) ->
    1;
next_message_id(#state{sent=Sent}) ->
    lists:max(orddict:fetch_keys(Sent)) + 1.

%%
%% @doc Call with module, function and args
%%
call_mfa(Module, Function, Args) ->
    try {ok, apply(Module, Function, Args)}
    catch
        error:{Language, Type, _Val, Trace}=Error
                when is_atom(Language) andalso is_atom(Type)
                andalso is_list(Trace) ->
            {error, Error};
        Type:Reason:Trace ->
            {error, {erlang, Type, Reason, Trace}}
    end.

%%
%% @doc Handle incoming call request
%%
incoming_call(Id, Module, Function, Args, 'L', State) ->
    handle_call_result(Id, call_mfa(Module, Function, Args), State);
incoming_call(Id, Module, Function, Args, _Context, State=#state{
        timeout=Timeout, calls=Calls}) ->
    case orddict:find(Id, Calls) of
        {ok, {_Pid, _Timer}=Info} ->
            {stop, {duplicate_incoming_call, Id, Info}, State};
        error ->
            Pid = spawn_call(Id, Module, Function, Args),
            Info = {Pid, erlport_utils:start_timer(Timeout,
                {erlport_timeout, {in, Id}})},
            Calls2 = orddict:store(Id, Info, Calls),
            {noreply, State#state{calls=Calls2}}
    end.
