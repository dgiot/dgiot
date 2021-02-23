%% Feel free to use, reuse and abuse the code in this file.

-module(reverse_protocol).
-behaviour(gen_statem).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([connected/3]).
-export([terminate/3]).
-export([code_change/4]).

-define(TIMEOUT, 5000).

-record(state, {socket, transport}).

%% API.

start_link(Ref, _Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

%% gen_statem.

callback_mode() ->
	state_functions.

init({Ref, Transport, _Opts = []}) ->
	{ok, Socket} = ranch:handshake(Ref),
	ok = Transport:setopts(Socket, [{active, once}, {packet, line}]),
	gen_statem:enter_loop(?MODULE, [], connected,
		#state{socket=Socket, transport=Transport},
		[?TIMEOUT]).

connected(info, {tcp, Socket, Data}, _StateData=#state{
		socket=Socket, transport=Transport})
		when byte_size(Data) > 1 ->
	Transport:setopts(Socket, [{active, once}]),
	Transport:send(Socket, reverse_binary(Data)),
	{keep_state_and_data, ?TIMEOUT};
connected(info, {tcp_closed, _Socket}, _StateData) ->
	{stop, normal};
connected(info, {tcp_error, _, Reason}, _StateData) ->
	{stop, Reason};
connected({call, From}, _Request, _StateData) ->
	gen_statem:reply(From, ok),
	keep_state_and_data;
connected(cast, _Msg, _StateData) ->
	keep_state_and_data;
connected(timeout, _Msg, _StateData) ->
	{stop, normal};
connected(_EventType, _Msg, _StateData) ->
	{stop, normal}.

terminate(Reason, StateName, StateData=#state{
		socket=Socket, transport=Transport})
		when Socket=/=undefined andalso Transport=/=undefined ->
	catch Transport:close(Socket),
	terminate(Reason, StateName,
		StateData#state{socket=undefined, transport=undefined});
terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% Internal.

reverse_binary(B) when is_binary(B) ->
	[list_to_binary(lists:reverse(binary_to_list(
		binary:part(B, {0, byte_size(B)-2})
	))), "\r\n"].
