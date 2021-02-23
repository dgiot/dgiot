%% Copyright (c) 2016-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(gun_http2).

-export([check_options/1]).
-export([name/0]).
-export([init/4]).
-export([handle/2]).
-export([close/1]).
-export([keepalive/1]).
-export([request/8]).
-export([request/9]).
-export([data/5]).
-export([cancel/3]).
-export([down/1]).

-record(stream, {
	id :: non_neg_integer(),
	ref :: reference(),
	reply_to :: pid(),
	%% Whether we finished sending data.
	local = nofin :: fin | nofin,
	%% Local flow control window (how much we can send).
	local_window :: integer(),
	%% Buffered data waiting for the flow control window to increase.
	local_buffer = queue:new() :: queue:queue(
		{fin | nofin, non_neg_integer(), iolist()}),
	local_buffer_size = 0 :: non_neg_integer(),
	local_trailers = undefined :: undefined | cow_http:headers(),
	%% Whether we finished receiving data.
	remote = nofin :: fin | nofin,
	%% Remote flow control window (how much we accept to receive).
	remote_window :: integer(),
	%% Content handlers state.
	handler_state :: undefined | gun_content_handler:state()
}).

-record(http2_state, {
	owner :: pid(),
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module(),
	opts = #{} :: map(), %% @todo
	content_handlers :: gun_content_handler:opt(),
	buffer = <<>> :: binary(),

	local_settings = #{
		initial_window_size => 65535,
		max_frame_size => 16384
	} :: map(),
	remote_settings = #{
		initial_window_size => 65535
	} :: map(),

	%% Connection-wide flow control window.
	local_window = 65535 :: integer(), %% How much we can send.
	remote_window = 65535 :: integer(), %% How much we accept to receive.

	streams = [] :: [#stream{}],
	stream_id = 1 :: non_neg_integer(),

	%% The client starts by sending a sequence of bytes as a preface,
	%% followed by a potentially empty SETTINGS frame. Then the connection
	%% is established and continues normally. An exception is when a HEADERS
	%% frame is sent followed by CONTINUATION frames: no other frame can be
	%% sent in between.
	parse_state = undefined :: normal
		| {continuation, cowboy_stream:streamid(), cowboy_stream:fin(), binary()},

	%% HPACK decoding and encoding state.
	decode_state = cow_hpack:init() :: cow_hpack:state(),
	encode_state = cow_hpack:init() :: cow_hpack:state()
}).

check_options(Opts) ->
	do_check_options(maps:to_list(Opts)).

do_check_options([]) ->
	ok;
do_check_options([Opt={content_handlers, Handlers}|Opts]) ->
	case gun_content_handler:check_option(Handlers) of
		ok -> do_check_options(Opts);
		error -> {error, {options, {http, Opt}}}
	end;
do_check_options([{keepalive, infinity}|Opts]) ->
	do_check_options(Opts);
do_check_options([{keepalive, K}|Opts]) when is_integer(K), K > 0 ->
	do_check_options(Opts);
%% @todo max_frame_size_sent
do_check_options([Opt|_]) ->
	{error, {options, {http2, Opt}}}.

name() -> http2.

init(Owner, Socket, Transport, Opts) ->
	Handlers = maps:get(content_handlers, Opts, [gun_data_h]),
	State = #http2_state{owner=Owner, socket=Socket,
		transport=Transport, opts=Opts, content_handlers=Handlers,
		parse_state=normal}, %% @todo Have a special parse state for preface.
	#http2_state{local_settings=Settings} = State,
	%% Send the HTTP/2 preface.
	Transport:send(Socket, [
		<< "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>,
		cow_http2:settings(Settings)
	]),
	State.

handle(Data, State=#http2_state{buffer=Buffer}) ->
	parse(<< Buffer/binary, Data/binary >>, State#http2_state{buffer= <<>>}).

parse(Data0, State0=#http2_state{buffer=Buffer, parse_state=PS}) ->
	%% @todo Parse states: Preface. Continuation.
	Data = << Buffer/binary, Data0/binary >>,
	case cow_http2:parse(Data) of
		{ok, Frame, Rest} when PS =:= normal ->
			case frame(Frame, State0) of
				close -> close;
				State1 -> parse(Rest, State1)
			end;
		{ok, Frame, Rest} when element(1, PS) =:= continuation ->
			case continuation_frame(Frame, State0) of
				close -> close;
				State1 -> parse(Rest, State1)
			end;
		{ignore, _} when element(1, PS) =:= continuation ->
			terminate(State0, {connection_error, protocol_error,
				'An invalid frame was received in the middle of a header block. (RFC7540 6.2)'});
		{ignore, Rest} ->
			parse(Rest, State0);
		{stream_error, StreamID, Reason, Human, Rest} ->
			parse(Rest, stream_reset(State0, StreamID, {stream_error, Reason, Human}));
		Error = {connection_error, _, _} ->
			terminate(State0, Error);
		more ->
			{state, State0#http2_state{buffer=Data}}
	end.

%% DATA frame.
frame({data, StreamID, IsFin, Data}, State0=#http2_state{remote_window=ConnWindow}) ->
	case get_stream_by_id(StreamID, State0) of
		Stream0 = #stream{remote=nofin, remote_window=StreamWindow, handler_state=Handlers0} ->
			Handlers = gun_content_handler:handle(IsFin, Data, Handlers0),
			{Stream, State} = send_window_update(
				Stream0#stream{remote_window=StreamWindow - byte_size(Data),
					handler_state=Handlers},
				State0#http2_state{remote_window=ConnWindow - byte_size(Data)}),
			remote_fin(Stream, State, IsFin);
		_ ->
			%% @todo protocol_error if not existing
			stream_reset(State0, StreamID, {stream_error, stream_closed,
				'DATA frame received for a closed or non-existent stream. (RFC7540 6.1)'})
	end;
%% Single HEADERS frame headers block.
frame({headers, StreamID, IsFin, head_fin, HeaderBlock}, State) ->
	stream_decode_init(State, StreamID, IsFin, HeaderBlock);
%% HEADERS frame starting a headers block. Enter continuation mode.
frame({headers, StreamID, IsFin, head_nofin, HeaderBlockFragment}, State) ->
	State#http2_state{parse_state={continuation, StreamID, IsFin, HeaderBlockFragment}};
%% Single HEADERS frame headers block with priority.
frame({headers, StreamID, IsFin, head_fin,
		_IsExclusive, _DepStreamID, _Weight, HeaderBlock}, State) ->
	stream_decode_init(State, StreamID, IsFin, HeaderBlock);
%% @todo HEADERS frame starting a headers block. Enter continuation mode.
%frame(State, {headers, StreamID, IsFin, head_nofin,
%		_IsExclusive, _DepStreamID, _Weight, HeaderBlockFragment}) ->
%	%% @todo Handle priority.
%	State#http2_state{parse_state={continuation, StreamID, IsFin, HeaderBlockFragment}};
%% @todo PRIORITY frame.
%frame(State, {priority, _StreamID, _IsExclusive, _DepStreamID, _Weight}) ->
%	%% @todo Validate StreamID?
%	%% @todo Handle priority.
%	State;
%% @todo RST_STREAM frame.
frame({rst_stream, StreamID, Reason}, State) ->
	stream_reset(State, StreamID, {stream_error, Reason, 'Stream reset by server.'});
%% SETTINGS frame.
frame({settings, Settings}, State=#http2_state{socket=Socket, transport=Transport,
		remote_settings=Settings0}) ->
	Transport:send(Socket, cow_http2:settings_ack()),
	State#http2_state{remote_settings=maps:merge(Settings0, Settings)};
%% Ack for a previously sent SETTINGS frame.
frame(settings_ack, State) -> %% @todo =#http2_state{next_settings=_NextSettings}) ->
	%% @todo Apply SETTINGS that require synchronization.
	State;
%% PUSH_PROMISE frame.
%% @todo Continuation.
frame({push_promise, StreamID, head_fin, PromisedStreamID, HeaderBlock},
		State=#http2_state{streams=Streams, decode_state=DecodeState0}) ->
	case get_stream_by_id(PromisedStreamID, State) of
		false ->
			case get_stream_by_id(StreamID, State) of
				#stream{ref=StreamRef, reply_to=ReplyTo} ->
					try cow_hpack:decode(HeaderBlock, DecodeState0) of
						{Headers0, DecodeState} ->
							{Method, Scheme, Authority, Path, Headers} = try
								{value, {_, Method0}, Headers1} = lists:keytake(<<":method">>, 1, Headers0),
								{value, {_, Scheme0}, Headers2} = lists:keytake(<<":scheme">>, 1, Headers1),
								{value, {_, Authority0}, Headers3} = lists:keytake(<<":authority">>, 1, Headers2),
								{value, {_, Path0}, Headers4} = lists:keytake(<<":path">>, 1, Headers3),
								{Method0, Scheme0, Authority0, Path0, Headers4}
							catch error:badmatch ->
								stream_reset(State, StreamID, {stream_error, protocol_error,
									'Malformed push promise; missing pseudo-header field. (RFC7540 8.1.2.3)'})
							end,
							NewStreamRef = make_ref(),
							ReplyTo ! {gun_push, self(), StreamRef, NewStreamRef, Method,
								iolist_to_binary([Scheme, <<"://">>, Authority, Path]), Headers},
							NewStream = new_stream(PromisedStreamID, NewStreamRef, ReplyTo,
								nofin, fin, State),
							State#http2_state{streams=[NewStream|Streams], decode_state=DecodeState}
					catch _:_ ->
						terminate(State, {connection_error, compression_error,
							'Error while trying to decode HPACK-encoded header block. (RFC7540 4.3)'})
					end;
				_ ->
					stream_reset(State, StreamID, {stream_error, stream_closed,
						'DATA frame received for a closed or non-existent stream. (RFC7540 6.1)'})
			end;
		_ ->
			stream_reset(State, StreamID, {stream_error, todo, ''})
	end;
%% PING frame.
frame({ping, Opaque}, State=#http2_state{socket=Socket, transport=Transport}) ->
	Transport:send(Socket, cow_http2:ping_ack(Opaque)),
	State;
%% Ack for a previously sent PING frame.
%%
%% @todo Might want to check contents but probably a waste of time.
frame({ping_ack, _Opaque}, State) ->
	State;
%% GOAWAY frame.
frame(Frame={goaway, StreamID, _, _}, State) ->
	terminate(State, StreamID, {stop, Frame, 'Client is going away.'});
%% Connection-wide WINDOW_UPDATE frame.
frame({window_update, Increment}, State=#http2_state{local_window=ConnWindow})
		when ConnWindow + Increment > 16#7fffffff ->
	terminate(State, {connection_error, flow_control_error,
		'The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)'});
frame({window_update, Increment}, State=#http2_state{local_window=ConnWindow}) ->
	send_data(State#http2_state{local_window=ConnWindow + Increment});
%% Stream-specific WINDOW_UPDATE frame.
frame({window_update, StreamID, Increment}, State0=#http2_state{streams=Streams0}) ->
	case lists:keyfind(StreamID, #stream.id, Streams0) of
		#stream{local_window=StreamWindow} when StreamWindow + Increment > 16#7fffffff ->
			stream_reset(State0, StreamID, {stream_error, flow_control_error,
				'The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)'});
		Stream0 = #stream{local_window=StreamWindow} ->
			{State, Stream} = send_data(State0,
				Stream0#stream{local_window=StreamWindow + Increment}),
			Streams = lists:keystore(StreamID, #stream.id, Streams0, Stream),
			State#http2_state{streams=Streams};
		false ->
			%% @todo Receiving this frame on a stream in the idle state is an error.
			%% WINDOW_UPDATE frames may be received for a short period of time
			%% after a stream is closed. They must be ignored.
			State0
	end;
%% Unexpected CONTINUATION frame.
frame({continuation, StreamID, _, _}, State) ->
	terminate(State, StreamID, {connection_error, protocol_error,
		'CONTINUATION frames MUST be preceded by a HEADERS frame. (RFC7540 6.10)'}).

continuation_frame({continuation, StreamID, head_fin, HeaderBlockFragment1},
		State=#http2_state{parse_state={continuation, StreamID, IsFin, HeaderBlockFragment0}}) ->
	HeaderBlock = << HeaderBlockFragment0/binary, HeaderBlockFragment1/binary >>,
	stream_decode_init(State#http2_state{parse_state=normal}, StreamID, IsFin, HeaderBlock);
continuation_frame({continuation, StreamID, head_nofin, HeaderBlockFragment1},
		State=#http2_state{parse_state=
			{continuation, StreamID, IsFin, HeaderBlockFragment0}}) ->
	State#http2_state{parse_state={continuation, StreamID, IsFin,
		<< HeaderBlockFragment0/binary, HeaderBlockFragment1/binary >>}};
continuation_frame(_, State) ->
	terminate(State, {connection_error, protocol_error,
		'An invalid frame was received in the middle of a header block. (RFC7540 6.2)'}).

send_window_update(Stream=#stream{id=StreamID, remote_window=StreamWindow0},
		State=#http2_state{socket=Socket, transport=Transport, remote_window=ConnWindow0}) ->
	%% @todo We should make the windows configurable.
	MinConnWindow = 8000000,
	MinStreamWindow = 1000000,
	ConnWindow = if
		ConnWindow0 =< MinConnWindow ->
			Transport:send(Socket, cow_http2:window_update(MinConnWindow)),
			ConnWindow0 + MinConnWindow;
		true ->
			ConnWindow0
	end,
	StreamWindow = if
		StreamWindow0 =< MinStreamWindow ->
			Transport:send(Socket, cow_http2:window_update(StreamID, MinStreamWindow)),
			StreamWindow0 + MinStreamWindow;
		true ->
			StreamWindow0
	end,
	{Stream#stream{remote_window=StreamWindow},
		State#http2_state{remote_window=ConnWindow}}.

close(#http2_state{streams=Streams}) ->
	close_streams(Streams).

close_streams([]) ->
	ok;
close_streams([#stream{ref=StreamRef, reply_to=ReplyTo}|Tail]) ->
	ReplyTo ! {gun_error, self(), StreamRef, {closed,
		"The connection was lost."}},
	close_streams(Tail).

keepalive(State=#http2_state{socket=Socket, transport=Transport}) ->
	Transport:send(Socket, cow_http2:ping(0)),
	State.

request(State=#http2_state{socket=Socket, transport=Transport, encode_state=EncodeState0,
		streams=Streams, stream_id=StreamID}, StreamRef, ReplyTo,
		Method, Host, Port, Path, Headers) ->
	{HeaderBlock, EncodeState} = prepare_headers(EncodeState0, Transport, Method, Host, Port, Path, Headers),
	IsFin = case (false =/= lists:keyfind(<<"content-type">>, 1, Headers))
			orelse (false =/= lists:keyfind(<<"content-length">>, 1, Headers)) of
		true -> nofin;
		false -> fin
	end,
	Transport:send(Socket, cow_http2:headers(StreamID, IsFin, HeaderBlock)),
	Stream = new_stream(StreamID, StreamRef, ReplyTo, nofin, IsFin, State),
	State#http2_state{streams=[Stream|Streams], stream_id=StreamID + 2, encode_state=EncodeState}.

%% @todo Handle Body > 16MB. (split it out into many frames)
request(State0=#http2_state{socket=Socket, transport=Transport, encode_state=EncodeState0,
		streams=Streams, stream_id=StreamID}, StreamRef, ReplyTo,
		Method, Host, Port, Path, Headers0, Body) ->
	Headers = lists:keystore(<<"content-length">>, 1, Headers0,
		{<<"content-length">>, integer_to_binary(iolist_size(Body))}),
	{HeaderBlock, EncodeState} = prepare_headers(EncodeState0, Transport, Method, Host, Port, Path, Headers),
	Transport:send(Socket, cow_http2:headers(StreamID, nofin, HeaderBlock)),
	Stream0 = new_stream(StreamID, StreamRef, ReplyTo, nofin, nofin, State0),
	{State, Stream} = send_data(State0, Stream0, fin, Body),
	State#http2_state{streams=[Stream|Streams], stream_id=StreamID + 2, encode_state=EncodeState}.

prepare_headers(EncodeState, Transport, Method, Host0, Port, Path, Headers0) ->
	Host2 = case Host0 of
		{local, _SocketPath} -> <<>>;
		Tuple when is_tuple(Tuple) -> inet:ntoa(Tuple);
		_ -> Host0
	end,
	Authority = case lists:keyfind(<<"host">>, 1, Headers0) of
		{_, Host} -> Host;
		_ -> [Host2, $:, integer_to_binary(Port)]
	end,
	%% @todo We also must remove any header found in the connection header.
	Headers1 =
		lists:keydelete(<<"host">>, 1,
		lists:keydelete(<<"connection">>, 1,
		lists:keydelete(<<"keep-alive">>, 1,
		lists:keydelete(<<"proxy-connection">>, 1,
		lists:keydelete(<<"transfer-encoding">>, 1,
		lists:keydelete(<<"upgrade">>, 1, Headers0)))))),
	Headers = [
		{<<":method">>, Method},
		{<<":scheme">>, case Transport of
			gun_tls -> <<"https">>;
			gun_tcp -> <<"http">>
		end},
		{<<":authority">>, Authority},
		{<<":path">>, Path}
	|Headers1],
	cow_hpack:encode(Headers, EncodeState).

data(State0, StreamRef, ReplyTo, IsFin, Data) ->
	case get_stream_by_ref(StreamRef, State0) of
		#stream{local=fin} ->
			error_stream_closed(State0, StreamRef, ReplyTo);
		Stream0 = #stream{} ->
			{State, Stream} = send_data(State0, Stream0, IsFin, Data),
			maybe_delete_stream(State, Stream);
		false ->
			error_stream_not_found(State0, StreamRef, ReplyTo)
	end.

%% @todo Should we ever want to implement the PRIORITY mechanism,
%% this would be the place to do it. Right now, we just go over
%% all streams and send what we can until either everything is
%% sent or we run out of space in the window.
send_data(State=#http2_state{streams=Streams}) ->
	resume_streams(State, Streams, []).

%% When SETTINGS_INITIAL_WINDOW_SIZE changes we need to update
%% the local stream windows for all active streams and perhaps
%% resume sending data.
%update_streams_local_window(State=#http2_state{streams=Streams0}, Increment) ->
%	Streams = [
%		S#stream{local_window=StreamWindow + Increment}
%	|| S=#stream{local_window=StreamWindow} <- Streams0],
%	resume_streams(State, Streams, []).

%% When we receive an ack to a SETTINGS frame we sent we need to update
%% the remote stream windows for all active streams.
%update_streams_remote_window(State=#http2_state{streams=Streams0}, Increment) ->
%	Streams = [
%		S#stream{remote_window=StreamWindow + Increment}
%	|| S=#stream{remote_window=StreamWindow} <- Streams0],
%	State#http2_state{streams=Streams}.

resume_streams(State, [], Acc) ->
	State#http2_state{streams=lists:reverse(Acc)};
%% While technically we should never get < 0 here, let's be on the safe side.
resume_streams(State=#http2_state{local_window=ConnWindow}, Streams, Acc)
		when ConnWindow =< 0 ->
	State#http2_state{streams=lists:reverse(Acc, Streams)};
%% We rely on send_data/2 to do all the necessary checks about the stream.
resume_streams(State0, [Stream0|Tail], Acc) ->
	{State1, Stream} = send_data(State0, Stream0),
	resume_streams(State1, Tail, [Stream|Acc]).

send_data(State, Stream=#stream{local=Local, local_buffer_size=0, local_trailers=Trailers})
		when (Trailers =/= undefined) andalso ((Local =:= idle) orelse (Local =:= nofin)) ->
	send_trailers(State, Stream#stream{local_trailers=undefined}, Trailers);
%% @todo It's possible that the stream terminates. We must remove it.
send_data(State=#http2_state{local_window=ConnWindow},
		Stream=#stream{local=IsFin, local_window=StreamWindow, local_buffer_size=BufferSize})
		when ConnWindow =< 0; IsFin =:= fin; StreamWindow =< 0; BufferSize =:= 0 ->
	{State, Stream};
send_data(State0, Stream0=#stream{local_buffer=Q0, local_buffer_size=BufferSize}) ->
	%% We know there is an item in the queue.
	{{value, {IsFin, DataSize, Data}}, Q} = queue:out(Q0),
	{State, Stream} = send_data(State0,
		Stream0#stream{local_buffer=Q, local_buffer_size=BufferSize - DataSize},
		IsFin, Data, in_r),
	send_data(State, Stream).

send_data(State, Stream, IsFin, Data) ->
	send_data(State, Stream, IsFin, Data, in).

%% We can send trailers immediately if the queue is empty, otherwise we queue.
%% We always send trailer frames even if the window is empty.
send_data(State, Stream=#stream{local_buffer_size=0}, fin, {trailers, Trailers}, _) ->
	send_trailers(State, Stream, Trailers);
send_data(State, Stream, fin, {trailers, Trailers}, _) ->
	{State, Stream#stream{local_trailers=Trailers}};
%% Send data immediately if we can, buffer otherwise.
send_data(State=#http2_state{local_window=ConnWindow},
		Stream=#stream{local_window=StreamWindow}, IsFin, Data, In)
		when ConnWindow =< 0; StreamWindow =< 0 ->
	{State, queue_data(Stream, IsFin, Data, In)};
send_data(State=#http2_state{socket=Socket, transport=Transport, opts=Opts,
		remote_settings=RemoteSettings, local_window=ConnWindow},
		Stream=#stream{id=StreamID, local_window=StreamWindow}, IsFin, Data, In) ->
	RemoteMaxFrameSize = maps:get(max_frame_size, RemoteSettings, 16384),
	ConfiguredMaxFrameSize = maps:get(max_frame_size_sent, Opts, infinity),
	MaxSendSize = min(
		min(ConnWindow, StreamWindow),
		min(RemoteMaxFrameSize, ConfiguredMaxFrameSize)
	),
	case Data of
%		{sendfile, Offset, Bytes, Path} when Bytes =< MaxSendSize ->
%			Transport:send(Socket, cow_http2:data_header(StreamID, IsFin, Bytes)),
%			Transport:sendfile(Socket, Path, Offset, Bytes),
%			{State#http2_state{local_window=ConnWindow - Bytes},
%				Stream#stream{local=IsFin, local_window=StreamWindow - Bytes}};
%		{sendfile, Offset, Bytes, Path} ->
%			Transport:send(Socket, cow_http2:data_header(StreamID, nofin, MaxSendSize)),
%			Transport:sendfile(Socket, Path, Offset, MaxSendSize),
%			send_data(State#http2_state{local_window=ConnWindow - MaxSendSize},
%				Stream#stream{local_window=StreamWindow - MaxSendSize},
%				IsFin, {sendfile, Offset + MaxSendSize, Bytes - MaxSendSize, Path}, In);
		Iolist0 ->
			IolistSize = iolist_size(Iolist0),
			if
				IolistSize =< MaxSendSize ->
					Transport:send(Socket, cow_http2:data(StreamID, IsFin, Iolist0)),
					{State#http2_state{local_window=ConnWindow - IolistSize},
						Stream#stream{local=IsFin, local_window=StreamWindow - IolistSize}};
				true ->
					{Iolist, More} = cow_iolists:split(MaxSendSize, Iolist0),
					Transport:send(Socket, cow_http2:data(StreamID, nofin, Iolist)),
					send_data(State#http2_state{local_window=ConnWindow - MaxSendSize},
						Stream#stream{local_window=StreamWindow - MaxSendSize},
						IsFin, More, In)
			end
	end.

send_trailers(State=#http2_state{socket=Socket, transport=Transport, encode_state=EncodeState0},
		Stream=#stream{id=StreamID}, Trailers) ->
	{HeaderBlock, EncodeState} = cow_hpack:encode(Trailers, EncodeState0),
	Transport:send(Socket, cow_http2:headers(StreamID, fin, HeaderBlock)),
	{State#http2_state{encode_state=EncodeState}, Stream#stream{local=fin}}.

queue_data(Stream=#stream{local_buffer=Q0, local_buffer_size=Size0}, IsFin, Data, In) ->
	DataSize = case Data of
%		{sendfile, _, Bytes, _} -> Bytes;
		Iolist -> iolist_size(Iolist)
	end,
	Q = queue:In({IsFin, DataSize, Data}, Q0),
	Stream#stream{local_buffer=Q, local_buffer_size=Size0 + DataSize}.

cancel(State=#http2_state{socket=Socket, transport=Transport},
		StreamRef, ReplyTo) ->
	case get_stream_by_ref(StreamRef, State) of
		#stream{id=StreamID} ->
			Transport:send(Socket, cow_http2:rst_stream(StreamID, cancel)),
			delete_stream(StreamID, State);
		false ->
			error_stream_not_found(State, StreamRef, ReplyTo)
	end.

%% @todo Add unprocessed streams when GOAWAY handling is done.
down(#http2_state{streams=Streams}) ->
	KilledStreams = [Ref || #stream{ref=Ref} <- Streams],
	{KilledStreams, []}.

terminate(#http2_state{socket=Socket, transport=Transport, streams=Streams}, Reason) ->
	%% Because a particular stream is unknown,
	%% we're sending the error message to all streams.
	%% @todo We should not send duplicate messages to processes.
	%% @todo We should probably also inform the owner process.
	_ = [ReplyTo ! {gun_error, self(), Reason} || #stream{reply_to=ReplyTo} <- Streams],
	%% @todo LastGoodStreamID
	Transport:send(Socket, cow_http2:goaway(0, terminate_reason(Reason), <<>>)),
	close.

terminate(State=#http2_state{socket=Socket, transport=Transport}, StreamID, Reason) ->
	case get_stream_by_id(StreamID, State) of
		#stream{reply_to=ReplyTo} ->
			ReplyTo ! {gun_error, self(), Reason},
			%% @todo LastGoodStreamID
			Transport:send(Socket, cow_http2:goaway(0, terminate_reason(Reason), <<>>)),
			close;
		_ ->
			terminate(State, Reason)
	end.

terminate_reason({connection_error, Reason, _}) -> Reason;
terminate_reason({stop, _, _}) -> no_error.

%% Stream functions.

stream_decode_init(State=#http2_state{decode_state=DecodeState0}, StreamID, IsFin, HeaderBlock) ->
	try cow_hpack:decode(HeaderBlock, DecodeState0) of
		{Headers, DecodeState} ->
			stream_pseudo_headers_init(State#http2_state{decode_state=DecodeState},
				StreamID, IsFin, Headers)
	catch _:_ ->
		terminate(State, {connection_error, compression_error,
			'Error while trying to decode HPACK-encoded header block. (RFC7540 4.3)'})
	end.

stream_pseudo_headers_init(State, StreamID, IsFin, Headers0) ->
	case pseudo_headers(Headers0, #{}) of
		{ok, PseudoHeaders, Headers} ->
			stream_resp_init(State, StreamID, IsFin, Headers, PseudoHeaders);
%% @todo When we handle trailers properly:
%		{ok, _, _} ->
%			stream_malformed(State, StreamID,
%				'A required pseudo-header was not found. (RFC7540 8.1.2.3)');
%% Or:
%		{ok, _, _} ->
%			stream_reset(State, StreamID, {stream_error, protocol_error,
%				'Malformed response; missing :status in HEADERS frame. (RFC7540 8.1.2.4)'})
		{error, HumanReadable} ->
			stream_reset(State, StreamID, {stream_error, protocol_error, HumanReadable})
	end.

pseudo_headers([{<<":status">>, _}|_], #{status := _}) ->
	{error, 'Multiple :status pseudo-headers were found. (RFC7540 8.1.2.3)'};
pseudo_headers([{<<":status">>, Status}|Tail], PseudoHeaders) ->
	try cow_http:status_to_integer(Status) of
		IntStatus ->
			pseudo_headers(Tail, PseudoHeaders#{status => IntStatus})
	catch _:_ ->
		{error, 'The :status pseudo-header value is invalid. (RFC7540 8.1.2.4)'}
	end;
pseudo_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, 'An unknown or invalid pseudo-header was found. (RFC7540 8.1.2.1)'};
pseudo_headers(Headers, PseudoHeaders) ->
	{ok, PseudoHeaders, Headers}.

stream_resp_init(State=#http2_state{content_handlers=Handlers0},
		StreamID, IsFin, Headers, PseudoHeaders) ->
	case get_stream_by_id(StreamID, State) of
		Stream = #stream{ref=StreamRef, reply_to=ReplyTo, remote=nofin} ->
			case PseudoHeaders of
				#{status := Status} when Status >= 100, Status =< 199 ->
					ReplyTo ! {gun_inform, self(), StreamRef, Status, Headers},
					State;
				#{status := Status} ->
					ReplyTo ! {gun_response, self(), StreamRef, IsFin, Status, Headers},
					Handlers = case IsFin of
						fin -> undefined;
						nofin ->
							gun_content_handler:init(ReplyTo, StreamRef,
								Status, Headers, Handlers0)
					end,
					remote_fin(Stream#stream{handler_state=Handlers}, State, IsFin);
				%% @todo For now we assume that it's a trailer if there's no :status.
				%% A better state machine is needed to distinguish between that and errors.
				_ ->
					%% @todo We probably want to pass this to gun_content_handler?
					ReplyTo ! {gun_trailers, self(), StreamRef, Headers},
					remote_fin(Stream, State, fin)
			end;
		_ ->
			stream_reset(State, StreamID, {stream_error, stream_closed,
				'HEADERS frame received for a closed or non-existent stream. (RFC7540 6.1)'})
	end.

stream_reset(State=#http2_state{socket=Socket, transport=Transport,
		streams=Streams0}, StreamID, StreamError={stream_error, Reason, _}) ->
	Transport:send(Socket, cow_http2:rst_stream(StreamID, Reason)),
	case lists:keytake(StreamID, #stream.id, Streams0) of
		{value, #stream{ref=StreamRef, reply_to=ReplyTo}, Streams} ->
			ReplyTo ! {gun_error, self(), StreamRef, StreamError},
			State#http2_state{streams=Streams};
		false ->
			%% @todo Unknown stream. Not sure what to do here. Check again once all
			%% terminate calls have been written.
			State
	end.

error_stream_closed(State, StreamRef, ReplyTo) ->
	ReplyTo ! {gun_error, self(), StreamRef, {badstate,
		"The stream has already been closed."}},
	State.

error_stream_not_found(State, StreamRef, ReplyTo) ->
	ReplyTo ! {gun_error, self(), StreamRef, {badstate,
		"The stream cannot be found."}},
	State.

%% Streams.
%% @todo probably change order of args and have state first?

new_stream(StreamID, StreamRef, ReplyTo, Remote, Local, #http2_state{
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow}}) ->
	#stream{id=StreamID, ref=StreamRef, reply_to=ReplyTo,
		remote=Remote, remote_window=RemoteWindow,
		local=Local, local_window=LocalWindow}.

get_stream_by_id(StreamID, #http2_state{streams=Streams}) ->
	lists:keyfind(StreamID, #stream.id, Streams).

get_stream_by_ref(StreamRef, #http2_state{streams=Streams}) ->
	lists:keyfind(StreamRef, #stream.ref, Streams).

delete_stream(StreamID, State=#http2_state{streams=Streams}) ->
	Streams2 = lists:keydelete(StreamID, #stream.id, Streams),
	State#http2_state{streams=Streams2}.

remote_fin(S=#stream{local=fin}, State, fin) ->
	delete_stream(S#stream.id, State);
%% We always replace the stream in the state because
%% the content handler state has changed.
remote_fin(S, State=#http2_state{streams=Streams}, IsFin) ->
	Streams2 = lists:keyreplace(S#stream.id, #stream.id, Streams,
		S#stream{remote=IsFin}),
	State#http2_state{streams=Streams2}.

maybe_delete_stream(State, Stream=#stream{local=fin, remote=fin}) ->
	delete_stream(Stream#stream.id, State);
maybe_delete_stream(State=#http2_state{streams=Streams}, Stream) ->
	State#http2_state{streams=
		lists:keyreplace(Stream#stream.id, #stream.id, Streams, Stream)}.
