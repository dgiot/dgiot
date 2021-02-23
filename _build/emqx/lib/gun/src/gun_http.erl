%% Copyright (c) 2014-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(gun_http).

-export([check_options/1]).
-export([name/0]).
-export([init/4]).
-export([handle/2]).
-export([close/1]).
-export([keepalive/1]).
-export([request/8]).
-export([request/9]).
-export([data/5]).
-export([connect/5]).
-export([cancel/3]).
-export([down/1]).
-export([ws_upgrade/7]).

-type io() :: head | {body, non_neg_integer()} | body_close | body_chunked | body_trailer.

%% @todo Make that a record.
-type connect_info() :: {connect, reference(), gun:connect_destination()}.

%% @todo Make that a record.
-type websocket_info() :: {websocket, reference(), binary(), [binary()], gun:ws_opts()}. %% key, extensions, options

-record(stream, {
	ref :: reference() | connect_info() | websocket_info(),
	reply_to :: pid(),
	method :: binary(),
	is_alive :: boolean(),
	handler_state :: undefined | gun_content_handler:state()
}).

-record(http_state, {
	owner :: pid(),
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module(),
	version = 'HTTP/1.1' :: cow_http:version(),
	content_handlers :: gun_content_handler:opt(),
	connection = keepalive :: keepalive | close,
	buffer = <<>> :: binary(),
	streams = [] :: [#stream{}],
	in = head :: io(),
	in_state = {0, 0} :: {non_neg_integer(), non_neg_integer()},
	out = head :: io(),
	transform_header_name :: fun((binary()) -> binary())
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
do_check_options([{transform_header_name, F}|Opts]) when is_function(F) ->
	do_check_options(Opts);
do_check_options([{version, V}|Opts]) when V =:= 'HTTP/1.1'; V =:= 'HTTP/1.0' ->
	do_check_options(Opts);
do_check_options([Opt|_]) ->
	{error, {options, {http, Opt}}}.

name() -> http.

init(Owner, Socket, Transport, Opts) ->
	Version = maps:get(version, Opts, 'HTTP/1.1'),
	Handlers = maps:get(content_handlers, Opts, [gun_data_h]),
	TransformHeaderName = maps:get(transform_header_name, Opts, fun (N) -> N end),
	#http_state{owner=Owner, socket=Socket, transport=Transport, version=Version,
		content_handlers=Handlers, transform_header_name=TransformHeaderName}.

%% Stop looping when we got no more data.
handle(<<>>, State) ->
	{state, State};
%% Close when server responds and we don't have any open streams.
handle(_, #http_state{streams=[]}) ->
	close;
%% Wait for the full response headers before trying to parse them.
handle(Data, State=#http_state{in=head, buffer=Buffer}) ->
	Data2 = << Buffer/binary, Data/binary >>,
	case binary:match(Data2, <<"\r\n\r\n">>) of
		nomatch -> {state, State#http_state{buffer=Data2}};
		{_, _} -> handle_head(Data2, State#http_state{buffer= <<>>})
	end;
%% Everything sent to the socket until it closes is part of the response body.
handle(Data, State=#http_state{in=body_close}) ->
	{state, send_data_if_alive(Data, State, nofin)};
%% Chunked transfer-encoding may contain both data and trailers.
handle(Data, State=#http_state{in=body_chunked, in_state=InState,
		buffer=Buffer, connection=Conn}) ->
	Buffer2 = << Buffer/binary, Data/binary >>,
	case cow_http_te:stream_chunked(Buffer2, InState) of
		more ->
			{state, State#http_state{buffer=Buffer2}};
		{more, Data2, InState2} ->
			{state, send_data_if_alive(Data2,
				State#http_state{buffer= <<>>, in_state=InState2},
				nofin)};
		{more, Data2, Length, InState2} when is_integer(Length) ->
			%% @todo See if we can recv faster than one message at a time.
			{state, send_data_if_alive(Data2,
				State#http_state{buffer= <<>>, in_state=InState2},
				nofin)};
		{more, Data2, Rest, InState2} ->
			%% @todo See if we can recv faster than one message at a time.
			{state, send_data_if_alive(Data2,
				State#http_state{buffer=Rest, in_state=InState2},
				nofin)};
		{done, HasTrailers, Rest} ->
			IsFin = case HasTrailers of
				trailers -> nofin;
				no_trailers -> fin
			end,
			%% I suppose it doesn't hurt to append an empty binary.
			State1 = send_data_if_alive(<<>>, State, IsFin),
			case {HasTrailers, Conn} of
				{trailers, _} ->
					handle(Rest, State1#http_state{buffer = <<>>, in=body_trailer});
				{no_trailers, keepalive} ->
					handle(Rest, end_stream(State1#http_state{buffer= <<>>}));
				{no_trailers, close} ->
					close
			end;
		{done, Data2, HasTrailers, Rest} ->
			IsFin = case HasTrailers of
				trailers -> nofin;
				no_trailers -> fin
			end,
			State1 = send_data_if_alive(Data2, State, IsFin),
			case {HasTrailers, Conn} of
				{trailers, _} ->
					handle(Rest, State1#http_state{buffer = <<>>, in=body_trailer});
				{no_trailers, keepalive} ->
					handle(Rest, end_stream(State1#http_state{buffer= <<>>}));
				{no_trailers, close} ->
					close
			end
	end;
handle(Data, State=#http_state{in=body_trailer, buffer=Buffer, connection=Conn,
		streams=[#stream{ref=StreamRef, reply_to=ReplyTo}|_]}) ->
	Data2 = << Buffer/binary, Data/binary >>,
	case binary:match(Data2, <<"\r\n\r\n">>) of
		nomatch -> {state, State#http_state{buffer=Data2}};
		{_, _} ->
			{Trailers, Rest} = cow_http:parse_headers(Data2),
			%% @todo We probably want to pass this to gun_content_handler?
			ReplyTo ! {gun_trailers, self(), stream_ref(StreamRef), Trailers},
			case Conn of
				keepalive ->
					handle(Rest, end_stream(State#http_state{buffer= <<>>}));
				close ->
					close
			end
	end;
%% We know the length of the rest of the body.
handle(Data, State=#http_state{in={body, Length}, connection=Conn}) ->
	DataSize = byte_size(Data),
	if
		%% More data coming.
		DataSize < Length ->
			{state, send_data_if_alive(Data,
				State#http_state{in={body, Length - DataSize}},
				nofin)};
		%% Stream finished, no rest.
		DataSize =:= Length ->
			State1 = send_data_if_alive(Data, State, fin),
			case Conn of
				keepalive -> {state, end_stream(State1)};
				close -> close
			end;
		%% Stream finished, rest.
		true ->
			<< Body:Length/binary, Rest/bits >> = Data,
			State1 = send_data_if_alive(Body, State, fin),
			case Conn of
				keepalive -> handle(Rest, end_stream(State1));
				close -> close
			end
	end.

handle_head(Data, State=#http_state{socket=Socket, version=ClientVersion,
		content_handlers=Handlers0, connection=Conn,
		streams=[Stream=#stream{ref=StreamRef, reply_to=ReplyTo,
			method=Method, is_alive=IsAlive}|Tail]}) ->
	{Version, Status, _, Rest} = cow_http:parse_status_line(Data),
	{Headers, Rest2} = cow_http:parse_headers(Rest),
	case {Status, StreamRef} of
		{101, {websocket, RealStreamRef, WsKey, WsExtensions, WsOpts}} ->
			ws_handshake(Rest2, State, RealStreamRef, Headers, WsKey, WsExtensions, WsOpts);
		{_, {connect, RealStreamRef, Destination}} when Status >= 200, Status < 300 ->
			case IsAlive of
				false ->
					ok;
				true ->
					ReplyTo ! {gun_response, self(), RealStreamRef,
						fin, Status, Headers},
					ok
			end,
			%% We expect there to be no additional data after the CONNECT response.
			<<>> = Rest2,
			State2 = end_stream(State#http_state{streams=[Stream|Tail]}),
			NewHost = maps:get(host, Destination),
			NewPort = maps:get(port, Destination),
			case Destination of
				#{transport := tls} ->
					TLSOpts = maps:get(tls_opts, Destination, []),
					TLSTimeout = maps:get(tls_handshake_timeout, Destination, infinity),
					case gun_tls:connect(Socket, TLSOpts, TLSTimeout) of
						{ok, TLSSocket} ->
							case ssl:negotiated_protocol(TLSSocket) of
								{ok, <<"h2">>} ->
									[{origin, <<"https">>, NewHost, NewPort, connect},
										{switch_transport, gun_tls, TLSSocket},
										{switch_protocol, gun_http2, State2}];
								_ ->
									[{state, State2#http_state{socket=TLSSocket, transport=gun_tls}},
										{origin, <<"https">>, NewHost, NewPort, connect},
										{switch_transport, gun_tls, TLSSocket}]
							end;
						Error ->
							Error
					end;
				_ ->
					case maps:get(protocols, Destination, [http]) of
						[http] ->
							[{state, State2},
								{origin, <<"http">>, NewHost, NewPort, connect}];
						[http2] ->
							[{origin, <<"http">>, NewHost, NewPort, connect},
								{switch_protocol, gun_http2, State2}]
					end
			end;
		{_, _} when Status >= 100, Status =< 199 ->
			ReplyTo ! {gun_inform, self(), stream_ref(StreamRef), Status, Headers},
			handle(Rest2, State);
		_ ->
			In = response_io_from_headers(Method, Version, Status, Headers),
			IsFin = case In of head -> fin; _ -> nofin end,
			Handlers = case IsAlive of
				false ->
					undefined;
				true ->
					ReplyTo ! {gun_response, self(), stream_ref(StreamRef),
						IsFin, Status, Headers},
					case IsFin of
						fin -> undefined;
						nofin ->
							gun_content_handler:init(ReplyTo, StreamRef,
								Status, Headers, Handlers0)
					end
			end,
			Conn2 = if
				Conn =:= close -> close;
				Version =:= 'HTTP/1.0' -> close;
				ClientVersion =:= 'HTTP/1.0' -> close;
				true -> conn_from_headers(Version, Headers)
			end,
			%% We always reset in_state even if not chunked.
			if
				IsFin =:= fin, Conn2 =:= close ->
					close;
				IsFin =:= fin ->
					handle(Rest2, end_stream(State#http_state{in=In,
						in_state={0, 0}, connection=Conn2,
						streams=[Stream#stream{handler_state=Handlers}|Tail]}));
				true ->
					handle(Rest2, State#http_state{in=In,
						in_state={0, 0}, connection=Conn2,
						streams=[Stream#stream{handler_state=Handlers}|Tail]})
			end
	end.

stream_ref({connect, StreamRef, _}) -> StreamRef;
stream_ref({websocket, StreamRef, _, _, _}) -> StreamRef;
stream_ref(StreamRef) -> StreamRef.

send_data_if_alive(<<>>, State, nofin) ->
	State;
%% @todo What if we receive data when the HEAD method was used?
send_data_if_alive(Data, State=#http_state{streams=[Stream=#stream{
		is_alive=true, handler_state=Handlers0}|Tail]}, IsFin) ->
	Handlers = gun_content_handler:handle(IsFin, Data, Handlers0),
	State#http_state{streams=[Stream#stream{handler_state=Handlers}|Tail]};
send_data_if_alive(_, State, _) ->
	State.

close(State=#http_state{in=body_close, streams=[_|Tail]}) ->
	_ = send_data_if_alive(<<>>, State, fin),
	close_streams(Tail);
close(#http_state{streams=Streams}) ->
	close_streams(Streams).

close_streams([]) ->
	ok;
close_streams([#stream{is_alive=false}|Tail]) ->
	close_streams(Tail);
close_streams([#stream{ref=StreamRef, reply_to=ReplyTo}|Tail]) ->
	ReplyTo ! {gun_error, self(), StreamRef, {closed,
		"The connection was lost."}},
	close_streams(Tail).

%% We don't send a keep-alive when a CONNECT request was initiated.
keepalive(State=#http_state{streams=[#stream{ref={connect, _, _}}]}) ->
	State;
%% We can only keep-alive by sending an empty line in-between streams.
keepalive(State=#http_state{socket=Socket, transport=Transport, out=head}) ->
	Transport:send(Socket, <<"\r\n">>),
	State;
keepalive(State) ->
	State.

request(State=#http_state{socket=Socket, transport=Transport, version=Version,
		out=head}, StreamRef, ReplyTo, Method, Host, Port, Path, Headers) ->
	Host2 = case Host of
		{local, _SocketPath} -> <<>>;
		Tuple when is_tuple(Tuple) -> inet:ntoa(Tuple);
		_ -> Host
	end,
	Headers2 = lists:keydelete(<<"transfer-encoding">>, 1, Headers),
	Headers3 = case lists:keymember(<<"host">>, 1, Headers) of
		false -> [{<<"host">>, [Host2, $:, integer_to_binary(Port)]}|Headers2];
		true -> Headers2
	end,
	%% We use Headers2 because this is the smallest list.
	Conn = conn_from_headers(Version, Headers2),
	Out = request_io_from_headers(Headers2),
	Headers4 = case Out of
		body_chunked when Version =:= 'HTTP/1.0' -> Headers3;
		body_chunked -> [{<<"transfer-encoding">>, <<"chunked">>}|Headers3];
		_ -> Headers3
	end,
	Headers5 = transform_header_names(State, Headers4),
	Transport:send(Socket, cow_http:request(Method, Path, Version, Headers5)),
	new_stream(State#http_state{connection=Conn, out=Out}, StreamRef, ReplyTo, Method).

request(State=#http_state{socket=Socket, transport=Transport, version=Version,
		out=head}, StreamRef, ReplyTo, Method, Host, Port, Path, Headers, Body) ->
	Host2 = case Host of
		{local, _SocketPath} -> <<>>;
		Tuple when is_tuple(Tuple) -> inet:ntoa(Tuple);
		_ -> Host
	end,
	Headers2 = lists:keydelete(<<"content-length">>, 1,
		lists:keydelete(<<"transfer-encoding">>, 1, Headers)),
	Headers3 = case lists:keymember(<<"host">>, 1, Headers) of
		false -> [{<<"host">>, [Host2, $:, integer_to_binary(Port)]}|Headers2];
		true -> Headers2
	end,
	Headers4 = transform_header_names(State, Headers3),
	%% We use Headers2 because this is the smallest list.
	Conn = conn_from_headers(Version, Headers2),
	Transport:send(Socket, [
		cow_http:request(Method, Path, Version, [
			{<<"content-length">>, integer_to_binary(iolist_size(Body))}
		|Headers4]),
		Body]),
	new_stream(State#http_state{connection=Conn}, StreamRef, ReplyTo, Method).

transform_header_names(#http_state{transform_header_name = Fun}, Headers) ->
	lists:keymap(Fun, 1, Headers).

%% We are expecting a new stream.
data(State=#http_state{out=head}, StreamRef, ReplyTo, _, _) ->
	error_stream_closed(State, StreamRef, ReplyTo);
%% There are no active streams.
data(State=#http_state{streams=[]}, StreamRef, ReplyTo, _, _) ->
	error_stream_not_found(State, StreamRef, ReplyTo);
%% We can only send data on the last created stream.
data(State=#http_state{socket=Socket, transport=Transport, version=Version,
		out=Out, streams=Streams}, StreamRef, ReplyTo, IsFin, Data) ->
	case lists:last(Streams) of
		#stream{ref=StreamRef, is_alive=true} ->
			DataLength = iolist_size(Data),
			case Out of
				body_chunked when Version =:= 'HTTP/1.1', IsFin =:= fin ->
					case Data of
						<<>> ->
							Transport:send(Socket, cow_http_te:last_chunk());
						_ ->
							Transport:send(Socket, [
								cow_http_te:chunk(Data),
								cow_http_te:last_chunk()
							])
					end,
					State#http_state{out=head};
				body_chunked when Version =:= 'HTTP/1.1' ->
					Transport:send(Socket, cow_http_te:chunk(Data)),
					State;
				{body, Length} when DataLength =< Length ->
					Transport:send(Socket, Data),
					Length2 = Length - DataLength,
					if
						Length2 =:= 0, IsFin =:= fin ->
							State#http_state{out=head};
						Length2 > 0, IsFin =:= nofin ->
							State#http_state{out={body, Length2}}
					end;
				body_chunked -> %% HTTP/1.0
					Transport:send(Socket, Data),
					State
			end;
		_ ->
			error_stream_not_found(State, StreamRef, ReplyTo)
	end.

connect(State=#http_state{streams=Streams}, StreamRef, ReplyTo, _, _) when Streams =/= [] ->
	ReplyTo ! {gun_error, self(), StreamRef, {badstate,
		"CONNECT can only be used with HTTP/1.1 when no other streams are active."}},
	State;
connect(State=#http_state{socket=Socket, transport=Transport, version=Version},
		StreamRef, ReplyTo, Destination=#{host := Host0}, Headers0) ->
	Host = case Host0 of
		Tuple when is_tuple(Tuple) -> inet:ntoa(Tuple);
		_ -> Host0
	end,
	Port = maps:get(port, Destination, 1080),
	Authority = [Host, $:, integer_to_binary(Port)],
	Headers1 = lists:keydelete(<<"content-length">>, 1,
		lists:keydelete(<<"transfer-encoding">>, 1, Headers0)),
	Headers2 = case lists:keymember(<<"host">>, 1, Headers1) of
		false -> [{<<"host">>, Authority}|Headers1];
		true -> Headers1
	end,
	HasProxyAuthorization = lists:keymember(<<"proxy-authorization">>, 1, Headers2),
	Headers3 = case {HasProxyAuthorization, Destination} of
		{false, #{username := UserID, password := Password}} ->
			[{<<"proxy-authorization">>, [
					<<"Basic ">>,
					base64:encode(iolist_to_binary([UserID, $:, Password]))]}
				|Headers2];
		_ ->
			Headers2
	end,
	Headers = transform_header_names(State, Headers3),
	Transport:send(Socket, [
		cow_http:request(<<"CONNECT">>, Authority, Version, Headers)
	]),
	new_stream(State, {connect, StreamRef, Destination}, ReplyTo, <<"CONNECT">>).

%% We can't cancel anything, we can just stop forwarding messages to the owner.
cancel(State, StreamRef, ReplyTo) ->
	case is_stream(State, StreamRef) of
		true ->
			cancel_stream(State, StreamRef);
		false ->
			error_stream_not_found(State, StreamRef, ReplyTo)
	end.

%% HTTP does not provide any way to figure out what streams are unprocessed.
down(#http_state{streams=Streams}) ->
	KilledStreams = [case Ref of
		{connect, Ref2, _} -> Ref2;
		{websocket, Ref2, _, _, _} -> Ref2;
		_ -> Ref
	end || #stream{ref=Ref} <- Streams],
	{KilledStreams, []}.

error_stream_closed(State, StreamRef, ReplyTo) ->
	ReplyTo ! {gun_error, self(), StreamRef, {badstate,
		"The stream has already been closed."}},
	State.

error_stream_not_found(State, StreamRef, ReplyTo) ->
	ReplyTo ! {gun_error, self(), StreamRef, {badstate,
		"The stream cannot be found."}},
	State.

%% Headers information retrieval.

conn_from_headers(Version, Headers) ->
	case lists:keyfind(<<"connection">>, 1, Headers) of
		false when Version =:= 'HTTP/1.0' ->
			close;
		false ->
			keepalive;
		{_, ConnHd} ->
			ConnList = cow_http_hd:parse_connection(ConnHd),
			case lists:member(<<"keep-alive">>, ConnList) of
				true -> keepalive;
				false -> close
			end
	end.

request_io_from_headers(Headers) ->
	case lists:keyfind(<<"content-length">>, 1, Headers) of
		{_, <<"0">>} ->
			head;
		{_, Length} ->
			{body, cow_http_hd:parse_content_length(Length)};
		_ ->
			case lists:keymember(<<"content-type">>, 1, Headers) of
				true -> body_chunked;
				false -> head
			end
	end.

response_io_from_headers(<<"HEAD">>, _, _, _) ->
	head;
response_io_from_headers(_, _, Status, _) when (Status =:= 204) or (Status =:= 304) ->
	head;
response_io_from_headers(_, Version, _Status, Headers) ->
	case lists:keyfind(<<"content-length">>, 1, Headers) of
		{_, <<"0">>} ->
			head;
		{_, Length} ->
			{body, cow_http_hd:parse_content_length(Length)};
		_ when Version =:= 'HTTP/1.0' ->
			body_close;
		_ ->
			case lists:keyfind(<<"transfer-encoding">>, 1, Headers) of
				false ->
					body_close;
				{_, TE} ->
					case cow_http_hd:parse_transfer_encoding(TE) of
						[<<"chunked">>] -> body_chunked;
						[<<"identity">>] -> body_close
					end
			end
	end.

%% Streams.

new_stream(State=#http_state{streams=Streams}, StreamRef, ReplyTo, Method) ->
	State#http_state{streams=Streams
		++ [#stream{ref=StreamRef, reply_to=ReplyTo,
			method=iolist_to_binary(Method), is_alive=true}]}.

is_stream(#http_state{streams=Streams}, StreamRef) ->
	lists:keymember(StreamRef, #stream.ref, Streams).

cancel_stream(State=#http_state{streams=Streams}, StreamRef) ->
	Streams2 = [case Ref of
		StreamRef ->
			Tuple#stream{is_alive=false};
		_ ->
			Tuple
	end || Tuple = #stream{ref=Ref} <- Streams],
	State#http_state{streams=Streams2}.

end_stream(State=#http_state{streams=[_|Tail]}) ->
	State#http_state{in=head, streams=Tail}.

%% Websocket upgrade.

%% Ensure version is 1.1.
ws_upgrade(#http_state{version='HTTP/1.0'}, _, _, _, _, _, _) ->
	error; %% @todo
ws_upgrade(State=#http_state{socket=Socket, transport=Transport, owner=Owner, out=head},
		StreamRef, Host, Port, Path, Headers0, WsOpts) ->
	{Headers1, GunExtensions} = case maps:get(compress, WsOpts, false) of
		true -> {[{<<"sec-websocket-extensions">>,
				<<"permessage-deflate; client_max_window_bits; server_max_window_bits=15">>}
			|Headers0],
			[<<"permessage-deflate">>]};
		false -> {Headers0, []}
	end,
	Headers2 = case maps:get(protocols, WsOpts, []) of
		[] -> Headers1;
		ProtoOpt ->
			<< _, _, Proto/bits >> = iolist_to_binary([[<<", ">>, P] || {P, _} <- ProtoOpt]),
			[{<<"sec-websocket-protocol">>, Proto}|Headers1]
	end,
	Key = cow_ws:key(),
	Headers3 = [
		{<<"connection">>, <<"upgrade">>},
		{<<"upgrade">>, <<"websocket">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"sec-websocket-key">>, Key}
		|Headers2
	],
	IsSecure = Transport =:= gun_tls,
	Headers = case lists:keymember(<<"host">>, 1, Headers0) of
		true -> Headers3;
		false when Port =:= 80, not IsSecure -> [{<<"host">>, Host}|Headers3];
		false when Port =:= 443, IsSecure -> [{<<"host">>, Host}|Headers3];
		false -> [{<<"host">>, [Host, $:, integer_to_binary(Port)]}|Headers3]
	end,
	Transport:send(Socket, cow_http:request(<<"GET">>, Path, 'HTTP/1.1', Headers)),
	new_stream(State#http_state{connection=keepalive, out=head},
		{websocket, StreamRef, Key, GunExtensions, WsOpts}, Owner, <<"GET">>).

ws_handshake(Buffer, State, StreamRef, Headers, Key, GunExtensions, Opts) ->
	%% @todo check upgrade, connection
	case lists:keyfind(<<"sec-websocket-accept">>, 1, Headers) of
		false ->
			close;
		{_, Accept} ->
			case cow_ws:encode_key(Key) of
				Accept ->
					ws_handshake_extensions(Buffer, State, StreamRef,
						Headers, GunExtensions, Opts);
				_ ->
					close
			end
	end.

ws_handshake_extensions(Buffer, State, StreamRef, Headers, GunExtensions, Opts) ->
	case lists:keyfind(<<"sec-websocket-extensions">>, 1, Headers) of
		false ->
			ws_handshake_protocols(Buffer, State, StreamRef, Headers, #{}, Opts);
		{_, ExtHd} ->
			case ws_validate_extensions(cow_http_hd:parse_sec_websocket_extensions(ExtHd), GunExtensions, #{}, Opts) of
				close ->
					close;
				Extensions ->
					ws_handshake_protocols(Buffer, State, StreamRef, Headers, Extensions, Opts)
			end
	end.

ws_validate_extensions([], _, Acc, _) ->
	Acc;
ws_validate_extensions([{Name = <<"permessage-deflate">>, Params}|Tail], GunExts, Acc, Opts) ->
	case lists:member(Name, GunExts) of
		true ->
			case cow_ws:validate_permessage_deflate(Params, Acc, Opts) of
				{ok, Acc2} -> ws_validate_extensions(Tail, GunExts, Acc2, Opts);
				error -> close
			end;
		%% Fail the connection if extension was not requested.
		false ->
			close
	end;
%% Fail the connection on unknown extension.
ws_validate_extensions(_, _, _, _) ->
	close.

%% @todo Validate protocols.
ws_handshake_protocols(Buffer, State, StreamRef, Headers, Extensions, Opts) ->
	case lists:keyfind(<<"sec-websocket-protocol">>, 1, Headers) of
		false ->
			ws_handshake_end(Buffer, State, StreamRef, Headers, Extensions,
				maps:get(default_protocol, Opts, gun_ws_h), Opts);
		{_, Proto} ->
			ProtoOpt = maps:get(protocols, Opts, []),
			case lists:keyfind(Proto, 1, ProtoOpt) of
				{_, Handler} ->
					ws_handshake_end(Buffer, State, StreamRef,
						Headers, Extensions, Handler, Opts);
				false ->
					close
			end
	end.

ws_handshake_end(Buffer, #http_state{owner=Owner, socket=Socket, transport=Transport},
		StreamRef, Headers, Extensions, Handler, Opts) ->
	%% Send ourselves the remaining buffer, if any.
	_ = case Buffer of
		<<>> ->
			ok;
		_ ->
			{OK, _, _} = Transport:messages(),
			self() ! {OK, Socket, Buffer}
	end,
	gun_ws:init(Owner, Socket, Transport, StreamRef, Headers, Extensions, Handler, Opts).
