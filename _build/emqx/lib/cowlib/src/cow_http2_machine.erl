%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(cow_http2_machine).

-export([init/2]).
-export([init_stream/2]).
-export([init_upgrade_stream/2]).
-export([frame/2]).
-export([ignored_frame/1]).
-export([timeout/3]).
-export([prepare_headers/5]).
-export([prepare_push_promise/4]).
-export([prepare_trailers/3]).
-export([send_or_queue_data/4]).
-export([ensure_window/2]).
-export([ensure_window/3]).
-export([update_window/2]).
-export([update_window/3]).
-export([reset_stream/2]).
-export([get_connection_local_buffer_size/1]).
-export([get_local_setting/2]).
-export([get_last_streamid/1]).
-export([get_stream_local_buffer_size/2]).
-export([get_stream_local_state/2]).
-export([get_stream_remote_state/2]).
-export([is_lingering_stream/2]).

-type opts() :: #{
	connection_window_margin_size => 0..16#7fffffff,
	connection_window_update_threshold => 0..16#7fffffff,
	enable_connect_protocol => boolean(),
	initial_connection_window_size => 65535..16#7fffffff,
	initial_stream_window_size => 0..16#7fffffff,
	max_connection_window_size => 0..16#7fffffff,
	max_concurrent_streams => non_neg_integer() | infinity,
	max_decode_table_size => non_neg_integer(),
	max_encode_table_size => non_neg_integer(),
	max_frame_size_received => 16384..16777215,
	max_frame_size_sent => 16384..16777215 | infinity,
	max_stream_window_size => 0..16#7fffffff,
	preface_timeout => timeout(),
	settings_timeout => timeout(),
	stream_window_data_threshold => 0..16#7fffffff,
	stream_window_margin_size => 0..16#7fffffff,
	stream_window_update_threshold => 0..16#7fffffff
}.
-export_type([opts/0]).

%% The order of the fields is significant.
-record(sendfile, {
	offset :: non_neg_integer(),
	bytes :: pos_integer(),
	path :: file:name_all()
}).

-record(stream, {
	id = undefined :: cow_http2:streamid(),

	%% Request method.
	method = undefined :: binary(),

	%% Whether we finished sending data.
	local = idle :: idle | cow_http2:fin(),

	%% Local flow control window (how much we can send).
	local_window :: integer(),

	%% Buffered data waiting for the flow control window to increase.
	local_buffer = queue:new() ::
		queue:queue({cow_http2:fin(), non_neg_integer(), {data, iodata()} | #sendfile{}}),
	local_buffer_size = 0 :: non_neg_integer(),
	local_trailers = undefined :: undefined | cow_http:headers(),

	%% Whether we finished receiving data.
	remote = idle :: idle | cow_http2:fin(),

	%% Remote flow control window (how much we accept to receive).
	remote_window :: integer(),

	%% Size expected and read from the request body.
	remote_expected_size = undefined :: undefined | non_neg_integer(),
	remote_read_size = 0 :: non_neg_integer(),

	%% Unparsed te header. Used to know if we can send trailers.
	%% Note that we can always send trailers to the server.
	te :: undefined | binary()
}).

-type stream() :: #stream{}.

-type continued_frame() ::
	{headers, cow_http2:streamid(), cow_http2:fin(), cow_http2:head_fin(), binary()} |
	{push_promise, cow_http2:streamid(), cow_http2:head_fin(), cow_http2:streamid(), binary()}.

-record(http2_machine, {
	%% Whether the HTTP/2 endpoint is a client or a server.
	mode :: client | server,

	%% HTTP/2 SETTINGS customization.
	opts = #{} :: opts(),

	%% Connection-wide frame processing state.
	state = settings :: settings | normal
		| {continuation, request | response | trailers | push_promise, continued_frame()},

	%% Timer for the connection preface.
	preface_timer = undefined :: undefined | reference(),

	%% Timer for the ack for a SETTINGS frame we sent.
	settings_timer = undefined :: undefined | reference(),

	%% Settings are separate for each endpoint. In addition, settings
	%% must be acknowledged before they can be expected to be applied.
	local_settings = #{
%		header_table_size => 4096,
%		enable_push => true,
%		max_concurrent_streams => infinity,
		initial_window_size => 65535
%		max_frame_size => 16384
%		max_header_list_size => infinity
	} :: map(),
	next_settings = undefined :: undefined | map(),
	remote_settings = #{
		initial_window_size => 65535
	} :: map(),

	%% Connection-wide flow control window.
	local_window = 65535 :: integer(), %% How much we can send.
	remote_window = 65535 :: integer(), %% How much we accept to receive.

	%% Stream identifiers.
	local_streamid :: pos_integer(), %% The next streamid to be used.
	remote_streamid = 0 :: non_neg_integer(), %% The last streamid received.

	%% Currently active HTTP/2 streams. Streams may be initiated either
	%% by the client or by the server through PUSH_PROMISE frames.
	streams = [] :: [stream()],

	%% HTTP/2 streams that have recently been reset locally.
	%% We are expected to keep receiving additional frames after
	%% sending an RST_STREAM.
	local_lingering_streams = [] :: [cow_http2:streamid()],

	%% HTTP/2 streams that have recently been reset remotely.
	%% We keep a few of these around in order to reject subsequent
	%% frames on these streams.
	remote_lingering_streams = [] :: [cow_http2:streamid()],

	%% HPACK decoding and encoding state.
	decode_state = cow_hpack:init() :: cow_hpack:state(),
	encode_state = cow_hpack:init() :: cow_hpack:state()
}).

-opaque http2_machine() :: #http2_machine{}.
-export_type([http2_machine/0]).

-type pseudo_headers() :: #{} %% Trailers
	| #{ %% Responses.
		status := cow_http:status()
	} | #{ %% Normal CONNECT requests.
		method := binary(),
		authority := binary()
	} | #{ %% Other requests and extended CONNECT requests.
		method := binary(),
		scheme := binary(),
		authority := binary(),
		path := binary(),
		protocol => binary()
	}.

%% Returns true when the given StreamID is for a local-initiated stream.
-define(IS_SERVER_LOCAL(StreamID), ((StreamID rem 2) =:= 0)).
-define(IS_CLIENT_LOCAL(StreamID), ((StreamID rem 2) =:= 1)).
-define(IS_LOCAL(Mode, StreamID), (
	((Mode =:= server) andalso ?IS_SERVER_LOCAL(StreamID))
	orelse
	((Mode =:= client) andalso ?IS_CLIENT_LOCAL(StreamID))
)).

-spec init(client | server, opts()) -> {ok, iodata(), http2_machine()}.
init(client, Opts) ->
	NextSettings = settings_init(Opts),
	client_preface(#http2_machine{
		mode=client,
		opts=Opts,
		preface_timer=start_timer(preface_timeout, Opts),
		settings_timer=start_timer(settings_timeout, Opts),
		next_settings=NextSettings,
		local_streamid=1
	});
init(server, Opts) ->
	NextSettings = settings_init(Opts),
	common_preface(#http2_machine{
		mode=server,
		opts=Opts,
		preface_timer=start_timer(preface_timeout, Opts),
		settings_timer=start_timer(settings_timeout, Opts),
		next_settings=NextSettings,
		local_streamid=2
	}).

start_timer(Name, Opts) ->
	case maps:get(Name, Opts, 5000) of
		infinity -> undefined;
		Timeout -> erlang:start_timer(Timeout, self(), {?MODULE, Name})
	end.

client_preface(State0) ->
	{ok, CommonPreface, State} = common_preface(State0),
	{ok, [
		<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>,
		CommonPreface
	], State}.

%% We send next_settings and use defaults until we get an ack.
%%
%% We also send a WINDOW_UPDATE frame for the connection when
%% the user specified an initial_connection_window_size.
common_preface(State=#http2_machine{opts=Opts, next_settings=NextSettings}) ->
	case maps:get(initial_connection_window_size, Opts, 65535) of
		65535 ->
			{ok, cow_http2:settings(NextSettings), State};
		Size ->
			{ok, [
				cow_http2:settings(NextSettings),
				cow_http2:window_update(Size - 65535)
			], update_window(Size - 65535, State)}
	end.

settings_init(Opts) ->
	S0 = setting_from_opt(#{}, Opts, max_decode_table_size,
		header_table_size, 4096),
	S1 = setting_from_opt(S0, Opts, max_concurrent_streams,
		max_concurrent_streams, infinity),
	S2 = setting_from_opt(S1, Opts, initial_stream_window_size,
		initial_window_size, 65535),
	S3 = setting_from_opt(S2, Opts, max_frame_size_received,
		max_frame_size, 16384),
	%% @todo max_header_list_size
	setting_from_opt(S3, Opts, enable_connect_protocol,
		enable_connect_protocol, false).

setting_from_opt(Settings, Opts, OptName, SettingName, Default) ->
	case maps:get(OptName, Opts, Default) of
		Default -> Settings;
		Value -> Settings#{SettingName => Value}
	end.

-spec init_stream(binary(), State)
	-> {ok, cow_http2:streamid(), State} when State::http2_machine().
init_stream(Method, State=#http2_machine{mode=client, local_streamid=LocalStreamID,
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow}}) ->
	Stream = #stream{id=LocalStreamID, method=Method,
		local_window=LocalWindow, remote_window=RemoteWindow},
	{ok, LocalStreamID, stream_store(Stream, State#http2_machine{
		local_streamid=LocalStreamID + 2})}.

-spec init_upgrade_stream(binary(), State)
	-> {ok, cow_http2:streamid(), State} when State::http2_machine().
init_upgrade_stream(Method, State=#http2_machine{mode=server, remote_streamid=0,
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow}}) ->
	Stream = #stream{id=1, method=Method,
		remote=fin, remote_expected_size=0,
		local_window=LocalWindow, remote_window=RemoteWindow, te=undefined},
	{ok, 1, stream_store(Stream, State#http2_machine{remote_streamid=1})}.

-spec frame(cow_http2:frame(), State)
	-> {ok, State}
	| {ok, {data, cow_http2:streamid(), cow_http2:fin(), binary()}, State}
	| {ok, {headers, cow_http2:streamid(), cow_http2:fin(),
		cow_http:headers(), pseudo_headers(), non_neg_integer() | undefined}, State}
	| {ok, {trailers, cow_http2:streamid(), cow_http:headers()}, State}
	| {ok, {rst_stream, cow_http2:streamid(), cow_http2:error()}, State}
	| {ok, {push_promise, cow_http2:streamid(), cow_http2:streamid(),
		cow_http:headers(), pseudo_headers()}, State}
	| {ok, {goaway, cow_http2:streamid(), cow_http2:error(), binary()}, State}
	| {send, [{cow_http2:streamid(), cow_http2:fin(),
		[{data, iodata()} | #sendfile{} | {trailers, cow_http:headers()}]}], State}
	| {error, {stream_error, cow_http2:streamid(), cow_http2:error(), atom()}, State}
	| {error, {connection_error, cow_http2:error(), atom()}, State}
	when State::http2_machine().
frame(Frame, State=#http2_machine{state=settings, preface_timer=TRef}) ->
	ok = case TRef of
		undefined -> ok;
		_ -> erlang:cancel_timer(TRef, [{async, true}, {info, false}])
	end,
	settings_frame(Frame, State#http2_machine{state=normal, preface_timer=undefined});
frame(Frame, State=#http2_machine{state={continuation, _, _}}) ->
	continuation_frame(Frame, State);
frame(settings_ack, State=#http2_machine{state=normal}) ->
	settings_ack_frame(State);
frame(Frame, State=#http2_machine{state=normal}) ->
	case element(1, Frame) of
		data -> data_frame(Frame, State);
		headers -> headers_frame(Frame, State);
		priority -> priority_frame(Frame, State);
		rst_stream -> rst_stream_frame(Frame, State);
		settings -> settings_frame(Frame, State);
		push_promise -> push_promise_frame(Frame, State);
		ping -> ping_frame(Frame, State);
		ping_ack -> ping_ack_frame(Frame, State);
		goaway -> goaway_frame(Frame, State);
		window_update -> window_update_frame(Frame, State);
		continuation -> unexpected_continuation_frame(Frame, State);
		_ -> ignored_frame(State)
	end.

%% DATA frame.

data_frame({data, StreamID, _, _}, State=#http2_machine{mode=Mode,
		local_streamid=LocalStreamID, remote_streamid=RemoteStreamID})
		when (?IS_LOCAL(Mode, StreamID) andalso (StreamID >= LocalStreamID))
		orelse ((not ?IS_LOCAL(Mode, StreamID)) andalso (StreamID > RemoteStreamID)) ->
	{error, {connection_error, protocol_error,
		'DATA frame received on a stream in idle state. (RFC7540 5.1)'},
		State};
data_frame({data, _, _, Data}, State=#http2_machine{remote_window=ConnWindow})
		when byte_size(Data) > ConnWindow ->
	{error, {connection_error, flow_control_error,
		'DATA frame overflowed the connection flow control window. (RFC7540 6.9, RFC7540 6.9.1)'},
		State};
data_frame(Frame={data, StreamID, _, Data}, State0=#http2_machine{
		remote_window=ConnWindow, local_lingering_streams=Lingering}) ->
	DataLen = byte_size(Data),
	State = State0#http2_machine{remote_window=ConnWindow - DataLen},
	case stream_get(StreamID, State) of
		#stream{remote_window=StreamWindow} when StreamWindow < DataLen ->
			stream_reset(StreamID, State, flow_control_error,
				'DATA frame overflowed the stream flow control window. (RFC7540 6.9, RFC7540 6.9.1)');
		Stream = #stream{remote=nofin} ->
			data_frame(Frame, State, Stream, DataLen);
		#stream{remote=idle} ->
			stream_reset(StreamID, State, protocol_error,
				'DATA frame received before a HEADERS frame. (RFC7540 8.1, RFC7540 8.1.2.6)');
		#stream{remote=fin} ->
			stream_reset(StreamID, State, stream_closed,
				'DATA frame received for a half-closed (remote) stream. (RFC7540 5.1)');
		undefined ->
			%% After we send an RST_STREAM frame and terminate a stream,
			%% the remote endpoint might still be sending us some more
			%% frames until it can process this RST_STREAM.
			case lists:member(StreamID, Lingering) of
				true ->
					{ok, State};
				false ->
					{error, {connection_error, stream_closed,
						'DATA frame received for a closed stream. (RFC7540 5.1)'},
						State}
			end
	end.

data_frame(Frame={data, _, IsFin, _}, State0, Stream0=#stream{id=StreamID,
		remote_window=StreamWindow, remote_read_size=StreamRead}, DataLen) ->
	Stream = Stream0#stream{remote=IsFin,
		remote_window=StreamWindow - DataLen,
		remote_read_size=StreamRead + DataLen},
	State = stream_store(Stream, State0),
	case is_body_size_valid(Stream) of
		true ->
			{ok, Frame, State};
		false ->
			stream_reset(StreamID, State, protocol_error,
				'The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)')
	end.

%% It's always valid when no content-length header was specified.
is_body_size_valid(#stream{remote_expected_size=undefined}) ->
	true;
%% We didn't finish reading the body but the size is already larger than expected.
is_body_size_valid(#stream{remote=nofin, remote_expected_size=Expected,
		remote_read_size=Read}) when Read > Expected ->
	false;
is_body_size_valid(#stream{remote=nofin}) ->
	true;
is_body_size_valid(#stream{remote=fin, remote_expected_size=Expected,
		remote_read_size=Expected}) ->
	true;
%% We finished reading the body and the size read is not the one expected.
is_body_size_valid(_) ->
	false.

%% HEADERS frame.
%%
%% We always close the connection when we detect errors before
%% decoding the headers to not waste resources on non-compliant
%% endpoints, making us stricter than the RFC requires.

%% Convenience record to manipulate the tuple.
%% The order of the fields matter.
-record(headers, {
	id :: cow_http2:streamid(),
	fin :: cow_http2:fin(),
	head :: cow_http2:head_fin(),
	data :: binary()
}).

headers_frame(Frame=#headers{}, State=#http2_machine{mode=Mode}) ->
	case Mode of
		server -> server_headers_frame(Frame, State);
		client -> client_headers_frame(Frame, State)
	end;
%% @todo Handle the PRIORITY data, but only if this returns an ok tuple.
%% @todo Do not lose the PRIORITY information if CONTINUATION frames follow.
headers_frame({headers, StreamID, IsFin, IsHeadFin,
		_IsExclusive, _DepStreamID, _Weight, HeaderData},
		State=#http2_machine{mode=Mode}) ->
	HeadersFrame = #headers{id=StreamID, fin=IsFin, head=IsHeadFin, data=HeaderData},
	case Mode of
		server -> server_headers_frame(HeadersFrame, State);
		client -> client_headers_frame(HeadersFrame, State)
	end.

%% Reject HEADERS frames with even-numbered streamid.
server_headers_frame(#headers{id=StreamID}, State)
		when ?IS_SERVER_LOCAL(StreamID) ->
	{error, {connection_error, protocol_error,
		'HEADERS frame received with even-numbered streamid. (RFC7540 5.1.1)'},
		State};
%% HEADERS frame on an idle stream: new request.
server_headers_frame(Frame=#headers{id=StreamID, head=IsHeadFin},
		State=#http2_machine{mode=server, remote_streamid=RemoteStreamID})
		when StreamID > RemoteStreamID ->
	case IsHeadFin of
		head_fin ->
			headers_decode(Frame, State, request, undefined);
		head_nofin ->
			{ok, State#http2_machine{state={continuation, request, Frame}}}
	end;
%% Either a HEADERS frame received on (half-)closed stream,
%% or a HEADERS frame containing the trailers.
server_headers_frame(Frame=#headers{id=StreamID, fin=IsFin, head=IsHeadFin}, State) ->
	case stream_get(StreamID, State) of
		%% Trailers.
		Stream = #stream{remote=nofin} when IsFin =:= fin ->
			case IsHeadFin of
				head_fin ->
					headers_decode(Frame, State, trailers, Stream);
				head_nofin ->
					{ok, State#http2_machine{state={continuation, trailers, Frame}}}
			end;
		#stream{remote=nofin} ->
			{error, {connection_error, protocol_error,
				'Trailing HEADERS frame received without the END_STREAM flag set. (RFC7540 8.1, RFC7540 8.1.2.6)'},
				State};
		_ ->
			{error, {connection_error, stream_closed,
				'HEADERS frame received on a stream in closed or half-closed state. (RFC7540 5.1)'},
				State}
	end.

%% Either a HEADERS frame received on an (half-)closed stream,
%% or a HEADERS frame containing the response or the trailers.
client_headers_frame(Frame=#headers{id=StreamID, fin=IsFin, head=IsHeadFin},
		State=#http2_machine{local_streamid=LocalStreamID, remote_streamid=RemoteStreamID})
		when (?IS_CLIENT_LOCAL(StreamID) andalso (StreamID < LocalStreamID))
		orelse ((not ?IS_CLIENT_LOCAL(StreamID)) andalso (StreamID =< RemoteStreamID)) ->
	case stream_get(StreamID, State) of
		Stream = #stream{remote=idle} ->
			case IsHeadFin of
				head_fin ->
					headers_decode(Frame, State, response, Stream);
				head_nofin ->
					{ok, State#http2_machine{state={continuation, response, Frame}}}
			end;
		Stream = #stream{remote=nofin} when IsFin =:= fin ->
			case IsHeadFin of
				head_fin ->
					headers_decode(Frame, State, trailers, Stream);
				head_nofin ->
					{ok, State#http2_machine{state={continuation, trailers, Frame}}}
			end;
		#stream{remote=nofin} ->
			{error, {connection_error, protocol_error,
				'Trailing HEADERS frame received without the END_STREAM flag set. (RFC7540 8.1, RFC7540 8.1.2.6)'},
				State};
		_ ->
			{error, {connection_error, stream_closed,
				'HEADERS frame received on a stream in closed or half-closed state. (RFC7540 5.1)'},
				State}
	end;
%% Reject HEADERS frames received on idle streams.
client_headers_frame(_, State) ->
	{error, {connection_error, protocol_error,
		'HEADERS frame received on an idle stream. (RFC7540 5.1.1)'},
		State}.

headers_decode(Frame=#headers{head=head_fin, data=HeaderData},
		State=#http2_machine{decode_state=DecodeState0}, Type, Stream) ->
	try cow_hpack:decode(HeaderData, DecodeState0) of
		{Headers, DecodeState} when Type =:= request ->
			headers_enforce_concurrency_limit(Frame,
				State#http2_machine{decode_state=DecodeState}, Type, Stream, Headers);
		{Headers, DecodeState} ->
			headers_pseudo_headers(Frame,
				State#http2_machine{decode_state=DecodeState}, Type, Stream, Headers)
	catch _:_ ->
		{error, {connection_error, compression_error,
			'Error while trying to decode HPACK-encoded header block. (RFC7540 4.3)'},
			State}
	end.

headers_enforce_concurrency_limit(Frame=#headers{id=StreamID},
		State=#http2_machine{local_settings=LocalSettings, streams=Streams},
		Type, Stream, Headers) ->
	MaxConcurrentStreams = maps:get(max_concurrent_streams, LocalSettings, infinity),
	%% Using < is correct because this new stream is not included
	%% in the Streams variable yet and so we'll end up with +1 stream.
	case length(Streams) < MaxConcurrentStreams of
		true ->
			headers_pseudo_headers(Frame, State, Type, Stream, Headers);
		false ->
			{error, {stream_error, StreamID, refused_stream,
				'Maximum number of concurrent streams has been reached. (RFC7540 5.1.2)'},
				State}
	end.

headers_pseudo_headers(Frame, State=#http2_machine{local_settings=LocalSettings},
		Type, Stream, Headers0) when Type =:= request; Type =:= push_promise ->
	IsExtendedConnectEnabled = maps:get(enable_connect_protocol, LocalSettings, false),
	case request_pseudo_headers(Headers0, #{}) of
		%% Extended CONNECT method (RFC8441).
		{ok, PseudoHeaders=#{method := <<"CONNECT">>, scheme := _,
			authority := _, path := _, protocol := _}, Headers}
			when IsExtendedConnectEnabled ->
			headers_regular_headers(Frame, State, Type, Stream, PseudoHeaders, Headers);
		{ok, #{method := <<"CONNECT">>, scheme := _,
			authority := _, path := _}, _}
			when IsExtendedConnectEnabled ->
			headers_malformed(Frame, State,
				'The :protocol pseudo-header MUST be sent with an extended CONNECT. (RFC8441 4)');
		{ok, #{protocol := _}, _} ->
			headers_malformed(Frame, State,
				'The :protocol pseudo-header is only defined for the extended CONNECT. (RFC8441 4)');
		%% Normal CONNECT (no scheme/path).
		{ok, PseudoHeaders=#{method := <<"CONNECT">>, authority := _}, Headers}
				when map_size(PseudoHeaders) =:= 2 ->
			headers_regular_headers(Frame, State, Type, Stream, PseudoHeaders, Headers);
		{ok, #{method := <<"CONNECT">>}, _} ->
			headers_malformed(Frame, State,
				'CONNECT requests only use the :method and :authority pseudo-headers. (RFC7540 8.3)');
		%% Other requests.
		{ok, PseudoHeaders=#{method := _, scheme := _, path := _}, Headers} ->
			headers_regular_headers(Frame, State, Type, Stream, PseudoHeaders, Headers);
		{ok, _, _} ->
			headers_malformed(Frame, State,
				'A required pseudo-header was not found. (RFC7540 8.1.2.3)');
		{error, HumanReadable} ->
			headers_malformed(Frame, State, HumanReadable)
	end;
headers_pseudo_headers(Frame=#headers{id=StreamID},
		State, Type=response, Stream, Headers0) ->
	case response_pseudo_headers(Headers0, #{}) of
		{ok, PseudoHeaders=#{status := _}, Headers} ->
			headers_regular_headers(Frame, State, Type, Stream, PseudoHeaders, Headers);
		{ok, _, _} ->
			stream_reset(StreamID, State, protocol_error,
				'A required pseudo-header was not found. (RFC7540 8.1.2.4)');
		{error, HumanReadable} ->
			stream_reset(StreamID, State, protocol_error, HumanReadable)
	end;
headers_pseudo_headers(Frame=#headers{id=StreamID},
		State, Type=trailers, Stream, Headers) ->
	case trailers_contain_pseudo_headers(Headers) of
		false ->
			headers_regular_headers(Frame, State, Type, Stream, #{}, Headers);
		true ->
			stream_reset(StreamID, State, protocol_error,
				'Trailer header blocks must not contain pseudo-headers. (RFC7540 8.1.2.1)')
	end.

headers_malformed(#headers{id=StreamID}, State, HumanReadable) ->
	{error, {stream_error, StreamID, protocol_error, HumanReadable}, State}.

request_pseudo_headers([{<<":method">>, _}|_], #{method := _}) ->
	{error, 'Multiple :method pseudo-headers were found. (RFC7540 8.1.2.3)'};
request_pseudo_headers([{<<":method">>, Method}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{method => Method});
request_pseudo_headers([{<<":scheme">>, _}|_], #{scheme := _}) ->
	{error, 'Multiple :scheme pseudo-headers were found. (RFC7540 8.1.2.3)'};
request_pseudo_headers([{<<":scheme">>, Scheme}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{scheme => Scheme});
request_pseudo_headers([{<<":authority">>, _}|_], #{authority := _}) ->
	{error, 'Multiple :authority pseudo-headers were found. (RFC7540 8.1.2.3)'};
request_pseudo_headers([{<<":authority">>, Authority}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{authority => Authority});
request_pseudo_headers([{<<":path">>, _}|_], #{path := _}) ->
	{error, 'Multiple :path pseudo-headers were found. (RFC7540 8.1.2.3)'};
request_pseudo_headers([{<<":path">>, Path}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{path => Path});
request_pseudo_headers([{<<":protocol">>, _}|_], #{protocol := _}) ->
	{error, 'Multiple :protocol pseudo-headers were found. (RFC7540 8.1.2.3)'};
request_pseudo_headers([{<<":protocol">>, Protocol}|Tail], PseudoHeaders) ->
	request_pseudo_headers(Tail, PseudoHeaders#{protocol => Protocol});
request_pseudo_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, 'An unknown or invalid pseudo-header was found. (RFC7540 8.1.2.1)'};
request_pseudo_headers(Headers, PseudoHeaders) ->
	{ok, PseudoHeaders, Headers}.

response_pseudo_headers([{<<":status">>, _}|_], #{status := _}) ->
	{error, 'Multiple :status pseudo-headers were found. (RFC7540 8.1.2.3)'};
response_pseudo_headers([{<<":status">>, Status}|Tail], PseudoHeaders) ->
	try cow_http:status_to_integer(Status) of
		IntStatus ->
			response_pseudo_headers(Tail, PseudoHeaders#{status => IntStatus})
	catch _:_ ->
		{error, 'The :status pseudo-header value is invalid. (RFC7540 8.1.2.4)'}
	end;
response_pseudo_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, 'An unknown or invalid pseudo-header was found. (RFC7540 8.1.2.1)'};
response_pseudo_headers(Headers, PseudoHeaders) ->
	{ok, PseudoHeaders, Headers}.

trailers_contain_pseudo_headers([]) ->
	false;
trailers_contain_pseudo_headers([{<<":", _/bits>>, _}|_]) ->
	true;
trailers_contain_pseudo_headers([_|Tail]) ->
	trailers_contain_pseudo_headers(Tail).

%% Rejecting invalid regular headers might be a bit too strong for clients.
headers_regular_headers(Frame=#headers{id=StreamID},
		State, Type, Stream, PseudoHeaders, Headers) ->
	case regular_headers(Headers, Type) of
		ok when Type =:= request ->
			request_expected_size(Frame, State, Type, Stream, PseudoHeaders, Headers);
		ok when Type =:= push_promise ->
			push_promise_frame(Frame, State, Stream, PseudoHeaders, Headers);
		ok when Type =:= response ->
			response_expected_size(Frame, State, Type, Stream, PseudoHeaders, Headers);
		ok when Type =:= trailers ->
			trailers_frame(Frame, State, Stream, Headers);
		{error, HumanReadable} when Type =:= request ->
			headers_malformed(Frame, State, HumanReadable);
		{error, HumanReadable} ->
			stream_reset(StreamID, State, protocol_error, HumanReadable)
	end.

regular_headers([{<<>>, _}|_], _) ->
	{error, 'Empty header names are not valid regular headers. (CVE-2019-9516)'};
regular_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, 'Pseudo-headers were found after regular headers. (RFC7540 8.1.2.1)'};
regular_headers([{<<"connection">>, _}|_], _) ->
	{error, 'The connection header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"keep-alive">>, _}|_], _) ->
	{error, 'The keep-alive header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"proxy-authenticate">>, _}|_], _) ->
	{error, 'The proxy-authenticate header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"proxy-authorization">>, _}|_], _) ->
	{error, 'The proxy-authorization header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"transfer-encoding">>, _}|_], _) ->
	{error, 'The transfer-encoding header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"upgrade">>, _}|_], _) ->
	{error, 'The upgrade header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"te">>, Value}|_], request) when Value =/= <<"trailers">> ->
	{error, 'The te header with a value other than "trailers" is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"te">>, _}|_], Type) when Type =/= request ->
	{error, 'The te header is only allowed in request headers. (RFC7540 8.1.2.2)'};
regular_headers([{Name, _}|Tail], Type) ->
	Pattern = [
		<<$A>>, <<$B>>, <<$C>>, <<$D>>, <<$E>>, <<$F>>, <<$G>>, <<$H>>, <<$I>>,
		<<$J>>, <<$K>>, <<$L>>, <<$M>>, <<$N>>, <<$O>>, <<$P>>, <<$Q>>, <<$R>>,
		<<$S>>, <<$T>>, <<$U>>, <<$V>>, <<$W>>, <<$X>>, <<$Y>>, <<$Z>>
	],
	case binary:match(Name, Pattern) of
		nomatch -> regular_headers(Tail, Type);
		_ -> {error, 'Header names must be lowercase. (RFC7540 8.1.2)'}
	end;
regular_headers([], _) ->
	ok.

request_expected_size(Frame=#headers{fin=IsFin}, State, Type, Stream, PseudoHeaders, Headers) ->
	case [CL || {<<"content-length">>, CL} <- Headers] of
		[] when IsFin =:= fin ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, 0);
		[] ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, undefined);
		[<<"0">>] when IsFin =:= fin ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, 0);
		[_] when IsFin =:= fin ->
			headers_malformed(Frame, State,
				'HEADERS frame with the END_STREAM flag contains a non-zero content-length. (RFC7540 8.1.2.6)');
		[BinLen] ->
			headers_parse_expected_size(Frame, State, Type, Stream,
				PseudoHeaders, Headers, BinLen);
		_ ->
			headers_malformed(Frame, State,
				'Multiple content-length headers were received. (RFC7230 3.3.2)')
	end.

response_expected_size(Frame=#headers{id=StreamID, fin=IsFin}, State, Type,
		Stream=#stream{method=Method}, PseudoHeaders=#{status := Status}, Headers) ->
	case [CL || {<<"content-length">>, CL} <- Headers] of
		[] when IsFin =:= fin ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, 0);
		[] ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, undefined);
		[_] when Status >= 100, Status =< 199 ->
			stream_reset(StreamID, State, protocol_error,
				'Content-length header received in a 1xx response. (RFC7230 3.3.2)');
		[_] when Status =:= 204 ->
			stream_reset(StreamID, State, protocol_error,
				'Content-length header received in a 204 response. (RFC7230 3.3.2)');
		[_] when Status >= 200, Status =< 299, Method =:= <<"CONNECT">> ->
			stream_reset(StreamID, State, protocol_error,
				'Content-length header received in a 2xx response to a CONNECT request. (RFC7230 3.3.2).');
		%% Responses to HEAD requests, and 304 responses may contain
		%% a content-length header that must be ignored. (RFC7230 3.3.2)
		[_] when Method =:= <<"HEAD">> ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, 0);
		[_] when Status =:= 304 ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, 0);
		[<<"0">>] when IsFin =:= fin ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, 0);
		[_] when IsFin =:= fin ->
			stream_reset(StreamID, State, protocol_error,
				'HEADERS frame with the END_STREAM flag contains a non-zero content-length. (RFC7540 8.1.2.6)');
		[BinLen] ->
			headers_parse_expected_size(Frame, State, Type, Stream,
				PseudoHeaders, Headers, BinLen);
		_ ->
			stream_reset(StreamID, State, protocol_error,
				'Multiple content-length headers were received. (RFC7230 3.3.2)')
	end.

headers_parse_expected_size(Frame=#headers{id=StreamID},
		State, Type, Stream, PseudoHeaders, Headers, BinLen) ->
	try cow_http_hd:parse_content_length(BinLen) of
		Len ->
			headers_frame(Frame, State, Type, Stream, PseudoHeaders, Headers, Len)
	catch
		_:_ ->
			HumanReadable = 'The content-length header is invalid. (RFC7230 3.3.2)',
			case Type of
				request -> headers_malformed(Frame, State, HumanReadable);
				response -> stream_reset(StreamID, State, protocol_error, HumanReadable)
			end
	end.

headers_frame(#headers{id=StreamID, fin=IsFin}, State0=#http2_machine{
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow}},
		Type, Stream0, PseudoHeaders, Headers, Len) ->
	{Stream, State1} = case Type of
		request ->
			TE = case lists:keyfind(<<"te">>, 1, Headers) of
				{_, TE0} -> TE0;
				false -> undefined
			end,
			{#stream{id=StreamID, method=maps:get(method, PseudoHeaders),
				remote=IsFin, remote_expected_size=Len,
				local_window=LocalWindow, remote_window=RemoteWindow, te=TE},
				State0#http2_machine{remote_streamid=StreamID}};
		response ->
			Stream1 = case PseudoHeaders of
				#{status := Status} when Status >= 100, Status =< 199 -> Stream0;
				_ -> Stream0#stream{remote=IsFin, remote_expected_size=Len}
			end,
			{Stream1, State0}
	end,
	State = stream_store(Stream, State1),
	{ok, {headers, StreamID, IsFin, Headers, PseudoHeaders, Len}, State}.

trailers_frame(#headers{id=StreamID}, State0, Stream0, Headers) ->
	Stream = Stream0#stream{remote=fin},
	State = stream_store(Stream, State0),
	case is_body_size_valid(Stream) of
		true ->
			{ok, {trailers, StreamID, Headers}, State};
		false ->
			stream_reset(StreamID, State, protocol_error,
				'The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)')
	end.

%% PRIORITY frame.
%%
%% @todo Handle PRIORITY frames.

priority_frame(_Frame, State) ->
	{ok, State}.

%% RST_STREAM frame.

rst_stream_frame({rst_stream, StreamID, _}, State=#http2_machine{mode=Mode,
		local_streamid=LocalStreamID, remote_streamid=RemoteStreamID})
		when (?IS_LOCAL(Mode, StreamID) andalso (StreamID >= LocalStreamID))
		orelse ((not ?IS_LOCAL(Mode, StreamID)) andalso (StreamID > RemoteStreamID)) ->
	{error, {connection_error, protocol_error,
		'RST_STREAM frame received on a stream in idle state. (RFC7540 5.1)'},
		State};
rst_stream_frame({rst_stream, StreamID, Reason}, State=#http2_machine{
		streams=Streams0, remote_lingering_streams=Lingering0}) ->
	Streams = lists:keydelete(StreamID, #stream.id, Streams0),
	%% We only keep up to 10 streams in this state. @todo Make it configurable?
	Lingering = [StreamID|lists:sublist(Lingering0, 10 - 1)],
	{ok, {rst_stream, StreamID, Reason},
		State#http2_machine{streams=Streams, remote_lingering_streams=Lingering}}.

%% SETTINGS frame.

settings_frame({settings, Settings}, State0=#http2_machine{
		opts=Opts, remote_settings=Settings0}) ->
	State1 = State0#http2_machine{remote_settings=maps:merge(Settings0, Settings)},
	State2 = maps:fold(fun
		(header_table_size, NewSize, State=#http2_machine{encode_state=EncodeState0}) ->
			MaxSize = maps:get(max_encode_table_size, Opts, 4096),
			EncodeState = cow_hpack:set_max_size(min(NewSize, MaxSize), EncodeState0),
			State#http2_machine{encode_state=EncodeState};
		(initial_window_size, NewWindowSize, State) ->
			OldWindowSize = maps:get(initial_window_size, Settings0, 65535),
			streams_update_local_window(State, NewWindowSize - OldWindowSize);
		(_, _, State) ->
			State
	end, State1, Settings),
	case Settings of
		#{initial_window_size := _} -> send_data(State2);
		_ -> {ok, State2}
	end;
%% We expect to receive a SETTINGS frame as part of the preface.
settings_frame(_F, State) ->
	{error, {connection_error, protocol_error,
		'The preface sequence must be followed by a SETTINGS frame. (RFC7540 3.5)'},
		State}.

%% When SETTINGS_INITIAL_WINDOW_SIZE changes we need to update
%% the local stream windows for all active streams and perhaps
%% resume sending data.
streams_update_local_window(State=#http2_machine{streams=Streams0}, Increment) ->
	Streams = [
		S#stream{local_window=StreamWindow + Increment}
	|| S=#stream{local_window=StreamWindow} <- Streams0],
	State#http2_machine{streams=Streams}.

%% Ack for a previously sent SETTINGS frame.

settings_ack_frame(State0=#http2_machine{settings_timer=TRef,
		local_settings=Local0, next_settings=NextSettings}) ->
	ok = case TRef of
		undefined -> ok;
		_ -> erlang:cancel_timer(TRef, [{async, true}, {info, false}])
	end,
	Local = maps:merge(Local0, NextSettings),
	State1 = State0#http2_machine{settings_timer=undefined,
		local_settings=Local, next_settings=#{}},
	{ok, maps:fold(fun
		(header_table_size, MaxSize, State=#http2_machine{decode_state=DecodeState0}) ->
			DecodeState = cow_hpack:set_max_size(MaxSize, DecodeState0),
			State#http2_machine{decode_state=DecodeState};
		(initial_window_size, NewWindowSize, State) ->
			OldWindowSize = maps:get(initial_window_size, Local0, 65535),
			streams_update_remote_window(State, NewWindowSize - OldWindowSize);
		(_, _, State) ->
			State
	end, State1, NextSettings)}.

%% When we receive an ack to a SETTINGS frame we sent we need to update
%% the remote stream windows for all active streams.
streams_update_remote_window(State=#http2_machine{streams=Streams0}, Increment) ->
	Streams = [
		S#stream{remote_window=StreamWindow + Increment}
	|| S=#stream{remote_window=StreamWindow} <- Streams0],
	State#http2_machine{streams=Streams}.

%% PUSH_PROMISE frame.

%% Convenience record to manipulate the tuple.
%% The order of the fields matter.
-record(push_promise, {
	id :: cow_http2:streamid(),
	head :: cow_http2:head_fin(),
	promised_id :: cow_http2:streamid(),
	data :: binary()
}).

push_promise_frame(_, State=#http2_machine{mode=server}) ->
	{error, {connection_error, protocol_error,
		'PUSH_PROMISE frames MUST NOT be sent by the client. (RFC7540 6.6)'},
		State};
push_promise_frame(_, State=#http2_machine{local_settings=#{enable_push := false}}) ->
	{error, {connection_error, protocol_error,
		'PUSH_PROMISE frame received despite SETTINGS_ENABLE_PUSH set to 0. (RFC7540 6.6)'},
		State};
push_promise_frame(#push_promise{promised_id=PromisedStreamID},
		State=#http2_machine{remote_streamid=RemoteStreamID})
		when PromisedStreamID =< RemoteStreamID ->
	{error, {connection_error, protocol_error,
		'PUSH_PROMISE frame received for a promised stream in closed or half-closed state. (RFC7540 5.1, RFC7540 6.6)'},
		State};
push_promise_frame(#push_promise{id=StreamID}, State)
		when not ?IS_CLIENT_LOCAL(StreamID) ->
	{error, {connection_error, protocol_error,
		'PUSH_PROMISE frame received on a server-initiated stream. (RFC7540 6.6)'},
		State};
push_promise_frame(Frame=#push_promise{id=StreamID, head=IsHeadFin,
		promised_id=PromisedStreamID, data=HeaderData}, State) ->
	case stream_get(StreamID, State) of
		Stream=#stream{remote=idle} ->
			case IsHeadFin of
				head_fin ->
					headers_decode(#headers{id=PromisedStreamID,
						fin=fin, head=IsHeadFin, data=HeaderData},
						State, push_promise, Stream);
				head_nofin ->
					{ok, State#http2_machine{state={continuation, push_promise, Frame}}}
			end;
		_ ->
%% @todo Check if the stream is lingering. If it is, decode the frame
%% and do what? That's the big question and why it's not implemented yet.
%   However, an endpoint that
%   has sent RST_STREAM on the associated stream MUST handle PUSH_PROMISE
%   frames that might have been created before the RST_STREAM frame is
%   received and processed. (RFC7540 6.6)
			{error, {connection_error, stream_closed,
				'PUSH_PROMISE frame received on a stream in closed or half-closed state. (RFC7540 5.1, RFC7540 6.6)'},
				State}
	end.

push_promise_frame(#headers{id=PromisedStreamID},
		State0=#http2_machine{
			local_settings=#{initial_window_size := RemoteWindow},
			remote_settings=#{initial_window_size := LocalWindow}},
		#stream{id=StreamID}, PseudoHeaders=#{method := Method}, Headers) ->
	TE = case lists:keyfind(<<"te">>, 1, Headers) of
		{_, TE0} -> TE0;
		false -> undefined
	end,
	PromisedStream = #stream{id=PromisedStreamID, method=Method,
		local=fin, local_window=LocalWindow,
		remote_window=RemoteWindow, te=TE},
	State = stream_store(PromisedStream,
		State0#http2_machine{remote_streamid=PromisedStreamID}),
	{ok, {push_promise, StreamID, PromisedStreamID, Headers, PseudoHeaders}, State}.

%% PING frame.

ping_frame({ping, _}, State) ->
	{ok, State}.

%% Ack for a previously sent PING frame.
%%
%% @todo Might want to check contents but probably a waste of time.

ping_ack_frame({ping_ack, _}, State) ->
	{ok, State}.

%% GOAWAY frame.

goaway_frame(Frame={goaway, _, _, _}, State) ->
	{ok, Frame, State}.

%% WINDOW_UPDATE frame.

%% Connection-wide WINDOW_UPDATE frame.
window_update_frame({window_update, Increment}, State=#http2_machine{local_window=ConnWindow})
		when ConnWindow + Increment > 16#7fffffff ->
	{error, {connection_error, flow_control_error,
		'The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)'},
		State};
window_update_frame({window_update, Increment}, State=#http2_machine{local_window=ConnWindow}) ->
	send_data(State#http2_machine{local_window=ConnWindow + Increment});
%% Stream-specific WINDOW_UPDATE frame.
window_update_frame({window_update, StreamID, _}, State=#http2_machine{mode=Mode,
		local_streamid=LocalStreamID, remote_streamid=RemoteStreamID})
		when (?IS_LOCAL(Mode, StreamID) andalso (StreamID >= LocalStreamID))
		orelse ((not ?IS_LOCAL(Mode, StreamID)) andalso (StreamID > RemoteStreamID)) ->
	{error, {connection_error, protocol_error,
		'WINDOW_UPDATE frame received on a stream in idle state. (RFC7540 5.1)'},
		State};
window_update_frame({window_update, StreamID, Increment},
		State0=#http2_machine{remote_lingering_streams=Lingering}) ->
	case stream_get(StreamID, State0) of
		#stream{local_window=StreamWindow} when StreamWindow + Increment > 16#7fffffff ->
			stream_reset(StreamID, State0, flow_control_error,
				'The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)');
		Stream0 = #stream{local_window=StreamWindow} ->
			send_data(Stream0#stream{local_window=StreamWindow + Increment}, State0);
		undefined ->
			%% WINDOW_UPDATE frames may be received for a short period of time
			%% after a stream is closed. They must be ignored.
			case lists:member(StreamID, Lingering) of
				false -> {ok, State0};
				true -> stream_reset(StreamID, State0, stream_closed,
					'WINDOW_UPDATE frame received after the stream was reset. (RFC7540 5.1)')
			end
	end.

%% CONTINUATION frame.

%% Convenience record to manipulate the tuple.
%% The order of the fields matter.
-record(continuation, {
	id :: cow_http2:streamid(),
	head :: cow_http2:head_fin(),
	data :: binary()
}).

unexpected_continuation_frame(#continuation{}, State) ->
	{error, {connection_error, protocol_error,
		'CONTINUATION frames MUST be preceded by a HEADERS or PUSH_PROMISE frame. (RFC7540 6.10)'},
		State}.

continuation_frame(#continuation{id=StreamID, head=head_fin, data=HeaderFragment1},
		State=#http2_machine{state={continuation, Type,
			Frame=#headers{id=StreamID, data=HeaderFragment0}}}) ->
	HeaderData = <<HeaderFragment0/binary, HeaderFragment1/binary>>,
	headers_decode(Frame#headers{head=head_fin, data=HeaderData},
		State#http2_machine{state=normal}, Type, stream_get(StreamID, State));
continuation_frame(#continuation{id=StreamID, head=head_fin, data=HeaderFragment1},
		State=#http2_machine{state={continuation, Type, #push_promise{
			id=StreamID, promised_id=PromisedStreamID, data=HeaderFragment0}}}) ->
	HeaderData = <<HeaderFragment0/binary, HeaderFragment1/binary>>,
	headers_decode(#headers{id=PromisedStreamID, fin=fin, head=head_fin, data=HeaderData},
		State#http2_machine{state=normal}, Type, undefined);
continuation_frame(#continuation{id=StreamID, data=HeaderFragment1},
		State=#http2_machine{state={continuation, Type, ContinuedFrame0}})
		when element(2, ContinuedFrame0) =:= StreamID ->
	ContinuedFrame = case ContinuedFrame0 of
		#headers{data=HeaderFragment0} ->
			HeaderData = <<HeaderFragment0/binary, HeaderFragment1/binary>>,
			ContinuedFrame0#headers{data=HeaderData};
		#push_promise{data=HeaderFragment0} ->
			HeaderData = <<HeaderFragment0/binary, HeaderFragment1/binary>>,
			ContinuedFrame0#push_promise{data=HeaderData}
	end,
	{ok, State#http2_machine{state={continuation, Type, ContinuedFrame}}};
continuation_frame(_F, State) ->
	{error, {connection_error, protocol_error,
		'An invalid frame was received in the middle of a header block. (RFC7540 6.2)'},
		State}.

%% Ignored frames.

-spec ignored_frame(State)
	-> {ok, State}
	| {error, {connection_error, protocol_error, atom()}, State}
	when State::http2_machine().
ignored_frame(State=#http2_machine{state={continuation, _, _}}) ->
	{error, {connection_error, protocol_error,
		'An invalid frame was received in the middle of a header block. (RFC7540 6.2)'},
		State};
%% @todo It might be useful to error out when we receive
%% too many unknown frames. (RFC7540 10.5)
ignored_frame(State) ->
	{ok, State}.

%% Timeouts.

-spec timeout(preface_timeout | settings_timeout, reference(), State)
	-> {ok, State}
	| {error, {connection_error, cow_http2:error(), atom()}, State}
	when State::http2_machine().
timeout(preface_timeout, TRef, State=#http2_machine{preface_timer=TRef}) ->
	{error, {connection_error, protocol_error,
		'The preface was not received in a reasonable amount of time.'},
		State};
timeout(settings_timeout, TRef, State=#http2_machine{settings_timer=TRef}) ->
	{error, {connection_error, settings_timeout,
		'The SETTINGS ack was not received within the configured time. (RFC7540 6.5.3)'},
		State};
timeout(_, _, State) ->
	{ok, State}.

%% Functions for sending a message header or body. Note that
%% this module does not send data directly, instead it returns
%% a value that can then be used to send the frames.

-spec prepare_headers(cow_http2:streamid(), State, idle | cow_http2:fin(),
	pseudo_headers(), cow_http:headers())
	-> {ok, cow_http2:fin(), iodata(), State} when State::http2_machine().
prepare_headers(StreamID, State=#http2_machine{encode_state=EncodeState0},
		IsFin0, PseudoHeaders, Headers0) ->
	Stream = #stream{method=Method, local=idle} = stream_get(StreamID, State),
	IsFin = case {IsFin0, Method} of
		{idle, _} -> nofin;
		{_, <<"HEAD">>} -> fin;
		_ -> IsFin0
	end,
	Headers = merge_pseudo_headers(PseudoHeaders, remove_http11_headers(Headers0)),
	{HeaderBlock, EncodeState} = cow_hpack:encode(Headers, EncodeState0),
	{ok, IsFin, HeaderBlock, stream_store(Stream#stream{local=IsFin0},
		State#http2_machine{encode_state=EncodeState})}.

-spec prepare_push_promise(cow_http2:streamid(), State, pseudo_headers(), cow_http:headers())
	-> {ok, cow_http2:streamid(), iodata(), State}
	| {error, no_push} when State::http2_machine().
prepare_push_promise(_, #http2_machine{remote_settings=#{enable_push := false}}, _, _) ->
	{error, no_push};
prepare_push_promise(StreamID, State=#http2_machine{encode_state=EncodeState0,
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow},
		local_streamid=LocalStreamID}, PseudoHeaders, Headers0) ->
	#stream{local=idle} = stream_get(StreamID, State),
	TE = case lists:keyfind(<<"te">>, 1, Headers0) of
		{_, TE0} -> TE0;
		false -> undefined
	end,
	Headers = merge_pseudo_headers(PseudoHeaders, remove_http11_headers(Headers0)),
	{HeaderBlock, EncodeState} = cow_hpack:encode(Headers, EncodeState0),
	{ok, LocalStreamID, HeaderBlock, stream_store(
		#stream{id=LocalStreamID, method=maps:get(method, PseudoHeaders),
			remote=fin, remote_expected_size=0,
			local_window=LocalWindow, remote_window=RemoteWindow, te=TE},
		State#http2_machine{encode_state=EncodeState, local_streamid=LocalStreamID + 2})}.

remove_http11_headers(Headers) ->
	RemoveHeaders0 = [
		<<"keep-alive">>,
		<<"proxy-connection">>,
		<<"transfer-encoding">>,
		<<"upgrade">>
	],
	RemoveHeaders = case lists:keyfind(<<"connection">>, 1, Headers) of
		false ->
			RemoveHeaders0;
		{_, ConnHd} ->
			%% We do not need to worry about any "close" header because
			%% that header name is reserved.
			Connection = cow_http_hd:parse_connection(ConnHd),
			Connection ++ [<<"connection">>|RemoveHeaders0]
	end,
	lists:filter(fun({Name, _}) ->
		not lists:member(Name, RemoveHeaders)
	end, Headers).

merge_pseudo_headers(PseudoHeaders, Headers0) ->
	lists:foldl(fun
		({status, Status}, Acc) when is_integer(Status) ->
			[{<<":status">>, integer_to_binary(Status)}|Acc];
		({Name, Value}, Acc) ->
			[{iolist_to_binary([$:, atom_to_binary(Name, latin1)]), Value}|Acc]
		end, Headers0, maps:to_list(PseudoHeaders)).

-spec prepare_trailers(cow_http2:streamid(), State, cow_http:headers())
	-> {ok, iodata(), State} when State::http2_machine().
prepare_trailers(StreamID, State=#http2_machine{encode_state=EncodeState0}, Trailers) ->
	Stream = #stream{local=nofin} = stream_get(StreamID, State),
	{HeaderBlock, EncodeState} = cow_hpack:encode(Trailers, EncodeState0),
	{ok, HeaderBlock, stream_store(Stream#stream{local=fin},
		State#http2_machine{encode_state=EncodeState})}.

-spec send_or_queue_data(cow_http2:streamid(), State, cow_http2:fin(), DataOrFileOrTrailers)
	-> {ok, State}
	| {send, [{cow_http2:streamid(), cow_http2:fin(), [DataOrFileOrTrailers]}], State}
	when State::http2_machine(), DataOrFileOrTrailers::
		{data, iodata()} | #sendfile{} | {trailers, cow_http:headers()}.
send_or_queue_data(StreamID, State0=#http2_machine{opts=Opts, local_window=ConnWindow},
		IsFin0, DataOrFileOrTrailers0) ->
	%% @todo Probably just ignore if the method was HEAD.
	Stream0 = #stream{
		local=nofin,
		local_window=StreamWindow,
		local_buffer_size=BufferSize,
		te=TE0
	} = stream_get(StreamID, State0),
	DataOrFileOrTrailers = case DataOrFileOrTrailers0 of
		{trailers, _} ->
			%% We only accept TE headers containing exactly "trailers" (RFC7540 8.1.2.1).
			TE = try cow_http_hd:parse_te(TE0) of
				{trailers, []} -> trailers;
				_ -> no_trailers
			catch _:_ ->
				%% If we can't parse the TE header, assume we can't send trailers.
				no_trailers
			end,
			case TE of
				trailers ->
					DataOrFileOrTrailers0;
				no_trailers ->
					{data, <<>>}
			end;
		_ ->
			DataOrFileOrTrailers0
	end,
	SendSize = BufferSize + case DataOrFileOrTrailers of
		{data, D} -> iolist_size(D);
		#sendfile{bytes=B} -> B;
		{trailers, _} -> 0
	end,
	MinSendSize = maps:get(stream_window_data_threshold, Opts, 16384),
	if
		%% If we cannot send the data all at once and the window
		%% is smaller than we are willing to send at a minimum,
		%% we queue the data directly.
		(StreamWindow < MinSendSize)
				andalso ((StreamWindow < SendSize) orelse (ConnWindow < SendSize)) ->
			{ok, stream_store(queue_data(Stream0, IsFin0, DataOrFileOrTrailers, in), State0)};
		true ->
			case send_or_queue_data(Stream0, State0, [], IsFin0, DataOrFileOrTrailers, in) of
				{ok, Stream, State, []} ->
					{ok, stream_store(Stream, State)};
				{ok, Stream=#stream{local=IsFin}, State, SendData} ->
					{send, [{StreamID, IsFin, lists:reverse(SendData)}], stream_store(Stream, State)}
			end
	end.

%% Internal data sending/queuing functions.

%% @todo Should we ever want to implement the PRIORITY mechanism,
%% this would be the place to do it. Right now, we just go over
%% all streams and send what we can until either everything is
%% sent or we run out of space in the window.
send_data(State0=#http2_machine{streams=Streams0}) ->
	case send_data_for_all_streams(Streams0, State0, [], []) of
		{ok, Streams, State, []} ->
			{ok, State#http2_machine{streams=Streams}};
		{ok, Streams, State, Send} ->
			{send, Send, State#http2_machine{streams=Streams}}
	end.

send_data_for_all_streams([], State, Acc, Send) ->
	{ok, lists:reverse(Acc), State, Send};
%% While technically we should never get < 0 here, let's be on the safe side.
send_data_for_all_streams(Tail, State=#http2_machine{local_window=ConnWindow}, Acc, Send)
		when ConnWindow =< 0 ->
	{ok, lists:reverse(Acc, Tail), State, Send};
%% We rely on send_data_for_one_stream/3 to do all the necessary checks about the stream.
send_data_for_all_streams([Stream0|Tail], State0, Acc, Send) ->
	case send_data_for_one_stream(Stream0, State0, []) of
		{ok, Stream, State, []} ->
			send_data_for_all_streams(Tail, State, [Stream|Acc], Send);
		%% We need to remove the stream here because we do not use stream_store/2.
		{ok, #stream{id=StreamID, local=fin, remote=fin}, State, SendData} ->
			send_data_for_all_streams(Tail, State, Acc,
				[{StreamID, fin, SendData}|Send]);
		{ok, Stream=#stream{id=StreamID, local=IsFin}, State, SendData} ->
			send_data_for_all_streams(Tail, State, [Stream|Acc],
				[{StreamID, IsFin, SendData}|Send])
	end.

send_data(Stream0, State0) ->
	case send_data_for_one_stream(Stream0, State0, []) of
		{ok, Stream, State, []} ->
			{ok, stream_store(Stream, State)};
		{ok, Stream=#stream{id=StreamID, local=IsFin}, State, SendData} ->
			{send, [{StreamID, IsFin, SendData}], stream_store(Stream, State)}
	end.

send_data_for_one_stream(Stream=#stream{local=nofin, local_buffer_size=0,
		local_trailers=Trailers}, State, SendAcc) when Trailers =/= undefined ->
	{ok, Stream, State, lists:reverse([{trailers, Trailers}|SendAcc])};
send_data_for_one_stream(Stream=#stream{local=nofin, local_buffer=Q0, local_buffer_size=0},
		State, SendAcc) ->
	case queue:len(Q0) of
		0 ->
			{ok, Stream, State, lists:reverse(SendAcc)};
		1 ->
			%% We know there is a final empty data frame in the queue.
			%% We need to mark the stream as complete.
			{{value, {fin, 0, _}}, Q} = queue:out(Q0),
			{ok, Stream#stream{local=fin, local_buffer=Q}, State, lists:reverse(SendAcc)}
	end;
send_data_for_one_stream(Stream=#stream{local=IsFin, local_window=StreamWindow,
		local_buffer_size=BufferSize}, State=#http2_machine{local_window=ConnWindow}, SendAcc)
		when ConnWindow =< 0; IsFin =:= fin; StreamWindow =< 0; BufferSize =:= 0 ->
	{ok, Stream, State, lists:reverse(SendAcc)};
send_data_for_one_stream(Stream0=#stream{local_window=StreamWindow,
		local_buffer=Q0, local_buffer_size=BufferSize},
		State0=#http2_machine{opts=Opts, local_window=ConnWindow}, SendAcc0) ->
	MinSendSize = maps:get(stream_window_data_threshold, Opts, 16384),
	if
		%% If we cannot send the entire buffer at once and the window
		%% is smaller than we are willing to send at a minimum, do nothing.
		%%
		%% We only do this check the first time we go through this function;
		%% we want to send as much data as possible IF we send some.
		(SendAcc0 =:= []) andalso (StreamWindow < MinSendSize)
				andalso ((StreamWindow < BufferSize) orelse (ConnWindow < BufferSize)) ->
			{ok, Stream0, State0, []};
		true ->
			%% We know there is an item in the queue.
			{{value, {IsFin, DataSize, Data}}, Q} = queue:out(Q0),
			Stream1 = Stream0#stream{local_buffer=Q, local_buffer_size=BufferSize - DataSize},
			{ok, Stream, State, SendAcc}
				= send_or_queue_data(Stream1, State0, SendAcc0, IsFin, Data, in_r),
			send_data_for_one_stream(Stream, State, SendAcc)
	end.

%% We can send trailers immediately if the queue is empty, otherwise we queue.
%% We always send trailer frames even if the window is empty.
send_or_queue_data(Stream=#stream{local_buffer_size=0},
		State, SendAcc, fin, {trailers, Trailers}, _) ->
	{ok, Stream, State, [{trailers, Trailers}|SendAcc]};
send_or_queue_data(Stream, State, SendAcc, fin, {trailers, Trailers}, _) ->
	{ok, Stream#stream{local_trailers=Trailers}, State, SendAcc};
%% Send data immediately if we can, buffer otherwise.
send_or_queue_data(Stream=#stream{local_window=StreamWindow},
		State=#http2_machine{local_window=ConnWindow},
		SendAcc, IsFin, Data, In)
		when ConnWindow =< 0; StreamWindow =< 0 ->
	{ok, queue_data(Stream, IsFin, Data, In), State, SendAcc};
send_or_queue_data(Stream=#stream{local_window=StreamWindow},
		State=#http2_machine{opts=Opts, remote_settings=RemoteSettings,
		local_window=ConnWindow}, SendAcc, IsFin, Data, In) ->
	RemoteMaxFrameSize = maps:get(max_frame_size, RemoteSettings, 16384),
	ConfiguredMaxFrameSize = maps:get(max_frame_size_sent, Opts, infinity),
	MaxSendSize = min(
		min(ConnWindow, StreamWindow),
		min(RemoteMaxFrameSize, ConfiguredMaxFrameSize)
	),
	case Data of
		File = #sendfile{bytes=Bytes} when Bytes =< MaxSendSize ->
			{ok, Stream#stream{local=IsFin, local_window=StreamWindow - Bytes},
				State#http2_machine{local_window=ConnWindow - Bytes},
				[File|SendAcc]};
		File = #sendfile{offset=Offset, bytes=Bytes} ->
			send_or_queue_data(Stream#stream{local_window=StreamWindow - MaxSendSize},
				State#http2_machine{local_window=ConnWindow - MaxSendSize},
				[File#sendfile{bytes=MaxSendSize}|SendAcc], IsFin,
				File#sendfile{offset=Offset + MaxSendSize, bytes=Bytes - MaxSendSize}, In);
		{data, Iolist0} ->
			IolistSize = iolist_size(Iolist0),
			if
				IolistSize =< MaxSendSize ->
					{ok, Stream#stream{local=IsFin, local_window=StreamWindow - IolistSize},
						State#http2_machine{local_window=ConnWindow - IolistSize},
						[{data, Iolist0}|SendAcc]};
				true ->
					{Iolist, More} = cow_iolists:split(MaxSendSize, Iolist0),
					send_or_queue_data(Stream#stream{local_window=StreamWindow - MaxSendSize},
						State#http2_machine{local_window=ConnWindow - MaxSendSize},
						[{data, Iolist}|SendAcc], IsFin, {data, More}, In)
			end
	end.

queue_data(Stream=#stream{local_buffer=Q0, local_buffer_size=Size0}, IsFin, Data, In) ->
	DataSize = case Data of
		{sendfile, _, Bytes, _} -> Bytes;
		{data, Iolist} -> iolist_size(Iolist)
	end,
	%% Never queue non-final empty data frames.
	case {DataSize, IsFin} of
		{0, nofin} ->
			Stream;
		_ ->
			Q = queue:In({IsFin, DataSize, Data}, Q0),
			Stream#stream{local_buffer=Q, local_buffer_size=Size0 + DataSize}
	end.

%% Public interface to update the flow control window.
%%
%% The ensure_window function applies heuristics to avoid updating the
%% window when it is not necessary. The update_window function updates
%% the window unconditionally.
%%
%% The ensure_window function should be called when requesting more
%% data (for example when reading a request or response body) as well
%% as when receiving new data. Failure to do so may result in the
%% window being depleted.
%%
%% The heuristics dictating whether the window must be updated and
%% what the window size is depends on three options (margin, max
%% and threshold) along with the Size argument. The window increment
%% returned by this function may therefore be smaller than the Size
%% argument. On the other hand the total window allocated over many
%% calls may end up being larger than the initial Size argument. As
%% a result, it is the responsibility of the caller to ensure that
%% the Size argument is never lower than 0.

-spec ensure_window(non_neg_integer(), State)
	-> ok | {ok, pos_integer(), State} when State::http2_machine().
ensure_window(Size, State=#http2_machine{opts=Opts, remote_window=RemoteWindow}) ->
	case ensure_window(Size, RemoteWindow, connection, Opts) of
		ok ->
			ok;
		{ok, Increment} ->
			{ok, Increment, State#http2_machine{remote_window=RemoteWindow + Increment}}
	end.

-spec ensure_window(cow_http2:streamid(), non_neg_integer(), State)
	-> ok | {ok, pos_integer(), State} when State::http2_machine().
ensure_window(StreamID, Size, State=#http2_machine{opts=Opts}) ->
	case stream_get(StreamID, State) of
		%% For simplicity's sake, we do not consider attempts to ensure the window
		%% of a terminated stream to be errors. We simply act as if the stream
		%% window is large enough.
		undefined ->
			ok;
		Stream = #stream{remote_window=RemoteWindow} ->
			case ensure_window(Size, RemoteWindow, stream, Opts) of
				ok ->
					ok;
				{ok, Increment} ->
					{ok, Increment, stream_store(Stream#stream{remote_window=RemoteWindow + Increment}, State)}
			end
	end.

%% No need to update the window when we are not expecting data.
ensure_window(0, _, _, _) ->
	ok;
%% No need to update the window when it is already high enough.
ensure_window(Size, Window, _, _) when Size =< Window ->
	ok;
ensure_window(Size0, Window, Type, Opts) ->
	Threshold = ensure_window_threshold(Type, Opts),
	if
		%% We do not update the window when it is higher than the threshold.
		Window > Threshold ->
			ok;
		true ->
			Margin = ensure_window_margin(Type, Opts),
			Size = Size0 + Margin,
			MaxWindow = ensure_window_max(Type, Opts),
			Increment = if
				%% We cannot go above the maximum window size.
				Size > MaxWindow -> MaxWindow - Window;
				true -> Size - Window
			end,
			case Increment of
				0 -> ok;
				_ -> {ok, Increment}
			end
	end.

%% Margin defaults to the default initial window size.
ensure_window_margin(connection, Opts) ->
	maps:get(connection_window_margin_size, Opts, 65535);
ensure_window_margin(stream, Opts) ->
	maps:get(stream_window_margin_size, Opts, 65535).

%% Max window defaults to the max value allowed by the protocol.
ensure_window_max(connection, Opts) ->
	maps:get(max_connection_window_size, Opts, 16#7fffffff);
ensure_window_max(stream, Opts) ->
	maps:get(max_stream_window_size, Opts, 16#7fffffff).

%% Threshold defaults to 10 times the default frame size.
ensure_window_threshold(connection, Opts) ->
	maps:get(connection_window_update_threshold, Opts, 163840);
ensure_window_threshold(stream, Opts) ->
	maps:get(stream_window_update_threshold, Opts, 163840).

-spec update_window(1..16#7fffffff, State)
	-> State when State::http2_machine().
update_window(Size, State=#http2_machine{remote_window=RemoteWindow})
		when Size > 0 ->
	State#http2_machine{remote_window=RemoteWindow + Size}.

-spec update_window(cow_http2:streamid(), 1..16#7fffffff, State)
	-> State when State::http2_machine().
update_window(StreamID, Size, State)
		when Size > 0 ->
	Stream = #stream{remote_window=RemoteWindow} = stream_get(StreamID, State),
	stream_store(Stream#stream{remote_window=RemoteWindow + Size}, State).

%% Public interface to reset streams.

-spec reset_stream(cow_http2:streamid(), State)
	-> {ok, State} | {error, not_found} when State::http2_machine().
reset_stream(StreamID, State=#http2_machine{streams=Streams0}) ->
	case lists:keytake(StreamID, #stream.id, Streams0) of
		{value, _, Streams} ->
			{ok, stream_linger(StreamID, State#http2_machine{streams=Streams})};
		false ->
			{error, not_found}
	end.

%% Retrieve the buffer size for all streams.

-spec get_connection_local_buffer_size(http2_machine()) -> non_neg_integer().
get_connection_local_buffer_size(#http2_machine{streams=Streams}) ->
	lists:foldl(fun(#stream{local_buffer_size=Size}, Acc) ->
		Acc + Size
	end, 0, Streams).

%% Retrieve a setting value, or its default value if not set.

-spec get_local_setting(atom(), http2_machine()) -> atom() | integer().
get_local_setting(Key, #http2_machine{local_settings=Settings}) ->
	maps:get(Key, Settings, default_setting_value(Key)).

default_setting_value(header_table_size) -> 4096;
default_setting_value(enable_push) -> true;
default_setting_value(max_concurrent_streams) -> infinity;
default_setting_value(initial_window_size) -> 65535;
default_setting_value(max_frame_size) -> 16384;
default_setting_value(max_header_list_size) -> infinity;
default_setting_value(enable_connect_protocol) -> false.

%% Function to obtain the last known streamid received
%% for the purposes of sending a GOAWAY frame and closing the connection.

-spec get_last_streamid(http2_machine()) -> cow_http2:streamid().
get_last_streamid(#http2_machine{remote_streamid=RemoteStreamID}) ->
	RemoteStreamID.

%% Retrieve the local buffer size for a stream.

-spec get_stream_local_buffer_size(cow_http2:streamid(), http2_machine())
	-> {ok, non_neg_integer()} | {error, not_found | closed}.
get_stream_local_buffer_size(StreamID, State=#http2_machine{mode=Mode,
		local_streamid=LocalStreamID, remote_streamid=RemoteStreamID}) ->
	case stream_get(StreamID, State) of
		#stream{local_buffer_size=Size} ->
			{ok, Size};
		undefined when (?IS_LOCAL(Mode, StreamID) andalso (StreamID < LocalStreamID))
				orelse ((not ?IS_LOCAL(Mode, StreamID)) andalso (StreamID =< RemoteStreamID)) ->
			{error, closed};
		undefined ->
			{error, not_found}
	end.

%% Retrieve the local state for a stream, including the state in the queue.

-spec get_stream_local_state(cow_http2:streamid(), http2_machine())
	-> {ok, idle | cow_http2:fin(), empty | nofin | fin} | {error, not_found | closed}.
get_stream_local_state(StreamID, State=#http2_machine{mode=Mode,
		local_streamid=LocalStreamID, remote_streamid=RemoteStreamID}) ->
	case stream_get(StreamID, State) of
		#stream{local=IsFin, local_buffer=Q, local_trailers=undefined} ->
			IsQueueFin = case queue:peek_r(Q) of
				empty -> empty;
				{value, {IsQueueFin0, _, _}} -> IsQueueFin0
			end,
			{ok, IsFin, IsQueueFin};
		%% Trailers are queued so the local state is fin after the queue is drained.
		#stream{local=IsFin} ->
			{ok, IsFin, fin};
		undefined when (?IS_LOCAL(Mode, StreamID) andalso (StreamID < LocalStreamID))
				orelse ((not ?IS_LOCAL(Mode, StreamID)) andalso (StreamID =< RemoteStreamID)) ->
			{error, closed};
		undefined ->
			{error, not_found}
	end.

%% Retrieve the remote state for a stream.

-spec get_stream_remote_state(cow_http2:streamid(), http2_machine())
	-> {ok, idle | cow_http2:fin()} | {error, not_found | closed}.
get_stream_remote_state(StreamID, State=#http2_machine{mode=Mode,
		local_streamid=LocalStreamID, remote_streamid=RemoteStreamID}) ->
	case stream_get(StreamID, State) of
		#stream{remote=IsFin} ->
			{ok, IsFin};
		undefined when (?IS_LOCAL(Mode, StreamID) andalso (StreamID < LocalStreamID))
				orelse ((not ?IS_LOCAL(Mode, StreamID)) andalso (StreamID =< RemoteStreamID)) ->
			{error, closed};
		undefined ->
			{error, not_found}
	end.

%% Query whether the stream was reset recently by the remote endpoint.

-spec is_lingering_stream(cow_http2:streamid(), http2_machine()) -> boolean().
is_lingering_stream(StreamID, #http2_machine{
		local_lingering_streams=Local, remote_lingering_streams=Remote}) ->
	case lists:member(StreamID, Local) of
		true -> true;
		false -> lists:member(StreamID, Remote)
	end.

%% Stream-related functions.

stream_get(StreamID, #http2_machine{streams=Streams}) ->
	case lists:keyfind(StreamID, #stream.id, Streams) of
		false -> undefined;
		Stream -> Stream
	end.

stream_store(#stream{id=StreamID, local=fin, remote=fin},
		State=#http2_machine{streams=Streams0}) ->
	Streams = lists:keydelete(StreamID, #stream.id, Streams0),
	State#http2_machine{streams=Streams};
stream_store(Stream=#stream{id=StreamID},
		State=#http2_machine{streams=Streams0}) ->
	Streams = lists:keystore(StreamID, #stream.id, Streams0, Stream),
	State#http2_machine{streams=Streams}.

%% @todo Don't send an RST_STREAM if one was already sent.
stream_reset(StreamID, State, Reason, HumanReadable) ->
	{error, {stream_error, StreamID, Reason, HumanReadable},
		stream_linger(StreamID, State)}.

stream_linger(StreamID, State=#http2_machine{local_lingering_streams=Lingering0}) ->
	%% We only keep up to 100 streams in this state. @todo Make it configurable?
	Lingering = [StreamID|lists:sublist(Lingering0, 100 - 1)],
	State#http2_machine{local_lingering_streams=Lingering}.
