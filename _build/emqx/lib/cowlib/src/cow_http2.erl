%% Copyright (c) 2015-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(cow_http2).

%% Parsing.
-export([parse_sequence/1]).
-export([parse/1]).
-export([parse/2]).
-export([parse_settings_payload/1]).

%% Building.
-export([data/3]).
-export([data_header/3]).
-export([headers/3]).
-export([priority/4]).
-export([rst_stream/2]).
-export([settings/1]).
-export([settings_payload/1]).
-export([settings_ack/0]).
-export([push_promise/3]).
-export([ping/1]).
-export([ping_ack/1]).
-export([goaway/3]).
-export([window_update/1]).
-export([window_update/2]).

-type streamid() :: pos_integer().
-export_type([streamid/0]).

-type fin() :: fin | nofin.
-export_type([fin/0]).

-type head_fin() :: head_fin | head_nofin.
-export_type([head_fin/0]).

-type exclusive() :: exclusive | shared.
-type weight() :: 1..256.
-type settings() :: map().

-type error() :: no_error
	| protocol_error
	| internal_error
	| flow_control_error
	| settings_timeout
	| stream_closed
	| frame_size_error
	| refused_stream
	| cancel
	| compression_error
	| connect_error
	| enhance_your_calm
	| inadequate_security
	| http_1_1_required
	| unknown_error.
-export_type([error/0]).

-type frame() :: {data, streamid(), fin(), binary()}
	| {headers, streamid(), fin(), head_fin(), binary()}
	| {headers, streamid(), fin(), head_fin(), exclusive(), streamid(), weight(), binary()}
	| {priority, streamid(), exclusive(), streamid(), weight()}
	| {rst_stream, streamid(), error()}
	| {settings, settings()}
	| settings_ack
	| {push_promise, streamid(), head_fin(), streamid(), binary()}
	| {ping, integer()}
	| {ping_ack, integer()}
	| {goaway, streamid(), error(), binary()}
	| {window_update, non_neg_integer()}
	| {window_update, streamid(), non_neg_integer()}
	| {continuation, streamid(), head_fin(), binary()}.
-export_type([frame/0]).

%% Parsing.

-spec parse_sequence(binary())
	-> {ok, binary()} | more | {connection_error, error(), atom()}.
parse_sequence(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", Rest/bits>>) ->
	{ok, Rest};
parse_sequence(Data) when byte_size(Data) >= 24 ->
	{connection_error, protocol_error,
		'The connection preface was invalid. (RFC7540 3.5)'};
parse_sequence(Data) ->
	Len = byte_size(Data),
	<<Preface:Len/binary, _/bits>> = <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>,
	case Data of
		Preface ->
			more;
		_ ->
			{connection_error, protocol_error,
				'The connection preface was invalid. (RFC7540 3.5)'}
	end.

parse(<< Len:24, _/bits >>, MaxFrameSize) when Len > MaxFrameSize ->
	{connection_error, frame_size_error, 'The frame size exceeded SETTINGS_MAX_FRAME_SIZE. (RFC7540 4.2)'};
parse(Data, _) ->
	parse(Data).

%%
%% DATA frames.
%%
parse(<< _:24, 0:8, _:9, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'DATA frames MUST be associated with a stream. (RFC7540 6.1)'};
parse(<< 0:24, 0:8, _:4, 1:1, _:35, _/bits >>) ->
	{connection_error, frame_size_error, 'DATA frames with padding flag MUST have a length > 0. (RFC7540 6.1)'};
parse(<< Len0:24, 0:8, _:4, 1:1, _:35, PadLen:8, _/bits >>) when PadLen >= Len0 ->
	{connection_error, protocol_error, 'Length of padding MUST be less than length of payload. (RFC7540 6.1)'};
%% No padding.
parse(<< Len:24, 0:8, _:4, 0:1, _:2, FlagEndStream:1, _:1, StreamID:31, Data:Len/binary, Rest/bits >>) ->
	{ok, {data, StreamID, parse_fin(FlagEndStream), Data}, Rest};
%% Padding.
parse(<< Len0:24, 0:8, _:4, 1:1, _:2, FlagEndStream:1, _:1, StreamID:31, PadLen:8, Rest0/bits >>)
		when byte_size(Rest0) >= Len0 - 1 ->
	Len = Len0 - PadLen - 1,
	case Rest0 of
		<< Data:Len/binary, 0:PadLen/unit:8, Rest/bits >> ->
			{ok, {data, StreamID, parse_fin(FlagEndStream), Data}, Rest};
		_ ->
			{connection_error, protocol_error, 'Padding octets MUST be set to zero. (RFC7540 6.1)'}
	end;
%%
%% HEADERS frames.
%%
parse(<< _:24, 1:8, _:9, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'HEADERS frames MUST be associated with a stream. (RFC7540 6.2)'};
parse(<< 0:24, 1:8, _:4, 1:1, _:35, _/bits >>) ->
	{connection_error, frame_size_error, 'HEADERS frames with padding flag MUST have a length > 0. (RFC7540 6.1)'};
parse(<< Len:24, 1:8, _:2, 1:1, _:37, _/bits >>) when Len < 5 ->
	{connection_error, frame_size_error, 'HEADERS frames with priority flag MUST have a length >= 5. (RFC7540 6.1)'};
parse(<< Len:24, 1:8, _:2, 1:1, _:1, 1:1, _:35, _/bits >>) when Len < 6 ->
	{connection_error, frame_size_error, 'HEADERS frames with padding and priority flags MUST have a length >= 6. (RFC7540 6.1)'};
parse(<< Len0:24, 1:8, _:4, 1:1, _:35, PadLen:8, _/bits >>) when PadLen >= Len0 ->
	{connection_error, protocol_error, 'Length of padding MUST be less than length of payload. (RFC7540 6.2)'};
parse(<< Len0:24, 1:8, _:2, 1:1, _:1, 1:1, _:35, PadLen:8, _/bits >>) when PadLen >= Len0 - 5 ->
	{connection_error, protocol_error, 'Length of padding MUST be less than length of payload. (RFC7540 6.2)'};
%% No padding, no priority.
parse(<< Len:24, 1:8, _:2, 0:1, _:1, 0:1, FlagEndHeaders:1, _:1, FlagEndStream:1, _:1, StreamID:31,
		HeaderBlockFragment:Len/binary, Rest/bits >>) ->
	{ok, {headers, StreamID, parse_fin(FlagEndStream), parse_head_fin(FlagEndHeaders), HeaderBlockFragment}, Rest};
%% Padding, no priority.
parse(<< Len0:24, 1:8, _:2, 0:1, _:1, 1:1, FlagEndHeaders:1, _:1, FlagEndStream:1, _:1, StreamID:31,
		PadLen:8, Rest0/bits >>) when byte_size(Rest0) >= Len0 - 1 ->
	Len = Len0 - PadLen - 1,
	case Rest0 of
		<< HeaderBlockFragment:Len/binary, 0:PadLen/unit:8, Rest/bits >> ->
			{ok, {headers, StreamID, parse_fin(FlagEndStream), parse_head_fin(FlagEndHeaders), HeaderBlockFragment}, Rest};
		_ ->
			{connection_error, protocol_error, 'Padding octets MUST be set to zero. (RFC7540 6.2)'}
	end;
%% No padding, priority.
parse(<< _:24, 1:8, _:2, 1:1, _:1, 0:1, _:4, StreamID:31, _:1, StreamID:31, _/bits >>) ->
	{connection_error, protocol_error,
		'HEADERS frames cannot define a stream that depends on itself. (RFC7540 5.3.1)'};
parse(<< Len0:24, 1:8, _:2, 1:1, _:1, 0:1, FlagEndHeaders:1, _:1, FlagEndStream:1, _:1, StreamID:31,
		E:1, DepStreamID:31, Weight:8, Rest0/bits >>) when byte_size(Rest0) >= Len0 - 5 ->
	Len = Len0 - 5,
	<< HeaderBlockFragment:Len/binary, Rest/bits >> = Rest0,
	{ok, {headers, StreamID, parse_fin(FlagEndStream), parse_head_fin(FlagEndHeaders),
		parse_exclusive(E), DepStreamID, Weight + 1, HeaderBlockFragment}, Rest};
%% Padding, priority.
parse(<< _:24, 1:8, _:2, 1:1, _:1, 1:1, _:4, StreamID:31, _:9, StreamID:31, _/bits >>) ->
	{connection_error, protocol_error,
		'HEADERS frames cannot define a stream that depends on itself. (RFC7540 5.3.1)'};
parse(<< Len0:24, 1:8, _:2, 1:1, _:1, 1:1, FlagEndHeaders:1, _:1, FlagEndStream:1, _:1, StreamID:31,
		PadLen:8, E:1, DepStreamID:31, Weight:8, Rest0/bits >>) when byte_size(Rest0) >= Len0 - 6 ->
	Len = Len0 - PadLen - 6,
	case Rest0 of
		<< HeaderBlockFragment:Len/binary, 0:PadLen/unit:8, Rest/bits >> ->
			{ok, {headers, StreamID, parse_fin(FlagEndStream), parse_head_fin(FlagEndHeaders),
				parse_exclusive(E), DepStreamID, Weight + 1, HeaderBlockFragment}, Rest};
		_ ->
			{connection_error, protocol_error, 'Padding octets MUST be set to zero. (RFC7540 6.2)'}
	end;
%%
%% PRIORITY frames.
%%
parse(<< 5:24, 2:8, _:9, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'PRIORITY frames MUST be associated with a stream. (RFC7540 6.3)'};
parse(<< 5:24, 2:8, _:9, StreamID:31, _:1, StreamID:31, _:8, Rest/bits >>) ->
	{stream_error, StreamID, protocol_error,
		'PRIORITY frames cannot make a stream depend on itself. (RFC7540 5.3.1)', Rest};
parse(<< 5:24, 2:8, _:9, StreamID:31, E:1, DepStreamID:31, Weight:8, Rest/bits >>) ->
	{ok, {priority, StreamID, parse_exclusive(E), DepStreamID, Weight + 1}, Rest};
%% @todo figure out how to best deal with frame size errors; if we have everything fine
%% if not we might want to inform the caller how much he should expect so that it can
%% decide if it should just close the connection
parse(<< BadLen:24, 2:8, _:9, StreamID:31, _:BadLen/binary, Rest/bits >>) ->
	{stream_error, StreamID, frame_size_error, 'PRIORITY frames MUST be 5 bytes wide. (RFC7540 6.3)', Rest};
%%
%% RST_STREAM frames.
%%
parse(<< 4:24, 3:8, _:9, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'RST_STREAM frames MUST be associated with a stream. (RFC7540 6.4)'};
parse(<< 4:24, 3:8, _:9, StreamID:31, ErrorCode:32, Rest/bits >>) ->
	{ok, {rst_stream, StreamID, parse_error_code(ErrorCode)}, Rest};
%% @todo same as priority
parse(<< _:24, 3:8, _:9, _:31, _/bits >>) ->
	{connection_error, frame_size_error, 'RST_STREAM frames MUST be 4 bytes wide. (RFC7540 6.4)'};
%%
%% SETTINGS frames.
%%
parse(<< 0:24, 4:8, _:7, 1:1, _:1, 0:31, Rest/bits >>) ->
	{ok, settings_ack, Rest};
parse(<< _:24, 4:8, _:7, 1:1, _:1, 0:31, _/bits >>) ->
	{connection_error, frame_size_error, 'SETTINGS frames with the ACK flag set MUST have a length of 0. (RFC7540 6.5)'};
parse(<< Len:24, 4:8, _:7, 0:1, _:1, 0:31, _/bits >>) when Len rem 6 =/= 0 ->
	{connection_error, frame_size_error, 'SETTINGS frames MUST have a length multiple of 6. (RFC7540 6.5)'};
parse(<< Len:24, 4:8, _:7, 0:1, _:1, 0:31, Rest/bits >>) when byte_size(Rest) >= Len ->
	parse_settings_payload(Rest, Len, #{});
parse(<< _:24, 4:8, _:8, _:1, StreamID:31, _/bits >>) when StreamID =/= 0 ->
	{connection_error, protocol_error, 'SETTINGS frames MUST NOT be associated with a stream. (RFC7540 6.5)'};
%%
%% PUSH_PROMISE frames.
%%
parse(<< Len:24, 5:8, _:40, _/bits >>) when Len < 4 ->
	{connection_error, frame_size_error, 'PUSH_PROMISE frames MUST have a length >= 4. (RFC7540 4.2, RFC7540 6.6)'};
parse(<< Len:24, 5:8, _:4, 1:1, _:35, _/bits >>) when Len < 5 ->
	{connection_error, frame_size_error, 'PUSH_PROMISE frames with padding flag MUST have a length >= 5. (RFC7540 4.2, RFC7540 6.6)'};
parse(<< _:24, 5:8, _:9, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'PUSH_PROMISE frames MUST be associated with a stream. (RFC7540 6.6)'};
parse(<< Len0:24, 5:8, _:4, 1:1, _:35, PadLen:8, _/bits >>) when PadLen >= Len0 - 4 ->
	{connection_error, protocol_error, 'Length of padding MUST be less than length of payload. (RFC7540 6.6)'};
parse(<< Len0:24, 5:8, _:4, 0:1, FlagEndHeaders:1, _:3, StreamID:31, _:1, PromisedStreamID:31, Rest0/bits >>)
		when byte_size(Rest0) >= Len0 - 4 ->
	Len = Len0 - 4,
	<< HeaderBlockFragment:Len/binary, Rest/bits >> = Rest0,
	{ok, {push_promise, StreamID, parse_head_fin(FlagEndHeaders), PromisedStreamID, HeaderBlockFragment}, Rest};
parse(<< Len0:24, 5:8, _:4, 1:1, FlagEndHeaders:1, _:2, StreamID:31, PadLen:8, _:1, PromisedStreamID:31, Rest0/bits >>)
		when byte_size(Rest0) >= Len0 - 5 ->
	Len = Len0 - 5,
	case Rest0 of
		<< HeaderBlockFragment:Len/binary, 0:PadLen/unit:8, Rest/bits >> ->
			{ok, {push_promise, StreamID, parse_head_fin(FlagEndHeaders), PromisedStreamID, HeaderBlockFragment}, Rest};
		_ ->
			{connection_error, protocol_error, 'Padding octets MUST be set to zero. (RFC7540 6.6)'}
	end;
%%
%% PING frames.
%%
parse(<< 8:24, 6:8, _:7, 1:1, _:1, 0:31, Opaque:64, Rest/bits >>) ->
	{ok, {ping_ack, Opaque}, Rest};
parse(<< 8:24, 6:8, _:7, 0:1, _:1, 0:31, Opaque:64, Rest/bits >>) ->
	{ok, {ping, Opaque}, Rest};
parse(<< 8:24, 6:8, _:104, _/bits >>) ->
	{connection_error, protocol_error, 'PING frames MUST NOT be associated with a stream. (RFC7540 6.7)'};
parse(<< Len:24, 6:8, _/bits >>) when Len =/= 8 ->
	{connection_error, frame_size_error, 'PING frames MUST be 8 bytes wide. (RFC7540 6.7)'};
%%
%% GOAWAY frames.
%%
parse(<< Len0:24, 7:8, _:9, 0:31, _:1, LastStreamID:31, ErrorCode:32, Rest0/bits >>) when byte_size(Rest0) >= Len0 - 8 ->
	Len = Len0 - 8,
	<< DebugData:Len/binary, Rest/bits >> = Rest0,
	{ok, {goaway, LastStreamID, parse_error_code(ErrorCode), DebugData}, Rest};
parse(<< Len:24, 7:8, _:40, _/bits >>) when Len < 8 ->
	{connection_error, frame_size_error, 'GOAWAY frames MUST have a length >= 8. (RFC7540 4.2, RFC7540 6.8)'};
parse(<< _:24, 7:8, _:40, _/bits >>) ->
	{connection_error, protocol_error, 'GOAWAY frames MUST NOT be associated with a stream. (RFC7540 6.8)'};
%%
%% WINDOW_UPDATE frames.
%%
parse(<< 4:24, 8:8, _:9, 0:31, _:1, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'WINDOW_UPDATE frames MUST have a non-zero increment. (RFC7540 6.9)'};
parse(<< 4:24, 8:8, _:9, 0:31, _:1, Increment:31, Rest/bits >>) ->
	{ok, {window_update, Increment}, Rest};
parse(<< 4:24, 8:8, _:9, StreamID:31, _:1, 0:31, Rest/bits >>) ->
	{stream_error, StreamID, protocol_error, 'WINDOW_UPDATE frames MUST have a non-zero increment. (RFC7540 6.9)', Rest};
parse(<< 4:24, 8:8, _:9, StreamID:31, _:1, Increment:31, Rest/bits >>) ->
	{ok, {window_update, StreamID, Increment}, Rest};
parse(<< Len:24, 8:8, _/bits >>) when Len =/= 4->
	{connection_error, frame_size_error, 'WINDOW_UPDATE frames MUST be 4 bytes wide. (RFC7540 6.9)'};
%%
%% CONTINUATION frames.
%%
parse(<< _:24, 9:8, _:9, 0:31, _/bits >>) ->
	{connection_error, protocol_error, 'CONTINUATION frames MUST be associated with a stream. (RFC7540 6.10)'};
parse(<< Len:24, 9:8, _:5, FlagEndHeaders:1, _:3, StreamID:31, HeaderBlockFragment:Len/binary, Rest/bits >>) ->
	{ok, {continuation, StreamID, parse_head_fin(FlagEndHeaders), HeaderBlockFragment}, Rest};
%%
%% Unknown frames are ignored.
%%
parse(<< Len:24, Type:8, _:40, _:Len/binary, Rest/bits >>) when Type > 9 ->
	{ignore, Rest};
%%
%% Incomplete frames.
%%
parse(_) ->
	more.

-ifdef(TEST).
parse_ping_test() ->
	Ping = ping(1234567890),
	_ = [more = parse(binary:part(Ping, 0, I)) || I <- lists:seq(1, byte_size(Ping) - 1)],
	{ok, {ping, 1234567890}, <<>>} = parse(Ping),
	{ok, {ping, 1234567890}, << 42 >>} = parse(<< Ping/binary, 42 >>),
	ok.

parse_windows_update_test() ->
	WindowUpdate = << 4:24, 8:8, 0:9, 0:31, 0:1, 12345:31 >>,
	_ = [more = parse(binary:part(WindowUpdate, 0, I)) || I <- lists:seq(1, byte_size(WindowUpdate) - 1)],
	{ok, {window_update, 12345}, <<>>} = parse(WindowUpdate),
	{ok, {window_update, 12345}, << 42 >>} = parse(<< WindowUpdate/binary, 42 >>),
	ok.

parse_settings_test() ->
	more = parse(<< 0:24, 4:8, 1:8, 0:8 >>),
	{ok, settings_ack, <<>>} = parse(<< 0:24, 4:8, 1:8, 0:32 >>),
	{connection_error, protocol_error, _} = parse(<< 0:24, 4:8, 1:8, 0:1, 1:31 >>),
	ok.
-endif.

parse_fin(0) -> nofin;
parse_fin(1) -> fin.

parse_head_fin(0) -> head_nofin;
parse_head_fin(1) -> head_fin.

parse_exclusive(0) -> shared;
parse_exclusive(1) -> exclusive.

parse_error_code( 0) -> no_error;
parse_error_code( 1) -> protocol_error;
parse_error_code( 2) -> internal_error;
parse_error_code( 3) -> flow_control_error;
parse_error_code( 4) -> settings_timeout;
parse_error_code( 5) -> stream_closed;
parse_error_code( 6) -> frame_size_error;
parse_error_code( 7) -> refused_stream;
parse_error_code( 8) -> cancel;
parse_error_code( 9) -> compression_error;
parse_error_code(10) -> connect_error;
parse_error_code(11) -> enhance_your_calm;
parse_error_code(12) -> inadequate_security;
parse_error_code(13) -> http_1_1_required;
parse_error_code(_) -> unknown_error.

parse_settings_payload(SettingsPayload) ->
	{ok, {settings, Settings}, <<>>}
		= parse_settings_payload(SettingsPayload, byte_size(SettingsPayload), #{}),
	Settings.

parse_settings_payload(Rest, 0, Settings) ->
	{ok, {settings, Settings}, Rest};
%% SETTINGS_HEADER_TABLE_SIZE.
parse_settings_payload(<< 1:16, Value:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{header_table_size => Value});
%% SETTINGS_ENABLE_PUSH.
parse_settings_payload(<< 2:16, 0:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{enable_push => false});
parse_settings_payload(<< 2:16, 1:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{enable_push => true});
parse_settings_payload(<< 2:16, _:32, _/bits >>, _, _) ->
	{connection_error, protocol_error, 'The SETTINGS_ENABLE_PUSH value MUST be 0 or 1. (RFC7540 6.5.2)'};
%% SETTINGS_MAX_CONCURRENT_STREAMS.
parse_settings_payload(<< 3:16, Value:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{max_concurrent_streams => Value});
%% SETTINGS_INITIAL_WINDOW_SIZE.
parse_settings_payload(<< 4:16, Value:32, _/bits >>, _, _) when Value > 16#7fffffff ->
	{connection_error, flow_control_error, 'The maximum SETTINGS_INITIAL_WINDOW_SIZE value is 0x7fffffff. (RFC7540 6.5.2)'};
parse_settings_payload(<< 4:16, Value:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{initial_window_size => Value});
%% SETTINGS_MAX_FRAME_SIZE.
parse_settings_payload(<< 5:16, Value:32, _/bits >>, _, _) when Value =< 16#3fff ->
	{connection_error, protocol_error, 'The SETTINGS_MAX_FRAME_SIZE value must be > 0x3fff. (RFC7540 6.5.2)'};
parse_settings_payload(<< 5:16, Value:32, Rest/bits >>, Len, Settings) when Value =< 16#ffffff ->
	parse_settings_payload(Rest, Len - 6, Settings#{max_frame_size => Value});
parse_settings_payload(<< 5:16, _:32, _/bits >>, _, _) ->
	{connection_error, protocol_error, 'The SETTINGS_MAX_FRAME_SIZE value must be =< 0xffffff. (RFC7540 6.5.2)'};
%% SETTINGS_MAX_HEADER_LIST_SIZE.
parse_settings_payload(<< 6:16, Value:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{max_header_list_size => Value});
%% SETTINGS_ENABLE_CONNECT_PROTOCOL.
parse_settings_payload(<< 8:16, 0:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{enable_connect_protocol => false});
parse_settings_payload(<< 8:16, 1:32, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings#{enable_connect_protocol => true});
parse_settings_payload(<< 8:16, _:32, _/bits >>, _, _) ->
	{connection_error, protocol_error, 'The SETTINGS_ENABLE_CONNECT_PROTOCOL value MUST be 0 or 1. (draft-h2-websockets-01 3)'};
%% Ignore unknown settings.
parse_settings_payload(<< _:48, Rest/bits >>, Len, Settings) ->
	parse_settings_payload(Rest, Len - 6, Settings).

%% Building.

data(StreamID, IsFin, Data) ->
	[data_header(StreamID, IsFin, iolist_size(Data)), Data].

data_header(StreamID, IsFin, Len) ->
	FlagEndStream = flag_fin(IsFin),
	<< Len:24, 0:15, FlagEndStream:1, 0:1, StreamID:31 >>.

%% @todo Check size of HeaderBlock and use CONTINUATION frames if needed.
headers(StreamID, IsFin, HeaderBlock) ->
	Len = iolist_size(HeaderBlock),
	FlagEndStream = flag_fin(IsFin),
	FlagEndHeaders = 1,
	[<< Len:24, 1:8, 0:5, FlagEndHeaders:1, 0:1, FlagEndStream:1, 0:1, StreamID:31 >>, HeaderBlock].

priority(StreamID, E, DepStreamID, Weight) ->
	FlagExclusive = exclusive(E),
	<< 5:24, 2:8, 0:9, StreamID:31, FlagExclusive:1, DepStreamID:31, Weight:8 >>.

rst_stream(StreamID, Reason) ->
	ErrorCode = error_code(Reason),
	<< 4:24, 3:8, 0:9, StreamID:31, ErrorCode:32 >>.

settings(Settings) ->
	Payload = settings_payload(Settings),
	Len = iolist_size(Payload),
	[<< Len:24, 4:8, 0:40 >>, Payload].

settings_payload(Settings) ->
	[case Key of
		header_table_size -> <<1:16, Value:32>>;
		enable_push when Value -> <<2:16, 1:32>>;
		enable_push -> <<2:16, 0:32>>;
		max_concurrent_streams when Value =:= infinity -> <<>>;
		max_concurrent_streams -> <<3:16, Value:32>>;
		initial_window_size -> <<4:16, Value:32>>;
		max_frame_size -> <<5:16, Value:32>>;
		max_header_list_size when Value =:= infinity -> <<>>;
		max_header_list_size -> <<6:16, Value:32>>;
		enable_connect_protocol when Value -> <<8:16, 1:32>>;
		enable_connect_protocol -> <<8:16, 0:32>>
	end || {Key, Value} <- maps:to_list(Settings)].

settings_ack() ->
	<< 0:24, 4:8, 1:8, 0:32 >>.

%% @todo Check size of HeaderBlock and use CONTINUATION frames if needed.
push_promise(StreamID, PromisedStreamID, HeaderBlock) ->
	Len = iolist_size(HeaderBlock) + 4,
	FlagEndHeaders = 1,
	[<< Len:24, 5:8, 0:5, FlagEndHeaders:1, 0:3, StreamID:31, 0:1, PromisedStreamID:31 >>, HeaderBlock].

ping(Opaque) ->
	<< 8:24, 6:8, 0:40, Opaque:64 >>.

ping_ack(Opaque) ->
	<< 8:24, 6:8, 0:7, 1:1, 0:32, Opaque:64 >>.

goaway(LastStreamID, Reason, DebugData) ->
	ErrorCode = error_code(Reason),
	Len = iolist_size(DebugData) + 8,
	[<< Len:24, 7:8, 0:41, LastStreamID:31, ErrorCode:32 >>, DebugData].

window_update(Increment) ->
	window_update(0, Increment).

window_update(StreamID, Increment) when Increment =< 16#7fffffff ->
	<< 4:24, 8:8, 0:8, StreamID:32, 0:1, Increment:31 >>.

flag_fin(nofin) -> 0;
flag_fin(fin) -> 1.

exclusive(shared) -> 0;
exclusive(exclusive) -> 1.

error_code(no_error) -> 0;
error_code(protocol_error) -> 1;
error_code(internal_error) -> 2;
error_code(flow_control_error) -> 3;
error_code(settings_timeout) -> 4;
error_code(stream_closed) -> 5;
error_code(frame_size_error) -> 6;
error_code(refused_stream) -> 7;
error_code(cancel) -> 8;
error_code(compression_error) -> 9;
error_code(connect_error) -> 10;
error_code(enhance_your_calm) -> 11;
error_code(inadequate_security) -> 12;
error_code(http_1_1_required) -> 13.
