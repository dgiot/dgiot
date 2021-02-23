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

-module(cow_ws).

-export([key/0]).
-export([encode_key/1]).

-export([negotiate_permessage_deflate/3]).
-export([negotiate_x_webkit_deflate_frame/3]).

-export([validate_permessage_deflate/3]).

-export([parse_header/3]).
-export([parse_payload/9]).
-export([make_frame/4]).

-export([frame/2]).
-export([masked_frame/2]).

-type close_code() :: 1000..1003 | 1006..1011 | 3000..4999.
-export_type([close_code/0]).

-type extensions() :: map().
-export_type([extensions/0]).

-type deflate_opts() :: #{
	%% Compression parameters.
	level => zlib:zlevel(),
	mem_level => zlib:zmemlevel(),
	strategy => zlib:zstrategy(),

	%% Whether the compression context will carry over between frames.
	server_context_takeover => takeover | no_takeover,
	client_context_takeover => takeover | no_takeover,

	%% LZ77 sliding window size limits.
	server_max_window_bits => 8..15,
	client_max_window_bits => 8..15
}.
-export_type([deflate_opts/0]).

-type frag_state() :: undefined | {fin | nofin, text | binary, rsv()}.
-export_type([frag_state/0]).

-type frame() :: close | ping | pong
	| {text | binary | close | ping | pong, iodata()}
	| {close, close_code(), iodata()}
	| {fragment, fin | nofin, text | binary | continuation, iodata()}.
-export_type([frame/0]).

-type frame_type() :: fragment | text | binary | close | ping | pong.
-export_type([frame_type/0]).

-type mask_key() :: undefined | 0..16#ffffffff.
-export_type([mask_key/0]).

-type rsv() :: <<_:3>>.
-export_type([rsv/0]).

-type utf8_state() :: 0..8 | undefined.
-export_type([utf8_state/0]).

%% @doc Generate a key for the Websocket handshake request.

-spec key() -> binary().
key() ->
	base64:encode(crypto:strong_rand_bytes(16)).

%% @doc Encode the key into the accept value for the Websocket handshake response.

-spec encode_key(binary()) -> binary().
encode_key(Key) ->
	base64:encode(crypto:hash(sha, [Key, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"])).

%% @doc Negotiate the permessage-deflate extension.

-spec negotiate_permessage_deflate(
	[binary() | {binary(), binary()}], Exts, deflate_opts())
	-> ignore | {ok, iolist(), Exts} when Exts::extensions().
%% Ignore if deflate already negotiated.
negotiate_permessage_deflate(_, #{deflate := _}, _) ->
	ignore;
negotiate_permessage_deflate(Params, Extensions, Opts) ->
	case lists:usort(Params) of
		%% Ignore if multiple parameters with the same name.
		Params2 when length(Params) =/= length(Params2) ->
			ignore;
		Params2 ->
			negotiate_permessage_deflate1(Params2, Extensions, Opts)
	end.

negotiate_permessage_deflate1(Params, Extensions, Opts) ->
	%% We are allowed to send back no_takeover even if the client
	%% accepts takeover. Therefore we use no_takeover if any of
	%% the inputs have it.
	ServerTakeover = maps:get(server_context_takeover, Opts, takeover),
	ClientTakeover = maps:get(client_context_takeover, Opts, takeover),
	%% We can send back window bits smaller than or equal to what
	%% the client sends us.
	ServerMaxWindowBits = maps:get(server_max_window_bits, Opts, 15),
	ClientMaxWindowBits = maps:get(client_max_window_bits, Opts, 15),
	%% We may need to send back no_context_takeover depending on configuration.
	RespParams0 = case ServerTakeover of
		takeover -> [];
		no_takeover -> [<<"; server_no_context_takeover">>]
	end,
	RespParams1 = case ClientTakeover of
		takeover -> RespParams0;
		no_takeover -> [<<"; client_no_context_takeover">>|RespParams0]
	end,
	Negotiated0 = #{
		server_context_takeover => ServerTakeover,
		client_context_takeover => ClientTakeover,
		server_max_window_bits => ServerMaxWindowBits,
		client_max_window_bits => ClientMaxWindowBits
	},
	case negotiate_params(Params, Negotiated0, RespParams1) of
		ignore ->
			ignore;
		{#{server_max_window_bits := SB}, _} when SB > ServerMaxWindowBits ->
			ignore;
		{#{client_max_window_bits := CB}, _} when CB > ClientMaxWindowBits ->
			ignore;
		{Negotiated, RespParams2} ->
			%% We add the configured max window bits if necessary.
			RespParams = case Negotiated of
				#{server_max_window_bits_set := true} -> RespParams2;
				_ when ServerMaxWindowBits =:= 15 -> RespParams2;
				_ -> [<<"; server_max_window_bits=">>,
					integer_to_binary(ServerMaxWindowBits)|RespParams2]
			end,
			{Inflate, Deflate} = init_permessage_deflate(
				maps:get(client_max_window_bits, Negotiated),
				maps:get(server_max_window_bits, Negotiated), Opts),
			{ok, [<<"permessage-deflate">>, RespParams], Extensions#{
				deflate => Deflate,
				deflate_takeover => maps:get(server_context_takeover, Negotiated),
				inflate => Inflate,
				inflate_takeover => maps:get(client_context_takeover, Negotiated)}}
	end.

negotiate_params([], Negotiated, RespParams) ->
	{Negotiated, RespParams};
%% We must only send the client_max_window_bits parameter if the
%% request explicitly indicated the client supports it.
negotiate_params([<<"client_max_window_bits">>|Tail], Negotiated, RespParams) ->
	CB = maps:get(client_max_window_bits, Negotiated),
	negotiate_params(Tail, Negotiated#{client_max_window_bits_set => true},
		[<<"; client_max_window_bits=">>, integer_to_binary(CB)|RespParams]);
negotiate_params([{<<"client_max_window_bits">>, Max}|Tail], Negotiated, RespParams) ->
	CB0 = maps:get(client_max_window_bits, Negotiated, undefined),
	case parse_max_window_bits(Max) of
		error ->
			ignore;
		CB when CB =< CB0 ->
			negotiate_params(Tail, Negotiated#{client_max_window_bits => CB},
				[<<"; client_max_window_bits=">>, Max|RespParams]);
		%% When the client sends window bits larger than the server wants
		%% to use, we use what the server defined.
		_ ->
			negotiate_params(Tail, Negotiated,
				[<<"; client_max_window_bits=">>, integer_to_binary(CB0)|RespParams])
	end;
negotiate_params([{<<"server_max_window_bits">>, Max}|Tail], Negotiated, RespParams) ->
	SB0 = maps:get(server_max_window_bits, Negotiated, undefined),
	case parse_max_window_bits(Max) of
		error ->
			ignore;
		SB when SB =< SB0 ->
			negotiate_params(Tail, Negotiated#{
				server_max_window_bits => SB,
				server_max_window_bits_set => true},
				[<<"; server_max_window_bits=">>, Max|RespParams]);
		%% When the client sends window bits larger than the server wants
		%% to use, we use what the server defined. The parameter will be
		%% set only when this function returns.
		_ ->
			negotiate_params(Tail, Negotiated, RespParams)
	end;
%% We only need to send the no_context_takeover parameter back
%% here if we didn't already define it via configuration.
negotiate_params([<<"client_no_context_takeover">>|Tail], Negotiated, RespParams) ->
	case maps:get(client_context_takeover, Negotiated) of
		no_takeover ->
			negotiate_params(Tail, Negotiated, RespParams);
		takeover ->
			negotiate_params(Tail, Negotiated#{client_context_takeover => no_takeover},
				[<<"; client_no_context_takeover">>|RespParams])
	end;
negotiate_params([<<"server_no_context_takeover">>|Tail], Negotiated, RespParams) ->
	case maps:get(server_context_takeover, Negotiated) of
		no_takeover ->
			negotiate_params(Tail, Negotiated, RespParams);
		takeover ->
			negotiate_params(Tail, Negotiated#{server_context_takeover => no_takeover},
				[<<"; server_no_context_takeover">>|RespParams])
	end;
%% Ignore if unknown parameter; ignore if parameter with invalid or missing value.
negotiate_params(_, _, _) ->
	ignore.

parse_max_window_bits(<<"8">>) -> 8;
parse_max_window_bits(<<"9">>) -> 9;
parse_max_window_bits(<<"10">>) -> 10;
parse_max_window_bits(<<"11">>) -> 11;
parse_max_window_bits(<<"12">>) -> 12;
parse_max_window_bits(<<"13">>) -> 13;
parse_max_window_bits(<<"14">>) -> 14;
parse_max_window_bits(<<"15">>) -> 15;
parse_max_window_bits(_) -> error.

%% A negative WindowBits value indicates that zlib headers are not used.
init_permessage_deflate(InflateWindowBits, DeflateWindowBits, Opts) ->
	Inflate = zlib:open(),
	ok = zlib:inflateInit(Inflate, -InflateWindowBits),
	Deflate = zlib:open(),
	%% zlib 1.2.11+ now rejects -8. It used to transform it to -9.
	%% We need to use 9 when 8 is requested for interoperability.
	DeflateWindowBits2 = case DeflateWindowBits of
		8 -> 9;
		_ -> DeflateWindowBits
	end,
	ok = zlib:deflateInit(Deflate,
		maps:get(level, Opts, best_compression),
		deflated,
		-DeflateWindowBits2,
		maps:get(mem_level, Opts, 8),
		maps:get(strategy, Opts, default)),
	%% Set the owner pid of the zlib contexts if requested.
	case Opts of
		#{owner := Pid} -> set_owner(Pid, Inflate, Deflate);
		_ -> ok
	end,
	{Inflate, Deflate}.

-ifdef(OTP_RELEASE).
%% Using is_port/1 on a zlib context results in a Dialyzer warning in OTP 21.
%% This function helps silence that warning while staying compatible
%% with all supported versions.

set_owner(Pid, Inflate, Deflate) ->
	zlib:set_controlling_process(Inflate, Pid),
	zlib:set_controlling_process(Deflate, Pid).
-else.
%% The zlib port became a reference in OTP 20.1+. There
%% was however no way to change the controlling process
%% until the OTP 20.1.3 patch version. Since we can't
%% enable compression for 20.1, 20.1.1 and 20.1.2 we
%% explicitly crash. The caller should ignore this extension.

set_owner(Pid, Inflate, Deflate) when is_port(Inflate) ->
	true = erlang:port_connect(Inflate, Pid),
	true = unlink(Inflate),
	true = erlang:port_connect(Deflate, Pid),
	true = unlink(Deflate),
	ok;
set_owner(Pid, Inflate, Deflate) ->
	case erlang:function_exported(zlib, set_controlling_process, 2) of
		true ->
			zlib:set_controlling_process(Inflate, Pid),
			zlib:set_controlling_process(Deflate, Pid);
		false ->
			exit({error, incompatible_zlib_version,
				'OTP 20.1, 20.1.1 and 20.1.2 are missing required functionality.'})
	end.
-endif.

%% @doc Negotiate the x-webkit-deflate-frame extension.
%%
%% The implementation is very basic and none of the parameters
%% are currently supported.

-spec negotiate_x_webkit_deflate_frame(
	[binary() | {binary(), binary()}], Exts, deflate_opts())
	-> ignore | {ok, binary(), Exts} when Exts::extensions().
negotiate_x_webkit_deflate_frame(_, #{deflate := _}, _) ->
	ignore;
negotiate_x_webkit_deflate_frame(_Params, Extensions, Opts) ->
	% Since we are negotiating an unconstrained deflate-frame
	% then we must be willing to accept frames using the
	% maximum window size which is 2^15.
	{Inflate, Deflate} = init_permessage_deflate(15, 15, Opts),
	{ok, <<"x-webkit-deflate-frame">>,
		Extensions#{
			deflate => Deflate,
			deflate_takeover => takeover,
			inflate => Inflate,
			inflate_takeover => takeover}}.

%% @doc Validate the negotiated permessage-deflate extension.

%% Error when more than one deflate extension was negotiated.
validate_permessage_deflate(_, #{deflate := _}, _) ->
	error;
validate_permessage_deflate(Params, Extensions, Opts) ->
	case lists:usort(Params) of
		%% Error if multiple parameters with the same name.
		Params2 when length(Params) =/= length(Params2) ->
			error;
		Params2 ->
			case parse_response_permessage_deflate_params(Params2, 15, takeover, 15, takeover) of
				error ->
					error;
				{ClientWindowBits, ClientTakeOver, ServerWindowBits, ServerTakeOver} ->
					{Inflate, Deflate} = init_permessage_deflate(ServerWindowBits, ClientWindowBits, Opts),
					{ok, Extensions#{
						deflate => Deflate,
						deflate_takeover => ClientTakeOver,
						inflate => Inflate,
						inflate_takeover => ServerTakeOver}}
			end
	end.

parse_response_permessage_deflate_params([], CB, CTO, SB, STO) ->
	{CB, CTO, SB, STO};
parse_response_permessage_deflate_params([{<<"client_max_window_bits">>, Max}|Tail], _, CTO, SB, STO) ->
	case parse_max_window_bits(Max) of
		error -> error;
		CB -> parse_response_permessage_deflate_params(Tail, CB, CTO, SB, STO)
	end;
parse_response_permessage_deflate_params([<<"client_no_context_takeover">>|Tail], CB, _, SB, STO) ->
	parse_response_permessage_deflate_params(Tail, CB, no_takeover, SB, STO);
parse_response_permessage_deflate_params([{<<"server_max_window_bits">>, Max}|Tail], CB, CTO, _, STO) ->
	case parse_max_window_bits(Max) of
		error -> error;
		SB -> parse_response_permessage_deflate_params(Tail, CB, CTO, SB, STO)
	end;
parse_response_permessage_deflate_params([<<"server_no_context_takeover">>|Tail], CB, CTO, SB, _) ->
	parse_response_permessage_deflate_params(Tail, CB, CTO, SB, no_takeover);
%% Error if unknown parameter; error if parameter with invalid or missing value.
parse_response_permessage_deflate_params(_, _, _, _, _) ->
	error.

%% @doc Parse and validate the Websocket frame header.
%%
%% This function also updates the fragmentation state according to
%% information found in the frame's header.

-spec parse_header(binary(), extensions(), frag_state())
	-> error | more | {frame_type(), frag_state(), rsv(), non_neg_integer(), mask_key(), binary()}.
%% RSV bits MUST be 0 unless an extension is negotiated
%% that defines meanings for non-zero values.
parse_header(<< _:1, Rsv:3, _/bits >>, Extensions, _) when Extensions =:= #{}, Rsv =/= 0 -> error;
%% Last 2 RSV bits MUST be 0 if deflate-frame extension is used.
parse_header(<< _:2, 1:1, _/bits >>, #{deflate := _}, _) -> error;
parse_header(<< _:3, 1:1, _/bits >>, #{deflate := _}, _) -> error;
%% Invalid opcode. Note that these opcodes may be used by extensions.
parse_header(<< _:4, 3:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 4:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 5:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 6:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 7:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 11:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 12:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 13:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 14:4, _/bits >>, _, _) -> error;
parse_header(<< _:4, 15:4, _/bits >>, _, _) -> error;
%% Control frames MUST NOT be fragmented.
parse_header(<< 0:1, _:3, Opcode:4, _/bits >>, _, _) when Opcode >= 8 -> error;
%% A frame MUST NOT use the zero opcode unless fragmentation was initiated.
parse_header(<< _:4, 0:4, _/bits >>, _, undefined) -> error;
%% Non-control opcode when expecting control message or next fragment.
parse_header(<< _:4, 1:4, _/bits >>, _, {_, _, _}) -> error;
parse_header(<< _:4, 2:4, _/bits >>, _, {_, _, _}) -> error;
parse_header(<< _:4, 3:4, _/bits >>, _, {_, _, _}) -> error;
parse_header(<< _:4, 4:4, _/bits >>, _, {_, _, _}) -> error;
parse_header(<< _:4, 5:4, _/bits >>, _, {_, _, _}) -> error;
parse_header(<< _:4, 6:4, _/bits >>, _, {_, _, _}) -> error;
parse_header(<< _:4, 7:4, _/bits >>, _, {_, _, _}) -> error;
%% Close control frame length MUST be 0 or >= 2.
parse_header(<< _:4, 8:4, _:1, 1:7, _/bits >>, _, _) -> error;
%% Close control frame with incomplete close code. Need more data.
parse_header(Data = << _:4, 8:4, 0:1, Len:7, _/bits >>, _, _) when Len > 1, byte_size(Data) < 4 -> more;
parse_header(Data = << _:4, 8:4, 1:1, Len:7, _/bits >>, _, _) when Len > 1, byte_size(Data) < 8 -> more;
%% 7 bits payload length.
parse_header(<< Fin:1, Rsv:3/bits, Opcode:4, 0:1, Len:7, Rest/bits >>, _, FragState) when Len < 126 ->
	parse_header(Opcode, Fin, FragState, Rsv, Len, undefined, Rest);
parse_header(<< Fin:1, Rsv:3/bits, Opcode:4, 1:1, Len:7, MaskKey:32, Rest/bits >>, _, FragState) when Len < 126 ->
	parse_header(Opcode, Fin, FragState, Rsv, Len, MaskKey, Rest);
%% 16 bits payload length.
parse_header(<< Fin:1, Rsv:3/bits, Opcode:4, 0:1, 126:7, Len:16, Rest/bits >>, _, FragState) when Len > 125, Opcode < 8 ->
	parse_header(Opcode, Fin, FragState, Rsv, Len, undefined, Rest);
parse_header(<< Fin:1, Rsv:3/bits, Opcode:4, 1:1, 126:7, Len:16, MaskKey:32, Rest/bits >>, _, FragState) when Len > 125, Opcode < 8 ->
	parse_header(Opcode, Fin, FragState, Rsv, Len, MaskKey, Rest);
%% 63 bits payload length.
parse_header(<< Fin:1, Rsv:3/bits, Opcode:4, 0:1, 127:7, 0:1, Len:63, Rest/bits >>, _, FragState) when Len > 16#ffff, Opcode < 8 ->
	parse_header(Opcode, Fin, FragState, Rsv, Len, undefined, Rest);
parse_header(<< Fin:1, Rsv:3/bits, Opcode:4, 1:1, 127:7, 0:1, Len:63, MaskKey:32, Rest/bits >>, _, FragState) when Len > 16#ffff, Opcode < 8 ->
	parse_header(Opcode, Fin, FragState, Rsv, Len, MaskKey, Rest);
%% When payload length is over 63 bits, the most significant bit MUST be 0.
parse_header(<< _:9, 127:7, 1:1, _/bits >>, _, _) -> error;
%% For the next two clauses, it can be one of the following:
%%
%% * The minimal number of bytes MUST be used to encode the length
%% * All control frames MUST have a payload length of 125 bytes or less
parse_header(<< _:8, 0:1, 126:7, _:16, _/bits >>, _, _) -> error;
parse_header(<< _:8, 1:1, 126:7, _:48, _/bits >>, _, _) -> error;
parse_header(<< _:8, 0:1, 127:7, _:64, _/bits >>, _, _) -> error;
parse_header(<< _:8, 1:1, 127:7, _:96, _/bits >>, _, _) -> error;
%% Need more data.
parse_header(_, _, _) -> more.

parse_header(Opcode, Fin, FragState, Rsv, Len, MaskKey, Rest) ->
	Type = opcode_to_frame_type(Opcode),
	Type2 = case Fin of
		0 -> fragment;
		1 -> Type
	end,
	{Type2, frag_state(Type, Fin, Rsv, FragState), Rsv, Len, MaskKey, Rest}.

opcode_to_frame_type(0) -> fragment;
opcode_to_frame_type(1) -> text;
opcode_to_frame_type(2) -> binary;
opcode_to_frame_type(8) -> close;
opcode_to_frame_type(9) -> ping;
opcode_to_frame_type(10) -> pong.

frag_state(Type, 0, Rsv, undefined) -> {nofin, Type, Rsv};
frag_state(fragment, 0, _, FragState = {nofin, _, _}) -> FragState;
frag_state(fragment, 1, _, {nofin, Type, Rsv}) -> {fin, Type, Rsv};
frag_state(_, 1, _, FragState) -> FragState.

%% @doc Parse and validate the frame's payload.
%%
%% Validation is only required for text and close frames which feature
%% a UTF-8 payload.

-spec parse_payload(binary(), mask_key(), utf8_state(), non_neg_integer(),
		frame_type(), non_neg_integer(), frag_state(), extensions(), rsv())
	-> {ok, binary(), utf8_state(), binary()}
	| {ok, close_code(), binary(), utf8_state(), binary()}
	| {more, binary(), utf8_state()}
	| {more, close_code(), binary(), utf8_state()}
	| {error, badframe | badencoding}.
%% Empty last frame of compressed message.
parse_payload(Data, _, Utf8State, _, _, 0, {fin, _, << 1:1, 0:2 >>},
		#{inflate := Inflate, inflate_takeover := TakeOver}, _) ->
	_ = zlib:inflate(Inflate, << 0, 0, 255, 255 >>),
	case TakeOver of
		no_takeover -> zlib:inflateReset(Inflate);
		takeover -> ok
	end,
	{ok, <<>>, Utf8State, Data};
%% Compressed fragmented frame.
parse_payload(Data, MaskKey, Utf8State, ParsedLen, Type, Len, FragState = {_, _, << 1:1, 0:2 >>},
		#{inflate := Inflate, inflate_takeover := TakeOver}, _) ->
	{Data2, Rest, Eof} = split_payload(Data, Len),
	Payload = inflate_frame(unmask(Data2, MaskKey, ParsedLen), Inflate, TakeOver, FragState, Eof),
	validate_payload(Payload, Rest, Utf8State, ParsedLen, Type, FragState, Eof);
%% Compressed frame.
parse_payload(Data, MaskKey, Utf8State, ParsedLen, Type, Len, FragState,
		#{inflate := Inflate, inflate_takeover := TakeOver}, << 1:1, 0:2 >>) when Type =:= text; Type =:= binary ->
	{Data2, Rest, Eof} = split_payload(Data, Len),
	Payload = inflate_frame(unmask(Data2, MaskKey, ParsedLen), Inflate, TakeOver, FragState, Eof),
	validate_payload(Payload, Rest, Utf8State, ParsedLen, Type, FragState, Eof);
%% Empty frame.
parse_payload(Data, _, Utf8State, 0, _, 0, _, _, _)
		when Utf8State =:= 0; Utf8State =:= undefined ->
	{ok, <<>>, Utf8State, Data};
%% Start of close frame.
parse_payload(Data, MaskKey, Utf8State, 0, Type = close, Len, FragState, _, << 0:3 >>) ->
	{<< MaskedCode:2/binary, Data2/bits >>, Rest, Eof} = split_payload(Data, Len),
	<< CloseCode:16 >> = unmask(MaskedCode, MaskKey, 0),
	case validate_close_code(CloseCode) of
		ok ->
			Payload = unmask(Data2, MaskKey, 2),
			case validate_payload(Payload, Rest, Utf8State, 2, Type, FragState, Eof) of
				{ok, _, Utf8State2, _} -> {ok, CloseCode, Payload, Utf8State2, Rest};
				{more, _, Utf8State2} -> {more, CloseCode, Payload, Utf8State2};
				Error -> Error
			end;
		error ->
			{error, badframe}
	end;
%% Normal frame.
parse_payload(Data, MaskKey, Utf8State, ParsedLen, Type, Len, FragState, _, << 0:3 >>) ->
	{Data2, Rest, Eof} = split_payload(Data, Len),
	Payload = unmask(Data2, MaskKey, ParsedLen),
	validate_payload(Payload, Rest, Utf8State, ParsedLen, Type, FragState, Eof).

split_payload(Data, Len) ->
	case byte_size(Data) of
		Len ->
			{Data, <<>>, true};
		DataLen when DataLen < Len ->
			{Data, <<>>, false};
		_ ->
			<< Data2:Len/binary, Rest/bits >> = Data,
			{Data2, Rest, true}
	end.

validate_close_code(Code) ->
	if
		Code < 1000 -> error;
		Code =:= 1004 -> error;
		Code =:= 1005 -> error;
		Code =:= 1006 -> error;
		Code > 1011, Code < 3000 -> error;
		Code > 4999 -> error;
		true -> ok
	end.

unmask(Data, undefined, _) ->
	Data;
unmask(Data, MaskKey, 0) ->
	mask(Data, MaskKey, <<>>);
%% We unmask on the fly so we need to continue from the right mask byte.
unmask(Data, MaskKey, UnmaskedLen) ->
	Left = UnmaskedLen rem 4,
	Right = 4 - Left,
	MaskKey2 = (MaskKey bsl (Left * 8)) + (MaskKey bsr (Right * 8)),
	mask(Data, MaskKey2, <<>>).

mask(<<>>, _, Unmasked) ->
	Unmasked;
mask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	mask(Rest, MaskKey, << Acc/binary, T:32 >>);
mask(<< O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:24 >>;
mask(<< O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:16 >>;
mask(<< O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:8 >>.

inflate_frame(Data, Inflate, TakeOver, FragState, true)
		when FragState =:= undefined; element(1, FragState) =:= fin ->
	Data2 = zlib:inflate(Inflate, << Data/binary, 0, 0, 255, 255 >>),
	case TakeOver of
		no_takeover -> zlib:inflateReset(Inflate);
		takeover -> ok
	end,
	iolist_to_binary(Data2);
inflate_frame(Data, Inflate, _T, _F, _E) ->
	iolist_to_binary(zlib:inflate(Inflate, Data)).

%% The Utf8State variable can be set to 'undefined' to disable the validation.
validate_payload(Payload, _, undefined, _, _, _, false) ->
	{more, Payload, undefined};
validate_payload(Payload, Rest, undefined, _, _, _, true) ->
	{ok, Payload, undefined, Rest};
%% Text frames and close control frames MUST have a payload that is valid UTF-8.
validate_payload(Payload, Rest, Utf8State, _, Type, _, Eof) when Type =:= text; Type =:= close ->
	case validate_utf8(Payload, Utf8State) of
		1 -> {error, badencoding};
		Utf8State2 when not Eof -> {more, Payload, Utf8State2};
		0 when Eof -> {ok, Payload, 0, Rest};
		_ -> {error, badencoding}
	end;
validate_payload(Payload, Rest, Utf8State, _, fragment, {Fin, text, _}, Eof) ->
	case validate_utf8(Payload, Utf8State) of
		1 -> {error, badencoding};
		0 when Eof -> {ok, Payload, 0, Rest};
		Utf8State2 when Eof, Fin =:= nofin -> {ok, Payload, Utf8State2, Rest};
		Utf8State2 when not Eof -> {more, Payload, Utf8State2};
		_ -> {error, badencoding}
	end;
validate_payload(Payload, _, Utf8State, _, _, _, false) ->
	{more, Payload, Utf8State};
validate_payload(Payload, Rest, Utf8State, _, _, _, true) ->
	{ok, Payload, Utf8State, Rest}.

%% Based on the Flexible and Economical UTF-8 Decoder algorithm by
%% Bjoern Hoehrmann <bjoern@hoehrmann.de> (http://bjoern.hoehrmann.de/utf-8/decoder/dfa/).
%%
%% The original algorithm has been unrolled into all combinations of values for C and State
%% each with a clause. The common clauses were then grouped together.
%%
%% This function returns 0 on success, 1 on error, and 2..8 on incomplete data.
validate_utf8(<<>>, State) -> State;
validate_utf8(<< C, Rest/bits >>, 0) when C < 128 -> validate_utf8(Rest, 0);
validate_utf8(<< C, Rest/bits >>, 2) when C >= 128, C < 144 -> validate_utf8(Rest, 0);
validate_utf8(<< C, Rest/bits >>, 3) when C >= 128, C < 144 -> validate_utf8(Rest, 2);
validate_utf8(<< C, Rest/bits >>, 5) when C >= 128, C < 144 -> validate_utf8(Rest, 2);
validate_utf8(<< C, Rest/bits >>, 7) when C >= 128, C < 144 -> validate_utf8(Rest, 3);
validate_utf8(<< C, Rest/bits >>, 8) when C >= 128, C < 144 -> validate_utf8(Rest, 3);
validate_utf8(<< C, Rest/bits >>, 2) when C >= 144, C < 160 -> validate_utf8(Rest, 0);
validate_utf8(<< C, Rest/bits >>, 3) when C >= 144, C < 160 -> validate_utf8(Rest, 2);
validate_utf8(<< C, Rest/bits >>, 5) when C >= 144, C < 160 -> validate_utf8(Rest, 2);
validate_utf8(<< C, Rest/bits >>, 6) when C >= 144, C < 160 -> validate_utf8(Rest, 3);
validate_utf8(<< C, Rest/bits >>, 7) when C >= 144, C < 160 -> validate_utf8(Rest, 3);
validate_utf8(<< C, Rest/bits >>, 2) when C >= 160, C < 192 -> validate_utf8(Rest, 0);
validate_utf8(<< C, Rest/bits >>, 3) when C >= 160, C < 192 -> validate_utf8(Rest, 2);
validate_utf8(<< C, Rest/bits >>, 4) when C >= 160, C < 192 -> validate_utf8(Rest, 2);
validate_utf8(<< C, Rest/bits >>, 6) when C >= 160, C < 192 -> validate_utf8(Rest, 3);
validate_utf8(<< C, Rest/bits >>, 7) when C >= 160, C < 192 -> validate_utf8(Rest, 3);
validate_utf8(<< C, Rest/bits >>, 0) when C >= 194, C < 224 -> validate_utf8(Rest, 2);
validate_utf8(<< 224, Rest/bits >>, 0) -> validate_utf8(Rest, 4);
validate_utf8(<< C, Rest/bits >>, 0) when C >= 225, C < 237 -> validate_utf8(Rest, 3);
validate_utf8(<< 237, Rest/bits >>, 0) -> validate_utf8(Rest, 5);
validate_utf8(<< C, Rest/bits >>, 0) when C =:= 238; C =:= 239 -> validate_utf8(Rest, 3);
validate_utf8(<< 240, Rest/bits >>, 0) -> validate_utf8(Rest, 6);
validate_utf8(<< C, Rest/bits >>, 0) when C =:= 241; C =:= 242; C =:= 243 -> validate_utf8(Rest, 7);
validate_utf8(<< 244, Rest/bits >>, 0) -> validate_utf8(Rest, 8);
validate_utf8(_, _) -> 1.

%% @doc Return a frame tuple from parsed state and data.

-spec make_frame(frame_type(), binary(), close_code(), frag_state()) -> frame().
%% Fragmented frame.
make_frame(fragment, Payload, _, {Fin, Type, _}) -> {fragment, Fin, Type, Payload};
make_frame(text, Payload, _, _) -> {text, Payload};
make_frame(binary, Payload, _, _) -> {binary, Payload};
make_frame(close, <<>>, undefined, _) -> close;
make_frame(close, Payload, CloseCode, _) -> {close, CloseCode, Payload};
make_frame(ping, <<>>, _, _) -> ping;
make_frame(ping, Payload, _, _) -> {ping, Payload};
make_frame(pong, <<>>, _, _) -> pong;
make_frame(pong, Payload, _, _) -> {pong, Payload}.

%% @doc Construct an unmasked Websocket frame.

-spec frame(frame(), extensions()) -> iodata().
%% Control frames. Control packets must not be > 125 in length.
frame(close, _) ->
	<< 1:1, 0:3, 8:4, 0:8 >>;
frame(ping, _) ->
	<< 1:1, 0:3, 9:4, 0:8 >>;
frame(pong, _) ->
	<< 1:1, 0:3, 10:4, 0:8 >>;
frame({close, Payload}, Extensions) ->
	frame({close, 1000, Payload}, Extensions);
frame({close, StatusCode, Payload}, _) ->
	Len = 2 + iolist_size(Payload),
	true = Len =< 125,
	[<< 1:1, 0:3, 8:4, 0:1, Len:7, StatusCode:16 >>, Payload];
frame({ping, Payload}, _) ->
	Len = iolist_size(Payload),
	true = Len =< 125,
	[<< 1:1, 0:3, 9:4, 0:1, Len:7 >>, Payload];
frame({pong, Payload}, _) ->
	Len = iolist_size(Payload),
	true = Len =< 125,
	[<< 1:1, 0:3, 10:4, 0:1, Len:7 >>, Payload];
%% Data frames, deflate-frame extension.
frame({text, Payload}, #{deflate := Deflate, deflate_takeover := TakeOver})
		when Deflate =/= false ->
	Payload2 = deflate_frame(Payload, Deflate, TakeOver),
	Len = payload_length(Payload2),
	[<< 1:1, 1:1, 0:2, 1:4, 0:1, Len/bits >>, Payload2];
frame({binary, Payload}, #{deflate := Deflate, deflate_takeover := TakeOver})
		when Deflate =/= false ->
	Payload2 = deflate_frame(Payload, Deflate, TakeOver),
	Len = payload_length(Payload2),
	[<< 1:1, 1:1, 0:2, 2:4, 0:1, Len/bits >>, Payload2];
%% Data frames.
frame({text, Payload}, _) ->
	Len = payload_length(Payload),
	[<< 1:1, 0:3, 1:4, 0:1, Len/bits >>, Payload];
frame({binary, Payload}, _) ->
	Len = payload_length(Payload),
	[<< 1:1, 0:3, 2:4, 0:1, Len/bits >>, Payload].

%% @doc Construct a masked Websocket frame.
%%
%% We use a mask key of 0 if there is no payload for close, ping and pong frames.

-spec masked_frame(frame(), extensions()) -> iodata().
%% Control frames. Control packets must not be > 125 in length.
masked_frame(close, _) ->
	<< 1:1, 0:3, 8:4, 1:1, 0:39 >>;
masked_frame(ping, _) ->
	<< 1:1, 0:3, 9:4, 1:1, 0:39 >>;
masked_frame(pong, _) ->
	<< 1:1, 0:3, 10:4, 1:1, 0:39 >>;
masked_frame({close, Payload}, Extensions) ->
	frame({close, 1000, Payload}, Extensions);
masked_frame({close, StatusCode, Payload}, _) ->
	Len = 2 + iolist_size(Payload),
	true = Len =< 125,
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	[<< 1:1, 0:3, 8:4, 1:1, Len:7 >>, MaskKeyBin, mask(iolist_to_binary([<< StatusCode:16 >>, Payload]), MaskKey, <<>>)];
masked_frame({ping, Payload}, _) ->
	Len = iolist_size(Payload),
	true = Len =< 125,
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	[<< 1:1, 0:3, 9:4, 1:1, Len:7 >>, MaskKeyBin, mask(iolist_to_binary(Payload), MaskKey, <<>>)];
masked_frame({pong, Payload}, _) ->
	Len = iolist_size(Payload),
	true = Len =< 125,
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	[<< 1:1, 0:3, 10:4, 1:1, Len:7 >>, MaskKeyBin, mask(iolist_to_binary(Payload), MaskKey, <<>>)];
%% Data frames, deflate-frame extension.
masked_frame({text, Payload}, #{deflate := Deflate, deflate_takeover := TakeOver})
		when Deflate =/= false ->
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	Payload2 = mask(deflate_frame(Payload, Deflate, TakeOver), MaskKey, <<>>),
	Len = payload_length(Payload2),
	[<< 1:1, 1:1, 0:2, 1:4, 1:1, Len/bits >>, MaskKeyBin, Payload2];
masked_frame({binary, Payload}, #{deflate := Deflate, deflate_takeover := TakeOver})
		when Deflate =/= false ->
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	Payload2 = mask(deflate_frame(Payload, Deflate, TakeOver), MaskKey, <<>>),
	Len = payload_length(Payload2),
	[<< 1:1, 1:1, 0:2, 2:4, 1:1, Len/bits >>, MaskKeyBin, Payload2];
%% Data frames.
masked_frame({text, Payload}, _) ->
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	Len = payload_length(Payload),
	[<< 1:1, 0:3, 1:4, 1:1, Len/bits >>, MaskKeyBin, mask(iolist_to_binary(Payload), MaskKey, <<>>)];
masked_frame({binary, Payload}, _) ->
	MaskKeyBin = << MaskKey:32 >> = crypto:strong_rand_bytes(4),
	Len = payload_length(Payload),
	[<< 1:1, 0:3, 2:4, 1:1, Len/bits >>, MaskKeyBin, mask(iolist_to_binary(Payload), MaskKey, <<>>)].

payload_length(Payload) ->
	case iolist_size(Payload) of
		N when N =< 125 -> << N:7 >>;
		N when N =< 16#ffff -> << 126:7, N:16 >>;
		N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
	end.

deflate_frame(Payload, Deflate, TakeOver) ->
	Deflated = iolist_to_binary(zlib:deflate(Deflate, Payload, sync)),
	case TakeOver of
		no_takeover -> zlib:deflateReset(Deflate);
		takeover -> ok
	end,
	Len = byte_size(Deflated) - 4,
	case Deflated of
		<< Body:Len/binary, 0:8, 0:8, 255:8, 255:8 >> -> Body;
		_ -> Deflated
	end.
