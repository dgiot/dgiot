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

%% The current implementation is not suitable for use in
%% intermediaries as the information about headers that
%% should never be indexed is currently lost.

-module(cow_hpack).

-export([init/0]).
-export([init/1]).
-export([set_max_size/2]).

-export([decode/1]).
-export([decode/2]).
-export([decode/3]).

-export([encode/1]).
-export([encode/2]).
-export([encode/3]).

-record(state, {
	size = 0 :: non_neg_integer(),
	max_size = 4096 :: non_neg_integer(),
	configured_max_size = 4096 :: non_neg_integer(),
	dyn_table = [] :: [{pos_integer(), {binary(), binary()}}]
}).

-opaque state() :: #state{}.
-export_type([state/0]).

-type opts() :: map().
-export_type([opts/0]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

%% State initialization.

-spec init() -> state().
init() ->
	#state{}.

-spec init(non_neg_integer()) -> state().
init(MaxSize) ->
	#state{max_size=MaxSize, configured_max_size=MaxSize}.

%% Update the configured max size.
%%
%% When decoding, the local endpoint also needs to send a SETTINGS
%% frame with this value and it is then up to the remote endpoint
%% to decide what actual limit it will use. The actual limit is
%% signaled via dynamic table size updates in the encoded data.
%%
%% When encoding, the local endpoint will call this function after
%% receiving a SETTINGS frame with this value. The encoder will
%% then use this value as the new max after signaling via a dynamic
%% table size update. The value given as argument may be lower
%% than the one received in the SETTINGS.

-spec set_max_size(non_neg_integer(), State) -> State when State::state().
set_max_size(MaxSize, State) ->
	State#state{configured_max_size=MaxSize}.

%% Decoding.

-spec decode(binary()) -> {cow_http:headers(), state()}.
decode(Data) ->
	decode(Data, init(), #{}).

-spec decode(binary(), State) -> {cow_http:headers(), State} when State::state().
decode(Data, State) ->
	decode(Data, State, #{}).

-spec decode(binary(), State, opts()) -> {cow_http:headers(), State} when State::state().
%% Dynamic table size update is only allowed at the beginning of a HEADERS block.
decode(<< 0:2, 1:1, Rest/bits >>, State=#state{configured_max_size=ConfigMaxSize}, Opts) ->
	{MaxSize, Rest2} = dec_int5(Rest),
	if
		MaxSize =< ConfigMaxSize ->
			State2 = table_update_size(MaxSize, State),
			decode(Rest2, State2, Opts)
	end;
decode(Data, State, Opts) ->
	decode(Data, State, Opts, []).

decode(<<>>, State, _, Acc) ->
	{lists:reverse(Acc), State};
%% Indexed header field representation.
decode(<< 1:1, Rest/bits >>, State, Opts, Acc) ->
	dec_indexed(Rest, State, Opts, Acc);
%% Literal header field with incremental indexing: new name.
decode(<< 0:1, 1:1, 0:6, Rest/bits >>, State, Opts, Acc) ->
	dec_lit_index_new_name(Rest, State, Opts, Acc);
%% Literal header field with incremental indexing: indexed name.
decode(<< 0:1, 1:1, Rest/bits >>, State, Opts, Acc) ->
	dec_lit_index_indexed_name(Rest, State, Opts, Acc);
%% Literal header field without indexing: new name.
decode(<< 0:8, Rest/bits >>, State, Opts, Acc) ->
	dec_lit_no_index_new_name(Rest, State, Opts, Acc);
%% Literal header field without indexing: indexed name.
decode(<< 0:4, Rest/bits >>, State, Opts, Acc) ->
	dec_lit_no_index_indexed_name(Rest, State, Opts, Acc);
%% Literal header field never indexed: new name.
%% @todo Keep track of "never indexed" headers.
decode(<< 0:3, 1:1, 0:4, Rest/bits >>, State, Opts, Acc) ->
	dec_lit_no_index_new_name(Rest, State, Opts, Acc);
%% Literal header field never indexed: indexed name.
%% @todo Keep track of "never indexed" headers.
decode(<< 0:3, 1:1, Rest/bits >>, State, Opts, Acc) ->
	dec_lit_no_index_indexed_name(Rest, State, Opts, Acc).

%% Indexed header field representation.

dec_indexed(Rest, State, Opts, Acc) ->
	{Index, Rest2} = dec_int7(Rest),
	{Name, Value} = table_get(Index, State),
	decode(Rest2, State, Opts, [{Name, Value}|Acc]).

%% Literal header field with incremental indexing.

dec_lit_index_new_name(Rest, State, Opts, Acc) ->
	{Name, Rest2} = dec_str(Rest),
	dec_lit_index(Rest2, State, Opts, Acc, Name).

dec_lit_index_indexed_name(Rest, State, Opts, Acc) ->
	{Index, Rest2} = dec_int6(Rest),
	Name = table_get_name(Index, State),
	dec_lit_index(Rest2, State, Opts, Acc, Name).

dec_lit_index(Rest, State, Opts, Acc, Name) ->
	{Value, Rest2} = dec_str(Rest),
	State2 = table_insert({Name, Value}, State),
	decode(Rest2, State2, Opts, [{Name, Value}|Acc]).

%% Literal header field without indexing.

dec_lit_no_index_new_name(Rest, State, Opts, Acc) ->
	{Name, Rest2} = dec_str(Rest),
	dec_lit_no_index(Rest2, State, Opts, Acc, Name).

dec_lit_no_index_indexed_name(Rest, State, Opts, Acc) ->
	{Index, Rest2} = dec_int4(Rest),
	Name = table_get_name(Index, State),
	dec_lit_no_index(Rest2, State, Opts, Acc, Name).

dec_lit_no_index(Rest, State, Opts, Acc, Name) ->
	{Value, Rest2} = dec_str(Rest),
	decode(Rest2, State, Opts, [{Name, Value}|Acc]).

%% @todo Literal header field never indexed.

%% Decode an integer.

%% The HPACK format has 4 different integer prefixes length (from 4 to 7)
%% and each can be used to create an indefinite length integer if all bits
%% of the prefix are set to 1.

dec_int4(<< 2#1111:4, Rest/bits >>) ->
	dec_big_int(Rest, 15, 0);
dec_int4(<< Int:4, Rest/bits >>) ->
	{Int, Rest}.

dec_int5(<< 2#11111:5, Rest/bits >>) ->
	dec_big_int(Rest, 31, 0);
dec_int5(<< Int:5, Rest/bits >>) ->
	{Int, Rest}.

dec_int6(<< 2#111111:6, Rest/bits >>) ->
	dec_big_int(Rest, 63, 0);
dec_int6(<< Int:6, Rest/bits >>) ->
	{Int, Rest}.

dec_int7(<< 2#1111111:7, Rest/bits >>) ->
	dec_big_int(Rest, 127, 0);
dec_int7(<< Int:7, Rest/bits >>) ->
	{Int, Rest}.

dec_big_int(<< 0:1, Value:7, Rest/bits >>, Int, M) ->
	{Int + (Value bsl M), Rest};
dec_big_int(<< 1:1, Value:7, Rest/bits >>, Int, M) ->
	dec_big_int(Rest, Int + (Value bsl M), M + 7).

%% Decode a string.

dec_str(<< 0:1, Rest/bits >>) ->
	{Length, Rest2} = dec_int7(Rest),
	<< Str:Length/binary, Rest3/bits >> = Rest2,
	{Str, Rest3};
dec_str(<< 1:1, Rest/bits >>) ->
	{Length, Rest2} = dec_int7(Rest),
	dec_huffman(Rest2, Length * 8, <<>>).

%% HPACK uses a static code table for Huffman encoded strings.
%% It has been converted into one clause per code in the following function.

%% EOS.
dec_huffman(Rest, 0, String) -> {String, Rest};
dec_huffman(<<2#1:1, Rest/bits>>, 1, String) -> {String, Rest};
dec_huffman(<<2#11:2, Rest/bits>>, 2, String) -> {String, Rest};
dec_huffman(<<2#111:3, Rest/bits>>, 3, String) -> {String, Rest};
dec_huffman(<<2#1111:4, Rest/bits>>, 4, String) -> {String, Rest};
dec_huffman(<<2#11111:5, Rest/bits>>, 5, String) -> {String, Rest};
dec_huffman(<<2#111111:6, Rest/bits>>, 6, String) -> {String, Rest};
dec_huffman(<<2#1111111:7, Rest/bits>>, 7, String) -> {String, Rest};
%% Static code table.
dec_huffman(<<2#00000:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 48>>);
dec_huffman(<<2#00001:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 49>>);
dec_huffman(<<2#00010:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 50>>);
dec_huffman(<<2#00011:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 97>>);
dec_huffman(<<2#00100:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 99>>);
dec_huffman(<<2#00101:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 101>>);
dec_huffman(<<2#00110:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 105>>);
dec_huffman(<<2#00111:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 111>>);
dec_huffman(<<2#01000:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 115>>);
dec_huffman(<<2#01001:5, R/bits>>, L, A) -> dec_huffman(R, L - 5, <<A/binary, 116>>);
dec_huffman(<<2#010100:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 32>>);
dec_huffman(<<2#010101:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 37>>);
dec_huffman(<<2#010110:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 45>>);
dec_huffman(<<2#010111:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 46>>);
dec_huffman(<<2#011000:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 47>>);
dec_huffman(<<2#011001:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 51>>);
dec_huffman(<<2#011010:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 52>>);
dec_huffman(<<2#011011:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 53>>);
dec_huffman(<<2#011100:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 54>>);
dec_huffman(<<2#011101:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 55>>);
dec_huffman(<<2#011110:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 56>>);
dec_huffman(<<2#011111:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 57>>);
dec_huffman(<<2#100000:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 61>>);
dec_huffman(<<2#100001:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 65>>);
dec_huffman(<<2#100010:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 95>>);
dec_huffman(<<2#100011:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 98>>);
dec_huffman(<<2#100100:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 100>>);
dec_huffman(<<2#100101:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 102>>);
dec_huffman(<<2#100110:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 103>>);
dec_huffman(<<2#100111:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 104>>);
dec_huffman(<<2#101000:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 108>>);
dec_huffman(<<2#101001:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 109>>);
dec_huffman(<<2#101010:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 110>>);
dec_huffman(<<2#101011:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 112>>);
dec_huffman(<<2#101100:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 114>>);
dec_huffman(<<2#101101:6, R/bits>>, L, A) -> dec_huffman(R, L - 6, <<A/binary, 117>>);
dec_huffman(<<2#1011100:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 58>>);
dec_huffman(<<2#1011101:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 66>>);
dec_huffman(<<2#1011110:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 67>>);
dec_huffman(<<2#1011111:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 68>>);
dec_huffman(<<2#1100000:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 69>>);
dec_huffman(<<2#1100001:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 70>>);
dec_huffman(<<2#1100010:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 71>>);
dec_huffman(<<2#1100011:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 72>>);
dec_huffman(<<2#1100100:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 73>>);
dec_huffman(<<2#1100101:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 74>>);
dec_huffman(<<2#1100110:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 75>>);
dec_huffman(<<2#1100111:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 76>>);
dec_huffman(<<2#1101000:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 77>>);
dec_huffman(<<2#1101001:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 78>>);
dec_huffman(<<2#1101010:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 79>>);
dec_huffman(<<2#1101011:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 80>>);
dec_huffman(<<2#1101100:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 81>>);
dec_huffman(<<2#1101101:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 82>>);
dec_huffman(<<2#1101110:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 83>>);
dec_huffman(<<2#1101111:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 84>>);
dec_huffman(<<2#1110000:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 85>>);
dec_huffman(<<2#1110001:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 86>>);
dec_huffman(<<2#1110010:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 87>>);
dec_huffman(<<2#1110011:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 89>>);
dec_huffman(<<2#1110100:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 106>>);
dec_huffman(<<2#1110101:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 107>>);
dec_huffman(<<2#1110110:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 113>>);
dec_huffman(<<2#1110111:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 118>>);
dec_huffman(<<2#1111000:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 119>>);
dec_huffman(<<2#1111001:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 120>>);
dec_huffman(<<2#1111010:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 121>>);
dec_huffman(<<2#1111011:7, R/bits>>, L, A) -> dec_huffman(R, L - 7, <<A/binary, 122>>);
dec_huffman(<<2#11111000:8, R/bits>>, L, A) -> dec_huffman(R, L - 8, <<A/binary, 38>>);
dec_huffman(<<2#11111001:8, R/bits>>, L, A) -> dec_huffman(R, L - 8, <<A/binary, 42>>);
dec_huffman(<<2#11111010:8, R/bits>>, L, A) -> dec_huffman(R, L - 8, <<A/binary, 44>>);
dec_huffman(<<2#11111011:8, R/bits>>, L, A) -> dec_huffman(R, L - 8, <<A/binary, 59>>);
dec_huffman(<<2#11111100:8, R/bits>>, L, A) -> dec_huffman(R, L - 8, <<A/binary, 88>>);
dec_huffman(<<2#11111101:8, R/bits>>, L, A) -> dec_huffman(R, L - 8, <<A/binary, 90>>);
dec_huffman(<<2#1111111000:10, R/bits>>, L, A) -> dec_huffman(R, L - 10, <<A/binary, 33>>);
dec_huffman(<<2#1111111001:10, R/bits>>, L, A) -> dec_huffman(R, L - 10, <<A/binary, 34>>);
dec_huffman(<<2#1111111010:10, R/bits>>, L, A) -> dec_huffman(R, L - 10, <<A/binary, 40>>);
dec_huffman(<<2#1111111011:10, R/bits>>, L, A) -> dec_huffman(R, L - 10, <<A/binary, 41>>);
dec_huffman(<<2#1111111100:10, R/bits>>, L, A) -> dec_huffman(R, L - 10, <<A/binary, 63>>);
dec_huffman(<<2#11111111010:11, R/bits>>, L, A) -> dec_huffman(R, L - 11, <<A/binary, 39>>);
dec_huffman(<<2#11111111011:11, R/bits>>, L, A) -> dec_huffman(R, L - 11, <<A/binary, 43>>);
dec_huffman(<<2#11111111100:11, R/bits>>, L, A) -> dec_huffman(R, L - 11, <<A/binary, 124>>);
dec_huffman(<<2#111111111010:12, R/bits>>, L, A) -> dec_huffman(R, L - 12, <<A/binary, 35>>);
dec_huffman(<<2#111111111011:12, R/bits>>, L, A) -> dec_huffman(R, L - 12, <<A/binary, 62>>);
dec_huffman(<<2#1111111111000:13, R/bits>>, L, A) -> dec_huffman(R, L - 13, <<A/binary, 0>>);
dec_huffman(<<2#1111111111001:13, R/bits>>, L, A) -> dec_huffman(R, L - 13, <<A/binary, 36>>);
dec_huffman(<<2#1111111111010:13, R/bits>>, L, A) -> dec_huffman(R, L - 13, <<A/binary, 64>>);
dec_huffman(<<2#1111111111011:13, R/bits>>, L, A) -> dec_huffman(R, L - 13, <<A/binary, 91>>);
dec_huffman(<<2#1111111111100:13, R/bits>>, L, A) -> dec_huffman(R, L - 13, <<A/binary, 93>>);
dec_huffman(<<2#1111111111101:13, R/bits>>, L, A) -> dec_huffman(R, L - 13, <<A/binary, 126>>);
dec_huffman(<<2#11111111111100:14, R/bits>>, L, A) -> dec_huffman(R, L - 14, <<A/binary, 94>>);
dec_huffman(<<2#11111111111101:14, R/bits>>, L, A) -> dec_huffman(R, L - 14, <<A/binary, 125>>);
dec_huffman(<<2#111111111111100:15, R/bits>>, L, A) -> dec_huffman(R, L - 15, <<A/binary, 60>>);
dec_huffman(<<2#111111111111101:15, R/bits>>, L, A) -> dec_huffman(R, L - 15, <<A/binary, 96>>);
dec_huffman(<<2#111111111111110:15, R/bits>>, L, A) -> dec_huffman(R, L - 15, <<A/binary, 123>>);
dec_huffman(<<2#1111111111111110000:19, R/bits>>, L, A) -> dec_huffman(R, L - 19, <<A/binary, 92>>);
dec_huffman(<<2#1111111111111110001:19, R/bits>>, L, A) -> dec_huffman(R, L - 19, <<A/binary, 195>>);
dec_huffman(<<2#1111111111111110010:19, R/bits>>, L, A) -> dec_huffman(R, L - 19, <<A/binary, 208>>);
dec_huffman(<<2#11111111111111100110:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 128>>);
dec_huffman(<<2#11111111111111100111:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 130>>);
dec_huffman(<<2#11111111111111101000:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 131>>);
dec_huffman(<<2#11111111111111101001:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 162>>);
dec_huffman(<<2#11111111111111101010:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 184>>);
dec_huffman(<<2#11111111111111101011:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 194>>);
dec_huffman(<<2#11111111111111101100:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 224>>);
dec_huffman(<<2#11111111111111101101:20, R/bits>>, L, A) -> dec_huffman(R, L - 20, <<A/binary, 226>>);
dec_huffman(<<2#111111111111111011100:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 153>>);
dec_huffman(<<2#111111111111111011101:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 161>>);
dec_huffman(<<2#111111111111111011110:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 167>>);
dec_huffman(<<2#111111111111111011111:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 172>>);
dec_huffman(<<2#111111111111111100000:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 176>>);
dec_huffman(<<2#111111111111111100001:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 177>>);
dec_huffman(<<2#111111111111111100010:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 179>>);
dec_huffman(<<2#111111111111111100011:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 209>>);
dec_huffman(<<2#111111111111111100100:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 216>>);
dec_huffman(<<2#111111111111111100101:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 217>>);
dec_huffman(<<2#111111111111111100110:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 227>>);
dec_huffman(<<2#111111111111111100111:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 229>>);
dec_huffman(<<2#111111111111111101000:21, R/bits>>, L, A) -> dec_huffman(R, L - 21, <<A/binary, 230>>);
dec_huffman(<<2#1111111111111111010010:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 129>>);
dec_huffman(<<2#1111111111111111010011:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 132>>);
dec_huffman(<<2#1111111111111111010100:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 133>>);
dec_huffman(<<2#1111111111111111010101:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 134>>);
dec_huffman(<<2#1111111111111111010110:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 136>>);
dec_huffman(<<2#1111111111111111010111:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 146>>);
dec_huffman(<<2#1111111111111111011000:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 154>>);
dec_huffman(<<2#1111111111111111011001:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 156>>);
dec_huffman(<<2#1111111111111111011010:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 160>>);
dec_huffman(<<2#1111111111111111011011:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 163>>);
dec_huffman(<<2#1111111111111111011100:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 164>>);
dec_huffman(<<2#1111111111111111011101:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 169>>);
dec_huffman(<<2#1111111111111111011110:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 170>>);
dec_huffman(<<2#1111111111111111011111:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 173>>);
dec_huffman(<<2#1111111111111111100000:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 178>>);
dec_huffman(<<2#1111111111111111100001:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 181>>);
dec_huffman(<<2#1111111111111111100010:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 185>>);
dec_huffman(<<2#1111111111111111100011:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 186>>);
dec_huffman(<<2#1111111111111111100100:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 187>>);
dec_huffman(<<2#1111111111111111100101:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 189>>);
dec_huffman(<<2#1111111111111111100110:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 190>>);
dec_huffman(<<2#1111111111111111100111:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 196>>);
dec_huffman(<<2#1111111111111111101000:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 198>>);
dec_huffman(<<2#1111111111111111101001:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 228>>);
dec_huffman(<<2#1111111111111111101010:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 232>>);
dec_huffman(<<2#1111111111111111101011:22, R/bits>>, L, A) -> dec_huffman(R, L - 22, <<A/binary, 233>>);
dec_huffman(<<2#11111111111111111011000:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 1>>);
dec_huffman(<<2#11111111111111111011001:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 135>>);
dec_huffman(<<2#11111111111111111011010:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 137>>);
dec_huffman(<<2#11111111111111111011011:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 138>>);
dec_huffman(<<2#11111111111111111011100:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 139>>);
dec_huffman(<<2#11111111111111111011101:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 140>>);
dec_huffman(<<2#11111111111111111011110:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 141>>);
dec_huffman(<<2#11111111111111111011111:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 143>>);
dec_huffman(<<2#11111111111111111100000:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 147>>);
dec_huffman(<<2#11111111111111111100001:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 149>>);
dec_huffman(<<2#11111111111111111100010:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 150>>);
dec_huffman(<<2#11111111111111111100011:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 151>>);
dec_huffman(<<2#11111111111111111100100:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 152>>);
dec_huffman(<<2#11111111111111111100101:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 155>>);
dec_huffman(<<2#11111111111111111100110:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 157>>);
dec_huffman(<<2#11111111111111111100111:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 158>>);
dec_huffman(<<2#11111111111111111101000:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 165>>);
dec_huffman(<<2#11111111111111111101001:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 166>>);
dec_huffman(<<2#11111111111111111101010:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 168>>);
dec_huffman(<<2#11111111111111111101011:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 174>>);
dec_huffman(<<2#11111111111111111101100:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 175>>);
dec_huffman(<<2#11111111111111111101101:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 180>>);
dec_huffman(<<2#11111111111111111101110:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 182>>);
dec_huffman(<<2#11111111111111111101111:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 183>>);
dec_huffman(<<2#11111111111111111110000:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 188>>);
dec_huffman(<<2#11111111111111111110001:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 191>>);
dec_huffman(<<2#11111111111111111110010:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 197>>);
dec_huffman(<<2#11111111111111111110011:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 231>>);
dec_huffman(<<2#11111111111111111110100:23, R/bits>>, L, A) -> dec_huffman(R, L - 23, <<A/binary, 239>>);
dec_huffman(<<2#111111111111111111101010:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 9>>);
dec_huffman(<<2#111111111111111111101011:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 142>>);
dec_huffman(<<2#111111111111111111101100:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 144>>);
dec_huffman(<<2#111111111111111111101101:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 145>>);
dec_huffman(<<2#111111111111111111101110:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 148>>);
dec_huffman(<<2#111111111111111111101111:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 159>>);
dec_huffman(<<2#111111111111111111110000:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 171>>);
dec_huffman(<<2#111111111111111111110001:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 206>>);
dec_huffman(<<2#111111111111111111110010:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 215>>);
dec_huffman(<<2#111111111111111111110011:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 225>>);
dec_huffman(<<2#111111111111111111110100:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 236>>);
dec_huffman(<<2#111111111111111111110101:24, R/bits>>, L, A) -> dec_huffman(R, L - 24, <<A/binary, 237>>);
dec_huffman(<<2#1111111111111111111101100:25, R/bits>>, L, A) -> dec_huffman(R, L - 25, <<A/binary, 199>>);
dec_huffman(<<2#1111111111111111111101101:25, R/bits>>, L, A) -> dec_huffman(R, L - 25, <<A/binary, 207>>);
dec_huffman(<<2#1111111111111111111101110:25, R/bits>>, L, A) -> dec_huffman(R, L - 25, <<A/binary, 234>>);
dec_huffman(<<2#1111111111111111111101111:25, R/bits>>, L, A) -> dec_huffman(R, L - 25, <<A/binary, 235>>);
dec_huffman(<<2#11111111111111111111100000:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 192>>);
dec_huffman(<<2#11111111111111111111100001:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 193>>);
dec_huffman(<<2#11111111111111111111100010:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 200>>);
dec_huffman(<<2#11111111111111111111100011:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 201>>);
dec_huffman(<<2#11111111111111111111100100:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 202>>);
dec_huffman(<<2#11111111111111111111100101:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 205>>);
dec_huffman(<<2#11111111111111111111100110:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 210>>);
dec_huffman(<<2#11111111111111111111100111:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 213>>);
dec_huffman(<<2#11111111111111111111101000:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 218>>);
dec_huffman(<<2#11111111111111111111101001:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 219>>);
dec_huffman(<<2#11111111111111111111101010:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 238>>);
dec_huffman(<<2#11111111111111111111101011:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 240>>);
dec_huffman(<<2#11111111111111111111101100:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 242>>);
dec_huffman(<<2#11111111111111111111101101:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 243>>);
dec_huffman(<<2#11111111111111111111101110:26, R/bits>>, L, A) -> dec_huffman(R, L - 26, <<A/binary, 255>>);
dec_huffman(<<2#111111111111111111111011110:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 203>>);
dec_huffman(<<2#111111111111111111111011111:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 204>>);
dec_huffman(<<2#111111111111111111111100000:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 211>>);
dec_huffman(<<2#111111111111111111111100001:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 212>>);
dec_huffman(<<2#111111111111111111111100010:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 214>>);
dec_huffman(<<2#111111111111111111111100011:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 221>>);
dec_huffman(<<2#111111111111111111111100100:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 222>>);
dec_huffman(<<2#111111111111111111111100101:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 223>>);
dec_huffman(<<2#111111111111111111111100110:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 241>>);
dec_huffman(<<2#111111111111111111111100111:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 244>>);
dec_huffman(<<2#111111111111111111111101000:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 245>>);
dec_huffman(<<2#111111111111111111111101001:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 246>>);
dec_huffman(<<2#111111111111111111111101010:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 247>>);
dec_huffman(<<2#111111111111111111111101011:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 248>>);
dec_huffman(<<2#111111111111111111111101100:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 250>>);
dec_huffman(<<2#111111111111111111111101101:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 251>>);
dec_huffman(<<2#111111111111111111111101110:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 252>>);
dec_huffman(<<2#111111111111111111111101111:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 253>>);
dec_huffman(<<2#111111111111111111111110000:27, R/bits>>, L, A) -> dec_huffman(R, L - 27, <<A/binary, 254>>);
dec_huffman(<<2#1111111111111111111111100010:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 2>>);
dec_huffman(<<2#1111111111111111111111100011:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 3>>);
dec_huffman(<<2#1111111111111111111111100100:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 4>>);
dec_huffman(<<2#1111111111111111111111100101:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 5>>);
dec_huffman(<<2#1111111111111111111111100110:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 6>>);
dec_huffman(<<2#1111111111111111111111100111:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 7>>);
dec_huffman(<<2#1111111111111111111111101000:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 8>>);
dec_huffman(<<2#1111111111111111111111101001:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 11>>);
dec_huffman(<<2#1111111111111111111111101010:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 12>>);
dec_huffman(<<2#1111111111111111111111101011:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 14>>);
dec_huffman(<<2#1111111111111111111111101100:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 15>>);
dec_huffman(<<2#1111111111111111111111101101:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 16>>);
dec_huffman(<<2#1111111111111111111111101110:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 17>>);
dec_huffman(<<2#1111111111111111111111101111:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 18>>);
dec_huffman(<<2#1111111111111111111111110000:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 19>>);
dec_huffman(<<2#1111111111111111111111110001:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 20>>);
dec_huffman(<<2#1111111111111111111111110010:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 21>>);
dec_huffman(<<2#1111111111111111111111110011:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 23>>);
dec_huffman(<<2#1111111111111111111111110100:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 24>>);
dec_huffman(<<2#1111111111111111111111110101:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 25>>);
dec_huffman(<<2#1111111111111111111111110110:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 26>>);
dec_huffman(<<2#1111111111111111111111110111:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 27>>);
dec_huffman(<<2#1111111111111111111111111000:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 28>>);
dec_huffman(<<2#1111111111111111111111111001:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 29>>);
dec_huffman(<<2#1111111111111111111111111010:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 30>>);
dec_huffman(<<2#1111111111111111111111111011:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 31>>);
dec_huffman(<<2#1111111111111111111111111100:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 127>>);
dec_huffman(<<2#1111111111111111111111111101:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 220>>);
dec_huffman(<<2#1111111111111111111111111110:28, R/bits>>, L, A) -> dec_huffman(R, L - 28, <<A/binary, 249>>);
dec_huffman(<<2#111111111111111111111111111100:30, R/bits>>, L, A) -> dec_huffman(R, L - 30, <<A/binary, 10>>);
dec_huffman(<<2#111111111111111111111111111101:30, R/bits>>, L, A) -> dec_huffman(R, L - 30, <<A/binary, 13>>);
dec_huffman(<<2#111111111111111111111111111110:30, R/bits>>, L, A) -> dec_huffman(R, L - 30, <<A/binary, 22>>).

-ifdef(TEST).
req_decode_test() ->
	%% First request (raw then huffman).
	{Headers1, State1} = decode(<< 16#828684410f7777772e6578616d706c652e636f6d:160 >>),
	{Headers1, State1} = decode(<< 16#828684418cf1e3c2e5f23a6ba0ab90f4ff:136 >>),
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	#state{size=57, dyn_table=[{57,{<<":authority">>, <<"www.example.com">>}}]} = State1,
	%% Second request (raw then huffman).
	{Headers2, State2} = decode(<< 16#828684be58086e6f2d6361636865:112 >>, State1),
	{Headers2, State2} = decode(<< 16#828684be5886a8eb10649cbf:96 >>, State1),
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	#state{size=110, dyn_table=[
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State2,
	%% Third request (raw then huffman).
	{Headers3, State3} = decode(<< 16#828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565:232 >>, State2),
	{Headers3, State3} = decode(<< 16#828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf:192 >>, State2),
	Headers3 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/index.html">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"custom-key">>, <<"custom-value">>}
	],
	#state{size=164, dyn_table=[
		{54,{<<"custom-key">>, <<"custom-value">>}},
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State3,
	ok.

resp_decode_test() ->
	%% Use a max_size of 256 to trigger header evictions.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Second response (raw then huffman).
	{Headers2, State2} = decode(<< 16#4803333037c1c0bf:64 >>, State1),
	{Headers2, State2} = decode(<< 16#4883640effc1c0bf:64 >>, State1),
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}}]} = State2,
	%% Third response (raw then huffman).
	{Headers3, State3} = decode(<< 16#88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31:784 >>, State2),
	{Headers3, State3} = decode(<< 16#88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007:632 >>, State2),
	Headers3 = [
		{<<":status">>, <<"200">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>},
		{<<"location">>, <<"https://www.example.com">>},
		{<<"content-encoding">>, <<"gzip">>},
		{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}
	],
	#state{size=215, dyn_table=[
		{98,{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}},
		{52,{<<"content-encoding">>, <<"gzip">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>}}]} = State3,
	ok.

table_update_decode_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update (raw then huffman).
	MaxSize = enc_big_int(512 - 31, []),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4803333037c1c0bf:64>>]),
		State2),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4883640effc1c0bf:64>>]),
		State2),
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=264, configured_max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State3,
	ok.

table_update_decode_smaller_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update smaller than the limit (raw then huffman).
	MaxSize = enc_big_int(400 - 31, []),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4803333037c1c0bf:64>>]),
		State2),
	{Headers2, State3} = decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4883640effc1c0bf:64>>]),
		State2),
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=264, configured_max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State3,
	ok.

table_update_decode_too_large_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update (raw then huffman).
	MaxSize = enc_big_int(1024 - 31, []),
	{'EXIT', _} = (catch decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4803333037c1c0bf:64>>]),
		State2)),
	{'EXIT', _} = (catch decode(
		iolist_to_binary([<< 2#00111111>>, MaxSize, <<16#4883640effc1c0bf:64>>]),
		State2)),
	ok.

table_update_decode_zero_test() ->
	State0 = init(256),
	%% First response (raw then huffman).
	{Headers1, State1} = decode(<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >>, State0),
	{Headers1, State1} = decode(<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >>, State0),
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Set a new configured max_size to avoid header evictions.
	State2 = set_max_size(512, State1),
	%% Second response with the table size update (raw then huffman).
	%% We set the table size to 0 to evict all values before setting
	%% it to 512 so we only get the second request indexed.
	MaxSize = enc_big_int(512 - 31, []),
	{Headers1, State3} = decode(iolist_to_binary([
		<<2#00100000, 2#00111111>>, MaxSize,
		<<16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560>>]),
		State2),
	{Headers1, State3} = decode(iolist_to_binary([
		<<2#00100000, 2#00111111>>, MaxSize,
		<<16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432>>]),
		State2),
	#state{size=222, configured_max_size=512, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State3,
	ok.
-endif.

%% Encoding.

-spec encode(cow_http:headers()) -> {iodata(), state()}.
encode(Headers) ->
	encode(Headers, init(), #{}, []).

-spec encode(cow_http:headers(), State) -> {iodata(), State} when State::state().
encode(Headers, State=#state{max_size=MaxSize, configured_max_size=MaxSize}) ->
	encode(Headers, State, #{}, []);
encode(Headers, State0=#state{configured_max_size=MaxSize}) ->
	{Data, State} = encode(Headers, State0#state{max_size=MaxSize}, #{}, []),
	{[enc_int5(MaxSize, 2#001), Data], State}.

-spec encode(cow_http:headers(), State, opts()) -> {iodata(), State} when State::state().
encode(Headers, State=#state{max_size=MaxSize, configured_max_size=MaxSize}, Opts) ->
	encode(Headers, State, Opts, []);
encode(Headers, State0=#state{configured_max_size=MaxSize}, Opts) ->
	{Data, State} = encode(Headers, State0#state{max_size=MaxSize}, Opts, []),
	{[enc_int5(MaxSize, 2#001), Data], State}.

%% @todo Handle cases where no/never indexing is expected.
encode([], State, _, Acc) ->
	{lists:reverse(Acc), State};
encode([_Header0 = {Name, Value0}|Tail], State, Opts, Acc) ->
	Value = iolist_to_binary(Value0),
	Header = {Name, Value},
	case table_find(Header, State) of
		%% Indexed header field representation.
		{field, Index} ->
			encode(Tail, State, Opts, [enc_int7(Index, 2#1)|Acc]);
		%% Literal header field representation: indexed name.
		{name, Index} ->
			State2 = table_insert(Header, State),
			encode(Tail, State2, Opts, [[enc_int6(Index, 2#01), enc_str(Value, Opts)]|Acc]);
		%% Literal header field representation: new name.
		not_found ->
			State2 = table_insert(Header, State),
			encode(Tail, State2, Opts, [[<< 0:1, 1:1, 0:6 >>, enc_str(Name, Opts), enc_str(Value, Opts)]|Acc])
	end.

%% Encode an integer.

enc_int5(Int, Prefix) when Int < 31 ->
	<< Prefix:3, Int:5 >>;
enc_int5(Int, Prefix) ->
	[<< Prefix:3, 2#11111:5 >>|enc_big_int(Int - 31, [])].

enc_int6(Int, Prefix) when Int < 63 ->
	<< Prefix:2, Int:6 >>;
enc_int6(Int, Prefix) ->
	[<< Prefix:2, 2#111111:6 >>|enc_big_int(Int - 63, [])].

enc_int7(Int, Prefix) when Int < 127 ->
	<< Prefix:1, Int:7 >>;
enc_int7(Int, Prefix) ->
	[<< Prefix:1, 2#1111111:7 >>|enc_big_int(Int - 127, [])].

enc_big_int(Int, Acc) when Int < 128 ->
	lists:reverse([<< Int:8 >>|Acc]);
enc_big_int(Int, Acc) ->
	enc_big_int(Int bsr 7, [<< 1:1, Int:7 >>|Acc]).

%% Encode a string.

enc_str(Str, Opts) ->
	case maps:get(huffman, Opts, true) of
		true ->
			Str2 = enc_huffman(Str, <<>>),
			[enc_int7(byte_size(Str2), 2#1), Str2];
		false ->
			[enc_int7(iolist_size(Str), 2#0), Str]
	end.

enc_huffman(<<>>, Acc) ->
	case bit_size(Acc) rem 8 of
		1 -> << Acc/bits, 2#1111111:7 >>;
		2 -> << Acc/bits, 2#111111:6 >>;
		3 -> << Acc/bits, 2#11111:5 >>;
		4 -> << Acc/bits, 2#1111:4 >>;
		5 -> << Acc/bits, 2#111:3 >>;
		6 -> << Acc/bits, 2#11:2 >>;
		7 -> << Acc/bits, 2#1:1 >>;
		0 -> Acc
	end;
enc_huffman(<< 0, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111000:13 >>);
enc_huffman(<< 1, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011000:23 >>);
enc_huffman(<< 2, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100010:28 >>);
enc_huffman(<< 3, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100011:28 >>);
enc_huffman(<< 4, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100100:28 >>);
enc_huffman(<< 5, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100101:28 >>);
enc_huffman(<< 6, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100110:28 >>);
enc_huffman(<< 7, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100111:28 >>);
enc_huffman(<< 8, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101000:28 >>);
enc_huffman(<< 9, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101010:24 >>);
enc_huffman(<< 10, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111111111100:30 >>);
enc_huffman(<< 11, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101001:28 >>);
enc_huffman(<< 12, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101010:28 >>);
enc_huffman(<< 13, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111111111101:30 >>);
enc_huffman(<< 14, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101011:28 >>);
enc_huffman(<< 15, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101100:28 >>);
enc_huffman(<< 16, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101101:28 >>);
enc_huffman(<< 17, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101110:28 >>);
enc_huffman(<< 18, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101111:28 >>);
enc_huffman(<< 19, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110000:28 >>);
enc_huffman(<< 20, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110001:28 >>);
enc_huffman(<< 21, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110010:28 >>);
enc_huffman(<< 22, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111111111110:30 >>);
enc_huffman(<< 23, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110011:28 >>);
enc_huffman(<< 24, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110100:28 >>);
enc_huffman(<< 25, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110101:28 >>);
enc_huffman(<< 26, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110110:28 >>);
enc_huffman(<< 27, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110111:28 >>);
enc_huffman(<< 28, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111000:28 >>);
enc_huffman(<< 29, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111001:28 >>);
enc_huffman(<< 30, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111010:28 >>);
enc_huffman(<< 31, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111011:28 >>);
enc_huffman(<< 32, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010100:6 >>);
enc_huffman(<< 33, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111000:10 >>);
enc_huffman(<< 34, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111001:10 >>);
enc_huffman(<< 35, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111010:12 >>);
enc_huffman(<< 36, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111001:13 >>);
enc_huffman(<< 37, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010101:6 >>);
enc_huffman(<< 38, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111000:8 >>);
enc_huffman(<< 39, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111010:11 >>);
enc_huffman(<< 40, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111010:10 >>);
enc_huffman(<< 41, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111011:10 >>);
enc_huffman(<< 42, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111001:8 >>);
enc_huffman(<< 43, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111011:11 >>);
enc_huffman(<< 44, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111010:8 >>);
enc_huffman(<< 45, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010110:6 >>);
enc_huffman(<< 46, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010111:6 >>);
enc_huffman(<< 47, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011000:6 >>);
enc_huffman(<< 48, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00000:5 >>);
enc_huffman(<< 49, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00001:5 >>);
enc_huffman(<< 50, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00010:5 >>);
enc_huffman(<< 51, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011001:6 >>);
enc_huffman(<< 52, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011010:6 >>);
enc_huffman(<< 53, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011011:6 >>);
enc_huffman(<< 54, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011100:6 >>);
enc_huffman(<< 55, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011101:6 >>);
enc_huffman(<< 56, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011110:6 >>);
enc_huffman(<< 57, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011111:6 >>);
enc_huffman(<< 58, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011100:7 >>);
enc_huffman(<< 59, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111011:8 >>);
enc_huffman(<< 60, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111100:15 >>);
enc_huffman(<< 61, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100000:6 >>);
enc_huffman(<< 62, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111011:12 >>);
enc_huffman(<< 63, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111100:10 >>);
enc_huffman(<< 64, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111010:13 >>);
enc_huffman(<< 65, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100001:6 >>);
enc_huffman(<< 66, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011101:7 >>);
enc_huffman(<< 67, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011110:7 >>);
enc_huffman(<< 68, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011111:7 >>);
enc_huffman(<< 69, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100000:7 >>);
enc_huffman(<< 70, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100001:7 >>);
enc_huffman(<< 71, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100010:7 >>);
enc_huffman(<< 72, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100011:7 >>);
enc_huffman(<< 73, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100100:7 >>);
enc_huffman(<< 74, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100101:7 >>);
enc_huffman(<< 75, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100110:7 >>);
enc_huffman(<< 76, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100111:7 >>);
enc_huffman(<< 77, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101000:7 >>);
enc_huffman(<< 78, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101001:7 >>);
enc_huffman(<< 79, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101010:7 >>);
enc_huffman(<< 80, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101011:7 >>);
enc_huffman(<< 81, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101100:7 >>);
enc_huffman(<< 82, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101101:7 >>);
enc_huffman(<< 83, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101110:7 >>);
enc_huffman(<< 84, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101111:7 >>);
enc_huffman(<< 85, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110000:7 >>);
enc_huffman(<< 86, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110001:7 >>);
enc_huffman(<< 87, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110010:7 >>);
enc_huffman(<< 88, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111100:8 >>);
enc_huffman(<< 89, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110011:7 >>);
enc_huffman(<< 90, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111101:8 >>);
enc_huffman(<< 91, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111011:13 >>);
enc_huffman(<< 92, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111110000:19 >>);
enc_huffman(<< 93, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111100:13 >>);
enc_huffman(<< 94, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111100:14 >>);
enc_huffman(<< 95, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100010:6 >>);
enc_huffman(<< 96, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111101:15 >>);
enc_huffman(<< 97, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00011:5 >>);
enc_huffman(<< 98, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100011:6 >>);
enc_huffman(<< 99, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00100:5 >>);
enc_huffman(<< 100, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100100:6 >>);
enc_huffman(<< 101, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00101:5 >>);
enc_huffman(<< 102, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100101:6 >>);
enc_huffman(<< 103, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100110:6 >>);
enc_huffman(<< 104, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100111:6 >>);
enc_huffman(<< 105, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00110:5 >>);
enc_huffman(<< 106, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110100:7 >>);
enc_huffman(<< 107, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110101:7 >>);
enc_huffman(<< 108, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101000:6 >>);
enc_huffman(<< 109, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101001:6 >>);
enc_huffman(<< 110, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101010:6 >>);
enc_huffman(<< 111, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00111:5 >>);
enc_huffman(<< 112, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101011:6 >>);
enc_huffman(<< 113, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110110:7 >>);
enc_huffman(<< 114, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101100:6 >>);
enc_huffman(<< 115, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#01000:5 >>);
enc_huffman(<< 116, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#01001:5 >>);
enc_huffman(<< 117, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101101:6 >>);
enc_huffman(<< 118, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110111:7 >>);
enc_huffman(<< 119, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111000:7 >>);
enc_huffman(<< 120, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111001:7 >>);
enc_huffman(<< 121, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111010:7 >>);
enc_huffman(<< 122, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111011:7 >>);
enc_huffman(<< 123, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111110:15 >>);
enc_huffman(<< 124, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111100:11 >>);
enc_huffman(<< 125, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111101:14 >>);
enc_huffman(<< 126, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111101:13 >>);
enc_huffman(<< 127, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111100:28 >>);
enc_huffman(<< 128, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111100110:20 >>);
enc_huffman(<< 129, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010010:22 >>);
enc_huffman(<< 130, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111100111:20 >>);
enc_huffman(<< 131, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101000:20 >>);
enc_huffman(<< 132, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010011:22 >>);
enc_huffman(<< 133, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010100:22 >>);
enc_huffman(<< 134, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010101:22 >>);
enc_huffman(<< 135, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011001:23 >>);
enc_huffman(<< 136, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010110:22 >>);
enc_huffman(<< 137, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011010:23 >>);
enc_huffman(<< 138, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011011:23 >>);
enc_huffman(<< 139, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011100:23 >>);
enc_huffman(<< 140, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011101:23 >>);
enc_huffman(<< 141, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011110:23 >>);
enc_huffman(<< 142, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101011:24 >>);
enc_huffman(<< 143, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011111:23 >>);
enc_huffman(<< 144, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101100:24 >>);
enc_huffman(<< 145, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101101:24 >>);
enc_huffman(<< 146, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010111:22 >>);
enc_huffman(<< 147, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100000:23 >>);
enc_huffman(<< 148, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101110:24 >>);
enc_huffman(<< 149, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100001:23 >>);
enc_huffman(<< 150, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100010:23 >>);
enc_huffman(<< 151, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100011:23 >>);
enc_huffman(<< 152, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100100:23 >>);
enc_huffman(<< 153, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011100:21 >>);
enc_huffman(<< 154, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011000:22 >>);
enc_huffman(<< 155, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100101:23 >>);
enc_huffman(<< 156, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011001:22 >>);
enc_huffman(<< 157, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100110:23 >>);
enc_huffman(<< 158, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100111:23 >>);
enc_huffman(<< 159, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101111:24 >>);
enc_huffman(<< 160, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011010:22 >>);
enc_huffman(<< 161, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011101:21 >>);
enc_huffman(<< 162, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101001:20 >>);
enc_huffman(<< 163, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011011:22 >>);
enc_huffman(<< 164, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011100:22 >>);
enc_huffman(<< 165, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101000:23 >>);
enc_huffman(<< 166, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101001:23 >>);
enc_huffman(<< 167, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011110:21 >>);
enc_huffman(<< 168, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101010:23 >>);
enc_huffman(<< 169, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011101:22 >>);
enc_huffman(<< 170, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011110:22 >>);
enc_huffman(<< 171, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110000:24 >>);
enc_huffman(<< 172, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011111:21 >>);
enc_huffman(<< 173, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011111:22 >>);
enc_huffman(<< 174, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101011:23 >>);
enc_huffman(<< 175, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101100:23 >>);
enc_huffman(<< 176, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100000:21 >>);
enc_huffman(<< 177, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100001:21 >>);
enc_huffman(<< 178, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100000:22 >>);
enc_huffman(<< 179, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100010:21 >>);
enc_huffman(<< 180, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101101:23 >>);
enc_huffman(<< 181, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100001:22 >>);
enc_huffman(<< 182, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101110:23 >>);
enc_huffman(<< 183, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101111:23 >>);
enc_huffman(<< 184, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101010:20 >>);
enc_huffman(<< 185, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100010:22 >>);
enc_huffman(<< 186, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100011:22 >>);
enc_huffman(<< 187, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100100:22 >>);
enc_huffman(<< 188, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110000:23 >>);
enc_huffman(<< 189, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100101:22 >>);
enc_huffman(<< 190, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100110:22 >>);
enc_huffman(<< 191, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110001:23 >>);
enc_huffman(<< 192, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100000:26 >>);
enc_huffman(<< 193, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100001:26 >>);
enc_huffman(<< 194, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101011:20 >>);
enc_huffman(<< 195, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111110001:19 >>);
enc_huffman(<< 196, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100111:22 >>);
enc_huffman(<< 197, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110010:23 >>);
enc_huffman(<< 198, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101000:22 >>);
enc_huffman(<< 199, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101100:25 >>);
enc_huffman(<< 200, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100010:26 >>);
enc_huffman(<< 201, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100011:26 >>);
enc_huffman(<< 202, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100100:26 >>);
enc_huffman(<< 203, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111011110:27 >>);
enc_huffman(<< 204, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111011111:27 >>);
enc_huffman(<< 205, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100101:26 >>);
enc_huffman(<< 206, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110001:24 >>);
enc_huffman(<< 207, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101101:25 >>);
enc_huffman(<< 208, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111110010:19 >>);
enc_huffman(<< 209, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100011:21 >>);
enc_huffman(<< 210, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100110:26 >>);
enc_huffman(<< 211, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100000:27 >>);
enc_huffman(<< 212, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100001:27 >>);
enc_huffman(<< 213, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100111:26 >>);
enc_huffman(<< 214, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100010:27 >>);
enc_huffman(<< 215, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110010:24 >>);
enc_huffman(<< 216, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100100:21 >>);
enc_huffman(<< 217, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100101:21 >>);
enc_huffman(<< 218, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101000:26 >>);
enc_huffman(<< 219, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101001:26 >>);
enc_huffman(<< 220, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111101:28 >>);
enc_huffman(<< 221, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100011:27 >>);
enc_huffman(<< 222, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100100:27 >>);
enc_huffman(<< 223, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100101:27 >>);
enc_huffman(<< 224, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101100:20 >>);
enc_huffman(<< 225, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110011:24 >>);
enc_huffman(<< 226, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101101:20 >>);
enc_huffman(<< 227, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100110:21 >>);
enc_huffman(<< 228, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101001:22 >>);
enc_huffman(<< 229, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100111:21 >>);
enc_huffman(<< 230, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111101000:21 >>);
enc_huffman(<< 231, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110011:23 >>);
enc_huffman(<< 232, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101010:22 >>);
enc_huffman(<< 233, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101011:22 >>);
enc_huffman(<< 234, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101110:25 >>);
enc_huffman(<< 235, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101111:25 >>);
enc_huffman(<< 236, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110100:24 >>);
enc_huffman(<< 237, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110101:24 >>);
enc_huffman(<< 238, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101010:26 >>);
enc_huffman(<< 239, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110100:23 >>);
enc_huffman(<< 240, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101011:26 >>);
enc_huffman(<< 241, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100110:27 >>);
enc_huffman(<< 242, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101100:26 >>);
enc_huffman(<< 243, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101101:26 >>);
enc_huffman(<< 244, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100111:27 >>);
enc_huffman(<< 245, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101000:27 >>);
enc_huffman(<< 246, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101001:27 >>);
enc_huffman(<< 247, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101010:27 >>);
enc_huffman(<< 248, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101011:27 >>);
enc_huffman(<< 249, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111110:28 >>);
enc_huffman(<< 250, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101100:27 >>);
enc_huffman(<< 251, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101101:27 >>);
enc_huffman(<< 252, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101110:27 >>);
enc_huffman(<< 253, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101111:27 >>);
enc_huffman(<< 254, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111110000:27 >>);
enc_huffman(<< 255, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101110:26 >>).

-ifdef(TEST).
req_encode_test() ->
	%% First request (raw then huffman).
	Headers1 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>}
	],
	{Raw1, State1} = encode(Headers1, init(), #{huffman => false}),
	<< 16#828684410f7777772e6578616d706c652e636f6d:160 >> = iolist_to_binary(Raw1),
	{Huff1, State1} = encode(Headers1),
	<< 16#828684418cf1e3c2e5f23a6ba0ab90f4ff:136 >> = iolist_to_binary(Huff1),
	#state{size=57, dyn_table=[{57,{<<":authority">>, <<"www.example.com">>}}]} = State1,
	%% Second request (raw then huffman).
	Headers2 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"cache-control">>, <<"no-cache">>}
	],
	{Raw2, State2} = encode(Headers2, State1, #{huffman => false}),
	<< 16#828684be58086e6f2d6361636865:112 >> = iolist_to_binary(Raw2),
	{Huff2, State2} = encode(Headers2, State1),
	<< 16#828684be5886a8eb10649cbf:96 >> = iolist_to_binary(Huff2),
	#state{size=110, dyn_table=[
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State2,
	%% Third request (raw then huffman).
	Headers3 = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/index.html">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"custom-key">>, <<"custom-value">>}
	],
	{Raw3, State3} = encode(Headers3, State2, #{huffman => false}),
	<< 16#828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565:232 >> = iolist_to_binary(Raw3),
	{Huff3, State3} = encode(Headers3, State2),
	<< 16#828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf:192 >> = iolist_to_binary(Huff3),
	#state{size=164, dyn_table=[
		{54,{<<"custom-key">>, <<"custom-value">>}},
		{53,{<<"cache-control">>, <<"no-cache">>}},
		{57,{<<":authority">>, <<"www.example.com">>}}]} = State3,
	ok.

resp_encode_test() ->
	%% Use a max_size of 256 to trigger header evictions.
	State0 = init(256),
	%% First response (raw then huffman).
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Raw1, State1} = encode(Headers1, State0, #{huffman => false}),
	<< 16#4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d:560 >> = iolist_to_binary(Raw1),
	{Huff1, State1} = encode(Headers1, State0),
	<< 16#488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3:432 >> = iolist_to_binary(Huff1),
	#state{size=222, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = State1,
	%% Second response (raw then huffman).
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Raw2, State2} = encode(Headers2, State1, #{huffman => false}),
	<< 16#4803333037c1c0bf:64 >> = iolist_to_binary(Raw2),
	{Huff2, State2} = encode(Headers2, State1),
	<< 16#4883640effc1c0bf:64 >> = iolist_to_binary(Huff2),
	#state{size=222, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}}]} = State2,
	%% Third response (raw then huffman).
	Headers3 = [
		{<<":status">>, <<"200">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>},
		{<<"location">>, <<"https://www.example.com">>},
		{<<"content-encoding">>, <<"gzip">>},
		{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}
	],
	{Raw3, State3} = encode(Headers3, State2, #{huffman => false}),
	<< 16#88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31:784 >> = iolist_to_binary(Raw3),
	{Huff3, State3} = encode(Headers3, State2),
	<< 16#88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007:632 >> = iolist_to_binary(Huff3),
	#state{size=215, dyn_table=[
		{98,{<<"set-cookie">>, <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>}},
		{52,{<<"content-encoding">>, <<"gzip">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>}}]} = State3,
	ok.

%% This test assumes that table updates work correctly when decoding.
table_update_encode_test() ->
	%% Use a max_size of 256 to trigger header evictions
	%% when the code is not updating the max size.
	DecState0 = EncState0 = init(256),
	%% First response.
	Headers1 = [
		{<<":status">>, <<"302">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Encoded1, EncState1} = encode(Headers1, EncState0),
	{Headers1, DecState1} = decode(iolist_to_binary(Encoded1), DecState0),
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = DecState1,
	#state{size=222, configured_max_size=256, dyn_table=[
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = EncState1,
	%% Set a new configured max_size to avoid header evictions.
	DecState2 = set_max_size(512, DecState1),
	EncState2 = set_max_size(512, EncState1),
	%% Second response.
	Headers2 = [
		{<<":status">>, <<"307">>},
		{<<"cache-control">>, <<"private">>},
		{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
		{<<"location">>, <<"https://www.example.com">>}
	],
	{Encoded2, EncState3} = encode(Headers2, EncState2),
	{Headers2, DecState3} = decode(iolist_to_binary(Encoded2), DecState2),
	#state{size=264, max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = DecState3,
	#state{size=264, max_size=512, dyn_table=[
		{42,{<<":status">>, <<"307">>}},
		{63,{<<"location">>, <<"https://www.example.com">>}},
		{65,{<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>}},
		{52,{<<"cache-control">>, <<"private">>}},
		{42,{<<":status">>, <<"302">>}}]} = EncState3,
	ok.

encode_iolist_test() ->
	Headers = [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>},
		{<<":authority">>, <<"www.example.com">>},
		{<<"content-type">>, [<<"image">>,<<"/">>,<<"png">>,<<>>]}
	],
	{_, _} = encode(Headers),
	ok.
-endif.

%% Static and dynamic tables.

%% @todo There must be a more efficient way.
table_find(Header = {Name, _}, State) ->
	case table_find_field(Header, State) of
		not_found ->
			case table_find_name(Name, State) of
				NotFound = not_found ->
					NotFound;
				Found ->
					{name, Found}
			end;
		Found ->
			{field, Found}
	end.

table_find_field({<<":authority">>, <<>>}, _) -> 1;
table_find_field({<<":method">>, <<"GET">>}, _) -> 2;
table_find_field({<<":method">>, <<"POST">>}, _) -> 3;
table_find_field({<<":path">>, <<"/">>}, _) -> 4;
table_find_field({<<":path">>, <<"/index.html">>}, _) -> 5;
table_find_field({<<":scheme">>, <<"http">>}, _) -> 6;
table_find_field({<<":scheme">>, <<"https">>}, _) -> 7;
table_find_field({<<":status">>, <<"200">>}, _) -> 8;
table_find_field({<<":status">>, <<"204">>}, _) -> 9;
table_find_field({<<":status">>, <<"206">>}, _) -> 10;
table_find_field({<<":status">>, <<"304">>}, _) -> 11;
table_find_field({<<":status">>, <<"400">>}, _) -> 12;
table_find_field({<<":status">>, <<"404">>}, _) -> 13;
table_find_field({<<":status">>, <<"500">>}, _) -> 14;
table_find_field({<<"accept-charset">>, <<>>}, _) -> 15;
table_find_field({<<"accept-encoding">>, <<"gzip, deflate">>}, _) -> 16;
table_find_field({<<"accept-language">>, <<>>}, _) -> 17;
table_find_field({<<"accept-ranges">>, <<>>}, _) -> 18;
table_find_field({<<"accept">>, <<>>}, _) -> 19;
table_find_field({<<"access-control-allow-origin">>, <<>>}, _) -> 20;
table_find_field({<<"age">>, <<>>}, _) -> 21;
table_find_field({<<"allow">>, <<>>}, _) -> 22;
table_find_field({<<"authorization">>, <<>>}, _) -> 23;
table_find_field({<<"cache-control">>, <<>>}, _) -> 24;
table_find_field({<<"content-disposition">>, <<>>}, _) -> 25;
table_find_field({<<"content-encoding">>, <<>>}, _) -> 26;
table_find_field({<<"content-language">>, <<>>}, _) -> 27;
table_find_field({<<"content-length">>, <<>>}, _) -> 28;
table_find_field({<<"content-location">>, <<>>}, _) -> 29;
table_find_field({<<"content-range">>, <<>>}, _) -> 30;
table_find_field({<<"content-type">>, <<>>}, _) -> 31;
table_find_field({<<"cookie">>, <<>>}, _) -> 32;
table_find_field({<<"date">>, <<>>}, _) -> 33;
table_find_field({<<"etag">>, <<>>}, _) -> 34;
table_find_field({<<"expect">>, <<>>}, _) -> 35;
table_find_field({<<"expires">>, <<>>}, _) -> 36;
table_find_field({<<"from">>, <<>>}, _) -> 37;
table_find_field({<<"host">>, <<>>}, _) -> 38;
table_find_field({<<"if-match">>, <<>>}, _) -> 39;
table_find_field({<<"if-modified-since">>, <<>>}, _) -> 40;
table_find_field({<<"if-none-match">>, <<>>}, _) -> 41;
table_find_field({<<"if-range">>, <<>>}, _) -> 42;
table_find_field({<<"if-unmodified-since">>, <<>>}, _) -> 43;
table_find_field({<<"last-modified">>, <<>>}, _) -> 44;
table_find_field({<<"link">>, <<>>}, _) -> 45;
table_find_field({<<"location">>, <<>>}, _) -> 46;
table_find_field({<<"max-forwards">>, <<>>}, _) -> 47;
table_find_field({<<"proxy-authenticate">>, <<>>}, _) -> 48;
table_find_field({<<"proxy-authorization">>, <<>>}, _) -> 49;
table_find_field({<<"range">>, <<>>}, _) -> 50;
table_find_field({<<"referer">>, <<>>}, _) -> 51;
table_find_field({<<"refresh">>, <<>>}, _) -> 52;
table_find_field({<<"retry-after">>, <<>>}, _) -> 53;
table_find_field({<<"server">>, <<>>}, _) -> 54;
table_find_field({<<"set-cookie">>, <<>>}, _) -> 55;
table_find_field({<<"strict-transport-security">>, <<>>}, _) -> 56;
table_find_field({<<"transfer-encoding">>, <<>>}, _) -> 57;
table_find_field({<<"user-agent">>, <<>>}, _) -> 58;
table_find_field({<<"vary">>, <<>>}, _) -> 59;
table_find_field({<<"via">>, <<>>}, _) -> 60;
table_find_field({<<"www-authenticate">>, <<>>}, _) -> 61;
table_find_field(Header, #state{dyn_table=DynamicTable}) ->
	table_find_field_dyn(Header, DynamicTable, 62).

table_find_field_dyn(_, [], _) -> not_found;
table_find_field_dyn(Header, [{_, Header}|_], Index) -> Index;
table_find_field_dyn(Header, [_|Tail], Index) -> table_find_field_dyn(Header, Tail, Index + 1).

table_find_name(<<":authority">>, _) -> 1;
table_find_name(<<":method">>, _) -> 2;
table_find_name(<<":path">>, _) -> 4;
table_find_name(<<":scheme">>, _) -> 6;
table_find_name(<<":status">>, _) -> 8;
table_find_name(<<"accept-charset">>, _) -> 15;
table_find_name(<<"accept-encoding">>, _) -> 16;
table_find_name(<<"accept-language">>, _) -> 17;
table_find_name(<<"accept-ranges">>, _) -> 18;
table_find_name(<<"accept">>, _) -> 19;
table_find_name(<<"access-control-allow-origin">>, _) -> 20;
table_find_name(<<"age">>, _) -> 21;
table_find_name(<<"allow">>, _) -> 22;
table_find_name(<<"authorization">>, _) -> 23;
table_find_name(<<"cache-control">>, _) -> 24;
table_find_name(<<"content-disposition">>, _) -> 25;
table_find_name(<<"content-encoding">>, _) -> 26;
table_find_name(<<"content-language">>, _) -> 27;
table_find_name(<<"content-length">>, _) -> 28;
table_find_name(<<"content-location">>, _) -> 29;
table_find_name(<<"content-range">>, _) -> 30;
table_find_name(<<"content-type">>, _) -> 31;
table_find_name(<<"cookie">>, _) -> 32;
table_find_name(<<"date">>, _) -> 33;
table_find_name(<<"etag">>, _) -> 34;
table_find_name(<<"expect">>, _) -> 35;
table_find_name(<<"expires">>, _) -> 36;
table_find_name(<<"from">>, _) -> 37;
table_find_name(<<"host">>, _) -> 38;
table_find_name(<<"if-match">>, _) -> 39;
table_find_name(<<"if-modified-since">>, _) -> 40;
table_find_name(<<"if-none-match">>, _) -> 41;
table_find_name(<<"if-range">>, _) -> 42;
table_find_name(<<"if-unmodified-since">>, _) -> 43;
table_find_name(<<"last-modified">>, _) -> 44;
table_find_name(<<"link">>, _) -> 45;
table_find_name(<<"location">>, _) -> 46;
table_find_name(<<"max-forwards">>, _) -> 47;
table_find_name(<<"proxy-authenticate">>, _) -> 48;
table_find_name(<<"proxy-authorization">>, _) -> 49;
table_find_name(<<"range">>, _) -> 50;
table_find_name(<<"referer">>, _) -> 51;
table_find_name(<<"refresh">>, _) -> 52;
table_find_name(<<"retry-after">>, _) -> 53;
table_find_name(<<"server">>, _) -> 54;
table_find_name(<<"set-cookie">>, _) -> 55;
table_find_name(<<"strict-transport-security">>, _) -> 56;
table_find_name(<<"transfer-encoding">>, _) -> 57;
table_find_name(<<"user-agent">>, _) -> 58;
table_find_name(<<"vary">>, _) -> 59;
table_find_name(<<"via">>, _) -> 60;
table_find_name(<<"www-authenticate">>, _) -> 61;
table_find_name(Name, #state{dyn_table=DynamicTable}) ->
	table_find_name_dyn(Name, DynamicTable, 62).

table_find_name_dyn(_, [], _) -> not_found;
table_find_name_dyn(Name, [{Name, _}|_], Index) -> Index;
table_find_name_dyn(Name, [_|Tail], Index) -> table_find_name_dyn(Name, Tail, Index + 1).

table_get(1, _) -> {<<":authority">>, <<>>};
table_get(2, _) -> {<<":method">>, <<"GET">>};
table_get(3, _) -> {<<":method">>, <<"POST">>};
table_get(4, _) -> {<<":path">>, <<"/">>};
table_get(5, _) -> {<<":path">>, <<"/index.html">>};
table_get(6, _) -> {<<":scheme">>, <<"http">>};
table_get(7, _) -> {<<":scheme">>, <<"https">>};
table_get(8, _) -> {<<":status">>, <<"200">>};
table_get(9, _) -> {<<":status">>, <<"204">>};
table_get(10, _) -> {<<":status">>, <<"206">>};
table_get(11, _) -> {<<":status">>, <<"304">>};
table_get(12, _) -> {<<":status">>, <<"400">>};
table_get(13, _) -> {<<":status">>, <<"404">>};
table_get(14, _) -> {<<":status">>, <<"500">>};
table_get(15, _) -> {<<"accept-charset">>, <<>>};
table_get(16, _) -> {<<"accept-encoding">>, <<"gzip, deflate">>};
table_get(17, _) -> {<<"accept-language">>, <<>>};
table_get(18, _) -> {<<"accept-ranges">>, <<>>};
table_get(19, _) -> {<<"accept">>, <<>>};
table_get(20, _) -> {<<"access-control-allow-origin">>, <<>>};
table_get(21, _) -> {<<"age">>, <<>>};
table_get(22, _) -> {<<"allow">>, <<>>};
table_get(23, _) -> {<<"authorization">>, <<>>};
table_get(24, _) -> {<<"cache-control">>, <<>>};
table_get(25, _) -> {<<"content-disposition">>, <<>>};
table_get(26, _) -> {<<"content-encoding">>, <<>>};
table_get(27, _) -> {<<"content-language">>, <<>>};
table_get(28, _) -> {<<"content-length">>, <<>>};
table_get(29, _) -> {<<"content-location">>, <<>>};
table_get(30, _) -> {<<"content-range">>, <<>>};
table_get(31, _) -> {<<"content-type">>, <<>>};
table_get(32, _) -> {<<"cookie">>, <<>>};
table_get(33, _) -> {<<"date">>, <<>>};
table_get(34, _) -> {<<"etag">>, <<>>};
table_get(35, _) -> {<<"expect">>, <<>>};
table_get(36, _) -> {<<"expires">>, <<>>};
table_get(37, _) -> {<<"from">>, <<>>};
table_get(38, _) -> {<<"host">>, <<>>};
table_get(39, _) -> {<<"if-match">>, <<>>};
table_get(40, _) -> {<<"if-modified-since">>, <<>>};
table_get(41, _) -> {<<"if-none-match">>, <<>>};
table_get(42, _) -> {<<"if-range">>, <<>>};
table_get(43, _) -> {<<"if-unmodified-since">>, <<>>};
table_get(44, _) -> {<<"last-modified">>, <<>>};
table_get(45, _) -> {<<"link">>, <<>>};
table_get(46, _) -> {<<"location">>, <<>>};
table_get(47, _) -> {<<"max-forwards">>, <<>>};
table_get(48, _) -> {<<"proxy-authenticate">>, <<>>};
table_get(49, _) -> {<<"proxy-authorization">>, <<>>};
table_get(50, _) -> {<<"range">>, <<>>};
table_get(51, _) -> {<<"referer">>, <<>>};
table_get(52, _) -> {<<"refresh">>, <<>>};
table_get(53, _) -> {<<"retry-after">>, <<>>};
table_get(54, _) -> {<<"server">>, <<>>};
table_get(55, _) -> {<<"set-cookie">>, <<>>};
table_get(56, _) -> {<<"strict-transport-security">>, <<>>};
table_get(57, _) -> {<<"transfer-encoding">>, <<>>};
table_get(58, _) -> {<<"user-agent">>, <<>>};
table_get(59, _) -> {<<"vary">>, <<>>};
table_get(60, _) -> {<<"via">>, <<>>};
table_get(61, _) -> {<<"www-authenticate">>, <<>>};
table_get(Index, #state{dyn_table=DynamicTable}) ->
	{_, Header} = lists:nth(Index - 61, DynamicTable),
	Header.

table_get_name(1, _) -> <<":authority">>;
table_get_name(2, _) -> <<":method">>;
table_get_name(3, _) -> <<":method">>;
table_get_name(4, _) -> <<":path">>;
table_get_name(5, _) -> <<":path">>;
table_get_name(6, _) -> <<":scheme">>;
table_get_name(7, _) -> <<":scheme">>;
table_get_name(8, _) -> <<":status">>;
table_get_name(9, _) -> <<":status">>;
table_get_name(10, _) -> <<":status">>;
table_get_name(11, _) -> <<":status">>;
table_get_name(12, _) -> <<":status">>;
table_get_name(13, _) -> <<":status">>;
table_get_name(14, _) -> <<":status">>;
table_get_name(15, _) -> <<"accept-charset">>;
table_get_name(16, _) -> <<"accept-encoding">>;
table_get_name(17, _) -> <<"accept-language">>;
table_get_name(18, _) -> <<"accept-ranges">>;
table_get_name(19, _) -> <<"accept">>;
table_get_name(20, _) -> <<"access-control-allow-origin">>;
table_get_name(21, _) -> <<"age">>;
table_get_name(22, _) -> <<"allow">>;
table_get_name(23, _) -> <<"authorization">>;
table_get_name(24, _) -> <<"cache-control">>;
table_get_name(25, _) -> <<"content-disposition">>;
table_get_name(26, _) -> <<"content-encoding">>;
table_get_name(27, _) -> <<"content-language">>;
table_get_name(28, _) -> <<"content-length">>;
table_get_name(29, _) -> <<"content-location">>;
table_get_name(30, _) -> <<"content-range">>;
table_get_name(31, _) -> <<"content-type">>;
table_get_name(32, _) -> <<"cookie">>;
table_get_name(33, _) -> <<"date">>;
table_get_name(34, _) -> <<"etag">>;
table_get_name(35, _) -> <<"expect">>;
table_get_name(36, _) -> <<"expires">>;
table_get_name(37, _) -> <<"from">>;
table_get_name(38, _) -> <<"host">>;
table_get_name(39, _) -> <<"if-match">>;
table_get_name(40, _) -> <<"if-modified-since">>;
table_get_name(41, _) -> <<"if-none-match">>;
table_get_name(42, _) -> <<"if-range">>;
table_get_name(43, _) -> <<"if-unmodified-since">>;
table_get_name(44, _) -> <<"last-modified">>;
table_get_name(45, _) -> <<"link">>;
table_get_name(46, _) -> <<"location">>;
table_get_name(47, _) -> <<"max-forwards">>;
table_get_name(48, _) -> <<"proxy-authenticate">>;
table_get_name(49, _) -> <<"proxy-authorization">>;
table_get_name(50, _) -> <<"range">>;
table_get_name(51, _) -> <<"referer">>;
table_get_name(52, _) -> <<"refresh">>;
table_get_name(53, _) -> <<"retry-after">>;
table_get_name(54, _) -> <<"server">>;
table_get_name(55, _) -> <<"set-cookie">>;
table_get_name(56, _) -> <<"strict-transport-security">>;
table_get_name(57, _) -> <<"transfer-encoding">>;
table_get_name(58, _) -> <<"user-agent">>;
table_get_name(59, _) -> <<"vary">>;
table_get_name(60, _) -> <<"via">>;
table_get_name(61, _) -> <<"www-authenticate">>;
table_get_name(Index, #state{dyn_table=DynamicTable}) ->
	{_, {Name, _}} = lists:nth(Index - 61, DynamicTable),
	Name.

table_insert(Entry = {Name, Value}, State=#state{size=Size, max_size=MaxSize, dyn_table=DynamicTable}) ->
	EntrySize = byte_size(Name) + byte_size(Value) + 32,
	{DynamicTable2, Size2} = if
		Size + EntrySize > MaxSize ->
			table_resize(DynamicTable, MaxSize - EntrySize, 0, []);
		true ->
			{DynamicTable, Size}
	end,
	State#state{size=Size2 + EntrySize, dyn_table=[{EntrySize, Entry}|DynamicTable2]}.

table_resize([], _, Size, Acc) ->
	{lists:reverse(Acc), Size};
table_resize([{EntrySize, _}|_], MaxSize, Size, Acc) when Size + EntrySize > MaxSize ->
	{lists:reverse(Acc), Size};
table_resize([Entry = {EntrySize, _}|Tail], MaxSize, Size, Acc) ->
	table_resize(Tail, MaxSize, Size + EntrySize, [Entry|Acc]).

table_update_size(0, State) ->
	State#state{size=0, max_size=0, dyn_table=[]};
table_update_size(MaxSize, State=#state{max_size=MaxSize}) ->
	State;
table_update_size(MaxSize, State=#state{dyn_table=DynTable}) ->
	{DynTable2, Size} = table_resize(DynTable, MaxSize, 0, []),
	State#state{size=Size, max_size=MaxSize, dyn_table=DynTable2}.

-ifdef(TEST).
prop_str_raw() ->
	?FORALL(Str, binary(), begin
		{Str, <<>>} =:= dec_str(iolist_to_binary(enc_str(Str, #{huffman => false})))
	end).

prop_str_huffman() ->
	?FORALL(Str, binary(), begin
		{Str, <<>>} =:= dec_str(iolist_to_binary(enc_str(Str, #{huffman => true})))
	end).
-endif.
