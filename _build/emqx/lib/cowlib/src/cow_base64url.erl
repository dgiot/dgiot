%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

%% This module implements "base64url" following the algorithm
%% found in Appendix C of RFC7515. The option #{padding => false}
%% must be given to reproduce this variant exactly. The default
%% will leave the padding characters.
-module(cow_base64url).

-export([decode/1]).
-export([decode/2]).
-export([encode/1]).
-export([encode/2]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

decode(Enc) ->
	decode(Enc, #{}).

decode(Enc0, Opts) ->
	Enc1 = << << case C of
		$- -> $+;
		$_ -> $/;
		_ -> C
	end >> || << C >> <= Enc0 >>,
	Enc = case Opts of
		#{padding := false} ->
			case byte_size(Enc1) rem 4 of
				0 -> Enc1;
				2 -> << Enc1/binary, "==" >>;
				3 -> << Enc1/binary, "=" >>
			end;
		_ ->
			Enc1
	end,
	base64:decode(Enc).

encode(Dec) ->
	encode(Dec, #{}).

encode(Dec, Opts) ->
	encode(base64:encode(Dec), Opts, <<>>).

encode(<<$+, R/bits>>, Opts, Acc) -> encode(R, Opts, <<Acc/binary, $->>);
encode(<<$/, R/bits>>, Opts, Acc) -> encode(R, Opts, <<Acc/binary, $_>>);
encode(<<$=, _/bits>>, #{padding := false}, Acc) -> Acc;
encode(<<C, R/bits>>, Opts, Acc) -> encode(R, Opts, <<Acc/binary, C>>);
encode(<<>>, _, Acc) -> Acc.

-ifdef(TEST).

rfc7515_test() ->
	Dec = <<3,236,255,224,193>>,
	Enc = <<"A-z_4ME">>,
	Pad = <<"A-z_4ME=">>,
	Dec = decode(<<Enc/binary,$=>>),
	Dec = decode(Enc, #{padding => false}),
	Pad = encode(Dec),
	Enc = encode(Dec, #{padding => false}),
	ok.

prop_identity() ->
	?FORALL(B, binary(), B =:= decode(encode(B))).

prop_identity_no_padding() ->
	?FORALL(B, binary(), B =:= decode(encode(B, #{padding => false}), #{padding => false})).

-endif.
