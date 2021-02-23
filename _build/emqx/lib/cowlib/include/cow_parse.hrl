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

-ifndef(COW_PARSE_HRL).
-define(COW_PARSE_HRL, 1).

-define(IS_ALPHA(C),
	(C =:= $a) or (C =:= $b) or (C =:= $c) or (C =:= $d) or (C =:= $e) or
	(C =:= $f) or (C =:= $g) or (C =:= $h) or (C =:= $i) or (C =:= $j) or
	(C =:= $k) or (C =:= $l) or (C =:= $m) or (C =:= $n) or (C =:= $o) or
	(C =:= $p) or (C =:= $q) or (C =:= $r) or (C =:= $s) or (C =:= $t) or
	(C =:= $u) or (C =:= $v) or (C =:= $w) or (C =:= $x) or (C =:= $y) or
	(C =:= $z) or
	(C =:= $A) or (C =:= $B) or (C =:= $C) or (C =:= $D) or (C =:= $E) or
	(C =:= $F) or (C =:= $G) or (C =:= $H) or (C =:= $I) or (C =:= $J) or
	(C =:= $K) or (C =:= $L) or (C =:= $M) or (C =:= $N) or (C =:= $O) or
	(C =:= $P) or (C =:= $Q) or (C =:= $R) or (C =:= $S) or (C =:= $T) or
	(C =:= $U) or (C =:= $V) or (C =:= $W) or (C =:= $X) or (C =:= $Y) or
	(C =:= $Z)
).

-define(IS_ALPHANUM(C), ?IS_ALPHA(C) or ?IS_DIGIT(C)).
-define(IS_CHAR(C), C > 0, C < 128).

-define(IS_DIGIT(C),
	(C =:= $0) or (C =:= $1) or (C =:= $2) or (C =:= $3) or (C =:= $4) or
	(C =:= $5) or (C =:= $6) or (C =:= $7) or (C =:= $8) or (C =:= $9)).

-define(IS_ETAGC(C), C =:= 16#21; C >= 16#23, C =/= 16#7f).

-define(IS_HEX(C),
	?IS_DIGIT(C) or
	(C =:= $a) or (C =:= $b) or (C =:= $c) or
	(C =:= $d) or (C =:= $e) or (C =:= $f) or
	(C =:= $A) or (C =:= $B) or (C =:= $C) or
	(C =:= $D) or (C =:= $E) or (C =:= $F)).

-define(IS_LHEX(C),
	?IS_DIGIT(C) or
	(C =:= $a) or (C =:= $b) or (C =:= $c) or
	(C =:= $d) or (C =:= $e) or (C =:= $f)).

-define(IS_TOKEN(C),
	?IS_ALPHA(C) or ?IS_DIGIT(C) or
	(C =:= $!) or (C =:= $#) or (C =:= $$) or (C =:= $%) or (C =:= $&) or
	(C =:= $') or (C =:= $*) or (C =:= $+) or (C =:= $-) or (C =:= $.) or
	(C =:= $^) or (C =:= $_) or (C =:= $`) or (C =:= $|) or (C =:= $~)).

-define(IS_TOKEN68(C),
	?IS_ALPHA(C) or ?IS_DIGIT(C) or
	(C =:= $-) or (C =:= $.) or (C =:= $_) or
	(C =:= $~) or (C =:= $+) or (C =:= $/)).

-define(IS_URI_UNRESERVED(C),
	?IS_ALPHA(C) or ?IS_DIGIT(C) or
	(C =:= $-) or (C =:= $.) or (C =:= $_) or (C =:= $~)).

-define(IS_URI_SUB_DELIMS(C),
	(C =:= $!) or (C =:= $$) or (C =:= $&) or (C =:= $') or
	(C =:= $() or (C =:= $)) or (C =:= $*) or (C =:= $+) or
	(C =:= $,) or (C =:= $;) or (C =:= $=)).

-define(IS_VCHAR(C), C =:= $\t; C > 31, C < 127).
-define(IS_VCHAR_OBS(C), C =:= $\t; C > 31, C =/= 127).
-define(IS_WS(C), (C =:= $\s) or (C =:= $\t)).
-define(IS_WS_COMMA(C), ?IS_WS(C) or (C =:= $,)).

-endif.
