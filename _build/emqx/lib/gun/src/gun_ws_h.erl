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

-module(gun_ws_h).

-export([init/4]).
-export([handle/2]).

-record(state, {
	reply_to :: pid(),
	stream_ref :: reference(),
	frag_buffer = <<>> :: binary()
}).

init(ReplyTo, StreamRef, _, _) ->
	#state{reply_to=ReplyTo, stream_ref=StreamRef}.

handle({fragment, nofin, _, Payload},
		State=#state{frag_buffer=SoFar}) ->
	State#state{frag_buffer= << SoFar/binary, Payload/binary >>};
handle({fragment, fin, Type, Payload},
		State=#state{reply_to=ReplyTo, stream_ref=StreamRef, frag_buffer=SoFar}) ->
	ReplyTo ! {gun_ws, self(), StreamRef, {Type, << SoFar/binary, Payload/binary >>}},
	State#state{frag_buffer= <<>>};
handle(Frame, State=#state{reply_to=ReplyTo, stream_ref=StreamRef}) ->
	ReplyTo ! {gun_ws, self(), StreamRef, Frame},
	State.
