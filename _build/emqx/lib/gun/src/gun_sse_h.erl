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

-module(gun_sse_h).
-behavior(gun_content_handler).

-export([init/5]).
-export([handle/3]).

-record(state, {
	reply_to :: pid(),
	stream_ref :: reference(),
	sse_state :: cow_sse:state()
}).

%% @todo In the future we want to allow different media types.
%% @todo For text/event-stream specifically, the parameters must be ignored.

-spec init(pid(), reference(), _, cow_http:headers(), _)
	-> {ok, #state{}} | disable.
init(ReplyTo, StreamRef, _, Headers, _) ->
	case lists:keyfind(<<"content-type">>, 1, Headers) of
		{_, <<"text/event-stream">>} ->
			{ok, #state{reply_to=ReplyTo, stream_ref=StreamRef,
				sse_state=cow_sse:init()}};
		_ ->
			disable
	end.

-spec handle(_, binary(), State) -> {done, State} when State::#state{}.
handle(IsFin, Data, State=#state{reply_to=ReplyTo, stream_ref=StreamRef, sse_state=SSE0}) ->
	case cow_sse:parse(Data, SSE0) of
		{event, Event, SSE} ->
			ReplyTo ! {gun_sse, self(), StreamRef, Event},
			handle(IsFin, <<>>, State#state{sse_state=SSE});
		{more, SSE} ->
			_ = case IsFin of
				fin ->
					ReplyTo ! {gun_sse, self(), StreamRef, fin};
				_ ->
					ok
			end,
			{done, State#state{sse_state=SSE}}
	end.
