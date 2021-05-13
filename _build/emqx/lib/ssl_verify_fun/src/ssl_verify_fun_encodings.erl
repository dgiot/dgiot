%%% -*- erlang -*-
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2016 Ilya Khaprov <ilya.khaprov@publitechs.com>

-module(ssl_verify_fun_encodings).

-export([decode_dns_name/1,
         get_string/1]).

%%====================================================================
%% Public API
%%====================================================================

-spec decode_dns_name(term()) -> string().
decode_dns_name(DNSName) ->
  unicode:characters_to_list(DNSName).

-spec decode(term(), atom()) -> string().
decode(X, Charset) ->
  case unicode:characters_to_list(X, Charset) of
    Decoded when is_list(Decoded) -> Decoded;
    _ -> {error, invalid}
  end.

%% i'm not sure if bmpString is exactly utf16...
%% or if universalString is exactly utf32 or
%% the possible implications of mismatched chars
-spec get_string({atom(), term()}) -> {ok, string()} | {error, invalid}.
get_string({universalString, Str}) ->
  {ok, Str};
get_string({bmpString, Str}) ->
  {ok, Str};
get_string({utf8String, Str}) ->
  {ok, decode(Str, utf8)};
get_string({printableString, Str}) ->
  case is_printable_string(Str) of
    true ->
      {ok, Str};
    _ ->
      {error, invalid}
  end;
get_string({teletexString, Str}) ->
  ascii_only(Str);
get_string(_) ->
  {error, invalid}.

%%====================================================================
%% Private Parts
%%====================================================================

ascii_only(Str) ->
  case is_ascii(Str) of
    true ->
      {ok, Str};
    false ->
      {error, invalid}
  end.

is_printable_string(Str) ->
  lists:all(fun is_printable/1, Str).

is_printable(Ch) when Ch >= $a andalso Ch =< $z ->
  true;
is_printable(Ch) when Ch >= $A andalso Ch =< $Z ->
  true;
is_printable(Ch) when Ch >= $0 andalso Ch =< $9 ->
  true;
is_printable(Ch) when Ch >= $' andalso Ch =< $/ -> %% include $* to allow wildcards
  true;
is_printable($ ) ->
  true;
is_printable($:) ->
  true;
is_printable($=) ->
  true;
is_printable($?) ->
  true;
is_printable(_) ->
  false.

is_ascii(Str) ->
  lists:all(fun(C) -> C >= 16#20 andalso C =< 16#7E end, Str).
