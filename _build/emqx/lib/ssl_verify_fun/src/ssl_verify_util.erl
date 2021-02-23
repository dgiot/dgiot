-module(ssl_verify_util).

-export([bin_to_hexstr/1,
         hexstr_to_bin/1]).

-export_type([hexstr/0]).

-type hexstr() :: {string() | binary()}.

%%====================================================================
%% Public API
%%====================================================================

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
                  X <- binary_to_list(Bin)]).

hexstr_to_bin(S) when is_binary(S) ->
  hexstr_to_bin(binary_to_list(S));
hexstr_to_bin(S) when is_list(S) and (length(S) rem 2 =:= 0) ->
  hexstr_to_bin(S, []);
hexstr_to_bin(_) ->
  invalid.
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hexstr_to_bin(T, [V | Acc]).
