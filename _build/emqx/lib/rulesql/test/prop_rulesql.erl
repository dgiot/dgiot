-module(prop_rulesql).

-include_lib("proper/include/proper.hrl").

-include("sql_lex.hrl").

prop_keywords() ->
    ?FORALL(Key, identifier(),
            begin
                %io:format("---- key: ~p~n", [Key]),
                is_reserved(Key) == rulesql:is_reserved(Key)
            end).

prop_parse_sql_with_keywords() ->
    ?FORALL(Key, identifier(),
            begin
                Sql = select_key(Key),
                %io:format("---- Sql: ~p~n", [Sql]),
                case is_reserved(Key) of
                    false ->
                        {ok, _} = rulesql:parsetree(Sql),
                        true;
                    true ->
                        {parse_error, _} = rulesql:parsetree(Sql),
                        true
                end
            end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

identifier() ->
    CharSet = list(oneof([
            range(48, 57), %% [0-9]
            range(65, 90), %% [A-Z]
            range(97, 122), %% [a-z]
            $_,
            $$,
            $@,
            $~
          ])),
    Letter = oneof([
            range(65, 90), %% [A-Z]
            range(97, 122) %% [a-z]
          ]),
    ?LET({First, Chars}, {Letter, CharSet}, list_to_binary([First | Chars])).

is_reserved(Key) ->
    lists:member(string:uppercase(Key), ?RESERVED_KEYS).

select_key(Key) ->
    <<"select ", Key/binary, " from abc">>.
