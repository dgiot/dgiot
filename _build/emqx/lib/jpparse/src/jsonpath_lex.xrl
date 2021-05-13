%% -*- erlang -*-
Definitions.

Non_Quoted_Identifier = [A-Za-z0-9_@\$~]*
Quoted_Identifier = (\"((\$|[^\"]*)*(\"\")*)*\")

Rules.

% delimiters
(\:\:)                              : {token, {list_to_atom(?debug(TokenChars)), TokenLine}}.
[\:\(\)\[\]\{\}\#\,\|\-\+\*\/\\%]   : {token, {list_to_atom(?debug(TokenChars)), TokenLine}}.
({Non_Quoted_Identifier}|{Quoted_Identifier})(\.({Non_Quoted_Identifier}|{Quoted_Identifier}))*
                                    : {token, {'STRING', TokenLine, ?debug(TokenChars)}}.

([\s\t\r\n]+)                       : skip_token.    %% white space

Erlang code.

-define(debug(T), T).
%-define(debug(T), begin io:format(user, "Token ~p~n", [T]), T end).
