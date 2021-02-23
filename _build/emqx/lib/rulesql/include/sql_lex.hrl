-ifndef(SQL_LEX_HRL).
-define(SQL_LEX_HRL, true).

-ifdef(NODEBUG).
    -define(D(Format), undefined).
    -define(D(Format, Args), undefined).
-else.
    -define(D(Format), ?D(Format, [])).
    -define(D(Format, Args),
        io:format(user, "~p:~p:~p ===> "Format,
                  [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).
-endif.

-define(E(Format), ?E(Format, [])).
-define(E(Format, Args), io:format(user, "~p:~p:~p ===> "Format, [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% May not be used as identifier !!!
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(KEYWORDS, [
    {"^(?i)(AND)$",             'AND'},
    {"^(?i)(AS)$",              'AS'},
    {"^(?i)(CASE)$",            'CASE'},
    {"^(?i)(IF)$",              'IF'},
    {"^(?i)(ELSE)$",            'ELSE'},
    {"^(?i)(ELSIF)$",           'ELSIF'},
    {"^(?i)(FROM)$",            'FROM'},
    {"^(?i)(IN)$",              'IN'},
    {"^(?i)(NOT)$",             'NOT'},
    {"^(?i)(OR)$",              'OR'},
    {"^(?i)(SELECT)$",          'SELECT'},
    {"^(?i)(WHEN)$",            'WHEN'},
    {"^(?i)(THEN)$",            'THEN'},
    {"^(?i)(FOREACH)$",         'FOREACH'},
    {"^(?i)(INCASE)$",          'INCASE'},
    {"^(?i)(END)$",             'END'},
    {"^(?i)(DO)$",              'DO'},
    {"^(?i)(WHERE)$",           'WHERE'}
]).

-define(RESERVED_KEYS, ([atom_to_binary(__Key__, utf8) || {_, __Key__} <- ?KEYWORDS])).

-endif.
