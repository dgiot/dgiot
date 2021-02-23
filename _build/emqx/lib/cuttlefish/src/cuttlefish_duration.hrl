-define(FMT(F,A), lists:flatten(io_lib:format(F,A))).
-define(FORTNIGHT, 1209600000).
-define(WEEK,      604800000).
-define(DAY,       86400000).
-define(HOUR,      3600000).
-define(MINUTE,    60000).
-define(SECOND,    1000).

-define(MULTIPLIERS,
        [{f, ?FORTNIGHT},
         {w, ?WEEK},
         {d, ?DAY},
         {h, ?HOUR},
         {m, ?MINUTE},
         {s, ?SECOND},
         {ms, 1}]).
