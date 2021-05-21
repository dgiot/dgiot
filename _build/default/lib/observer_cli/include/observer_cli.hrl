-define(COLUMN, 134).
-define(INIT_TIME_REF, undefined).

-define(MIN_INTERVAL, 1000).
-define(DEFAULT_INTERVAL, 1500).
-define(DISABLE, disable).
-define(ENABLE, enable).

-record(home, {
    func = proc_count :: atom(),
    type = memory :: atom(),
    cur_page = 1 :: pos_integer(),
    pages = [{1, 1}] :: list(),
    interval = ?DEFAULT_INTERVAL :: pos_integer(),
    scheduler_usage = application:get_env(observer_cli, scheduler_usage, ?DISABLE) ::
        ?DISABLE | ?ENABLE
}).

-record(ets, {
    interval = 2000 :: integer(),
    attr = memory :: atom(),
    cur_page = 1 :: integer()
}).

-record(system, {interval = 2000 :: integer()}).

-record(db, {
    interval = ?DEFAULT_INTERVAL :: integer(),
    hide_sys = true :: boolean(),
    cur_page = 1 :: integer(),
    attr = memory :: atom()
}).

-record(help, {interval = ?DEFAULT_INTERVAL :: integer()}).
-record(inet, {
    interval = ?DEFAULT_INTERVAL :: integer(),
    func = inet_count :: atom(),
    type = cnt :: atom(),
    cur_page = 1 :: pos_integer(),
    pages = [{1, 1}] :: list()
}).

-record(process, {interval = ?DEFAULT_INTERVAL :: integer()}).

-record(plug, {cur_index = 1 :: pos_integer(), plugs = [] :: map() | []}).

-record(view_opts, {
    home = #home{} :: home(),
    ets = #ets{} :: ets(),
    sys = #system{} :: system(),
    db = #db{} :: db(),
    help = #help{} :: help(),
    inet = #inet{} :: inet(),
    process = #process{} :: process(),
    port = ?DEFAULT_INTERVAL :: pos_integer(),
    plug = #plug{} :: plug(),
    auto_row = true :: boolean()
}).

-export_type([view_opts/0]).

-type view_opts() :: #view_opts{}.
-type home() :: #home{}.
-type system() :: #system{}.
-type ets() :: #ets{}.
-type db() :: #db{}.
-type help() :: #help{}.
-type inet() :: #inet{}.
-type process() :: #process{}.
-type plug() :: #plug{}.

-define(CURSOR_TOP, <<"\e[H">>).
-define(CLEAR, <<"\e[H\e[J">>).

-define(RESET_BG, <<"\e[49m">>).
-define(RESET, <<"\e[0m">>).
-define(GRAY_BG, <<"\e[7m">>).
-define(YELLOW, <<"\e[33m">>).
-define(RED, <<"\e[31m">>).
-define(L_RED, <<"\e[48m">>).
-define(GREEN, <<"\e[32;1m">>).
-define(L_GREEN, <<"\e[92m">>).
-define(CHOOSE_BG, <<"\e[42m">>).
-define(RED_BG, <<"\e[48;2;184;0;0m">>).
-define(L_GRAY_BG, <<"\e[48;2;80;80;80m">>).
-define(UNDERLINE, <<"\e[4m">>).

-define(NEW_LINE, "\e[0m\n|").
-define(I, <<" | ">>).
-define(I2, <<"|">>).
-define(W(_C_, _A_, _W_), {extend_color, _C_, _A_, _W_}).
-define(W2(_C_, _A_, _W_), {extend_color_2, _C_, _A_, _W_}).
-define(W(_A_, _W_), {extend, _A_, _W_}).

-define(SELECT(Text), observer_cli_lib:select(Text)).
-define(UNSELECT(Text), observer_cli_lib:unselect(Text)).

-define(render(_FA_), observer_cli_lib:render(_FA_)).
-define(output(_F_, _A_), io:format(iolist_to_binary(_F_), _A_)).
-define(output(_L_), ?output(_L_, [])).
