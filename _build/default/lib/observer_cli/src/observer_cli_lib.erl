%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_lib).

-include("observer_cli.hrl").

-compile(inline).

%% API
-export([parse_cmd/3]).
-export([uptime/0]).
-export([to_percent/1]).
-export([to_list/1]).
-export([green/1]).
-export([to_byte/1]).
-export([mfa_to_list/1]).
-export([render/1]).
-export([next_redraw/2]).
-export([render_menu/2]).
-export([get_terminal_rows/1]).
-export([select/1]).
-export([unselect/1]).
-export([parse_integer/1]).
-export([render_last_line/1]).
-export([exit_processes/1]).
-export([update_page_pos/3]).
-export([get_pos/4]).
-export([sublist/3]).
-export([sbcs_to_mbcs/2]).

-define(select(Title), ?RED_BG, Title, ?RESET_BG).
-define(unselect(Title), ?L_GRAY_BG, Title, ?RESET_BG).

-spec uptime() -> list().
uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    Time = [
        integer_to_list(D),
        "Days ",
        integer_to_list(H),
        ":",
        integer_to_list(M),
        ":",
        integer_to_list(S)
    ],
    [?W(?GREEN, Time, 16)].

%% @doc 0.982342 -> 98.23%, 1 -> 100.0%
-spec to_percent(float()) -> string().
to_percent(Float) when Float < 0.1 -> [$0, erlang:float_to_list(Float * 100, [{decimals, 2}]), $%];
to_percent(Float) when Float < 1 -> [erlang:float_to_list(Float * 100, [{decimals, 2}]), $%];
to_percent(undefined) -> "******";
to_percent(_) -> "100.0%".

-spec to_list(term()) -> list().
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Pid) when is_pid(Pid) -> erlang:pid_to_list(Pid);
to_list(Binary) when is_binary(Binary) -> erlang:binary_to_list(Binary);
to_list(Port) when is_port(Port) -> erlang:port_to_list(Port);
to_list(Ref) when is_reference(Ref) -> erlang:ref_to_list(Ref);
to_list(Float) when is_float(Float) -> erlang:float_to_list(Float, [{decimals, 4}]);
to_list(Val) -> Val.

get_menu_title(home, MnesiaTitle) ->
    [
        ?select("Home(H)"),
        "|",
        ?unselect("Network(N)"),
        "|",
        ?unselect("System(S)"),
        "|",
        ?unselect("Ets(E)"),
        unselect(MnesiaTitle),
        "|",
        ?unselect("App(A)"),
        "|",
        ?unselect("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ];
get_menu_title(ets, MnesiaTitle) ->
    [
        ?unselect("Home(H)"),
        "|",
        ?unselect("Network(N)"),
        "|",
        ?unselect("System(S)"),
        "|",
        ?select("Ets(E)"),
        unselect(MnesiaTitle),
        "|",
        ?unselect("App(A)"),
        "|",
        ?unselect("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ];
get_menu_title(allocator, MnesiaTitle) ->
    [
        ?unselect("Home(H)"),
        "|",
        ?unselect("Network(N)"),
        "|",
        ?select("System(S)"),
        "|",
        ?unselect("Ets(E)"),
        unselect(MnesiaTitle),
        "|",
        ?unselect("App(A)"),
        "|",
        ?unselect("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ];
get_menu_title(doc, MnesiaTitle) ->
    [
        ?unselect("Home(H)"),
        "|",
        ?unselect("Network(N)"),
        "|",
        ?unselect("System(S)"),
        "|",
        ?unselect("Ets(E)"),
        unselect(MnesiaTitle),
        "|",
        ?unselect("App(A)"),
        "|",
        ?select("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ];
get_menu_title(inet, MnesiaTitle) ->
    [
        ?unselect("Home(H)"),
        "|",
        ?select("Network(N)"),
        "|",
        ?unselect("System(S)"),
        "|",
        ?unselect("Ets(E)"),
        unselect(MnesiaTitle),
        "|",
        ?unselect("App(A)"),
        "|",
        ?unselect("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ];
get_menu_title(mnesia, MnesiaTitle) ->
    [
        ?unselect("Home(H)"),
        "|",
        ?unselect("Network(N)"),
        "|",
        ?unselect("System(S)"),
        "|",
        ?unselect("Ets(E)"),
        select(MnesiaTitle),
        "|",
        ?unselect("App(A)"),
        "|",
        ?unselect("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ];
get_menu_title(app, MnesiaTitle) ->
    [
        ?unselect("Home(H)"),
        "|",
        ?unselect("Network(N)"),
        "|",
        ?unselect("System(S)"),
        "|",
        ?unselect("Ets(E)"),
        unselect(MnesiaTitle),
        "|",
        ?select("App(A)"),
        "|",
        ?unselect("Doc(D)"),
        "|",
        ?unselect("Plugin(P)")
    ].

-spec select(string()) -> list().
select(Title) -> [?RED_BG, Title, ?RESET_BG].

-spec unselect(string()) -> list().
unselect(Title) -> [?L_GRAY_BG, Title, ?RESET_BG].

-spec green(list()) -> list().
green(String) -> "\e[32;1m" ++ String ++ "\e[0m".

-spec to_byte(pos_integer()) -> list().
%% byte
to_byte(Byte) when is_integer(Byte), Byte < 1024 ->
    [erlang:integer_to_list(Byte), $\s, $B];
%% kilobyte
to_byte(Byte) when Byte < 1024 * 1024 ->
    [erlang:float_to_list(Byte / 1024, [{decimals, 4}]), $\s, $K, $B];
%% megabyte
to_byte(Byte) when Byte < 1024 * 1024 * 1024 ->
    [erlang:float_to_list(Byte / (1024 * 1024), [{decimals, 4}]), $\s, $M, $B];
%% megabyte
to_byte(Byte) when is_integer(Byte) ->
    [erlang:float_to_list(Byte / (1024 * 1024 * 1024), [{decimals, 4}]), $\s, $G, $B];
%% process died
to_byte(Byte) ->
    [to_list(Byte)].

-spec mfa_to_list({atom(), atom(), integer()} | term()) -> nonempty_string().
mfa_to_list({Module, Fun, Arg}) ->
    [atom_to_list(Module), ":", atom_to_list(Fun), "/", integer_to_list(Arg)];
mfa_to_list(Function) ->
    io_lib:format("~p", [Function]).

-spec render(list()) -> iolist().
render(FA) ->
    {F, A} = tidy_format_args([" \e[0m|~n" | lists:reverse(["|" | FA])], true, [], []),
    io_lib:format(erlang:iolist_to_binary(F), A).

%{erlang:iolist_to_binary(F), A}.

-spec render_menu(atom(), string()) -> iolist().
render_menu(Type, Text) ->
    MnesiaTitle =
        case ets:info(schema, owner) of
            undefined -> "";
            _ -> "/Mnesia(M)"
        end,
    Title = get_menu_title(Type, MnesiaTitle),
    UpTime = uptime(),
    TitleWidth = ?COLUMN + 151 - erlang:length(UpTime),
    ?render([?W([Title | Text], TitleWidth) | UpTime]).

tidy_format_args([], _NeedLine, FAcc, AAcc) ->
    {FAcc, AAcc};
tidy_format_args([{extend, A, W} | Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts">>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{extend, A, W} | Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts", ?I/binary>>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{extend_color, C, A, W} | Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts">>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{extend_color, C, A, W} | Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?I2/binary>>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{extend_color_2, C, A, W} | Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts">>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{extend_color_2, C, A, W} | Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?RESET/binary, ?I2/binary>>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([F | Rest], NeedLine, FAcc, AAcc) ->
    tidy_format_args(Rest, NeedLine, [F | FAcc], AAcc).

to_str({byte, Bytes}) -> to_byte(Bytes);
to_str(Term) -> to_list(Term).

-spec parse_cmd(#view_opts{}, atom(), list()) -> atom() | string() | tuple().
parse_cmd(ViewOpts, Module, Args) ->
    case to_list(io:get_line("")) of
        "H\n" ->
            Module:clean(Args),
            observer_cli:start(ViewOpts);
        "S\n" ->
            Module:clean(Args),
            observer_cli_system:start(ViewOpts);
        "A\n" ->
            Module:clean(Args),
            observer_cli_application:start(ViewOpts);
        "N\n" ->
            Module:clean(Args),
            observer_cli_inet:start(ViewOpts);
        "M\n" ->
            Module:clean(Args),
            observer_cli_mnesia:start(ViewOpts);
        "E\n" ->
            Module:clean(Args),
            observer_cli_ets:start(ViewOpts);
        "D\n" ->
            Module:clean(Args),
            observer_cli_help:start(ViewOpts);
        "P\n" ->
            Module:clean(Args),
            observer_cli_plugin:start(ViewOpts);
        %% inet
        "ic\n" ->
            inet_count;
        "iw\n" ->
            inet_window;
        "rc\n" ->
            recv_cnt;
        "ro\n" ->
            recv_oct;
        "sc\n" ->
            send_cnt;
        "so\n" ->
            send_oct;
        "cnt\n" ->
            cnt;
        "oct\n" ->
            oct;
        "q\n" ->
            quit;
        "Q\n" ->
            quit;
        %% backward
        "pu\n" ->
            page_up_top_n;
        %% forward
        "pd\n" ->
            page_down_top_n;
        %% backward
        "PU\n" ->
            page_up_top_n;
        %% forward
        "PD\n" ->
            page_down_top_n;
        %% backward
        "B\n" ->
            page_up_top_n;
        %% forward
        "F\n" ->
            page_down_top_n;
        %% home
        "p\n" ->
            pause_or_resume;
        "r\n" ->
            {func, proc_count, reductions};
        "b\n" ->
            {func, proc_count, binary_memory};
        "t\n" ->
            {func, proc_count, total_heap_size};
        "m\n" ->
            {func, proc_count, memory};
        "mq\n" ->
            {func, proc_count, message_queue_len};
        "rr\n" ->
            {func, proc_window, reductions};
        "bb\n" ->
            {func, proc_window, binary_memory};
        "tt\n" ->
            {func, proc_window, total_heap_size};
        "mm\n" ->
            {func, proc_window, memory};
        "mmq\n" ->
            {func, proc_window, message_queue_len};
        "\n" ->
            jump;
        "s\n" ->
            size;
        "hide\n" ->
            hide;
        "`\n" ->
            scheduler_usage;
        Number ->
            parse_integer(Number)
    end.

-spec next_redraw(reference(), pos_integer()) -> reference().
next_redraw(LastTimeRef, Interval) ->
    LastTimeRef =/= ?INIT_TIME_REF andalso erlang:cancel_timer(LastTimeRef),
    erlang:send_after(Interval, self(), redraw).

-spec get_terminal_rows(boolean()) -> integer().
get_terminal_rows(_AutoRow = false) ->
    application:get_env(observer_cli, default_row_size, 30);
get_terminal_rows(_AutoRow = true) ->
    case io:rows() of
        {error, _} -> application:get_env(observer_cli, default_row_size, 30);
        {ok, Rows} -> Rows
    end.

-spec parse_integer(string()) -> {term(), term()}.
parse_integer(Number) ->
    case string:to_integer(Number) of
        {error, _Reason} ->
            {input_str, Number -- "\n"};
        {Integer, _} ->
            if
                Integer >= ?MIN_INTERVAL ->
                    {new_interval, Integer};
                Integer > 0 ->
                    {jump, Integer};
                true ->
                    {input_str, Number -- "\n"}
            end
    end.

-spec render_last_line(string()) -> list().
render_last_line(Text) ->
    ?render([?UNDERLINE, ?GRAY_BG, ?W(Text, ?COLUMN + 2)]).

-spec exit_processes(list()) -> ok.
exit_processes(List) ->
    [
        begin
            erlang:unlink(Pid),
            erlang:exit(Pid, stop)
        end
        || Pid <- List
    ],
    flush(),
    ok.

-spec update_page_pos(pid() | pos_integer(), pos_integer(), list()) -> list().
update_page_pos(StorePid, Page, Pages) when is_pid(StorePid) ->
    Pos =
        case lists:keyfind(Page, 1, Pages) of
            false ->
                Row = observer_cli_store:lookup_row(StorePid),
                (Page - 1) * Row + 1;
            {_, P} ->
                P
        end,
    update_page_pos(Page, Pos, Pages);
update_page_pos(Page, Pos, Pages) ->
    [{Page, Pos} | lists:keydelete(Page, 1, Pages)].

-spec get_pos(pos_integer(), pos_integer(), list(), pos_integer()) ->
    {pos_integer(), pos_integer()}.
get_pos(_Page, _PageRow, _Pages, 0) ->
    {1, 1};
get_pos(Page, PageRow, Pages, TopLen) ->
    Start = erlang:min((Page - 1) * PageRow + 1, TopLen),
    case lists:keyfind(Page, 1, Pages) of
        {_, P} when P >= Start andalso P =< Start + PageRow -> {Start, P};
        _ -> {Start, Start}
    end.

flush() ->
    receive
        _Msg ->
            flush()
    after 100 -> ok
    end.

-spec sublist(list(), integer(), integer()) -> list().
sublist(AllEts, Rows, CurPage) ->
    SortEts = recon_lib:sublist_top_n_attrs(AllEts, Rows * CurPage),
    Start = Rows * (CurPage - 1) + 1,
    case erlang:length(SortEts) >= Start of
        true ->
            lists:sublist(SortEts, Start, Rows);
        false ->
            []
    end.

-spec sbcs_to_mbcs(list(), list()) -> list().
sbcs_to_mbcs(TypeList, STMList) ->
    FoldlFun = fun
        ({{Type, _}, New}, Acc) when is_number(New) ->
            case lists:member(Type, TypeList) of
                true -> maps:update_with(Type, fun(V) -> V + New end, 0, Acc);
                false -> Acc
            end;
        (_, Acc) ->
            Acc
    end,
    maps:to_list(lists:foldl(FoldlFun, #{}, STMList)).
