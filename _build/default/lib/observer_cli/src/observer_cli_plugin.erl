%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_plugin).

%% API
-export([start/1]).

-callback attributes(PreState) -> {[Rows], NewState} when
    PreState :: any(),
    Rows :: #{
        content => string() | integer() | {byte, pos_integer()},
        width => pos_integer(),
        color => binary()
    },
    NewState :: any().

-callback sheet_header() -> [SheetHeader] when
    SheetHeader :: #{title => string(), width => pos_integer(), shortcut => string()}.

-callback sheet_body(PreState) -> {SheetBody, NewState} when
    PreState :: any(),
    SheetBody :: list(),
    NewState :: any().

-define(LAST_LINE,
    "refresh: ~wms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current pages is ~w"
).

-include("observer_cli.hrl").

-spec start(ViewOpts) -> no_return() when ViewOpts :: view_opts().
start(#view_opts{plug = Plugs, auto_row = AutoRow} = ViewOpts) ->
    NewPlugs = init_config(Plugs),
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(?INIT_TIME_REF, NewPlugs, AutoRow, undefined, undefined)
    end),
    manager(Pid, ViewOpts#view_opts{plug = NewPlugs}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_config(#plug{plugs = []}) ->
    application:ensure_all_started(observer_cli),
    Plugs = application:get_env(observer_cli, plugins, []),
    {_, NewPlugs} = lists:foldl(
        fun(M = #{module := Mod}, {Index, Acc}) ->
            SheetWidth = get_sheet_width(Mod),
            Config = maps:merge(
                #{
                    cur_page => 1,
                    sort_column => 2,
                    interval => 1500,
                    sheet_width => SheetWidth
                },
                M
            ),
            {Index + 1, maps:put(Index, Config, Acc)}
        end,
        {1, #{}},
        Plugs
    ),
    #plug{cur_index = 1, plugs = NewPlugs};
init_config(Plugs) ->
    Plugs.

manager(ChildPid, ViewOpts) ->
    #view_opts{plug = PlugOpts = #plug{cur_index = CurIndex, plugs = Plugs}} = ViewOpts,
    case parse_cmd() of
        quit ->
            erlang:send(ChildPid, quit);
        go_home ->
            observer_cli_lib:exit_processes([ChildPid]),
            observer_cli:start(ViewOpts);
        {new_interval, NewMs} ->
            observer_cli_lib:exit_processes([ChildPid]),
            NewPlugs = update_plugins(CurIndex, Plugs, #{interval => NewMs}),
            start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}});
        page_down_top_n ->
            CurPlugs = maps:get(CurIndex, Plugs),
            CurPage = maps:get(cur_page, CurPlugs),
            NewPage = max(CurPage + 1, 1),
            NewPlugs = update_plugins(CurIndex, Plugs, #{cur_page => NewPage}),
            observer_cli_lib:exit_processes([ChildPid]),
            start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}});
        page_up_top_n ->
            CurPlugs = maps:get(CurIndex, Plugs),
            CurPage = maps:get(cur_page, CurPlugs),
            NewPage = max(CurPage - 1, 1),
            NewPlugs = update_plugins(CurIndex, Plugs, #{cur_page => NewPage}),
            observer_cli_lib:exit_processes([ChildPid]),
            start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}});
        {input_str, Cmd} ->
            case maybe_shortcut(Cmd, ViewOpts) of
                {ok, menu, Index} ->
                    observer_cli_lib:exit_processes([ChildPid]),
                    start(ViewOpts#view_opts{plug = PlugOpts#plug{cur_index = Index}});
                {ok, sheet, SortColumn} ->
                    NewPlugs = update_plugins(CurIndex, Plugs, #{sort_column => SortColumn}),
                    observer_cli_lib:exit_processes([ChildPid]),
                    start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}});
                {error, _} ->
                    manager(ChildPid, ViewOpts)
            end;
        _ ->
            manager(ChildPid, ViewOpts)
    end.

update_plugins(CurIndex, Lists, UpdateItems) ->
    CurPlugs = maps:get(CurIndex, Lists),
    NewPlugs = maps:merge(CurPlugs, UpdateItems),
    maps:put(CurIndex, NewPlugs, Lists).

maybe_shortcut(Cmd, ViewOpts) ->
    #view_opts{plug = #plug{cur_index = CurIndex, plugs = Plugs}} = ViewOpts,
    case match_menu_shortcut(Cmd, maps:to_list(Plugs)) of
        {ok, Index} ->
            {ok, menu, Index};
        {error, not_found} ->
            case maps:find(CurIndex, Plugs) of
                {ok, #{module := CurMod}} ->
                    try
                        case match_sheet_shortcut(Cmd, CurMod:sheet_header(), 1) of
                            {ok, Index} -> {ok, sheet, Index};
                            {error, _Reason} = Err -> Err
                        end
                    catch
                        error:undef ->
                            {error, not_found}
                    end;
                _ ->
                    {error, not_found}
            end
    end.

match_menu_shortcut(_Cmd, []) ->
    {error, not_found};
match_menu_shortcut(Cmd, [{Index, Plug} | Plugs]) ->
    case Plug of
        #{shortcut := Cmd} -> {ok, Index};
        _ -> match_menu_shortcut(Cmd, Plugs)
    end.

match_sheet_shortcut(_Cmd, [], _Index) -> {error, not_found};
match_sheet_shortcut(Shortcut, [#{shortcut := Shortcut} | _], Index) -> {ok, Index};
match_sheet_shortcut(Cmd, [_ | T], Index) -> match_sheet_shortcut(Cmd, T, Index + 1).

render_worker(
    LastTimeRef,
    #plug{cur_index = CurIndex, plugs = Plugs} = PlugInfo,
    AutoRow,
    PrevAttrs,
    PrevSheet
) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    case maps:find(CurIndex, Plugs) of
        {ok, #{interval := Interval, cur_page := CurPage, sheet_width := SheetWidth} = CurPlug} ->
            Menu = render_menu(PlugInfo, SheetWidth),
            {Labels, LabelLine, NewAttrs} = render_attributes(CurPlug, PrevAttrs),
            {SheetLine, NewSheet} = render_sheet(
                erlang:max(0, TerminalRow - LabelLine - 4),
                CurPlug,
                PrevSheet
            ),
            LastText = io_lib:format(?LAST_LINE, [Interval, CurPage]),
            LastLine = ?render([?UNDERLINE, ?GRAY_BG, ?W(LastText, SheetWidth)]),
            ?output([?CURSOR_TOP, Menu, Labels, SheetLine, LastLine]),
            NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
            receive
                quit -> quit;
                _ -> render_worker(NextTimeRef, PlugInfo, AutoRow, NewAttrs, NewSheet)
            end;
        error ->
            Menu = ?render([
                ?UNDERLINE,
                ?W(?UNSELECT("Home(H)"), 30),
                ?W(?SELECT("EmptyPlugin"), 144)
            ]),
            ErrInfo =
                "|Plugin Error: Can't find your observer_cli config.\n|Please visit \"How to write your own plugin\" in readme.\n",
            ?output([?CURSOR_TOP, Menu, ErrInfo])
    end.

parse_cmd() ->
    case observer_cli_lib:to_list(io:get_line("")) of
        "H\n" -> go_home;
        %% backward
        "B\n" -> page_up_top_n;
        %% forward
        "F\n" -> page_down_top_n;
        "q\n" -> quit;
        Number -> observer_cli_lib:parse_integer(Number)
    end.

render_menu(#plug{cur_index = CurIndex, plugs = Plugs}, SheetWidth) ->
    Num = maps:size(Plugs),
    Title = get_menu_title(CurIndex, Plugs, Num, []),
    [Time] = observer_cli_lib:uptime(),
    ?render([
        ?UNDERLINE,
        ?W(
            [
                ?UNSELECT("Home(H)"),
                "|",
                Title
            ],
            SheetWidth + Num * 21 + 1
        ),
        Time
    ]).

get_menu_title(CurIndex, Plugs, CurIndex, Acc) ->
    {ok, #{title := Title, shortcut := Shortcut}} = maps:find(CurIndex, Plugs),
    NewTitle = Title ++ "(" ++ Shortcut ++ ")",
    NewAcc = [?SELECT(NewTitle), "|" | Acc],
    get_menu_title(CurIndex, Plugs, CurIndex - 1, NewAcc);
get_menu_title(CurIndex, Plugs, Pos, Acc) ->
    case maps:find(Pos, Plugs) of
        error ->
            Acc;
        {ok, #{title := Title, shortcut := Shortcut}} ->
            NewTitle = Title ++ "(" ++ Shortcut ++ ")",
            NewAcc = [?UNSELECT(NewTitle), "|" | Acc],
            get_menu_title(CurIndex, Plugs, Pos - 1, NewAcc)
    end.

render_attributes(#{module := Module}, PrevAttrs) ->
    try
        {DiffAttrs, NewAttrs} = Module:attributes(PrevAttrs),
        Render = [
            begin
                L = [
                    begin
                        #{content := Content, width := Width} = Item,
                        case maps:find(color, Item) of
                            {ok, Color} -> ?W2(Color, Content, Width);
                            error -> ?W(Content, Width)
                        end
                    end
                    || Item <- Label
                ],
                ?render(L)
            end
            || Label <- DiffAttrs
        ],
        {Render, length(Render), NewAttrs}
    catch
        error:undef ->
            {[], 0, PrevAttrs}
    end.

render_sheet(Rows, #{sort_column := SortColumn, cur_page := CurPage, module := Module}, PrevSheet) ->
    try
        {Headers, Widths} = render_sheet_header(Module, SortColumn),
        {Body, NewSheet} = render_sheet_body(Module, CurPage, Rows, SortColumn, Widths, PrevSheet),
        {[Headers | Body], NewSheet}
    catch
        error:undef ->
            []
    end.

render_sheet_header(Module, SortRow) ->
    SheetHeader = Module:sheet_header(),
    {Headers, Widths, _} = lists:foldl(
        fun(
            #{title := Header, width := Width} = H,
            {HeaderAcc, WidthAcc, Index}
        ) ->
            Title =
                case maps:get(shortcut, H, "") of
                    "" -> Header;
                    Shortcut -> Header ++ "(" ++ Shortcut ++ ")"
                end,
            case Index =:= SortRow of
                true ->
                    {
                        [?UNDERLINE, ?W2(?RED_BG, Title, Width) | HeaderAcc],
                        [Width | WidthAcc],
                        Index - 1
                    };
                false ->
                    {
                        [?UNDERLINE, ?W2(?GRAY_BG, Title, Width) | HeaderAcc],
                        [Width | WidthAcc],
                        Index - 1
                    }
            end
        end,
        {[], [], length(SheetHeader)},
        lists:reverse(SheetHeader)
    ),
    {?render(Headers), Widths}.

render_sheet_body(Module, CurPage, Rows, SortRow, Widths, PrevSheet) ->
    {Diff, NewSheet} = Module:sheet_body(PrevSheet),
    DataSet = lists:map(
        fun(I) ->
            {0, lists:nth(SortRow, I), I}
        end,
        Diff
    ),
    SortData = observer_cli_lib:sublist(DataSet, Rows, CurPage),
    Line = [
        begin
            List = mix_content_width(Item, Widths, []),
            ?render(List)
        end
        || {_, _, Item} <- SortData
    ],
    {Line, NewSheet}.

mix_content_width([], _, Acc) ->
    lists:reverse(Acc);
%% first
mix_content_width([I | IRest], [W | WRest], []) ->
    IList = observer_cli_lib:to_list(I),
    mix_content_width(IRest, WRest, [?W(IList, W - 1)]);
%% last
mix_content_width([I], [W], Acc) ->
    IList = observer_cli_lib:to_list(I),
    mix_content_width([], [], [?W(IList, W - 1) | Acc]);
%% middle
mix_content_width([I | IRest], [W | WRest], Acc) ->
    IList = observer_cli_lib:to_list(I),
    mix_content_width(IRest, WRest, [?W(IList, W - 2) | Acc]).

get_sheet_width(Mod) ->
    try
        Width = lists:foldl(fun(#{width := W}, Acc) -> Acc + W + 1 end, 1, Mod:sheet_header()),
        case Width > 1 of
            true -> Width - 2;
            false -> ?COLUMN + 5
        end
    catch
        error:undef ->
            ?COLUMN + 5
    end.
