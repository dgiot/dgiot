%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_ets).

-include("observer_cli.hrl").

%% API
-export([start/1]).
-export([clean/1]).

-define(LAST_LINE,
    "q(quit) s(sort by size) m(sort by memory) pd/pu(page:down/up) F/B(forward/back)"
).

-spec start(ViewOpts) -> no_return() when ViewOpts :: view_opts().
start(
    #view_opts{
        ets = #ets{interval = Interval, attr = Attr, cur_page = CurPage},
        auto_row = AutoRow
    } = ViewOpts
) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(Interval, ?INIT_TIME_REF, Attr, CurPage, AutoRow)
    end),
    manager(Pid, ViewOpts).

-spec clean(list()) -> ok.
clean(Pids) -> observer_cli_lib:exit_processes(Pids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, #view_opts{ets = EtsOpts = #ets{cur_page = CurPage}} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [ChildPid]) of
        quit ->
            erlang:send(ChildPid, quit);
        {new_interval, NewMs} ->
            clean([ChildPid]),
            start(ViewOpts#view_opts{ets = EtsOpts#ets{interval = NewMs}});
        size ->
            clean([ChildPid]),
            start(ViewOpts#view_opts{ets = EtsOpts#ets{attr = size}});
        %% Home
        {func, proc_count, memory} ->
            clean([ChildPid]),
            start(ViewOpts#view_opts{ets = EtsOpts#ets{attr = memory}});
        page_down_top_n ->
            NewPage = max(CurPage + 1, 1),
            clean([ChildPid]),
            start(ViewOpts#view_opts{ets = EtsOpts#ets{cur_page = NewPage}});
        page_up_top_n ->
            NewPage = max(CurPage - 1, 1),
            clean([ChildPid]),
            start(ViewOpts#view_opts{ets = EtsOpts#ets{cur_page = NewPage}});
        _ ->
            manager(ChildPid, ViewOpts)
    end.

render_worker(Interval, LastTimeRef, Attr, CurPage, AutoRow) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(ets, Text),
    Ets = render_ets_info(erlang:max(0, TerminalRow - 4), CurPage, Attr),
    LastLine = observer_cli_lib:render_last_line(?LAST_LINE),
    ?output([?CURSOR_TOP, Menu, Ets, LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        _ -> render_worker(Interval, NextTimeRef, Attr, CurPage, AutoRow)
    end.

render_ets_info(Rows, CurPage, Attr) ->
    AllEts = [
        begin
            get_ets_info(Tab, Attr)
        end
        || Tab <- ets:all()
    ],
    WordSize = erlang:system_info(wordsize),
    SortEts = observer_cli_lib:sublist(AllEts, Rows, CurPage),
    {MemColor, SizeColor} =
        case Attr of
            memory -> {?RED_BG, ?GRAY_BG};
            _ -> {?GRAY_BG, ?RED_BG}
        end,
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Table Name", 37),
        ?UNDERLINE,
        ?W2(SizeColor, "Size", 14),
        ?UNDERLINE,
        ?W2(MemColor, "    Memory    ", 14),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Type", 15),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Protection", 12),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "KeyPos", 8),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Write/Read", 14),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Owner Pid", 15)
    ]),
    RowView = [
        begin
            Name = proplists:get_value(name, Ets),
            Memory = proplists:get_value(memory, Ets),
            Size = proplists:get_value(size, Ets),
            Type = proplists:get_value(type, Ets),
            Protect = proplists:get_value(protection, Ets),
            KeyPos = proplists:get_value(keypos, Ets),
            Write = observer_cli_lib:to_list(proplists:get_value(write_concurrency, Ets)),
            Read = observer_cli_lib:to_list(proplists:get_value(read_concurrency, Ets)),
            Owner = proplists:get_value(owner, Ets),
            ?render([
                ?W(Name, 36),
                ?W(Size, 12),
                ?W({byte, Memory * WordSize}, 12),
                ?W(Type, 13),
                ?W(Protect, 10),
                ?W(KeyPos, 6),
                ?W(Write ++ "/" ++ Read, 12),
                ?W(Owner, 14)
            ])
        end
        || {_, _, Ets} <- SortEts
    ],
    [Title | RowView].

get_ets_info(Tab, Attr) ->
    case catch ets:info(Tab) of
        {'EXIT', _} ->
            {
                0,
                0,
                [
                    %%it maybe die
                    {name, unread},
                    {write_concurrency, unread},
                    {read_concurrency, unread},
                    {compressed, unread},
                    {memory, unread},
                    {owner, unread},
                    {heir, unread},
                    {size, unread},
                    {node, unread},
                    {named_table, unread},
                    {type, unread},
                    {keypos, unread},
                    {protection, unread}
                ]
            };
        Info when is_list(Info) ->
            Owner = proplists:get_value(owner, Info),
            NewInfo =
                case is_reg(Owner) of
                    Owner -> Info;
                    Reg -> lists:keyreplace(Owner, 1, Info, {owner, Reg})
                end,
            {
                0,
                proplists:get_value(Attr, NewInfo),
                NewInfo
            }
    end.

is_reg(Owner) ->
    case process_info(Owner, registered_name) of
        {registered_name, Name} -> Name;
        _ -> Owner
    end.
