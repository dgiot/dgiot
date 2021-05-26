%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_mnesia).

%% API
-export([start/1]).
-export([clean/1]).

-include("observer_cli.hrl").

-define(LAST_LINE,
    "q(quit) s(sort by size) m(sort by memory) pd/pu(page:down/up) F/B(forward/back)"
    " hide(swith between hide and display system table)"
).

-spec start(#view_opts{}) -> any().
start(
    #view_opts{
        db = #db{
            interval = MillSecond,
            hide_sys = HideSys,
            cur_page = CurPage,
            attr = Attr
        },
        auto_row = AutoRow
    } = HomeOpts
) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(MillSecond, ?INIT_TIME_REF, HideSys, AutoRow, Attr, CurPage)
    end),
    manager(Pid, HomeOpts).

-spec clean(list()) -> ok.
clean(Pids) -> observer_cli_lib:exit_processes(Pids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

manager(ChildPid, #view_opts{db = DBOpts = #db{cur_page = CurPage, hide_sys = Hide}} = HomeOpts) ->
    case observer_cli_lib:parse_cmd(HomeOpts, ?MODULE, [ChildPid]) of
        quit ->
            erlang:send(ChildPid, quit);
        {new_interval, NewMs} = Msg ->
            erlang:send(ChildPid, Msg),
            manager(ChildPid, HomeOpts#view_opts{db = DBOpts#db{interval = NewMs}});
        hide ->
            NewHide = not Hide,
            erlang:send(ChildPid, {system_table, NewHide}),
            manager(ChildPid, HomeOpts#view_opts{db = DBOpts#db{hide_sys = NewHide}});
        size ->
            clean([ChildPid]),
            start(HomeOpts#view_opts{db = DBOpts#db{attr = size}});
        %% Home
        {func, proc_count, memory} ->
            clean([ChildPid]),
            start(HomeOpts#view_opts{db = DBOpts#db{attr = memory}});
        page_down_top_n ->
            NewPage = max(CurPage + 1, 1),
            clean([ChildPid]),
            start(HomeOpts#view_opts{db = DBOpts#db{cur_page = NewPage}});
        page_up_top_n ->
            NewPage = max(CurPage - 1, 1),
            clean([ChildPid]),
            start(HomeOpts#view_opts{db = DBOpts#db{cur_page = NewPage}});
        _ ->
            manager(ChildPid, HomeOpts)
    end.

render_worker(Interval, LastTimeRef, HideSystemTable, AutoRow, Attr, CurPage) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(TerminalRow - 5, 0),
    Text =
        "Interval: " ++
            integer_to_list(Interval) ++
            "ms" ++
            " HideSystemTable:" ++ atom_to_list(HideSystemTable),
    Menu = observer_cli_lib:render_menu(mnesia, Text),
    LastLine = observer_cli_lib:render_last_line(?LAST_LINE),
    case get_table_list(HideSystemTable, Attr) of
        {error, Reason} ->
            ErrInfo = io_lib:format("Mnesia Error   ~p~n", [Reason]),
            ?output([?CURSOR_TOP, Menu, ErrInfo, LastLine]);
        MnesiaList ->
            Info = render_mnesia(MnesiaList, Attr, Rows, CurPage),
            ?output([?CURSOR_TOP, Menu, Info, LastLine])
    end,
    TimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit ->
            quit;
        {new_interval, NewInterval} ->
            render_worker(NewInterval, TimeRef, HideSystemTable, AutoRow, Attr, CurPage);
        {system_table, NewHideSystemTable} ->
            render_worker(Interval, TimeRef, NewHideSystemTable, AutoRow, Attr, CurPage);
        _ ->
            render_worker(Interval, TimeRef, HideSystemTable, AutoRow, Attr, CurPage)
    end.

render_mnesia(MnesiaList, Attr, Rows, CurPage) ->
    SortMnesia = observer_cli_lib:sublist(MnesiaList, Rows, CurPage),
    {MemColor, SizeColor} =
        case Attr of
            memory -> {?RED_BG, ?GRAY_BG};
            _ -> {?GRAY_BG, ?RED_BG}
        end,
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Name", 25),
        ?UNDERLINE,
        ?W2(MemColor, "    Memory    ", 16),
        ?UNDERLINE,
        ?W2(SizeColor, "Size", 16),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Type", 12),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Storage", 15),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Owner", 14),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Index", 11),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Reg_name", 20)
    ]),
    View = [
        begin
            Name = proplists:get_value(name, Mnesia),
            Memory = proplists:get_value(memory, Mnesia),
            Size = proplists:get_value(size, Mnesia),
            Type = proplists:get_value(type, Mnesia),
            RegName = proplists:get_value(reg_name, Mnesia),
            Index = proplists:get_value(index, Mnesia),
            Owner = proplists:get_value(owner, Mnesia),
            Storage = proplists:get_value(storage, Mnesia),
            ?render([
                ?W(Name, 24),
                ?W({byte, Memory}, 14),
                ?W(Size, 14),
                ?W(Type, 10),
                ?W(Storage, 13),
                ?W(Owner, 12),
                ?W(Index, 9),
                ?W(RegName, 19)
            ])
        end
        || {_, _, Mnesia} <- SortMnesia
    ],
    [Title | View].

mnesia_tables() ->
    [
        ir_AliasDef,
        ir_ArrayDef,
        ir_AttributeDef,
        ir_ConstantDef,
        ir_Contained,
        ir_Container,
        ir_EnumDef,
        ir_ExceptionDef,
        ir_IDLType,
        ir_IRObject,
        ir_InterfaceDef,
        ir_ModuleDef,
        ir_ORB,
        ir_OperationDef,
        ir_PrimitiveDef,
        ir_Repository,
        ir_SequenceDef,
        ir_StringDef,
        ir_StructDef,
        ir_TypedefDef,
        ir_UnionDef,
        logTable,
        logTransferTable,
        mesh_meas,
        mesh_type,
        mnesia_clist,
        orber_CosNaming,
        orber_objkeys,
        user
    ].

get_table_list(HideSys, Attr) ->
    Owner = ets:info(schema, owner),
    case Owner of
        undefined -> {error, "Mnesia is not running on: " ++ atom_to_list(node())};
        _ -> get_table_list2(Owner, HideSys, Attr)
    end.

get_table_list2(Owner, HideSys, Attr) ->
    {registered_name, RegName} = process_info(Owner, registered_name),
    WordSize = erlang:system_info(wordsize),
    CollectFun = fun(Id, Acc) ->
        case HideSys andalso ordsets:is_element(Id, mnesia_tables()) orelse Id =:= schema of
            %% ignore system table
            true ->
                Acc;
            false ->
                Storage = mnesia:table_info(Id, storage_type),
                Size = mnesia:table_info(Id, size),
                Memory = mnesia:table_info(Id, memory) * WordSize,
                Tab0 = [
                    {name, Id},
                    {owner, Owner},
                    {size, Size},
                    {reg_name, RegName},
                    {type, mnesia:table_info(Id, type)},
                    {memory, Memory},
                    {storage, Storage},
                    {index, mnesia:table_info(Id, index)}
                ],
                Tab =
                    case Storage of
                        _ when Storage =:= ram_copies orelse Storage =:= disc_copies ->
                            [
                                {fixed, ets:info(Id, fixed)},
                                {compressed, ets:info(Id, compressed)}
                                | Tab0
                            ];
                        disc_only_copies ->
                            [{fixed, dets:info(Id, safe_fixed)} | Tab0];
                        _ ->
                            Tab0
                    end,
                [{0, proplists:get_value(Attr, Tab), Tab} | Acc]
        end
    end,
    lists:foldl(CollectFun, [], mnesia:system_info(tables)).
