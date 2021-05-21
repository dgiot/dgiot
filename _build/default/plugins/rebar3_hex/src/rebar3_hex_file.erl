-module(rebar3_hex_file).

-export([ expand_paths/2
        , update_app_src/2
        ]).

expand_paths(Paths, Dir) ->
    AbsDir = filename:absname(Dir),
    Files = lists:flatmap(fun dir_files1/1, [filename:join(Dir, P) || P <- Paths]),
    [{relative_path(F1, AbsDir), F1} || F1 <- filter_regular(Files)].

dir_files1(Dir) ->
    lists:flatmap(fun(Y) -> dir_files(Y) end, filelib:wildcard(Dir)).

filter_regular(Files) ->
    lists:filter(fun filelib:is_regular/1, [filename:absname(F) || F <- Files]).

dir_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
            filelib:wildcard(filename:join(Path, "**"));
        false ->
            [Path]
    end.

relative_path(AbsFile, AbsDir) ->
    rebar_dir:make_relative_path(AbsFile, AbsDir).

update_app_src(App, Version) ->
    AppSrcFile = rebar_app_info:app_file_src(App),
    AppSrc = rebar_file_utils:try_consult(AppSrcFile),
    [{application, Name, Details}] = AppSrc,
    NewDetails = lists:keyreplace(vsn, 1, Details, {vsn, rebar_utils:to_list(Version)}),
    {application, Name, NewDetails}.
