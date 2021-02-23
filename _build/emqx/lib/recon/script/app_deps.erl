#!/usr/bin/env escript
%% -*- erlang -*-
%%% Run with 'escript app_deps.erl'
%%% Change the path in filelib:wildcard/1 as required to capture
%%% all your dependencies.
%%%
%%% Rectangular nodes will represent library apps (no processes
%%% involved) and the circular nodes will represent regular apps.
%%% An arrow going from 'A -> B' means 'A depends on B'.
%%%
%%% This script depends on graphviz being present on the system.
-module(app_deps).
-export([main/1]).

main(_) ->
    AppFiles = filelib:wildcard("deps/*/ebin/*.app")
               ++
               filelib:wildcard("apps/*/ebin/*.app")
               ++
               filelib:wildcard("ebin/*.app")
               ++
               filelib:wildcard("_build/default/lib/*/ebin/*.app"),
    to_graphviz(read_deps(AppFiles)).

read_deps(AppFiles) ->
    [{App,
      proplists:get_value(applications, Props, []),
      apptype(Props)}
     || {ok, [{_,App,Props}]} <-
        [file:consult(AppFile) || AppFile <- AppFiles]].

apptype(Props) ->
    case proplists:get_value(mod, Props) of
        undefined -> library;
        _ -> regular
    end.

to_graphviz(Deps) ->
    AllApps = lists:usort(lists:flatten(
        [[{App,Type},DepList] || {App,DepList,Type} <- Deps]
    )),
    Bytes = ["digraph G { ",
             "K=0.25; ratio=0.75; overlap=\"9:prism\"; ",
             [io_lib:format("~p [shape=box] ", [App])
              || App <- libapps(AllApps -- [kernel,stdlib])],
             [[io_lib:format("~p->~p ", [App,Dep])
               || Dep <- DepList -- [kernel, stdlib]]
              || {App, DepList, _} <- Deps],
            "}"],
    file:write_file("app-deps.dot", Bytes),
    os:cmd("dot app-deps.dot -Tpng -o app-deps.png").

libapps([]) -> [];
libapps([{App,library}|Apps]) -> [App|libapps(Apps)];
libapps([{_,_}|Apps]) -> libapps(Apps);
libapps([App|Apps]) ->
    Dir = case code:lib_dir(App) of
        {error, _} -> ""; % not an OTP app
        DirPath -> DirPath
    end,
    Path = filename:join([Dir, "ebin", atom_to_list(App)++".app"]),
    case lists:prefix(code:lib_dir(), Path) of
        false ->
            [App|libapps(Apps)]; % not OTP app, we don't care
        true -> % deps of OTP deps: we don't care either.
            {ok, [{_,App,Props}]} = file:consult(Path),
            case apptype(Props) of
                library -> [App | libapps(Apps)];
                regular -> libapps(Apps)
            end
    end.
