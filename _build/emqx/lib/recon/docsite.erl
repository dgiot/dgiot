#!/usr/bin/env escript
%% -*- erlang -*-
%%!
%% This script takes the EDoc output page, and tries to bring them to
%% a more modern format suitable for a recon site.
%% Run with `escript docsite.erl' or as `./docsite.erl'. The script
%% will call the `./rebar doc' command and generate a static site in
%% the site/ directory, which can then be shoved wherever.
-module(docsite).
-mode(compile).
-export([main/0, main/1]).
-define(DOCPATH, "doc/").
-define(SITEPATH, "site/").

main() -> main([]).

main(_) ->
    ok = filelib:ensure_dir(?SITEPATH), % output directory
    Output = os:cmd("rebar3 edoc"), % build docs
    {match, _} = re:run(Output, "Running edoc"),
    Overview = ?DOCPATH "overview-summary.html",
    Modules = filelib:wildcard(filename:join(?DOCPATH, "recon*.html")),
    [Pre,Post] = base(["index.html" | Modules]),
    [write(?SITEPATH,
           case File of
               Overview -> "index.html";
               _ -> File
           end,
           [Pre,content(File),Post])
     || File <- [Overview|Modules]],
    halt(0).

content(File) ->
    filter(
     lists:reverse(
      lists:foldl(fun("<h1>"++_ = Line, init) -> {keep, [Line]};
                     ("<hr>"++_, {keep, Lines}) -> Lines;
                     (Line, {keep, Lines}) -> {keep, [Line|Lines]};
                     (_, Lines) -> Lines
                  end,
                  init,
                  lines(File))
     )
    ).

filter(Content) ->
    %% We filter using regexes. This is terrible but so easy.
    lists:foldl(fun({RE,Replacement},Text) ->
                    re:replace(Text, RE, Replacement, [global])
                end,
                unicode:characters_to_binary(Content, latin1, utf8),
                [{"h5>", "h6>"}, {"<h5", "<h6"},
                 {"h4>", "h5>"}, {"<h4", "<h5"},
                 {"h3>", "h4>"}, {"<h3", "<h4"},
                 {"h2>", "h3>"}, {"<h2", "<h3"},
                 {"h1>", "h2>"}, {"<h1", "<h2"},
                 {"tt>", "code>"}, {"<b>","<strong>"},
                 {"</b>", "</strong>"}]).


lines(File) ->
    {ok, IoDevice} = file:open(File, [read, raw]),
    get_lines(IoDevice).

get_lines(Device) ->
    case file:read_line(Device) of
        {ok, Data} -> [Data | get_lines(Device)];
        eof -> []
    end.

base(Files) ->
    Modules = [{filename:basename(F,".html"),
                filename:basename(F)} || F <- Files],
    [["
<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\">
    <link href='http://fonts.googleapis.com/css?family=Titillium+Web:400,300italic,600italic,700,600,700italic' rel='stylesheet' type='text/css'>
    <link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,700italic,400italic' rel='stylesheet' type='text/css'>
    <link rel=\"stylesheet\" href=\"screen.css\" media=\"screen, projection\" />
    <title>Recon Library</title>
  </head>
  <body>
    <header>
        <h1>Recon</h1>
        <nav>
          <ul>
",
[["<li><a href=\"", Path, "\">", Name, "</a>"] || {Name,Path} <- Modules],
"
          </ul>
        </nav>
    </header>
    <article>
    "],
"
    </article>
  </body>
</html>
"].

write(DropPath, OriginalPath, Content) ->
    NewPath = filename:join(DropPath, filename:basename(OriginalPath)),
    file:write_file(NewPath, Content).
