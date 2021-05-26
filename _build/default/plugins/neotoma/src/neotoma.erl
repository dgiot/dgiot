-module(neotoma).
-author("Sean Cribbs <seancribbs@gmail.com>").
-export([file/1, file/2, bootstrap/0]).
-export([main/1]).

-define(ALL_COMBINATORS, [p_eof, p_optional, p_not, p_assert, p_seq,
        p_choose, p_zero_or_more, p_one_or_more, p_label, p_scan,
        p_string, p_anything, p_charclass, p_regexp, line, column]).

-type option() :: {module, atom()} | {output, file:filename()} |  {transform_module, atom()} |
                  {neotoma_priv_dir, file:filename()}.

%% @doc Handler function for escript.
-spec main(list()) -> ok | no_return().
main([]) ->
    io:format("Usage: neotoma filename [-module output_module] [-output output_dir] [-transform_module transform_module]\n");
main([Filename | Args]) ->
    %% code:priv_dir is unreliable when called in escript context, but
    %% escript:script_name does what we want.
    PrivDir = filename:join([filename:dirname(escript:script_name()), "priv"]),
    file(Filename, [{neotoma_priv_dir, PrivDir} | parse_options(Args)]).

%% @doc Generates a parser from the specified file.
%% @equiv file(Filename, [])
-spec file(file:filename()) -> ok | {error, atom()}.
file(InputGrammar) ->
    file(InputGrammar, []).

%% @doc Generates a parser from the specified file with the given options.
-spec file(file:filename(), [option()]) -> ok | {error, atom()}.
file(InputGrammar, Options) ->
    Basename = filename:basename(InputGrammar, ".peg"),
    InputDir = filename:dirname(InputGrammar),
    ModuleName = proplists:get_value(module, Options, list_to_atom(Basename)),
    OutputDir = proplists:get_value(output, Options, InputDir),
    OutputFilename = filename:join(OutputDir, atom_to_list(ModuleName) ++ ".erl"),
    TransformModule = proplists:get_value(transform_module, Options, false),
    validate_params(filename:absname(InputGrammar),
                    ModuleName,
                    TransformModule,
                    filename:absname(OutputFilename)),
    Parsed = parse_grammar(InputGrammar),
    Rules = proplists:get_value(rules, Parsed),
    Root = proplists:get_value(root, Parsed),
    Code = proplists:get_value(code, Parsed),
    GenTransform = proplists:get_value(transform, Parsed),
    Combinators = proplists:get_value(combinators, Parsed, ?ALL_COMBINATORS),
    ModuleAttrs = generate_module_attrs(ModuleName, Combinators),
    EntryFuns = generate_entry_functions(Root),
    TransformFun = create_transform(TransformModule, OutputDir, GenTransform),
    PrivDir = proplists:get_value(neotoma_priv_dir, Options, code:priv_dir(neotoma)),
    {ok, PegIncludes} = file:read_file(filename:join([PrivDir, "peg_includes.hrl"])),
    file:write_file(OutputFilename, [ModuleAttrs, "\n", Code, "\n", EntryFuns, "\n", Rules, "\n", TransformFun, "\n", PegIncludes]).

-spec validate_params(file:filename(),atom(),atom(),file:filename()) -> 'ok'.
validate_params(InputGrammar, _, _, OutputFile) when InputGrammar =:= OutputFile ->
    throw({badarg, "Input and output file are the same!"});
validate_params(_,_, false, _) -> ok;
validate_params(_,_, TransformModule, _) when not is_atom(TransformModule) ->
    throw({badarg, "transform_module option must be an atom"});
validate_params(_,Basename, TransformModule, _) when Basename =:= TransformModule ->
    throw({badarg, "Transform module named same as parser module!"});
validate_params(_,_, TransformModule, OutputFile) ->
    OutMod = list_to_atom(filename:basename(OutputFile, ".erl")),
    case OutMod of
        TransformModule -> throw({badarg, "Transform module file same as parser output file!"});
        _ -> ok
    end.

-spec generate_module_attrs(atom(), [atom()]) -> iolist().
generate_module_attrs(ModName, Combinators) ->
    ["-module(", atom_to_list(ModName) ,").\n",
     "-export([parse/1,file/1]).\n",
     [ generate_combinator_macro(C) || Combinators /= undefined, C <- Combinators ],
     "\n"
     ].

generate_combinator_macro(C) ->
    ["-define(", atom_to_list(C), ",true).\n"].

-spec generate_entry_functions({iodata(),_}) -> iolist().
generate_entry_functions(Root) ->
    {RootRule,_} = Root,
     ["-spec file(file:name()) -> any().\n",
     "file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.\n\n",
     "-spec parse(binary() | list()) -> any().\n",
     "parse(List) when is_list(List) -> parse(unicode:characters_to_binary(List));\n",
     "parse(Input) when is_binary(Input) ->\n",
     "  _ = setup_memo(),\n",
     "  Result = case '",RootRule,"'(Input,{{line,1},{column,1}}) of\n",
     "             {AST, <<>>, _Index} -> AST;\n",
     "             Any -> Any\n"
     "           end,\n",
     "  release_memo(), Result.\n"].

-spec parse_grammar(file:filename()) -> any().
parse_grammar(InputFile) ->
    case neotoma_parse:file(InputFile) of
        {fail, Index} ->
            throw({grammar_error, {fail, Index}});
        {Parsed, Remainder, Index} ->
            io:format("WARNING: Grammar parse ended unexpectedly at ~p, generated parser may be incorrect.~nRemainder:~n~p",
                      [Index, Remainder]),
            Parsed;
        L when is_list(L) -> L;
        _ -> throw({error, {unknown, grammar, InputFile}})
    end.

-spec create_transform(atom() | boolean(),file:filename(),_) -> iolist().
create_transform(_,_,[]) -> [];
create_transform(false,_,_) ->
    "transform(_,Node,_Index) -> Node.";
create_transform(ModName,Dir,_) when is_atom(ModName) ->
    XfFile = filename:join(Dir, atom_to_list(ModName) ++ ".erl"),
    case filelib:is_regular(XfFile) of
        true -> io:format("'~s' already exists, skipping generation.~n", [XfFile]);
        false -> generate_transform_stub(XfFile, ModName)
    end,
    ["transform(Symbol,Node,Index) -> ",atom_to_list(ModName),":transform(Symbol, Node, Index)."].

-spec generate_transform_stub(file:filename(), atom()) -> 'ok' | {'error',atom()}.
generate_transform_stub(XfFile,ModName) ->
    Data = ["-module(",atom_to_list(ModName),").\n",
            "-export([transform/3]).\n\n",
            "%% Add clauses to this function to transform syntax nodes\n",
            "%% from the parser into semantic output.\n",
            "transform(Symbol, Node, _Index) when is_atom(Symbol) ->\n  Node."],
    file:write_file(XfFile, Data).

%% @doc Bootstraps the neotoma metagrammar.  Intended only for internal development!
%% @equiv file("src/neotoma_parse.peg")
-spec bootstrap() -> 'ok'.
bootstrap() ->
    file("priv/neotoma_parse.peg", [{output, "src/"}, {neotoma_priv_dir, "priv"}]).

%% @doc Parses arguments passed to escript
-spec parse_options(list()) -> list().
parse_options(["-module", ModName | Rest]) ->
    [{module, list_to_atom(ModName)} | parse_options(Rest)];
parse_options(["-output", Dir | Rest]) ->
    [{output, Dir} | parse_options(Rest)];
parse_options(["-transform_module", ModName | Rest]) ->
    [{transform_module, list_to_atom(ModName)} | parse_options(Rest)];
parse_options([]) ->
    [].
