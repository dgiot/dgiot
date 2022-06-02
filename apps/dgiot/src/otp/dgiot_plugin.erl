%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_plugin).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot.hrl").
%% API
-export([compile_module/1, compile_module/2, check_module/1, check_module/2, get_changed_modules/0, get_modules/1,
    get_modules/2, reload_module/1, reload_modules/0, reload_modules/1, reload_plugin/1, reload_plugin/2, all_changed/0,
    applications/1, applications/0]).

%% dynamic_compile
-export([load_from_string/1, load_from_string/2, from_string/1, from_string/2]).

-import(lists, [reverse/1, keyreplace/4]).

check_module(App) ->
    Sys_App =
        case dgiot_data:get({dgiot, sys_app}) of
            not_find ->
                ?SYS_APP;
            Plugins ->
                Plugins
        end,
    lists:member(App, Sys_App).

applications() ->
    applications(fun(_, _, _) -> true end).

applications(Filter) ->
    StartApps = application:which_applications(),
    applications(application:loaded_applications(), [], StartApps, Filter).


applications([], Acc, _, _) -> Acc;
applications([{App, Desc, Ver} | Apps], Acc, StartApps, Filter) ->
    case check_module(App) of
        true ->
            applications(Apps, Acc, StartApps, Filter);
        false ->
            Active = lists:keyfind(App, 1, StartApps) =/= false,
            Acc1 =
                case Filter(App, Ver, Active) of
                    {true, In} ->
                        [In#{
                            <<"desc">> => unicode:characters_to_binary(Desc),
                            <<"version">> => list_to_binary(Ver),
                            <<"active">> => Active
                        } | Acc];
                    true ->
                        [#{
                            <<"app">> => App,
                            <<"desc">> => unicode:characters_to_binary(Desc),
                            <<"version">> => list_to_binary(Ver),
                            <<"active">> => Active
                        } | Acc];
                    false ->
                        Acc
                end,
            applications(Apps, Acc1, StartApps, Filter)
    end.

compile_module(Code) ->
    compile_module(Code, []).

compile_module(Code, Opts) ->
    case re:run(Code, <<"-module\\(([^\\)]+)\\)\\.">>, [{capture, all_but_first, list}]) of
        {match, [ModuleName]} ->
            CompileOpts = [binary, return_errors, return_warnings],
            Result =
                case proplists:get_value(dir, Opts) of
                    undefined ->
                        case catch from_string(Code, CompileOpts) of
                            {'EXIT', {{badmatch, {error, {Line, erl_parse, Reason}}}, _}} ->
                                ?LOG(info, "~p~n", [Reason]),
                                {error, [{ModuleName, [{Line, erl_parse, Reason}]}], []};
                            {failed_to_read_include_file, Reason, Filename, IncludeSearchPath} ->
                                {error, [{ModuleName, [{Filename, ModuleName, {failed_to_read_include_file, IncludeSearchPath, Reason}}]}], []};
                            {incomplete_term, Text, Line} ->
                                {error, [{ModuleName, [{Line, incomplete_term, Text}]}], []};
                            Other ->
                                Other
                        end;
                    Root ->
                        Src = lists:concat([Root, "/priv/", ModuleName, ".erl"]),
                        filelib:ensure_dir(Src), file:write_file(Src, Code),
                        case compile:file(Src, CompileOpts) of
                            {error, Errors1, Warnings1} ->
                                {error, Errors1, Warnings1};
                            {ok, Module1, Bin, Warnings1} ->
                                filelib:ensure_dir(lists:concat([Root, "/ebin/", Module1, ".beam"])),
                                file:write_file(lists:concat([Root, "/ebin/", Module1, ".beam"]), Bin),
                                {ok, Module1, Bin, Warnings1}
                        end
                end,
            case Result of
                {error, Errors, Warnings} ->
                    {error, format_msg([{warning, Warnings}, {error, Errors}])};
                {ok, Module, Beam, Warnings} ->
                    {ok, Module, Beam, format_msg([{warning, Warnings}])}
            end;
        nomatch ->
            {error, [<<"not find -module">>]}
    end.


get_changed_modules() ->
    Mods = code:all_loaded(),
    Format =
        fun(Path) ->
            Map = filter(<<".*?">>, <<".*?">>, Path),
            Map#{is_changed => true}
        end,
    [Format(Path) || {Mod, Path} <- Mods, filter(<<".*?">>, <<".*?">>, Path) =/= nomatch, is_changed(Mod)].

get_modules(App) ->
    get_modules(App, <<".*?">>).
get_modules(App, Version) ->
    Mods = code:all_loaded(),
    Format =
        fun(Mod, Path) ->
            Map = filter(App, Version, Path),
            Map#{is_changed => is_changed(Mod)}
        end,
    [Format(Mod, Path) || {Mod, Path} <- Mods, filter(App, Version, Path) =/= nomatch].


reload_modules() ->
    Modules = all_changed(),
    reload_modules(Modules).

%% @doc Reload modules
-spec(reload_modules([atom()]) -> [{module, atom()} | {error, term()}]).
reload_modules(Modules) ->
    [reload_module(M) || M <- Modules].

%% @doc Reload a module
-spec(reload_module(atom()) -> {error, term()} | {module(), atom()}).
reload_module(Module) when is_atom(Module) ->
    code:purge(Module), code:load_file(Module).


reload_plugin(App) when is_atom(App) ->
    reload_plugin(atom_to_binary(App, utf8));
reload_plugin(App) when is_binary(App) ->
    reload_plugin(App, <<".*?">>).
reload_plugin(App, Version) ->
    Mods = code:all_loaded(),
    Fun =
        fun(M) ->
            case reload_module(M) of
                {module, M} ->
                    true;
                {error, Reason} ->
                    ?LOG(error, "~p->~p", [M, Reason]),
                    false
            end
        end,
    [Mod || {Mod, Path} <- Mods, filter(App, Version, Path) =/= nomatch, is_changed(Mod), Fun(Mod)].


filter(App, Version, Path) when is_list(Path); is_binary(Path) ->
    Re = <<"lib/(", App/binary, ")-(", Version/binary, ")/ebin/(.*beam)">>,
    case re:run(Path, Re, [{capture, all_but_first, binary}, global]) of
        {match, [[App1, Ver, ModFile]]} ->
            #{app => App1, version => Ver, path => ModFile};
        nomatch ->
            nomatch
    end;
filter(_, _, _) ->
    nomatch.


check_module(Check, Acc0) ->
    Fun =
        fun({App, _Desc, Vsn}, Acc) ->
            Dir = lists:concat(["lib/", App, "-", Vsn, "/ebin"]),
            case not check_module(App) andalso file:list_dir(Dir) of
                {ok, FS} ->
                    lists:foldl(
                        fun(FileName, Acc1) ->
                            case filename:extension(FileName) == ".beam" of
                                true ->
                                    Mod = list_to_atom(filename:basename(FileName, ".beam")),
                                    Check({App, Vsn, Mod}, Acc1);
                                false ->
                                    Acc1
                            end
                        end, Acc, FS);
                _ ->
                    Acc
            end
        end,
    Apps = dgiot:check_dgiot_app(),
    lists:foldl(Fun, Acc0, Apps).

all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].


is_changed(M) when is_atom(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
        false
    end.


module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam), Vsn;

module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs), Vsn.



format_msg(Err) ->
    format_msg(Err, []).
format_msg([], Acc) -> Acc;
format_msg([{error, Msg} | Other], Acc) ->
    format_msg(Other, format_error(error, Msg) ++ Acc);
format_msg([{warning, Msg} | Other], Acc) ->
    format_msg(Other, format_error(warning, Msg) ++ Acc).


format_error(Type, Errs) ->
    lists:foldr(
        fun({File, Msgs}, Acc) ->
            Path = filename:basename(File),
            lists:foldr(
                fun({Line, Mod, Term}, Acc1) ->
                    case Type of
                        error ->
                            [format_error(Path, Line, Mod, Term) | Acc1];
                        warning ->
                            [format_warning(Path, Line, Mod, Term) | Acc1]
                    end
                end, Acc, Msgs)
        end, [], Errs).

format_error(File, Line, Mod, Term) ->
    Err = dgiot_utils:format("~s Line:~w ~w ~p", [File, Line, Mod, Term]),
    <<"Error ", Err/binary>>.
format_warning(File, Line, Mod, Term) ->
    Warn = dgiot_utils:format("~s Line:~w ~w ~p", [File, Line, Mod, Term]),
    <<"Warn ", Warn/binary>>.


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%   Compile module from string and load into VM
%%--------------------------------------------------------------------
load_from_string(CodeStr) ->
    load_from_string(CodeStr, []).

load_from_string(CodeStr, CompileFormsOptions) ->
    {Mod, Bin} = from_string(CodeStr, CompileFormsOptions),
    code:load_binary(Mod, [], Bin).

%%--------------------------------------------------------------------
%% Function:
%% Description:
%%   Returns a binary that can be used with
%%           code:load_binary(Module, ModuleFilenameForInternalRecords, Binary).
%%--------------------------------------------------------------------
from_string(CodeStr) ->
    from_string(CodeStr, []).


from_string(CodeStr, CompileFormsOptions) when is_binary(CodeStr) ->
    from_string(string:trim(unicode:characters_to_list(CodeStr)), CompileFormsOptions);

% takes Options as for compile:forms/2
from_string(CodeStr, CompileFormsOptions) ->
    %% Initialise the macro dictionary with the default predefined macros,
    %% (adapted from epp.erl:predef_macros/1
    Filename = "compiled_from_string",
    %%Machine  = list_to_atom(erlang:system_info(machine)),
    Ms0 = dict:new(),
    % Ms1    = dict:store('FILE',          {[], "compiled_from_string"}, Ms0),
    % Ms2    = dict:store('LINE',          {[], 1}, Ms1),  % actually we might add special code for this
    % Ms3    = dict:store('MODULE',        {[], undefined},              Ms2),
    % Ms4    = dict:store('MODULE_STRING', {[], undefined},              Ms3),
    % Ms5    = dict:store('MACHINE',       {[], Machine},                Ms4),
    % InitMD = dict:store(Machine,         {[], true},                   Ms5),
    InitMD = Ms0,

    %% From the docs for compile:forms:
    %%    When encountering an -include or -include_dir directive, the compiler searches for header files in the following directories:
    %%      1. ".", the current working directory of the file server;
    %%      2. the base name of the compiled file;
    %%      3. the directories specified using the i option. The directory specified last is searched first.
    %% In this case, #2 is meaningless.
    IncludeSearchPath = ["." | reverse([Dir || {i, Dir} <- CompileFormsOptions])],
    {RevForms, _OutMacroDict} = scan_and_parse(CodeStr ++ "\n", Filename, 1, [], InitMD, IncludeSearchPath),
    Forms = [{attribute, 0, file, {"compiled_from_string", 0}} | reverse([{eof, 0} | RevForms])],

    %% note: 'binary' is forced as an implicit option, whether it is provided or not.
    compile:forms(Forms, CompileFormsOptions).

%%====================================================================
%% Internal functions
%%====================================================================
%%% Code from Mats Cronqvist
%%% See http://www.erlang.org/pipermail/erlang-questions/2007-March/025507.html
%%%## 'scan_and_parse'
%%%
%%% basically we call the OTP scanner and parser (erl_scan and
%%% erl_parse) line-by-line, but check each scanned line for (or
%%% definitions of) macros before parsing.
%% returns {ReverseForms, FinalMacroDict}
scan_and_parse([], _CurrFilename, _CurrLine, RevForms, MacroDict, _IncludeSearchPath) ->
    {RevForms, MacroDict};

scan_and_parse(RemainingText, CurrFilename, CurrLine, RevForms, MacroDict, IncludeSearchPath) ->
    case scanner(RemainingText, CurrLine, MacroDict) of
        {tokens, NLine, NRemainingText, Toks} ->
            {ok, Form0} = erl_parse:parse_form(Toks),
            {Form, Forms} = normalize_record(Form0),
            scan_and_parse(NRemainingText, CurrFilename, NLine, [Form | Forms ++ RevForms], MacroDict, IncludeSearchPath);
        {macro, NLine, NRemainingText, NMacroDict} ->
            scan_and_parse(NRemainingText, CurrFilename, NLine, RevForms, NMacroDict, IncludeSearchPath);
        {include, NLine, NRemainingText, IncludeFilename} ->
            IncludeFileRemainingTextents = read_include_file(IncludeFilename, IncludeSearchPath),
            %%io:format("include file ~p contents: ~n~p~nRemainingText = ~p~n", [IncludeFilename,IncludeFileRemainingTextents, RemainingText]),
            %% Modify the FILE macro to reflect the filename
            %%IncludeMacroDict = dict:store('FILE', {[],IncludeFilename}, MacroDict),
            IncludeMacroDict = MacroDict,

            %% Process the header file (inc. any nested header files)
            {RevIncludeForms, IncludedMacroDict} = scan_and_parse(IncludeFileRemainingTextents, IncludeFilename, 1, [], IncludeMacroDict, IncludeSearchPath),
            %io:format("include file results = ~p~n", [R]),
            %% Restore the FILE macro in the NEW MacroDict (so we keep any macros defined in the header file)
            %%NMacroDict = dict:store('FILE', {[],CurrFilename}, IncludedMacroDict),
            NMacroDict = IncludedMacroDict,

            %% Continue with the original file
            scan_and_parse(NRemainingText, CurrFilename, NLine, RevIncludeForms ++ RevForms, NMacroDict, IncludeSearchPath);
        done ->
            scan_and_parse([], CurrFilename, CurrLine, RevForms, MacroDict, IncludeSearchPath)
    end.

scanner(Text, Line, MacroDict) ->
    case erl_scan:tokens([], Text, Line) of
        {done, {ok, Toks, NLine}, LeftOverChars} ->
            case pre_proc(Toks, MacroDict) of
                {tokens, NToks} -> {tokens, NLine, LeftOverChars, NToks};
                {macro, NMacroDict} -> {macro, NLine, LeftOverChars, NMacroDict};
                {include, Filename} -> {include, NLine, LeftOverChars, Filename}
            end;
        {more, _Continuation} ->
            %% This is supposed to mean "term is not yet complete" (i.e. a '.' has
            %% not been reached yet).
            %% However, for some bizarre reason we also get this if there is a comment after the final '.' in a file.
            %% So we check to see if Text only consists of comments.
            case is_only_comments(Text) of
                true ->
                    done;
                false ->
                    throw({incomplete_term, Text, Line})
            end
    end.

is_only_comments(Text) -> is_only_comments(Text, not_in_comment).

is_only_comments([], _) -> true;
is_only_comments([$  | T], not_in_comment) ->
    is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\t | T], not_in_comment) ->
    is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\n | T], not_in_comment) ->
    is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$% | T], not_in_comment) -> is_only_comments(T, in_comment);     % found start of a comment
is_only_comments(_, not_in_comment) -> false;
% found any significant char NOT in a comment
is_only_comments([$\n | T], in_comment) -> is_only_comments(T, not_in_comment); % found end of a comment
is_only_comments([_ | T], in_comment) -> is_only_comments(T, in_comment).     % skipping over in-comment chars

%%%## 'pre-proc'
%%%
%%% have to implement a subset of the pre-processor, since epp insists
%%% on running on a file.
%%% only handles 2 cases;
%% -define(MACRO, something).
%% -define(MACRO(VAR1,VARN),{stuff,VAR1,more,stuff,VARN,extra,stuff}).
pre_proc([{'-', _}, {atom, _, define}, {'(', _}, {_, _, Name} | DefToks], MacroDict) ->
    false = dict:is_key(Name, MacroDict),
    case DefToks of
        [{',', _} | Macro] ->
            {macro, dict:store(Name, {[], macro_body_def(Macro, [])}, MacroDict)};
        [{'(', _} | Macro] ->
            {macro, dict:store(Name, macro_params_body_def(Macro, []), MacroDict)}
    end;

pre_proc([{'-', _}, {atom, _, include}, {'(', _}, {string, _, Filename}, {')', _}, {dot, _}], _MacroDict) ->
    {include, Filename};

pre_proc(Toks, MacroDict) ->
    {tokens, subst_macros(Toks, MacroDict)}.

macro_params_body_def([{')', _}, {',', _} | Toks], RevParams) ->
    {reverse(RevParams), macro_body_def(Toks, [])};
macro_params_body_def([{var, _, Param} | Toks], RevParams) ->
    macro_params_body_def(Toks, [Param | RevParams]);
macro_params_body_def([{',', _}, {var, _, Param} | Toks], RevParams) ->
    macro_params_body_def(Toks, [Param | RevParams]).

macro_body_def([{')', _}, {dot, _}], RevMacroBodyToks) ->
    reverse(RevMacroBodyToks);
macro_body_def([Tok | Toks], RevMacroBodyToks) ->
    macro_body_def(Toks, [Tok | RevMacroBodyToks]).

subst_macros(Toks, MacroDict) ->
    reverse(subst_macros_rev(Toks, MacroDict, [])).

%% returns a reversed list of tokes
subst_macros_rev([{'?', _}, {_, LineNum, 'LINE'} | Toks], MacroDict, RevOutToks) ->
    %% special-case for ?LINE, to avoid creating a new MacroDict for every line in the source file
    subst_macros_rev(Toks, MacroDict, [{integer, LineNum, LineNum}] ++ RevOutToks);

subst_macros_rev([{'?', _}, {_, _, Name}, {'(', _} = Paren | Toks], MacroDict, RevOutToks) ->
    case dict:fetch(Name, MacroDict) of
        {[], MacroValue} ->
            %% This macro does not have any vars, so ignore the fact that the invocation is followed by "(...stuff"
            %% Recursively expand any macro calls inside this macro's value
            %% TODO: avoid infinite expansion due to circular references (even indirect ones)
            RevExpandedOtherMacrosToks = subst_macros_rev(MacroValue, MacroDict, []),
            subst_macros_rev([Paren | Toks], MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks);
        ParamsAndBody ->
            %% This macro does have vars.
            %% Collect all of the passe arguments, in an ordered list
            {NToks, Arguments} = subst_macros_get_args(Toks, []),
            %% Expand the varibles
            ExpandedParamsToks = subst_macros_subst_args_for_vars(ParamsAndBody, Arguments),
            %% Recursively expand any macro calls inside this macro's value
            %% TODO: avoid infinite expansion due to circular references (even indirect ones)
            RevExpandedOtherMacrosToks = subst_macros_rev(ExpandedParamsToks, MacroDict, []),
            subst_macros_rev(NToks, MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks)
    end;

subst_macros_rev([{'?', _}, {_, _, Name} | Toks], MacroDict, RevOutToks) ->
    %% This macro invocation does not have arguments.
    %% Therefore the definition should not have parameters
    {[], MacroValue} = dict:fetch(Name, MacroDict),

    %% Recursively expand any macro calls inside this macro's value
    %% TODO: avoid infinite expansion due to circular references (even indirect ones)
    RevExpandedOtherMacrosToks = subst_macros_rev(MacroValue, MacroDict, []),
    subst_macros_rev(Toks, MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks);

subst_macros_rev([Tok | Toks], MacroDict, RevOutToks) ->
    subst_macros_rev(Toks, MacroDict, [Tok | RevOutToks]);
subst_macros_rev([], _MacroDict, RevOutToks) -> RevOutToks.

subst_macros_get_args([{')', _} | Toks], RevArgs) ->
    {Toks, reverse(RevArgs)};
subst_macros_get_args([{',', _}, {var, _, ArgName} | Toks], RevArgs) ->
    subst_macros_get_args(Toks, [ArgName | RevArgs]);
subst_macros_get_args([{var, _, ArgName} | Toks], RevArgs) ->
    subst_macros_get_args(Toks, [ArgName | RevArgs]).

subst_macros_subst_args_for_vars({[], BodyToks}, []) ->
    BodyToks;
subst_macros_subst_args_for_vars({[Param | Params], BodyToks}, [Arg | Args]) ->
    NBodyToks = keyreplace(Param, 3, BodyToks, {var, 1, Arg}),
    subst_macros_subst_args_for_vars({Params, NBodyToks}, Args).

read_include_file(Filename, IncludeSearchPath) ->
    case file:path_open(IncludeSearchPath, Filename, [read, raw, binary]) of
        {ok, IoDevice, FullName} ->
            {ok, Data} = file:read(IoDevice, filelib:file_size(FullName)),
            file:close(IoDevice),
            binary_to_list(Data);
        {error, Reason} ->
            throw({failed_to_read_include_file, Reason, Filename, IncludeSearchPath})
    end.

normalize_record({attribute, La, record, {Record, Fields}} = Form) ->
    case epp:normalize_typed_record_fields(Fields) of
        {typed, NewFields} ->
            {{attribute, La, record, {Record, NewFields}},
                [{attribute, La, type,
                    {{record, Record}, Fields, []}}]};
        not_typed ->
            {Form, []}
    end;
normalize_record(Form) ->
    {Form, []}.

