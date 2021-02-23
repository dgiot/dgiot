%% -------------------------------------------------------------------
%%
%% cuttlefish_escript: used by sh scripts to parse configs
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(cuttlefish_escript).

-define(STDOUT(Str, Args), io:format(Str ++ "~n", Args)).
-define(FORMAT(Str, Args), io_lib:format(Str, Args)).
-define(FORMAT_TEMPLATE, [time," [",level,"] ",msg,"\n"]).
-export([main/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-include("cuttlefish.hrl").
-define(ENVS, [{"EMQX_NODE_NAME", "node.name"},
               {"EMQX_NODE_COOKIE", "node.cookie"},
               {"EMQX_MAX_PORTS", "node.max_ports"},
               {"EMQX_MAX_PACKET_SIZE", "mqtt.max_packet_size"},
               {"EMQX_TCP_PORT", "listener.tcp.external"},
               {"EMQX_SSL_PORT", "listener.ssl.external"},
               {"EMQX_WS_PORT", "listener.ws.external"},
               {"EMQX_WSS_PORT", "listener.wss.external"}]).

cli_options() ->
%% Option Name, Short Code, Long Code, Argument Spec, Help Message
[
 {help,         $h, "help",        undefined,          "Print this usage page"},
 {etc_dir,      $e, "etc_dir",     {string, "/etc"},   "etc dir"},
 {dest_dir,     $d, "dest_dir",    string,             "specifies the directory to write the config file to"},
 {dest_file,    $f, "dest_file",   {string, "app"},    "the file name to write"},
 {schema_dir,   $s, "schema_dir",  string,             "a directory containing .schema files"},
 {schema_file,  $i, "schema_file", string,             "individual schema file, will be processed in command line order, after -s"},
 {conf_file,    $c, "conf_file",   string,             "a cuttlefish conf file, multiple files allowed"},
 {app_config,   $a, "app_config",  string,             "the advanced erlangy app.config"},
 {log_level,    $l, "log_level",   {string, "notice"}, "log level for cuttlefish output"},
 {print_schema, $p, "print",       undefined,          "prints schema mappings on stderr"},
 {max_history,  $m, "max_history", {integer, 3},       "the maximum number of generated config files to keep"}
].

%% LOL! I wanted this to be halt 0, but honestly, if this escript does anything
%% except return the path to a generated config file, it should return a non-zero
%% return code
print_help() ->
    getopt:usage(cli_options(),
                 escript:script_name()),
    stop_deactivate().

parse_and_command(Args) ->
    {ParsedArgs, Extra} = case getopt:parse(cli_options(), Args) of
        {ok, {P, H}} -> {P, H};
        _ -> {[help], []}
    end,
    {Command, ExtraArgs} = case {lists:member(help, ParsedArgs), Extra} of
        {false, []} -> {generate, []};
        {false, [Cmd|E]} -> {list_to_atom(Cmd), E};
        _ -> {help, []}
    end,
    {Command, ParsedArgs, ExtraArgs}.

%% @doc main method for generating erlang term config files
main(Args) ->
    {Command, ParsedArgs, Extra} = parse_and_command(Args),

    SuggestedLogLevel = list_to_atom(proplists:get_value(log_level, ParsedArgs)),
    LogLevel = case lists:member(SuggestedLogLevel, [debug, info, notice, warning,
                                    error, critical, alert, emergency]) of
                    true -> SuggestedLogLevel;
                    _ -> notice
               end,
    logger:remove_handler(default),
    logger:add_handler(cuttlefish, logger_std_h,
                       #{config => #{type =>standard_error},
                         formatter => {logger_formatter,
                                        #{legacy_header => false,
                                          single_line => true,
                                          template => ?FORMAT_TEMPLATE}},
                         filter_default => log,
                         filters => [],
                         level => all
                        }),

    logger:set_primary_config(level, LogLevel),
    case Command of
        help ->
            print_help();
        generate ->
            generate(ParsedArgs);
        effective ->
            effective(ParsedArgs);
        describe ->
            describe(ParsedArgs, Extra);
        _Other ->
            print_help()
    end.

%% This shows the effective configuration, including defaults
effective(ParsedArgs) ->
    ?logger:debug("cuttlefish `effective`", []),
    EtcDir = proplists:get_value(etc_dir, ParsedArgs),

    %% Should we even show this?
    {AppConfigExists, ExistingAppConfigName} = check_existence(EtcDir, "app.config"),
    {VMArgsExists, ExistingVMArgsName} = check_existence(EtcDir, "vm.args"),

    case {AppConfigExists, VMArgsExists} of
        {false, false} ->
            AdvancedConfigFile = filename:join(EtcDir, "advanced.config"),
            AdvConfig = case filelib:is_file(AdvancedConfigFile) of
                true ->
                    ?logger:debug("~s/advanced.config detected, overlaying proplists", [EtcDir]),
                    case file:consult(AdvancedConfigFile) of
                        {ok, [AdvancedConfig]} ->
                            AdvancedConfig;
                        {error, Error} ->
                            ?logger:error("Error parsing advanced.config: ~s", [file:format_error(Error)]),
                            stop_deactivate()
                    end;
                _ ->
                    []
            end,

            EffectiveConfig = cuttlefish_effective:build(
                load_conf(ParsedArgs),
                load_schema(ParsedArgs),
                AdvConfig),
            _ = [ ?STDOUT(Line, []) || Line <- EffectiveConfig],
            ok;
        _ ->
            ?STDOUT("Disabling cuttlefish, legacy configuration files found:", []),
            case AppConfigExists of
                true ->
                    ?STDOUT("  ~s", [ExistingAppConfigName]);
                _ ->
                    ok
            end,
            case VMArgsExists of
                true ->
                    ?STDOUT("  ~s", [ExistingVMArgsName]);
                _ ->
                    ok
            end,
            ?STDOUT("Effective config is only visible for cuttlefish conf files.", [])
    end,
    ok.

%% This is the function that dumps the docs for a single setting
describe(_ParsedArgs, []) ->
    %% No query, you get nothing.
    ?STDOUT("cuttlefish's describe command required a variable to query.", []),
    ?STDOUT("Try `describe setting.name`", []),
    stop_deactivate();
describe(ParsedArgs, [Query|_]) when is_list(Query) ->
    QDef = cuttlefish_variable:tokenize(Query),

    ?logger:debug("cuttlefish describe '~s'", [Query]),
    {_, Mappings, _} = load_schema(ParsedArgs),

    FindResults = fun(QueryVar) ->
    lists:filter(
        fun(X) ->
            cuttlefish_variable:is_fuzzy_match(QueryVar, cuttlefish_mapping:variable(X))
        end,
        Mappings)
    end,

    case FindResults(QDef) of
        [] ->
            ?STDOUT("Variable '~s' not found", [Query]);
        [Match|_] ->
            ?STDOUT("Documentation for ~s", [cuttlefish_variable:format(cuttlefish_mapping:variable(Match))]),
            _ = case {cuttlefish_mapping:doc(Match), cuttlefish_mapping:see(Match)} of
                {[], []} ->
                    ok;
                {[], See} ->
                    _ = [ begin
                          M = hd(FindResults(S)),
                          [ ?STDOUT("~s", [Line]) || Line <- cuttlefish_mapping:doc(M)]
                    end || S <- See],
                    ok;
                {Docs, []} ->
                    [ ?STDOUT("~s", [Line]) || Line <- Docs];
                {Docs, See} ->
                    _ = [ ?STDOUT("~s", [Line]) || Line <- Docs],
                    ?STDOUT("See also:", []),
                    [?STDOUT("    ~s", [cuttlefish_variable:format(S)]) || S <- See]
            end,
            ?STDOUT("", []),
            ValidValues = [
                            ?FORMAT("~n     - ~s", [cuttlefish_conf:pretty_datatype(Type)]) ||
                              Type <- lists:flatten([cuttlefish_mapping:datatype(Match)]) ],
            ?STDOUT("   Valid Values: ~s", [ValidValues]),
            case cuttlefish_mapping:has_default(Match) of
                true ->
                    ?STDOUT("   Default Value : ~s",
                            [format_datatype(cuttlefish_mapping:default(Match),
                                             cuttlefish_mapping:datatype(Match))]);
                false ->
                    ?STDOUT("   No default set", [])
            end,
            Conf = load_conf(ParsedArgs),
            case lists:keyfind(QDef, 1, Conf) of
                false ->
                    ConfFile = proplists:get_value(conf_file, ParsedArgs),
                    ?STDOUT("   Value not set in ~s", [ConfFile]);
                {_, CValue} ->
                    ConfiguredValue = format_datatype(CValue, cuttlefish_mapping:datatype(Match)),
                    ?STDOUT("   Set Value     : ~s", [ConfiguredValue])
            end,
            ?STDOUT("   Internal key  : ~s", [cuttlefish_mapping:mapping(Match)])
    end,
    stop_deactivate().

-ifndef(TEST).
stop_deactivate() ->
    init:stop(1),
    timer:sleep(250),
    stop_deactivate().

stop_ok() ->
    init:stop(0).
-endif.

-ifdef(TEST).
%% In test mode we don't want to kill the test VM prematurely.
stop_deactivate() ->
    throw(stop_deactivate).

stop_ok() ->
    ok.
-endif.

generate(ParsedArgs) ->
    EtcDir = proplists:get_value(etc_dir, ParsedArgs),

    {AppConfigExists, ExistingAppConfigName} = check_existence(EtcDir, "app.config"),
    {VMArgsExists, ExistingVMArgsName} = check_existence(EtcDir, "vm.args"),

    %% If /etc/app.config exists, use it and disable cuttlefish
    %% even though cuttlefish is awesome
    FilesToUse = case {AppConfigExists, VMArgsExists} of
        {true, true} ->
            ?logger:info("~s and ~s exists, disabling cuttlefish.", [ExistingAppConfigName, ExistingVMArgsName]),
            ?logger:info("If you'd like to know more about cuttlefish, check your local library!", []),
            ?logger:info(" or see http://github.com/basho/cuttlefish", []),
            {ExistingAppConfigName, ExistingVMArgsName};
        {true, false} ->
            ?logger:info("~s exists, generating vm.args", [ExistingAppConfigName]),
            {_, NewVMArgs} = engage_cuttlefish(ParsedArgs),
            {ExistingAppConfigName, NewVMArgs};
        {false, true} ->
            ?logger:info("~s exists, generating app.config", [ExistingVMArgsName]),
            {NewAppConfig, _} = engage_cuttlefish(ParsedArgs),
            {NewAppConfig, ExistingVMArgsName};
        _ ->
            ?logger:info("No app.config or vm.args detected in ~s, activating cuttlefish", [EtcDir]),
            engage_cuttlefish(ParsedArgs)
    end,

    case FilesToUse of
        %% this is nice and all, but currently all error paths of engage_cuttlefish end with
        %% stop_deactivate() hopefully factor that to be cleaner.
        error ->
            stop_deactivate();
        {AppConf, VMArgs} ->
            %% Note: we have added a parameter '-vm_args' to this. It appears redundant
            %% but it is not! the erlang vm allows us to access all arguments to the erl
            %% command EXCEPT '-args_file', so in order to get access to this file location
            %% from within the vm, we need to pass it in twice.
            ?STDOUT(" -config ~s -args_file ~s -vm_args ~s ", [AppConf, VMArgs, VMArgs]),
            stop_ok()
    end.

load_schema(ParsedArgs) ->
    SchemaDir = proplists:get_value(schema_dir, ParsedArgs),

    SchemaDirFiles = case SchemaDir of
        undefined -> [];
        _ -> [ filename:join(SchemaDir, Filename)  || Filename <- filelib:wildcard("*.schema", SchemaDir)]
    end,
    IndividualSchemaFiles = proplists:get_all_values(schema_file, ParsedArgs),
    SchemaFiles = SchemaDirFiles ++ IndividualSchemaFiles,

    SortedSchemaFiles = lists:sort(fun(A,B) -> A < B end, SchemaFiles),
    case length(SortedSchemaFiles) of
        0 ->
            ?logger:debug("No Schema files found in specified", []),
            stop_deactivate();
        _ ->
            ?logger:debug("SchemaFiles: ~p", [SortedSchemaFiles])
    end,

    Schema = cuttlefish_schema:files(SortedSchemaFiles),
    case proplists:is_defined(print_schema, ParsedArgs) of
        true ->
            _ = print_schema(Schema),
            Schema;
        _ ->
            Schema
    end.

load_conf(ParsedArgs) ->
    ConfFiles = proplists:get_all_values(conf_file, ParsedArgs),
    ?logger:debug("ConfFiles: ~p", [ConfFiles]),
    case cuttlefish_conf:files(ConfFiles) of
        {errorlist, Errors} ->
            _ = [ ?logger:error(cuttlefish_error:xlate(E)) ||
                    {error, E} <- Errors],
            stop_deactivate(),
            {errorlist, Errors};
        GoodConf ->
            GoodConf
    end.

change_conf(Conf) ->
    change_conf(Conf, ?ENVS).

change_conf(Conf, []) ->
    Conf;
change_conf(Conf, [{Env, Key} | Envs]) ->
    case os:getenv(Env) of
        false -> change_conf(Conf, Envs);
        Value ->
            NKey = cuttlefish_variable:tokenize(Key),
            NConf = case lists:keyfind(NKey, 1, Conf) of
                false -> Conf ++ [{NKey, Value}];
                _ -> lists:keyreplace(NKey, 1, Conf, {NKey, Value})
            end,
            change_conf(NConf, Envs)
    end.



-spec writable_destination_path([proplists:property()]) -> file:filename() | error.
writable_destination_path(ParsedArgs) ->
    EtcDir = proplists:get_value(etc_dir, ParsedArgs),
    DestinationPath = proplists:get_value(dest_dir, ParsedArgs, filename:join(EtcDir, "generated")),
    AbsoluteDestPath = case DestinationPath of
                           [$/|_] -> DestinationPath;
                           _      -> filename:join(element(2,file:get_cwd()), DestinationPath)
                       end,
    %% Check Permissions
    case filelib:ensure_dir(filename:join(AbsoluteDestPath, "weaksauce.dummy")) of
        %% filelib:ensure_dir/1 requires a dummy filename in the argument,
        %% I think that is weaksauce, hence "weaksauce.dummy"
        ok ->
            AbsoluteDestPath;
        {error, E} ->
            ?logger:error(
                "Error creating ~s: ~s",
                [AbsoluteDestPath, file:format_error(E)]),
            error
    end.

-spec engage_cuttlefish([proplists:property()]) -> {string(), string()} | error.
engage_cuttlefish(ParsedArgs) ->
    EtcDir = proplists:get_value(etc_dir, ParsedArgs),

    AbsPath = case writable_destination_path(ParsedArgs) of
                  error ->
                      stop_deactivate(),
                      error;
                  Path -> Path
    end,

    Date = calendar:local_time(),

    DestinationFilename = filename_maker(proplists:get_value(dest_file, ParsedArgs), Date, "config"),
    Destination = filename:join(AbsPath, DestinationFilename),

    DestinationVMArgsFilename = filename_maker("vm", Date, "args"),
    DestinationVMArgs = filename:join(AbsPath, DestinationVMArgsFilename),

    ?logger:debug("Generating config in: ~p", [Destination]),

    Schema = load_schema(ParsedArgs),
    ConfFile = proplists:get_value(conf_file, ParsedArgs),
    Conf = change_conf(load_conf(ParsedArgs)),
    NewConfig = case cuttlefish_generator:map(Schema, Conf, ConfFile) of
        {error, Phase, {errorlist, Errors}} ->
            ?logger:error("Error generating configuration in phase ~s", [Phase]),
            _ = [ cuttlefish_error:print(E) || E <- Errors],
            stop_deactivate();
        ValidConfig -> ValidConfig
    end,

    AdvancedConfigFile = filename:join(EtcDir, "advanced.config"),
    FinalConfig = case filelib:is_file(AdvancedConfigFile) of
        true ->
            ?logger:info("~s/advanced.config detected, overlaying proplists", [EtcDir]),
            case file:consult(AdvancedConfigFile) of
                {ok, [AdvancedConfig]} ->
                    cuttlefish_advanced:overlay(NewConfig, AdvancedConfig);
                {ok, OtherTerms} ->
                    ?logger:error("Error parsing ~s, incorrect format: ~p", [AdvancedConfigFile, OtherTerms]),
                    stop_deactivate();
                {error, Error} ->
                    ?logger:error("Error parsing ~s: ~s", [AdvancedConfigFile, file:format_error(Error)]),
                    stop_deactivate()
            end;
        _ ->
            %% Nothing to see here, these aren't the droids you're looking for.
            NewConfig
    end,

    case FinalConfig of
        {error, _X} ->
            error;
        _ ->
            FinalAppConfig = proplists:delete(vm_args, FinalConfig),
            FinalVMArgs = cuttlefish_vmargs:stringify(proplists:get_value(vm_args, FinalConfig)),

            %% Prune excess files
            MaxHistory = proplists:get_value(max_history, ParsedArgs, 3) - 1,
            prune(Destination, MaxHistory),
            prune(DestinationVMArgs, MaxHistory),

            case { file:write_file(Destination, io_lib:fwrite("~p.\n",[FinalAppConfig])),
                   file:write_file(DestinationVMArgs, string:join(FinalVMArgs, "\n"))} of
                {ok, ok} ->
                    {Destination, DestinationVMArgs};
                {Err1, Err2} ->
                    maybe_log_file_error(Destination, Err1),
                    maybe_log_file_error(DestinationVMArgs, Err2),
                    error
            end

    end.

-spec prune(file:name_all(), integer()) -> ok.
prune(Filename, MaxHistory) ->
    %% A Filename comes in /Abs/Path/To/something.YYYY.MM.DD.HH.mm.SS.ext
    %% We want `ls /Abs/Path/To/something.*.ext and delete all but the most
    %% recent MaxHistory
    Path = filename:dirname(Filename),
    Ext = filename:extension(Filename),
    Base = hd(string:tokens(filename:basename(Filename, Ext), ".")),
    Files =
        lists:sort(filelib:wildcard(Base ++ ".*" ++ Ext, Path)),

    delete([ filename:join([Path, F]) || F <- Files], MaxHistory),
    ok.

-spec delete(file:name_all(), integer()) -> ok.
delete(Files, MaxHistory) when length(Files) =< MaxHistory ->
    ok;
delete([File|Files], MaxHistory) ->
    case file:delete(File) of
        ok -> ok;
        {error, Reason} ->
            ?logger:error("Could not delete ~s, ~p", [File, Reason])
    end,
    delete(Files, MaxHistory).

-spec maybe_log_file_error(
        file:filename(), ok |
        {error, file:posix()  %% copied from file:format_error/1
                | badarg
                | terminated
                | system_limit
                | { integer(), module(), term() }}) -> ok.
maybe_log_file_error(_, ok) ->
    ok;
maybe_log_file_error(Filename, {error, Reason}) ->
    ?logger:error("Error writing ~s: ~s", [Filename, file:format_error(Reason)]),
    ok.

-spec check_existence(string(), string()) -> {boolean(), string()}.
check_existence(EtcDir, Filename) ->
    FullName = filename:join(EtcDir, Filename), %% Barfolomew
    Exists = filelib:is_file(FullName),
    ?logger:info("Checking ~s exists... ~p", [FullName, Exists]),
    {Exists, FullName}.

filename_maker(Filename, Date, Extension) ->
    {{Y, M, D}, {HH, MM, SS}} = Date,
    _DestinationFilename =
        io_lib:format("~s.~p.~s.~s.~s.~s.~s.~s",
            [Filename,
            Y,
            zero_pad(M),
            zero_pad(D),
            zero_pad(HH),
            zero_pad(MM),
            zero_pad(SS),
            Extension
        ]).

zero_pad(Integer) ->
    S = integer_to_list(Integer),
    case Integer > 9 of
        true -> S;
        _ -> [$0|S]
    end.

print_schema(Schema) ->
    ?logger:info("Printing Schema Mappings"),
    {_, Mappings, _} = Schema,

    {Max, ListOfMappings} = lists:foldr(
        fun(M, {OldMax, List}) ->
            CandidateMax = length(cuttlefish_mapping:mapping(M)),
            NewMax = case CandidateMax > OldMax of
                true -> CandidateMax;
                _ -> OldMax
            end,
            {NewMax, [{cuttlefish_mapping:mapping(M), cuttlefish_variable:format(cuttlefish_mapping:variable(M))}|List]}
        end,
        {0, []},
        Mappings
        ),
    [
        io:format(standard_error, "~s ~s~n",
            [string:left(M, Max+2, $\s), V])
    || {M, V} <- ListOfMappings].

format_datatype(Value, Datatypes) when is_list(Datatypes) ->
    %% We're not sure which datatype the default or set value is going
    %% to match, so let's find one that does.
    [H|_] = lists:dropwhile(
              fun(D0) ->
                      D = cuttlefish_datatypes:extended_from(D0),
                      case cuttlefish_datatypes:from_string(Value, D) of
                          {error, _} -> true;
                          _ -> false
                      end
              end, Datatypes),
    format_datatype(Value, cuttlefish_datatypes:extended_from(H));
format_datatype(Value, Datatype) ->
    cuttlefish_datatypes:to_string(cuttlefish_datatypes:from_string(Value, Datatype), Datatype).


-ifdef(TEST).

zero_pad_test() ->
    ?assertEqual("00", zero_pad(0)),
    ?assertEqual("01", zero_pad(1)),
    ?assertEqual("02", zero_pad(2)),
    ?assertEqual("03", zero_pad(3)),
    ?assertEqual("04", zero_pad(4)),
    ?assertEqual("05", zero_pad(5)),
    ?assertEqual("06", zero_pad(6)),
    ?assertEqual("07", zero_pad(7)),
    ?assertEqual("08", zero_pad(8)),
    ?assertEqual("09", zero_pad(9)),
    ?assertEqual("10", zero_pad(10)),
    ?assertEqual("11", zero_pad(11)),
    ?assertEqual("12", zero_pad(12)),
    ok.


-endif.
