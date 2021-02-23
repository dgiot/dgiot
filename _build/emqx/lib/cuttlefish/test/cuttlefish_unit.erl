-module(cuttlefish_unit).

-include("cuttlefish.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

generate_templated_config(FileName, Conf, Context) ->
    generate_templated_config(FileName, Conf, Context, {[], [], []}).

generate_templated_config(FileName, Conf, Context, PreexistingSchema) ->
    RenderedSchemas = case lists:all(fun(X) -> not is_list(X) end, FileName) of
        true -> %% it's a single depth list, aka string
            [{ cuttlefish_schema:string_fun_factory(), render_template(FileName, Context)}];
        _ -> %% It's a list of lists, aka multiple strings
            [{ cuttlefish_schema:string_fun_factory(), render_template(F, Context)} || F <- FileName]
    end,
    Schema = cuttlefish_schema:merger(RenderedSchemas ++ [ { fun(_, _) -> PreexistingSchema end, ""} ]),
    cuttlefish_generator:map(Schema, Conf).

render_template(FileName, Context0) ->
    Context = make_bbmustache_data(Context0),
    {ok, Bin} = file:read_file(FileName),
    Rendered = bbmustache:render(Bin, Context),
    unicode:characters_to_list(Rendered).

-spec generate_config(atom(), [string()]|string(), list()) -> list().
generate_config(strings, SchemaStrings, Conf) ->
    Schema = cuttlefish_schema:strings(SchemaStrings),
    cuttlefish_generator:map(Schema, Conf);

generate_config(string, SchemaString, Conf) ->
    Schema = cuttlefish_schema:strings([SchemaString]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(file, SchemaFile, Conf) ->
    generate_config(SchemaFile, Conf).

-spec generate_config(string(), list()) -> list().
generate_config(SchemaFile, Conf) ->
    Schema = cuttlefish_schema:files([SchemaFile]),
    cuttlefish_generator:map(Schema, Conf).

assert_valid_config(Config) ->
    case Config of
        List when is_list(List) ->
            ok;
        {error, Phase, {errorlist, Errors}} ->
            erlang:exit({assert_valid_config_failed,
                         [{phase, Phase},
                          {errorlist, Errors}]});
        Other ->
            erlang:exit({assert_valid_config_failed,
                         [{bad_value, Other}]})
    end.

assert_config(Config, Path, Value) ->
    ok = assert_valid_config(Config),
    ActualValue = case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            ?assertEqual({Path, Value}, {Path, nesting_error});
        notset ->
            ?assertEqual({Path, Value}, {Path, notset});
        {ok, X} -> X
    end,
    ?assertEqual({Path, Value}, {Path, ActualValue}).

assert_not_configured(Config, Path) ->
    ok = assert_valid_config(Config),
    case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            erlang:exit({assert_not_configured_failed,
                         [{bad_nesting, Path},
                          {config, Config}]});
        {ok, Value} ->
            erlang:exit({assert_not_configured_failed,
                         [{key, Path},
                          {configured_to, Value},
                          {config, Config}]});
        notset -> ok
    end.

%% @doc Asserts that the generated configuration is in error.
assert_error(Config) ->
    ?assertMatch({error, _, {errorlist, _}}, Config).

%% @doc Asserts that the generated configuration is in error, with the
%% error occurring in a specific phase.
assert_error_in_phase(Config, Phase) when is_atom(Phase) ->
    ?assertMatch({error, Phase, {errorlist, _}}, Config).

%% @doc Asserts that the generated configuration is in error, and the
%% given error message was emitted by the given phase.
assert_error(Config, Phase, Message) ->
    assert_error_in_phase(Config, Phase),
    assert_error_message(Config, Message).

%% @doc Asserts that the generated configuration is in error and has
%% the given error messages.
assert_errors(Config, [H|_]=Messages) when is_list(H) ->
    [ assert_error_message(Config, Message) || Message <- Messages ].

%% @doc Asserts that the generated configuration is in error, with
%% errors occuring in the given phase and containing the given
%% messages.
assert_errors(Config, Phase, [H|_]=Messages) when is_list(H) ->
    assert_error_in_phase(Config, Phase),
    [ assert_error_message(Config, Message) || Message <- Messages ].

%% @doc Asserts that the generated configuration is in error and
%% contains an error tuple that translates to the given error message
assert_error_message(Config, Message) ->
    ok = assert_error(Config),
    {errorlist, Errors} = element(3, Config),
    chase_message(Message, Errors, Errors).

chase_message(Message, [], Errors) ->
    erlang:exit({assert_error_message_failed,
                 [{expected, Message},
                  {actual, Errors}]});
chase_message(Message, [{error, ErrorTerm}|T], Errors) ->
    case lists:flatten(cuttlefish_error:xlate(ErrorTerm)) of
        Message ->
            ok;
        _ ->
            chase_message(Message, T, Errors)
    end.

-spec path(cuttlefish_variable:variable(),
           [{ string() | atom() | binary() , term()}]) ->
                  {ok, any()} | notset | {error, bad_nesting}.
path(_, []) ->
    {error, bad_nesting};
path(_, undefined) ->
    notset;
path([Last], Proplist) ->
    case lists:dropwhile(key_no_match(Last), Proplist) of
        [] -> notset;
        [{_, V}|_] -> {ok, V}
    end;
path([H|T], Proplist) when is_list(H)->
    case path([H], Proplist) of
        {ok, SmallerProplist} ->
            path(T, SmallerProplist);
        Other ->
            Other
    end.

-spec key_no_match(string()) -> fun((atom() | string() | binary()) -> boolean()).
key_no_match(Key) ->
    fun({E, _}) when is_atom(E) -> E =/= list_to_atom(Key);
       ({E, _}) when is_list(E) -> E =/= Key;
       ({E, _}) when is_binary(E) -> E =/= list_to_binary(Key);
       (_) -> true
    end.

-spec dump_to_file(any(), string()) -> ok.
dump_to_file(ErlangTerm, Filename) ->
    {ok, S} = file:open(Filename, [write,append]),
    io:format(S, "~p~n", [ErlangTerm]),
    _ = file:close(S),
    ok.

path_test() ->
    ?assertEqual(
       {ok, "disable"},
       path(["vm_args", "-smp"], [{vm_args, [{'-smp', "disable"}]}])),
    ok.

multiple_schema_generate_templated_config_test() ->
    Context = [
        {mustache, "mustache"}
              ],
    PrereqSchema = {[], [
        cuttlefish_mapping:parse(
        {mapping, "c", "app.c", [
            {default, "/c"}
                                ]})
                        ], []},

    Config = cuttlefish_unit:generate_templated_config(tp("sample_mustache.schema"), [], Context, PrereqSchema),
    ?logger:error("~p", [Config]),
    assert_config(Config, "app_a.setting_b", "/c/mustache/a.b"),
    ok.

%% test-path
tp(Name) ->
    filename:join([code:lib_dir(cuttlefish), "test", Name]).

make_bbmustache_data(L) -> lists:map(fun make_bbmustache_data_opt/1, L).

make_bbmustache_data_opt({K, V}) ->
    {make_bbmustache_data_opt_key(K), make_bbmustache_data_opt_val(V)}.

make_bbmustache_data_opt_key(K) when is_atom(K) -> atom_to_list(K);
make_bbmustache_data_opt_key(K) when is_binary(K) -> binary_to_list(K);
make_bbmustache_data_opt_key(K) when is_list(K) -> K.

make_bbmustache_data_opt_val(V) -> iolist_to_binary(V).

