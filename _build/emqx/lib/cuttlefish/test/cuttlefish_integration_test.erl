-module(cuttlefish_integration_test).

-include("cuttlefish.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% This test generates a default .conf file from the riak.schema. view it at ../generated.conf
generated_conf_file_test() ->
    {_, Mappings, _} = cuttlefish_schema:file(tp("riak.schema")),
    cuttlefish_conf:generate_file(Mappings, "../generated.conf"),
    %% Schema generated a conf file, let's parse it!
    Conf = cuttlefish_conf:file("../generated.conf"),
    ?assertEqual("8099", proplists:get_value(["handoff","port"], Conf)),
    ok.

%% This test generates a .config file from the riak.schema. view it at ../generated.config
generated_config_file_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [], %% conf_parse:file(tp("riak.conf")),
    NewConfig = cuttlefish_generator:map(Schema, Conf),

    file:write_file("../generated.config",io_lib:fwrite("~p.\n",[NewConfig])),
    ok.

breaks_on_fuzzy_and_strict_match_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [{["listener", "protobuf", "$name"], "127.0.0.1:8087"}],
    ?assertMatch({error, add_defaults, _}, cuttlefish_generator:map(Schema, Conf)),
    ok.

breaks_on_rhs_not_found_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [{["ring", "state_dir"], "$(tyktorp)/ring"}],
    ?assertMatch({error, rhs_subs, _}, cuttlefish_generator:map(Schema, Conf)),
    ok.

breaks_on_rhs_infinite_loop_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [
            {["ring", "state_dir"], "$(platform_data_dir)/ring"},
            {["platform_data_dir"], "$(ring.state_dir)/data"}
           ],
    ?assertMatch({error, rhs_subs, _}, cuttlefish_generator:map(Schema, Conf)),
    ok.

breaks_on_bad_enum_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [{["storage_backend"], penguin}],
    ?assertMatch({error, transform_datatypes, _}, cuttlefish_generator:map(Schema, Conf)),
    ok.

breaks_on_bad_validation_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [{["ring_size"], 10}],
    ?assertMatch({error, validation, _}, cuttlefish_generator:map(Schema, Conf)),
    ok.

%% Tests that the schema can generate a default app.config from nothing
all_the_marbles_test() ->
    Schema = cuttlefish_schema:file(tp("riak.schema")),
    Conf = [], %conf_parse:file(tp("riak.conf")),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    ?assert(is_proplist(NewConfig)),

    NewConfigWithoutVmargs = proplists:delete(vm_args, NewConfig),

    {ok, [AppConfig]} = file:consult(tp("default.config")),

    ?assert(is_proplist(AppConfig)),

    proplist_equals(AppConfig, NewConfigWithoutVmargs),
    ok.

multibackend_test() ->
    Schema = cuttlefish_schema:files([tp("riak.schema"), tp("multi_backend.schema")]),

    Conf = [
        {["storage_backend"], "multi"},
        {["multi_backend","bitcask_mult","storage_backend"], "bitcask"},
        {["multi_backend","bitcask_mult","bitcask","data_root"], "/path/to/dat/cask"},
        {["multi_backend","leveldb_mult","storage_backend"], "leveldb"},
        {["multi_backend","leveldb_mult","leveldb","data_root"], "/path/to/dat/level"},
        {["multi_backend","memory_mult","storage_backend"], "memory"},
        {["multi_backend","memory_mult","memory_backend","ttl"], "1d"},
        {["multi_backend","leveldb_mult2","storage_backend"], "leveldb"},
        {["multi_backend","leveldb_mult2","leveldb","data_root"], "/path/to/dat/level2"}
    ],

    NewConfig = cuttlefish_generator:map(Schema, Conf),
    KV = proplists:get_value(riak_kv, NewConfig),
    Multi = proplists:get_value(multi_backend, KV),

    {<<"bitcask_mult">>, riak_kv_bitcask_backend, BitcaskProps} = lists:keyfind(<<"bitcask_mult">>, 1, Multi),
    ?logger:info("BitcaskProps: ~p", [BitcaskProps]),
    ?assertEqual("/path/to/dat/cask", proplists:get_value(data_root, BitcaskProps)),
    ?assertEqual(4,                   proplists:get_value(open_timeout, BitcaskProps)),
    ?assertEqual(2147483648,          proplists:get_value(max_file_size, BitcaskProps)),
    ?assertEqual(60,                  proplists:get_value(frag_merge_trigger, BitcaskProps)),
    ?assertEqual(536870912,           proplists:get_value(dead_bytes_merge_trigger, BitcaskProps)),
    ?assertEqual(40,                  proplists:get_value(frag_threshold, BitcaskProps)),
    ?assertEqual(134217728,           proplists:get_value(dead_bytes_threshold, BitcaskProps)),
    ?assertEqual(10485760,            proplists:get_value(small_file_threshold, BitcaskProps)),
    ?assertEqual(-1,                  proplists:get_value(max_fold_age, BitcaskProps)),
    ?assertEqual(0,                   proplists:get_value(max_fold_puts, BitcaskProps)),
    ?assertEqual(-1,                  proplists:get_value(expiry_secs, BitcaskProps)),
    ?assertEqual(true,                proplists:get_value(require_hint_crc, BitcaskProps)),
    ?assertEqual(0,                   proplists:get_value(expiry_grace_time, BitcaskProps)),
    ?assertEqual(erlang,              proplists:get_value(io_mode, BitcaskProps)),
    ?assertEqual(none,                proplists:get_value(sync_strategy, BitcaskProps)),
    ?assertEqual(always,              proplists:get_value(merge_window, BitcaskProps)),

    {<<"leveldb_mult">>, riak_kv_eleveldb_backend, Level1Props} = lists:keyfind(<<"leveldb_mult">>, 1, Multi),
    ?assertEqual("/path/to/dat/level", proplists:get_value(data_root, Level1Props)),
    ?assertEqual(30, proplists:get_value(max_open_files, Level1Props)),
    ?assertEqual(8388608, proplists:get_value(cache_size, Level1Props)),
    ?assertEqual(false, proplists:get_value(sync, Level1Props)),
    ?assertEqual(15728640, proplists:get_value(write_buffer_size_min, Level1Props)),
    ?assertEqual(31457280, proplists:get_value(write_buffer_size_max, Level1Props)),
    ?assertEqual(4096, proplists:get_value(sst_block_size, Level1Props)),
    ?assertEqual(16, proplists:get_value(block_restart_interval, Level1Props)),
    ?assertEqual(true, proplists:get_value(verify_checksums, Level1Props)),
    ?assertEqual(true, proplists:get_value(verify_compaction, Level1Props)),
    ?assertEqual(true, proplists:get_value(use_bloomfilter, Level1Props)),

    {<<"leveldb_mult2">>, riak_kv_eleveldb_backend, Level2Props} = lists:keyfind(<<"leveldb_mult2">>, 1, Multi),

    ?assertEqual("/path/to/dat/level2", proplists:get_value(data_root, Level2Props)),
    ?assertEqual(30, proplists:get_value(max_open_files, Level2Props)),
    ?assertEqual(8388608, proplists:get_value(cache_size, Level2Props)),
    ?assertEqual(false, proplists:get_value(sync, Level2Props)),
    ?assertEqual(15728640, proplists:get_value(write_buffer_size_min, Level2Props)),
    ?assertEqual(31457280, proplists:get_value(write_buffer_size_max, Level2Props)),
    ?assertEqual(4096, proplists:get_value(sst_block_size, Level2Props)),
    ?assertEqual(16, proplists:get_value(block_restart_interval, Level2Props)),
    ?assertEqual(true, proplists:get_value(verify_checksums, Level2Props)),
    ?assertEqual(true, proplists:get_value(verify_compaction, Level2Props)),
    ?assertEqual(true, proplists:get_value(use_bloomfilter, Level2Props)),

    {<<"memory_mult">>, riak_kv_memory_backend, MemProps} = lists:keyfind(<<"memory_mult">>, 1, Multi),
    ?assertEqual(86400, proplists:get_value(ttl, MemProps)),
    ?assertEqual(4096, proplists:get_value(max_memory, MemProps)),
    ok.

unset_translation_test() ->
    Schema = cuttlefish_schema:files([tp("unset_translation.schema")]),
    Conf = [
        {["a", "b"], "8"}
    ],
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Props = proplists:get_value(erlang, NewConfig),
    ?logger:info("~p", [NewConfig]),
    ?assertEqual(8, proplists:get_value(key, Props)).

not_found_error_test() ->
    Schema = cuttlefish_schema:files([tp("throw_not_found.schema")]),
    Conf = [],
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    ?assertMatch({error, apply_translations, _}, NewConfig).

duration_test() ->
    Schema = cuttlefish_schema:files([tp("durations.schema")]),

    %% Test that the duration parsing doesn't emit "error" into the
    %% config instead of the extended type.
    Conf = conf_parse:parse(<<"a.b.c = foo\n">>),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    ?assertEqual(foo, proplists:get_value(duration_extended, proplists:get_value(cuttlefish, NewConfig))),

    %% Test that for a non-extended duration, a bad value results in
    %% an erroroneous config, not emitting error.
    Conf2 = conf_parse:parse(<<"b.c = fish\n">>),
    ErrConfig = cuttlefish_generator:map(Schema, Conf2),
    ?assertMatch({error, transform_datatypes, _}, ErrConfig).

proplist_equals(Expected, Actual) ->
    ExpectedKeys = lists:sort(proplists:get_keys(Expected)),
    ActualKeys = lists:sort(proplists:get_keys(Actual)),
    ?assertEqual(ExpectedKeys, ActualKeys),
    [ begin
        ExpectedValue = proplists:get_value(EKey, Expected),
        ActualValue = proplists:get_value(EKey, Actual, undefined),
        case {is_proplist(ExpectedValue), is_proplist(ActualValue)} of
            {true, true} ->
                proplist_equals(ExpectedValue, ActualValue);
            {false, false} ->
                ?assertEqual({EKey, ExpectedValue}, {EKey, ActualValue});
            _ ->
                ?assertEqual({EKey, ExpectedValue}, {EKey, ActualValue})
        end
    end || EKey <- ExpectedKeys].

is_proplist(Proplist) when is_list(Proplist) ->
    lists:all(
        fun(X) ->
            is_tuple(X) andalso tuple_size(X) =:= 2
        end,
        Proplist);
is_proplist(_) -> false.

%% test-path
tp(Name) ->
    filename:join([code:lib_dir(cuttlefish), "test", Name]).


