-module(erlang_vm_schema_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(SC_FILE, filename:join([code:lib_dir(cuttlefish), "test", "erlang_vm.schema"])).

%% basic schema test will check to make sure that all defaults from the schema
%% make it into the generated app.config
basic_schema_test() ->
    %% The defaults are defined in pf(riak_kv.schema) and multi_backend.schema.
    %% they are the files under test.
    Config = cuttlefish_unit:generate_templated_config(
        [?SC_FILE], [], context()),

    cuttlefish_unit:assert_config(Config, "vm_args.-smp", enable),
    cuttlefish_unit:assert_config(Config, "vm_args.+W", "w"),
    cuttlefish_unit:assert_config(Config, "vm_args.+K", true),
    cuttlefish_unit:assert_not_configured(Config, "vm_args.+S"),
    cuttlefish_unit:assert_config(Config, "vm_args.-name", "node@host"),
    cuttlefish_unit:assert_config(Config, "vm_args.-setcookie", "erlang"),
    cuttlefish_unit:assert_config(Config, "vm_args.+A", 64),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_FULLSWEEP_AFTER", 0),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_CRASH_DUMP", "dump"),
    cuttlefish_unit:assert_config(Config, "vm_args.+P", 256000),
    cuttlefish_unit:assert_not_configured(Config, "vm_args.+zdbbl"),
    cuttlefish_unit:assert_not_configured(Config, "vm_args.+sfwi"),
    cuttlefish_unit:assert_not_configured(Config, "vm_args.+scl"),
    cuttlefish_unit:assert_not_configured(Config, "vm_args.+sub"),
    cuttlefish_unit:assert_not_configured(Config, "vm_args.-kernel net_ticktime"),
    cuttlefish_unit:assert_not_configured(Config, "kernel.inet_dist_listen_min"),
    cuttlefish_unit:assert_not_configured(Config, "kernel.inet_dist_listen_max"),
    case cuttlefish:otp("R16", erlang:system_info(otp_release)) of
        true ->
            cuttlefish_unit:assert_config(Config, "vm_args.+Q", 262144),
            cuttlefish_unit:assert_config(Config, "vm_args.+e", 256000);
        _ ->
            cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_PORTS", 262144),
            cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_ETS_TABLES", 256000)
    end,
    ok.

override_schema_test() ->
    %% Conf represents the riak.conf file that would be read in by cuttlefish.
    %% this proplists is what would be output by the conf_parse module
    Conf = [
        {["erlang", "smp"], "disable"},
        {["erlang", "W"], "i"},
        {["erlang", "K"], off},
        {["erlang", "schedulers", "total"], 4},
        {["erlang", "schedulers", "online"], 4},
        {["nodename"], "mynode@myhost"},
        {["distributed_cookie"], "riak"},
        {["erlang", "async_threads"], 22},
        {["erlang", "max_ports"], 32000},
        {["erlang", "fullsweep_after"], 1},
        {["erlang", "crash_dump"], "place"},
        {["erlang", "max_ets_tables"], 128000},
        {["erlang", "process_limit"], 128001},
        {["erlang", "distribution_buffer_size"], 1024},
        {["erlang", "schedulers", "force_wakeup_interval"], 500},
        {["erlang", "schedulers", "compaction_of_load"], true},
        {["erlang", "schedulers", "utilization_balancing"], false},
        {["erlang", "distribution", "port_range", "minimum"], 6000},
        {["erlang", "distribution", "port_range", "maximum"], 7999},
        {["erlang", "distribution", "net_ticktime"], 43}
    ],

    Config = cuttlefish_unit:generate_templated_config(
        [?SC_FILE], Conf, context()),

    cuttlefish_unit:assert_config(Config, "vm_args.-smp", disable),
    cuttlefish_unit:assert_config(Config, "vm_args.+W", "i"),
    cuttlefish_unit:assert_config(Config, "vm_args.+K", false),
    cuttlefish_unit:assert_config(Config, "vm_args.+S", "4:4"),
    cuttlefish_unit:assert_config(Config, "vm_args.-name", "mynode@myhost"),
    cuttlefish_unit:assert_config(Config, "vm_args.-setcookie", "riak"),
    cuttlefish_unit:assert_config(Config, "vm_args.+A", 22),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_FULLSWEEP_AFTER", 1),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_CRASH_DUMP", "place"),
    cuttlefish_unit:assert_config(Config, "vm_args.+P", 128001),
    cuttlefish_unit:assert_config(Config, "vm_args.+zdbbl", 1),
    cuttlefish_unit:assert_config(Config, "vm_args.+sfwi", 500),
    cuttlefish_unit:assert_config(Config, "vm_args.+scl", true),
    cuttlefish_unit:assert_config(Config, "vm_args.+sub", false),
    cuttlefish_unit:assert_config(Config, "kernel.inet_dist_listen_min", 6000),
    cuttlefish_unit:assert_config(Config, "kernel.inet_dist_listen_max", 7999),
    cuttlefish_unit:assert_config(Config, "vm_args.-kernel net_ticktime", 43),

    %% These settings are version dependent, so we won't even test them here
    %% because we don't know what version you're running, so we'll cover it
    %% in two tests below
    case cuttlefish:otp("R16", erlang:system_info(otp_release)) of
        true ->
            cuttlefish_unit:assert_config(Config, "vm_args.+Q", 32000),
            cuttlefish_unit:assert_config(Config, "vm_args.+e", 128000);
        _ ->
            cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_PORTS", 32000),
            cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_ETS_TABLES", 128000)
    end,
    ok.

erlang_scheduler_test() ->
    Conf1 = [
        {["erlang", "schedulers", "total"], 4},
        {["erlang", "schedulers", "online"], 1}
    ],
    Config1 = cuttlefish_unit:generate_templated_config(
        [?SC_FILE], Conf1, context()),
    cuttlefish_unit:assert_config(Config1, "vm_args.+S", "4:1"),

    Conf2 = [
        {["erlang", "schedulers", "total"], 4}
    ],
    Config2 = cuttlefish_unit:generate_templated_config(
        [?SC_FILE], Conf2, context()),
    cuttlefish_unit:assert_config(Config2, "vm_args.+S", "4"),

    Conf3 = [
        {["erlang", "schedulers", "online"], 4}
    ],
    Config3 = cuttlefish_unit:generate_templated_config(
        [?SC_FILE], Conf3, context()),
    cuttlefish_unit:assert_config(Config3, "vm_args.+S", ":4"),

    Config4 = cuttlefish_unit:generate_templated_config(
        [?SC_FILE], [], context()),
    cuttlefish_unit:assert_not_configured(Config4, "vm_args.+S"),


    ok.

async_threads_stack_size_test() ->
    WordSize = erlang:system_info({wordsize, external}),
    TooSmall    = cuttlefish_bytesize:to_string(WordSize * 1024 * 10),
    TooLarge    = cuttlefish_bytesize:to_string(WordSize * 1024 * 9000),
    Indivisible = cuttlefish_bytesize:to_string(WordSize * 1024 * 16 - 2),
    Correct     = cuttlefish_bytesize:to_string(WordSize * 1024 * 32),
    MinSize     = cuttlefish_bytesize:to_string(WordSize * 1024 * 16),
    MaxSize     = cuttlefish_bytesize:to_string(WordSize * 1024 * 8192),
    CorrectRaw = 32,

    Conf0 = [],
    Config0 = cuttlefish_unit:generate_templated_config([?SC_FILE], Conf0, context()),
    cuttlefish_unit:assert_not_configured(Config0, "vm_args.+a"),

    Conf1 = [{["erlang", "async_threads", "stack_size"], Correct}],
    Config1 = cuttlefish_unit:generate_templated_config([?SC_FILE], Conf1, context()),
    cuttlefish_unit:assert_config(Config1, "vm_args.+a", CorrectRaw),

    Conf2 = [{["erlang", "async_threads", "stack_size"], TooSmall}],
    Config2 = cuttlefish_unit:generate_templated_config([?SC_FILE], Conf2, context()),
    cuttlefish_unit:assert_error_message(Config2, "erlang.async_threads.stack_size invalid, must be in the range of " ++ MinSize ++ " to " ++ MaxSize),

    Conf3 = [{["erlang", "async_threads", "stack_size"], TooLarge}],
    Config3 = cuttlefish_unit:generate_templated_config([?SC_FILE], Conf3, context()),
    cuttlefish_unit:assert_error_message(Config3, "erlang.async_threads.stack_size invalid, must be in the range of " ++ MinSize ++ " to " ++ MaxSize),

    Conf4 = [{["erlang", "async_threads", "stack_size"], Indivisible}],
    Config4 = cuttlefish_unit:generate_templated_config([?SC_FILE], Conf4, context()),
    cuttlefish_unit:assert_error_message(Config4, "erlang.async_threads.stack_size invalid, must be divisible by " ++ integer_to_list(WordSize)),

    ok.

%% this context() represents the substitution variables that rebar
%% will use during the build process.  riak_core's schema file is
%% written with some {{mustache_vars}} for substitution during
%% packaging cuttlefish doesn't have a great time parsing those, so we
%% perform the substitutions first, because that's how it would work
%% in real life.
context() ->
    [
        {node, "node@host"},
        {crash_dump, "dump"}
    ].

inet_dist_use_interface_test() ->
    InputConfig = "erlang.distribution.interface",
    GeneratedConfig = "kernel.inet_dist_use_interface",
    InputConfigPoint = string:tokens(InputConfig, "."),

    Pass =[
        {"127.0.0.1",{127,0,0,1}},
        {"0.0.0.0",{0,0,0,0}},
        {"fe80:1200::1",{65152,4608,0,0,0,0,0,1}}
    ],
    Fail = [
        "127.0.0.1:8080",
        "127.1",
        "fe80:1200::g",
        "Not an IP"
    ],

    lists:foreach(fun({Input, Expected}) ->
                Config = cuttlefish_unit:generate_templated_config(
                    [?SC_FILE], [{InputConfigPoint, Input}], context()),
                cuttlefish_unit:assert_config(Config, GeneratedConfig, Expected)
        end, Pass),
    lists:foreach(fun(Input) ->
                Config = cuttlefish_unit:generate_templated_config(
                    [?SC_FILE], [{InputConfigPoint, Input}], context()),
                cuttlefish_unit:assert_error_message(Config,
                    InputConfig ++ " invalid, must be a valid IPv4 or IPv6 address")
        end, Fail).

