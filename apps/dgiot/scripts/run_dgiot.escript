#!/usr/bin/env escript

main(_) ->
    start().

start() ->
    ok = application:load(mnesia),
    MnesiaName = lists:concat(["Mnesia.", atom_to_list(node())]),
    MnesiaDir = filename:join(["_build", "data", MnesiaName]),
    ok = application:set_env(mnesia, dir, MnesiaDir),
    SpecEmqxConfig = fun(_) -> ok end,
    start(SpecDgiotConfig).

start(SpecEmqxConfig) ->
    SchemaPath = filename:join(["priv", "dgiot.schema"]),
    ConfPath = filename:join(["etc", "dgiot.conf"]),
    dgiot_ct_helpers:start_app(dgiot, SchemaPath, ConfPath, SpecDgiotConfig).
