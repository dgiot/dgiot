#!/usr/bin/env escript
%%! -pa /data/dgiot/lib/ekka-0.8.1.11/ebin -proto_dist ekka -epmd_module ekka_epmd -start_epmd false -name tdrebind1@127.0.0.1 -setcookie emqxsecretcookie
%% Idempotent TD-channel rebind for headless dgiot labs.
%% dgiot builds these lazily on dashboard visits; this makes it reproducible:
%%   1. seed {ProductId, <<"TD">>} -> ChannelId bindings (ETS)
%%   2. bootstrap the http pool counter/profiles (dgiot_tdengine_http:start)
%%   3. create per-product database + stable (createdat + TAGS(devaddr))
%%   4. ALTER TABLE add columns for every isstorage thing-model property
%%   5. DESCRIBE -> save_fields (the {ProductId, describe_table} cache that
%%      format_sql needs to build TAGS/VALUES - without it SQL is malformed)
%%   6. restart the channel worker with url in its config (the pool
%%      insert_sql HTTP clause needs url/username/password; without them
%%      the catch-all clause silently drops every insert)
%% Usage: td_rebind.erl <node> <cookie> <channelId> <tdUrl> <tdUser> <tdPass> <productId>...
main([NodeStr, Cookie, ChanStr, TdUrl, TdUser, TdPass | Products]) ->
    case node() of
        'nonode@nohost' ->
            {ok, _} = net_kernel:start([list_to_atom("tdrb" ++
                integer_to_list(rand:uniform(999999)) ++ "@127.0.0.1"),
                longnames]);
        _ -> ok
    end,
    Node = list_to_atom(NodeStr),
    erlang:set_cookie(Node, list_to_atom(Cookie)),
    Chan = list_to_binary(ChanStr),
    Ctx = #{<<"url">> => list_to_binary(TdUrl),
            <<"username">> => list_to_binary(TdUser),
            <<"password">> => list_to_binary(TdPass)},
    Q = fun(Sql) -> rpc:call(Node, dgiot_tdengine_pool, run_sql,
                             [Ctx, execute_query, Sql], 15000) end,
    %% 1. bindings
    lists:foreach(fun(P) ->
        rpc:call(Node, dgiot_data, insert,
                 [{list_to_binary(P), <<"TD">>}, Chan], 5000)
    end, Products),
    io:format("1. bindings seeded: ~p~n", [length(Products)]),
    %% 2. http pool bootstrap
    rpc:call(Node, dgiot_tdengine_http, start, [], 15000),
    io:format("2. http pool ok~n"),
    lists:foreach(fun(P) ->
        Pid = list_to_binary(P),
        Db = <<"_", Pid/binary>>,
        Stable = <<"_", Pid/binary>>,
        Q(<<"CREATE DATABASE IF NOT EXISTS ", Db/binary, " KEEP 365;">>),
        Q(<<"CREATE STABLE IF NOT EXISTS ", Db/binary, ".", Stable/binary,
           " (createdat TIMESTAMP) TAGS (devaddr NCHAR(64));">>),
        %% 4. thing-model driven columns
        Cols = thing_columns(Node, Pid),
        {ok, #{<<"results">> := R0}} =
            Q(<<"DESCRIBE ", Db/binary, ".", Stable/binary, ";">>),
        Have = [maps:get(<<"field">>, M) || M <- R0],
        lists:foreach(fun({C, T}) ->
            case lists:member(C, Have) of
                true -> ok;
                false ->
                    Q(<<"ALTER TABLE ", Db/binary, ".", Stable/binary,
                       " ADD COLUMN ", C/binary, " ", T/binary, ";">>)
            end
        end, Cols),
        %% 5. describe cache
        {ok, #{<<"results">> := R}} =
            Q(<<"DESCRIBE ", Db/binary, ".", Stable/binary, ";">>),
        rpc:call(Node, dgiot_tdengine, save_fields, [Pid, R], 8000),
        io:format("3-5. ~s db/stable/columns/cache ok (~p cols)~n",
                  [P, length(R)])
    end, Products),
    %% 6. restart worker with full config
    rpc:call(Node, dgiot_channelx, delete, [<<"TD">>, Chan], 8000),
    Config = #{<<"ip">> => <<"127.0.0.1">>, <<"port">> => 6041,
               <<"username">> => list_to_binary(TdUser),
               <<"password">> => list_to_binary(TdPass),
               <<"driver">> => <<"HTTP">>, <<"db">> => <<"ChannelId">>,
               <<"os">> => <<"linux">>, <<"keep">> => 365,
               <<"url">> => list_to_binary(TdUrl)},
    A = rpc:call(Node, dgiot_channelx, add,
                 [<<"TD">>, Chan, dgiot_tdengine_channel, Config], 8000),
    io:format("6. channel worker restarted: ~p~n", [A]),
    halt(0).

thing_columns(Node, Pid) ->
    case rpc:call(Node, dgiot_product, lookup_prod, [Pid], 8000) of
        #{<<"thing">> := #{<<"properties">> := Props}} ->
            [{Id, T} || #{<<"identifier">> := Id, <<"dataType">> := Dt,
                         <<"isstorage">> := true} <- Props,
                        is_binary(Id),
                        T <- [type_map(maps:get(<<"type">>, Dt, <<"double">>))],
                        T =/= skip];
        _ -> []
    end.

type_map(<<"int">>) -> <<"INT">>;
type_map(<<"integer">>) -> <<"INT">>;
type_map(<<"long">>) -> <<"BIGINT">>;
type_map(<<"float">>) -> <<"DOUBLE">>;
type_map(<<"double">>) -> <<"DOUBLE">>;
type_map(<<"string">>) -> <<"NCHAR(64)">>;
type_map(<<"text">>) -> <<"NCHAR(200)">>;
type_map(<<"bool">>) -> skip;
type_map(_) -> <<"NCHAR(64)">>.
