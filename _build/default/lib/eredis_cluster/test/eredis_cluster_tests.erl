-module(eredis_cluster_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Setup, fun() -> eredis_cluster:start() end).
-define(Clearnup, fun(_) -> eredis_cluster:stop() end).

basic_test_() ->
    {inorder,
        {setup, ?Setup, ?Clearnup,
        [
            { "get and set",
            fun() ->
                ?assertEqual({ok, <<"OK">>}, eredis_cluster:q(["SET", "key", "value"])),
                ?assertEqual({ok, <<"value">>}, eredis_cluster:q(["GET","key"])),
                ?assertEqual({ok, undefined}, eredis_cluster:q(["GET","nonexists"]))
            end
            },

            { "binary",
            fun() ->
                ?assertEqual({ok, <<"OK">>}, eredis_cluster:q([<<"SET">>, <<"key_binary">>, <<"value_binary">>])),
                ?assertEqual({ok, <<"value_binary">>}, eredis_cluster:q([<<"GET">>,<<"key_binary">>])),
                ?assertEqual([{ok, <<"value_binary">>},{ok, <<"value_binary">>}], eredis_cluster:qp([[<<"GET">>,<<"key_binary">>],[<<"GET">>,<<"key_binary">>]]))
            end
            },

            { "delete test",
            fun() ->
                ?assertMatch({ok, _}, eredis_cluster:q(["DEL", "a"])),
                ?assertEqual({ok, <<"OK">>}, eredis_cluster:q(["SET", "b", "a"])),
                ?assertEqual({ok, <<"1">>}, eredis_cluster:q(["DEL", "b"])),
                ?assertEqual({ok, undefined}, eredis_cluster:q(["GET", "b"]))
            end
            },

            { "pipeline",
            fun () ->
                ?assertNotMatch([{ok, _},{ok, _},{ok, _}], eredis_cluster:qp([["SET", "a1", "aaa"], ["SET", "a2", "aaa"], ["SET", "a3", "aaa"]])),
                ?assertMatch([{ok, _},{ok, _},{ok, _}], eredis_cluster:qp([["LPUSH", "a", "aaa"], ["LPUSH", "a", "bbb"], ["LPUSH", "a", "ccc"]]))
            end
            },

            { "multi node get",
                fun () ->
                    N=1000,
                    Keys = [integer_to_list(I) || I <- lists:seq(1,N)],
                    [eredis_cluster:q(["SETEX", Key, "50", Key]) || Key <- Keys],
                    Guard1 = [{ok, integer_to_binary(list_to_integer(Key))} || Key <- Keys],
                    ?assertMatch(Guard1, eredis_cluster:qmn([["GET", Key] || Key <- Keys])),
                    eredis_cluster:q(["SETEX", "a", "50", "0"]),
                    Guard2 = [{ok, integer_to_binary(0)} || _Key <- lists:seq(1,5)],
                    ?assertMatch(Guard2, eredis_cluster:qmn([["GET", "a"] || _I <- lists:seq(1,5)]))
                end
            },

            % WARNING: This test will fail during rebalancing, as qmn does not guarantee transaction across nodes
            { "multi node",
                fun () ->
                    N=1000,
                    Keys = [integer_to_list(I) || I <- lists:seq(1,N)],
                    [eredis_cluster:q(["SETEX", Key, "50", Key]) || Key <- Keys],
                    Guard1 = [{ok, integer_to_binary(list_to_integer(Key)+1)} || Key <- Keys],
                    ?assertMatch(Guard1, eredis_cluster:qmn([["INCR", Key] || Key <- Keys])),
                    eredis_cluster:q(["SETEX", "a", "50", "0"]),
                    Guard2 = [{ok, integer_to_binary(Key)} || Key <- lists:seq(1,5)],
                    ?assertMatch(Guard2, eredis_cluster:qmn([["INCR", "a"] || _I <- lists:seq(1,5)]))
                end
            },

            { "transaction",
            fun () ->
                ?assertMatch({ok,[_,_,_]}, eredis_cluster:transaction([["get","abc"],["get","abc"],["get","abc"]])),
                ?assertMatch({error,_}, eredis_cluster:transaction([["get","abc"],["get","abcde"],["get","abcd1"]]))
            end
            },

            { "function transaction",
            fun () ->
                eredis_cluster:q(["SET", "efg", "12"]),
                Function = fun(Worker) ->
                    eredis_cluster:qw(Worker, ["WATCH", "efg"]),
                    {ok, Result} = eredis_cluster:qw(Worker, ["GET", "efg"]),
                    NewValue = binary_to_integer(Result) + 1,
                    timer:sleep(100),
                    lists:last(eredis_cluster:qw(Worker, [["MULTI"],["SET", "efg", NewValue],["EXEC"]]))
                end,
                PResult = rpc:pmap({eredis_cluster, transaction},["efg"],lists:duplicate(5, Function)),
                Nfailed = lists:foldr(fun({_, Result}, Acc) -> if Result == undefined -> Acc + 1; true -> Acc end end, 0, PResult),
                ?assertEqual(4, Nfailed)
            end
            },

            { "eval key",
            fun () ->
                eredis_cluster:q(["del", "foo"]),
                eredis_cluster:q(["eval","return redis.call('set',KEYS[1],'bar')", "1", "foo"]),
                ?assertEqual({ok, <<"bar">>}, eredis_cluster:q(["GET", "foo"]))
            end
            },

            { "evalsha",
            fun () ->
                % In this test the key "load" will be used because the "script
                % load" command will be executed in the redis server containing
                % the "load" key. The script should be propagated to other redis
                % client but for some reason it is not done on Travis test
                % environment. @TODO : fix travis redis cluster configuration,
                % or give the possibility to run a command on an arbitrary
                % redis server (no slot derived from key name)
                eredis_cluster:q(["del", "load"]),
                {ok, Hash} = eredis_cluster:q(["script","load","return redis.call('set',KEYS[1],'bar')"]),
                eredis_cluster:q(["evalsha", Hash, 1, "load"]),
                ?assertEqual({ok, <<"bar">>}, eredis_cluster:q(["GET", "load"]))
            end
            },

            { "bitstring support",
            fun () ->
                eredis_cluster:q([<<"set">>, <<"bitstring">>,<<"support">>]),
                ?assertEqual({ok, <<"support">>}, eredis_cluster:q([<<"GET">>, <<"bitstring">>]))
            end
            },

            { "flushdb",
            fun () ->
                eredis_cluster:q(["set", "zyx", "test"]),
                eredis_cluster:q(["set", "zyxw", "test"]),
                eredis_cluster:q(["set", "zyxwv", "test"]),
                eredis_cluster:flushdb(),
                ?assertEqual({ok, undefined}, eredis_cluster:q(["GET", "zyx"])),
                ?assertEqual({ok, undefined}, eredis_cluster:q(["GET", "zyxw"])),
                ?assertEqual({ok, undefined}, eredis_cluster:q(["GET", "zyxwv"]))
            end
            },

            { "atomic get set",
            fun () ->
                eredis_cluster:q(["set", "hij", 2]),
                Incr = fun(Var) -> binary_to_integer(Var) + 1 end,
                Result = rpc:pmap({eredis_cluster, update_key}, [Incr], lists:duplicate(5, "hij")),
                IntermediateValues = proplists:get_all_values(ok, Result),
                ?assertEqual([3,4,5,6,7], lists:sort(IntermediateValues)),
                ?assertEqual({ok, <<"7">>}, eredis_cluster:q(["get", "hij"]))
            end
            },

            { "atomic hget hset",
            fun () ->
                eredis_cluster:q(["hset", "klm", "nop", 2]),
                Incr = fun(Var) -> binary_to_integer(Var) + 1 end,
                Result = rpc:pmap({eredis_cluster, update_hash_field}, ["nop", Incr], lists:duplicate(5, "klm")),
                IntermediateValues = proplists:get_all_values(ok, Result),
                ?assertEqual([{<<"0">>,3},{<<"0">>,4},{<<"0">>,5},{<<"0">>,6},{<<"0">>,7}], lists:sort(IntermediateValues)),
                ?assertEqual({ok, <<"7">>}, eredis_cluster:q(["hget", "klm", "nop"]))
            end
            },

            { "eval",
            fun () ->
                Script = <<"return redis.call('set', KEYS[1], ARGV[1]);">>,
                ScriptHash = << << if N >= 10 -> N -10 + $a; true -> N + $0 end >> || <<N:4>> <= crypto:hash(sha, Script) >>,
                eredis_cluster:eval(Script, ScriptHash, ["qrs"], ["evaltest"]),
                ?assertEqual({ok, <<"evaltest">>}, eredis_cluster:q(["get", "qrs"]))
            end
            }

      ]
    }
}.
