-module(cuttlefish_escript_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

escript_utf8_test() ->
    cuttlefish_test_logger:bounce(error),

    ?assertThrow(stop_deactivate, cuttlefish_escript:main(
               "-d " ++ fixtures("escript_utf8_test/generated.config") ++
              " -s " ++ fixtures("escript_utf8_test/lib") ++
              " -e " ++ fixtures("escript_utf8_test/etc") ++
              " -c " ++ fixtures("escript_utf8_test/etc/utf8.conf generate")
            )),
    [Log] = cuttlefish_test_logger:get_logs(),
    ?assertMatch({match, _}, re:run(Log, "utf8.conf: Error converting value on line #1 to latin1")),
    ok.


advanced_config_format_test() ->
    cuttlefish_test_logger:bounce(error),
    ?assertThrow(stop_deactivate, cuttlefish_escript:main(
                                     "-d " ++ fixtures("acformat/generated.config") ++
                                    " -s " ++ fixtures("acformat/lib") ++
                                    " -e " ++ fixtures("acformat/etc") ++
                                    " -c " ++ fixtures("acformat/etc/acformat.conf generate")
                                   )),
    [Log] = cuttlefish_test_logger:get_logs(),
    ?assert(end_with(Log, <<"acformat/etc/advanced.config, incorrect format: [[a], [b]]">>)),
    ok.

end_with(Str, Suffix) ->
    case binary:match(Str, Suffix) of
        {_, L} when L =:= size(Suffix) -> true;
        _ -> false
    end.

escript_prune_test_() ->
    {timeout, 20, [
                   escript_prune("-m 3", 3),
                   escript_prune("", 3), %% default
                   escript_prune("-m 6", 6)
                  ]}.

escript_prune(DashM, ExpectedMax) ->
    %% Empty workspace
    Dir = fixtures("escript_prune_test"),
    GenDir = fixtures("escript_prune_test/generated.config"),
    case file:list_dir(GenDir) of
        {ok, FilenamesToDelete} ->
            [ file:delete(filename:join([GenDir, F])) || F <- FilenamesToDelete ];
        _ -> ok
    end,

    {_, _, T} = lists:foldl(
        fun(Counter, {PrevConfigs, PrevVMArgs, Tests}) ->
            io:format(user, "Running iteration: ~p~n", [Counter]),
            %% Timer to keep from generating more than one file per second
            timer:sleep(1100),
            Args = "-d " ++ GenDir ++ " -s " ++ filename:join(Dir, "lib") ++
                   " -e " ++ filename:join(Dir, "etc") ++ " " ++ DashM ++ " generate",
            cuttlefish_escript:main(Args),
            AppConfigs =
                lists:sort(
                    filelib:wildcard("app.*.config",
                                     fixtures("escript_prune_test/generated.config"))),
            VMArgs =
                lists:sort(
                    filelib:wildcard("vm.*.args",
                                     fixtures("escript_prune_test/generated.config"))),

            {AppConfigs,
             VMArgs,

             [?_assert(length(AppConfigs) =< ExpectedMax),
              ?_assert(length(VMArgs) =< ExpectedMax),
              compare_lists(PrevConfigs, AppConfigs),
              compare_lists(PrevVMArgs, VMArgs) | Tests]}
        end,
        {[], [], []},
        lists:seq(1,10)),
    T.

%% This function is asserting that Previous is the tail of Current OR
%% that the tail of Previous is equal to the first length(Previous)
%% elements of Current
compare_lists(Previous, Current) when (length(Previous) +1) =:= length(Current) ->
    compare_lists([stub|Previous], Current);
compare_lists([_|PTail] = Previous, Current) when length(Previous) =:= length(Current) ->
    NewPrevious = PTail ++ [lists:last(Current)],
    ?_assertEqual(NewPrevious, Current);
compare_lists(_Previous, _Current) ->
    ?_assert(false).

fixtures(Name) ->
    filename:join([code:lib_dir(cuttlefish), "test", "fixtures", Name]).

