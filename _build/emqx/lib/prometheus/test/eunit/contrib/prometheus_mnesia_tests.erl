-module(prometheus_mnesia_tests).

-include_lib("eunit/include/eunit.hrl").

table_disk_size_test() ->
  {ok, Root} = file:get_cwd(),
  MnesiaDir = Root ++ "/test/mnesia",
  set_custom_mnesia_dir(MnesiaDir),
  ?assertEqual(MnesiaDir, mnesia:system_info(directory)),

  ?assertEqual(3, prometheus_mnesia:table_disk_size(MnesiaDir, table)),
  ?assertEqual(21, prometheus_mnesia:table_disk_size(my_table)).

tm_info_test() ->
  try
    mnesia:start(),
    ?assertMatch({_, _}, prometheus_mnesia:tm_info())
  after
    mnesia:stop()
  end.

set_custom_mnesia_dir(Dir) ->
  try
    ets:lookup_element(mnesia_gvar, dir, 2),
    %% mnesia ets table initialized, mnesia_monitor:get_env first looks here
    ets:update_element(mnesua_gvar, dir, Dir)
  catch
    error:_ -> %% mnesia ets table not initialized,
      %% mnesia_monitor:get_env gets value from mnesia app env
      application:set_env(mnesia, dir, Dir)
  end.
