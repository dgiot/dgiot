-module(cuttlefish_escript_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(assertPrinted(___Text),
        begin
            ((fun() ->
                     case cuttlefish_test_group_leader:get_output() of
                         {ok, ___Output} ->
                             case re:run(___Output, ___Text) of
                                 {match, _} ->
                                     ok;
                                 nomatch ->
                                     erlang:error({assertPrinted_failed,
                                           [{module, ?MODULE},
                                            {line, ?LINE},
                                            {expected, ___Text},
                                            {actual, unicode:characters_to_list(___Output)}]})
                             end;
                         error ->
                             erlang:error({assertPrinted_failed,
                                           [{module, ?MODULE},
                                            {line, ?LINE},
                                            {expected, ___Text},
                                            {reason, timed_out_on_receive}]})
                     end
              end)())
        end).

-define(capturing(__Forms),
        begin
            ___OldLeader = group_leader(),
            group_leader(cuttlefish_test_group_leader:new_group_leader(self()), self()),
            try
                __Forms
            after
                cuttlefish_test_group_leader:tidy_up(___OldLeader)
            end
        end).

describe_test_() ->
     [
      {"`cuttlefish describe` prints documentation", fun describe_prints_docs/0},
      {"`cuttlefish describe` prints datatype's valid values", fun describe_prints_datatype/0},
      {"`cuttlefish describe` prints default", fun describe_prints_default/0},
      {"`cuttlefish describe` prints configured value", fun describe_prints_configured/0},
      {"`cuttlefish describe` prints erlang application key", fun describe_prints_app_key/0},
      {"`cuttlefish describe` prints message when no default exists", fun describe_prints_no_default/0},
      {"`cuttlefish describe` prints message when value not configured", fun describe_prints_not_configured/0}
     ].

describe(Key) ->
    ?assertThrow(stop_deactivate, cuttlefish_escript:main(["-i", tp("riak.schema"), "-c", tp("riak.conf"), "describe", Key])).

describe_prints_docs() ->
    ?capturing(begin
                   describe("ring_size"),
                   ?assertPrinted("Documentation for ring_size"),
                   ?assertPrinted("Default ring creation size\\.  Make sure it is a power of 2")
               end).

describe_prints_datatype() ->
    ?capturing(begin
                   describe("storage_backend"),
                   ?assertPrinted("- one of: bitcask, leveldb, memory, multi")
               end).

describe_prints_default() ->
    ?capturing(begin
                   describe("ring_size"),
                   ?assertPrinted("Default Value : 64")
               end).

describe_prints_configured() ->
    ?capturing(begin
                   describe("anti_entropy"),
                   ?assertPrinted("Set Value     : debug")
               end).


describe_prints_app_key() ->
    ?capturing(begin
                   describe("leveldb.bloomfilter"),
                   ?assertPrinted("Internal key  : eleveldb\\.use_bloomfilter")
               end).

describe_prints_no_default() ->
    ?capturing(begin
                   describe("listener.https.foo"),
                   ?assertPrinted("No default set")
               end).

describe_prints_not_configured() ->
    ?capturing(begin
                   describe("ssl.keyfile"),
                   Text = "Value not set in " ++ tp("riak.conf"),
                   ?assertPrinted(Text)
               end).

%% test-path
tp(Name) ->
    filename:join([code:lib_dir(cuttlefish), "test", Name]).

-endif.
