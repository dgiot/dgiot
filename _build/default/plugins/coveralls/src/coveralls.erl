%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2013-2016, Markus Ekholm
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of the <organization> nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL MARKUS EKHOLM BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% @copyright 2013-2016 (c) Markus Ekholm <markus@botten.org>
%%% @author Markus Ekholm <markus@botten.org>
%%% @doc coveralls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Module declaration

-module(coveralls).

%%=============================================================================
%% Exports

-export([ convert_file/2
        , convert_and_send_file/2
        ]).

%%=============================================================================
%% Records

-record(s, { importer      = fun cover:import/1
           , module_lister = fun cover:imported_modules/0
           , mod_info      = fun module_info_compile/1
           , file_reader   = fun file:read_file/1
           , wildcard_reader = fun filelib:wildcard/1
           , analyser      = fun cover:analyse/3
           , poster        = fun httpc:request/4
           , poster_init   = start_wrapper([fun ssl:start/0, fun inets:start/0])
           }).

%%=============================================================================
%% Defines

-define(COVERALLS_URL, "https://coveralls.io/api/v1/jobs").
%%-define(COVERALLS_URL, "http://127.0.0.1:8080").

-ifdef(random_only).
-define(random, random).
-else.
-define(random, rand).
-endif.

%%=============================================================================
%% API functions

%% @doc Import and convert cover file(s) `Filenames' to a json string
%%      representation suitable to post to coveralls.
%%
%%      Note that this function will crash if the modules mentioned in
%%      any of the `Filenames' are not availabe on the node.
%% @end
-spec convert_file(string() | [string()], map()) ->
                          string().
convert_file(Filenames, Report) ->
  convert_file(Filenames, Report, #s{}).

%% @doc Import and convert cover files `Filenames' to a json string and send the
%%      json to coveralls.
%% @end
-spec convert_and_send_file(string() | [string()], map()) -> ok.
convert_and_send_file(Filenames, Report) ->
  convert_and_send_file(Filenames, Report, #s{}).

%%=============================================================================
%% Internal functions

convert_file([L|_]=Filename, Report, S) when is_integer(L) ->
  %% single file or wildcard was specified
  WildcardReader = S#s.wildcard_reader,
  Filenames = WildcardReader(Filename),
  convert_file(Filenames, Report, S);
convert_file([[_|_]|_]=Filenames, Report, S) ->
  ok               = lists:foreach(
                       fun(Filename) -> ok = import(S, Filename) end,
                       Filenames),
  ConvertedModules = convert_modules(S),
  jsx:encode(Report#{source_files => ConvertedModules}, []).

convert_and_send_file(Filenames, Report, S) ->
  send(convert_file(Filenames, Report, S), S).

send(Json, #s{poster=Poster, poster_init=Init}) ->
  ok       = Init(),
  Boundary = ["----------", integer_to_list(?random:uniform(1000))],
  Type     = "multipart/form-data; boundary=" ++ Boundary,
  Body     = to_body(Json, Boundary),
  R        = Poster(post, {?COVERALLS_URL, [], Type, Body}, [], []),
  {ok, {{_, ReturnCode, _}, _, Message}} = R,
  case ReturnCode of
    200      -> ok;
    ErrCode  -> throw({error, {ErrCode, Message}})
  end.

%%-----------------------------------------------------------------------------
%% HTTP helpers

to_body(Json, Boundary) ->
  iolist_to_binary(["--", Boundary, "\r\n",
                    "Content-Disposition: form-data; name=\"json_file\"; "
                    "filename=\"json_file.json\" \r\n"
                    "Content-Type: application/json\r\n\r\n",
                    Json, "\r\n", "--", Boundary, "--", "\r\n"]).

%%-----------------------------------------------------------------------------
%% Callback mockery

import(#s{importer=F}, File) -> F(File).

imported_modules(#s{module_lister=F}) -> F().

analyze(#s{analyser=F}, Mod) -> F(Mod, calls, line).

compile_info(#s{mod_info=F}, Mod) -> F(Mod).

-ifdef(TEST).
module_info_compile(Mod) -> Mod:module_info(compile).
-else.
module_info_compile(Mod) ->
  code:load_file(Mod),
  case code:is_loaded(Mod) of
      {file, _} -> Mod:module_info(compile);
      _         -> []
  end.
-endif.

read_file(#s{file_reader=_F}, "") -> {ok, <<"">>};
read_file(#s{file_reader=F}, SrcFile) -> F(SrcFile).

start_wrapper(Funs) ->
  fun() ->
      lists:foreach(fun(F) -> ok = wrap_start(F) end, Funs)
  end.

wrap_start(StartFun) ->
  case StartFun() of
    {error,{already_started,_}} -> ok;
    ok                          -> ok
  end.

digit(I) when I < 10 -> <<($0 + I):8>>;
digit(I) -> <<($a -10 + I):8>>.

hex(<<>>) ->
  <<>>;
hex(<<I:4, R/bitstring>>) ->
  <<(digit(I))/binary, (hex(R))/binary>>.

%%-----------------------------------------------------------------------------
%% Converting modules

convert_modules(S) ->
  F = fun(Mod, L) -> convert_module(Mod, S, L) end,
  lists:foldr(F, [], imported_modules(S)).

convert_module(Mod, S, L) ->
  {ok, CoveredLines0} = analyze(S, Mod),
  %% Remove strange 0 indexed line
  FilterF      = fun({{_, X}, _}) -> X =/= 0 end,
  CoveredLines = lists:filter(FilterF, CoveredLines0),
  case proplists:get_value(source, compile_info(S, Mod), "") of
    "" -> L;
    SrcFile ->
          {ok, SrcBin} = read_file(S, SrcFile),
          Src0         = lists:flatten(io_lib:format("~s", [SrcBin])),
          SrcDigest    = erlang:md5(SrcBin),
          LinesCount   = count_lines(Src0),
          Cov          = create_cov(CoveredLines, LinesCount),
      [#{name          => unicode:characters_to_binary(relative_to_cwd(SrcFile), utf8, utf8),
         source_digest => hex(SrcDigest),
         coverage      => Cov}
       | L]
  end.

expand(Path) -> expand(filename:split(Path), []).

expand([], Acc)              -> filename:join(lists:reverse(Acc));
expand(["."|Tail], Acc)      -> expand(Tail, Acc);
expand([".."|Tail], [])      -> expand(Tail, []);
expand([".."|Tail], [_|Acc]) -> expand(Tail, Acc);
expand([Segment|Tail], Acc)  -> expand(Tail, [Segment|Acc]).

realpath(Path) -> realpath(filename:split(Path), "./").

realpath([], Acc)            -> filename:absname(expand(Acc));
realpath([Head | Tail], Acc) ->
  NewAcc0 = filename:join([Acc, Head]),
  NewAcc = case file:read_link(NewAcc0) of
    {ok, Link} ->
      case filename:pathtype(Link) of
        absolute -> realpath(Link);
        relative -> filename:join([Acc, Link])
      end;
    _ -> NewAcc0
  end,
  realpath(Tail, NewAcc).

relative_to_cwd(Path) ->
  case file:get_cwd() of
    {ok, Base} -> relative_to(Path, Base);
    _ -> Path
  end.

relative_to(Path, From) ->
  Path1 = realpath(Path),
  relative_to(filename:split(Path1), filename:split(From), Path).

relative_to([H|T1], [H|T2], Original)  -> relative_to(T1, T2, Original);
relative_to([_|_] = L1, [], _Original) -> filename:join(L1);
relative_to(_, _, Original)            -> Original.

create_cov(_CoveredLines, [])                                    ->
  [];
create_cov(CoveredLines, LinesCount) when is_integer(LinesCount) ->
  create_cov(CoveredLines, lists:seq(1, LinesCount));
create_cov([{{_,LineNo},Count}|CoveredLines], [LineNo|LineNos])  ->
  [Count | create_cov(CoveredLines, LineNos)];
create_cov(CoveredLines, [_|LineNos])                            ->
  [null | create_cov(CoveredLines, LineNos)].

%%-----------------------------------------------------------------------------
%% Generic helpers

count_lines("")      -> 1;
count_lines("\n")    -> 1;
count_lines([$\n|S]) -> 1 + count_lines(S);
count_lines([_|S])   -> count_lines(S).

%%=============================================================================
%% Tests

-ifdef(TEST).
-define(DEBUG, true).
-include_lib("eunit/include/eunit.hrl").

normalize_json_str(Str) when is_binary(Str) ->
  jsx:encode(jsx:decode(Str, [return_maps, {labels, existing_atom}]));
normalize_json_str(Str) when is_list(Str) ->
  normalize_json_str(iolist_to_binary(Str)).

convert_file_test() ->
  Expected =
      jsx:decode(
        <<"{\"service_job_id\": \"1234567890\","
          " \"service_name\": \"travis-ci\","
          " \"source_files\": ["
          "    {\"name\": \"example.rb\","
          "     \"source_digest\": \"3feb892deff06e7accbe2457eec4cd8b\","
          "     \"coverage\": [null,1,null]"
          "    },"
          "    {\"name\": \"two.rb\","
          "     \"source_digest\": \"fce46ee19702bd262b2e4907a005aff4\","
          "     \"coverage\": [null,1,0,null]"
          "    }"
          "  ]"
          "}">>, [return_maps, {labels, existing_atom}]),
  Report = #{service_job_id => <<"1234567890">>,
             service_name   => <<"travis-ci">>},
  Got = jsx:decode(
          convert_file("example.rb", Report, mock_s()),
          [return_maps, {labels, existing_atom}]),
  ?assertEqual(Expected, Got).

convert_and_send_file_test() ->
  Expected =
    normalize_json_str(
      "{\"service_job_id\": \"1234567890\","
      " \"service_name\": \"travis-ci\","
      " \"source_files\": ["
      "    {\"name\": \"example.rb\","
      "     \"source_digest\": \"3feb892deff06e7accbe2457eec4cd8b\","
      "     \"coverage\": [null,1,null]"
      "    },"
      "    {\"name\": \"two.rb\","
      "     \"source_digest\": \"fce46ee19702bd262b2e4907a005aff4\","
      "     \"coverage\": [null,1,0,null]"
      "    }"
      "  ]"
      "}"),
  Report = #{service_job_id => <<"1234567890">>,
             service_name   => <<"travis-ci">>},
  ?assertEqual(ok, convert_and_send_file("example.rb", Report, mock_s(Expected))).

send_test_() ->
  Expected =
    normalize_json_str(
      "{\"service_job_id\": \"1234567890\",\n"
      " \"service_name\": \"travis-ci\",\n"
      " \"source_files\": [\n"
      "    {\"name\": \"example.rb\",\n"
      "     \"source_digest\": \"\tdef four\\n  4\\nend\",\n"
      "     \"coverage\": [null,1,null]\n"
      "    }"
      "  ]"
      "}"),
  [ ?_assertEqual(ok, send(Expected, mock_s(Expected)))
  , ?_assertThrow({error, {_,_}}, send("foo", mock_s(<<"bar">>)))
  ].

%%-----------------------------------------------------------------------------
%% Generic helpers tests

count_lines_test_() ->
  [ ?_assertEqual(1, count_lines(""))
  , ?_assertEqual(1, count_lines("foo"))
  , ?_assertEqual(1, count_lines("bar\n"))
  , ?_assertEqual(2, count_lines("foo\nbar"))
  , ?_assertEqual(3, count_lines("foo\n\nbar"))
  , ?_assertEqual(2, count_lines("foo\nbar\n"))
  ].

expand_test_() ->
  [ ?_assertEqual("/a/b", expand(["/", "a", "b"], []))
  , ?_assertEqual("a/c" , expand(["a", "b", "..", ".", "c"], []))
  , ?_assertEqual("/"   , expand(["..", ".", "/"], []))
  ].

realpath_and_relative_test_() ->
  {setup,
   fun() -> %% setup
       {ok, Cwd} = file:get_cwd(),
       Root = string:strip(
                os:cmd("mktemp -d -t coveralls_tests.XXX"), right, $\n),
       ok = file:set_cwd(Root),
       {Cwd, Root}
   end,
   fun({Cwd, _Root}) -> %% teardown
       ok = file:set_cwd(Cwd)
   end,
   fun({_Cwd, Root}) -> %% tests
     Filename = "file",
     Dir1  = filename:join([Root, "_test_src", "dir1"]),
     Dir2  = filename:join([Root, "_test_src", "dir2"]),
     File1 = filename:join([Dir1, Filename]),
     File2 = filename:join([Dir2, Filename]),
     Link1 = filename:join([ Root
                           , "_test_build"
                           , "default"
                           , "lib"
                           , "mylib"
                           , "src"
                           , "dir1"
                           ]),
     Link2 = filename:join([ Root
                           , "_test_build"
                           , "default"
                           , "lib"
                           , "mylib"
                           , "src"
                           , "dir2"
                           ]),
     [ ?_assertEqual(ok,
                     filelib:ensure_dir(filename:join([Dir1, "dummy"])))
     , ?_assertEqual(ok,
                     filelib:ensure_dir(filename:join([Dir2, "dummy"])))
     , ?_assertEqual(ok,
                     file:write_file(File1, "data"))
     , ?_assertEqual(ok,
                     file:write_file(File2, "data"))
     , ?_assertEqual(ok,
                     filelib:ensure_dir(Link1))
     , ?_assertEqual(ok,
                     filelib:ensure_dir(Link2))
     , ?_assertEqual(ok,
                     file:make_symlink(Dir1, Link1))
     , ?_assertEqual(ok,
                     file:make_symlink(filename:join([ ".."
                                                     , ".."
                                                     , ".."
                                                     , ".."
                                                     , ".."
                                                     , "_test_src"
                                                     , "dir2"
                                                     ])
                                      , Link2))
     , ?_assertEqual(realpath(File1),
                     realpath(filename:join([Link1, Filename])))
     , ?_assertEqual(realpath(File2),
                     realpath(filename:join([Link2, Filename])))
     , ?_assertEqual(realpath(File1),
                     filename:absname(
                       relative_to_cwd(
                         filename:join([Link1, Filename]))))
     , ?_assertEqual(realpath(File2),
                     filename:absname(
                       relative_to_cwd(
                         filename:join([Link2, Filename]))))
     ]
   end}.

%%-----------------------------------------------------------------------------
%% Callback mockery tests
module_info_compile_test() ->
  ?assert(is_tuple(lists:keyfind(source, 1, module_info_compile(?MODULE)))).

start_wrapper_test_() ->
  F        = fun() -> ok end,
  StartedF = fun() -> {error,{already_started,mod}} end,
  ErrorF   = fun() -> {error, {error, mod}} end,
  [ ?_assertEqual(ok, (start_wrapper([F, StartedF]))())
  , ?_assertError(_, (start_wrapper([F, StartedF, ErrorF]))())
  ].

%%-----------------------------------------------------------------------------
%% Converting modules tests

create_cov_test() ->
  ?assertEqual([null, 3, null, 4, null],
               create_cov([{{foo, 2}, 3}, {{foo, 4}, 4}], 5)).

convert_module_test() ->
  Expected =
    [#{name          => <<"example.rb">>,
       source_digest => <<"3feb892deff06e7accbe2457eec4cd8b">>,
       coverage      => [null,1,null]}],
  ?assertEqual(Expected, convert_module('example.rb', mock_s(), [])).

convert_modules_test() ->
  Expected =
    [#{name          => <<"example.rb">>,
       source_digest => <<"3feb892deff06e7accbe2457eec4cd8b">>,
       coverage      => [null,1,null]
      },
     #{name          => <<"two.rb">>,
       source_digest => <<"fce46ee19702bd262b2e4907a005aff4">>,
       coverage      => [null,1,0,null]
      }],
  ?assertEqual(Expected,
               convert_modules(mock_s())).

%%-----------------------------------------------------------------------------
%% Setup helpers

mock_s() -> mock_s("").

mock_s(Json) ->
  #s{ importer      =
        fun(_) -> ok end
    , module_lister =
        fun() -> ['example.rb', 'two.rb'] end
    , mod_info      =
        fun('example.rb') -> [{source,"example.rb"}];
           ('two.rb')     -> [{source,"two.rb"}]
        end
    , file_reader   =
        fun("example.rb") ->
            {ok, <<"def four\n  4\nend">>};
           ("two.rb")     ->
            {ok, <<"def seven\n  eight\n  nine\nend">>}
        end
    , wildcard_reader = fun(AnyFile) -> [AnyFile] end
    , analyser      =
        fun('example.rb' , calls, line) -> {ok, [ {{'example.rb', 2}, 1} ]};
           ('two.rb'     , calls, line) -> {ok, [ {{'two.rb', 2}, 1}
                                                , {{'two.rb', 3}, 0}
                                                ]
                                           }
        end
    , poster_init   =
        fun() -> ok end
    , poster        =
        fun(post, {_, _, _, Body}, _, _) ->
            case binary:match(Body, Json) =/= nomatch of
              true  -> {ok, {{"", 200, ""}, "", ""}};
              false -> {ok, {{"", 666, ""}, "", "Not expected"}}
            end
        end
    }.

-endif.

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
