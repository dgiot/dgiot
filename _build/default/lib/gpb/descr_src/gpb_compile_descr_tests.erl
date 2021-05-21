%%% Copyright (C) 2019  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%%% MA  02110-1301  USA

-module(gpb_compile_descr_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gpb_descriptor.hrl").

%% ------------------------------------------------------------------

individual_descriptor_test() ->
    {#'FileDescriptorSet'{
       file=[#'FileDescriptorProto'{}=B1,
             #'FileDescriptorProto'{}=B2]},
     [{"main",B1},
      {"aux",B2}]} = compile_descriptors(
                   [{"main.proto",
                     ["syntax=\"proto2\";",
                      "import \"aux.proto\";",
                      "message M { };"]},
                    {"aux.proto",
                     ["syntax=\"proto2\";",
                      "message A { optional uint32 g = 1; }"]}],
                   []).

oneof_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "message Ma {",
           "  oneof u1 { uint32 f11 = 11;",
           "             uint32 f12 = 12;",
           "             uint32 f13 = 13; };",
           "  oneof u2 { uint32 f21 = 21;",
           "             uint32 f22 = 22; };",
           "}",
           "message Mb {",
           "  oneof u2 { uint32 g11 = 11; };",
           "  oneof u1 { uint32 g21 = 21; };",
           "}"]}],
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="main.proto",
          package=undefined,
          message_type =
              [#'DescriptorProto'{
                  name="Ma",
                  field=[#'FieldDescriptorProto'{name="f11", oneof_index=0},
                         #'FieldDescriptorProto'{name="f12", oneof_index=0},
                         #'FieldDescriptorProto'{name="f13", oneof_index=0},
                         #'FieldDescriptorProto'{name="f21", oneof_index=1},
                         #'FieldDescriptorProto'{name="f22", oneof_index=1}],
                  oneof_decl=[#'OneofDescriptorProto'{name="u1"},
                              #'OneofDescriptorProto'{name="u2"}]},
               #'DescriptorProto'{
                  name="Mb",
                  field=[#'FieldDescriptorProto'{name="g11", oneof_index=0},
                         #'FieldDescriptorProto'{name="g21", oneof_index=1}],
                  oneof_decl=[#'OneofDescriptorProto'{name="u2"},
                              #'OneofDescriptorProto'{name="u1"}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []).

refs_have_pkg_name_test() -> % only refs have package, names do not
    ProtosAsTxts1 =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "import \"aux.proto\";",
           "package top.p1;",
           "message M { optional S f1 = 1;",
           "            optional E f2 = 2;",
           "            oneof u { S f3 = 3; };",
           "            map<uint32,S> f4 = 4;",
           "            map<uint64,E> f5 = 5; };",
           "message S { optional uint32 f1 = 1; };",
           "enum E { a=0;};",
           "service SomeService {",
           "  rpc rpc1 (M) returns (S);",
           "}"]},
         {"aux.proto",
          ["syntax=\"proto2\";",
           "package aux.p2;",
           "message A { optional B g = 1; }",
           "message B { optional uint32 h = 1; }"]}],

    %% Without the use_packges option
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="top/p1/main.proto",
          package="top.p1",
          message_type =
              [#'DescriptorProto'{
                  name="M", % name, not a ref
                  field=[#'FieldDescriptorProto'{type_name=".top.p1.S"},
                         #'FieldDescriptorProto'{type_name=".top.p1.E"},
                         #'FieldDescriptorProto'{type_name=".top.p1.S"},
                         #'FieldDescriptorProto'{type_name=MapField1Ref},
                         #'FieldDescriptorProto'{type_name=MapField2Ref}]},
               #'DescriptorProto'{name="S"},
               #'DescriptorProto'{
                  name=MapField1Name,
                  field=[#'FieldDescriptorProto'{type = 'TYPE_UINT32'},
                         #'FieldDescriptorProto'{type_name = ".top.p1.S"}]},
               #'DescriptorProto'{
                  name=MapField2Name,
                  field=[#'FieldDescriptorProto'{type = 'TYPE_UINT64'},
                         #'FieldDescriptorProto'{type_name = ".top.p1.E"}]}],
          enum_type = [#'EnumDescriptorProto'{name="E"}],
          service =
              [#'ServiceDescriptorProto'{
                  name="SomeService",
                  method = [#'MethodDescriptorProto'{
                               name = "rpc1",
                               input_type=".top.p1.M",
                               output_type=".top.p1.S"}]}]}},
      {"aux",
       #'FileDescriptorProto'{}}]} =
        Descriptors1 =
        compile_descriptors(ProtosAsTxts1, []),
    ?assertEqual(MapField1Ref, ".top.p1." ++ MapField1Name),
    ?assertEqual(MapField2Ref, ".top.p1." ++ MapField2Name),

    %% With the use_packages option
    Descriptors1 = compile_descriptors(ProtosAsTxts1, [use_packages]),

    %% A proto with no package definition
    ProtosAsTxts2 =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "message M { optional S f1 = 1;",
           "            optional E f2 = 2;",
           "            oneof u { S f3 = 3; }; };",
           "message S { optional uint32 f1 = 1; };",
           "enum E { a=0; };",
           "service SomeService {",
           "  rpc rpc1 (M) returns (S);",
           "}"]}],
    %% Without the use_packges option
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="main.proto",
          package=undefined,
          message_type =
              [#'DescriptorProto'{
                  name="M", % name, not a ref
                  field=[#'FieldDescriptorProto'{type_name=".S"},
                         #'FieldDescriptorProto'{type_name=".E"},
                         #'FieldDescriptorProto'{type_name=".S"}]},
               #'DescriptorProto'{name="S"}],
          enum_type = [#'EnumDescriptorProto'{name="E"}],
          service =
              [#'ServiceDescriptorProto'{
                  name="SomeService",
                  method = [#'MethodDescriptorProto'{
                               name = "rpc1",
                               input_type=".M",
                               output_type=".S"}]}]}}]} =
        Descriptors2 =
        compile_descriptors(ProtosAsTxts2, []),
    %% With the use_packages option
    Descriptors2 = compile_descriptors(ProtosAsTxts2, [use_packages]),
    ok.

nested_definitions_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "message M {",
           "  optional S f1 = 1;",
           "  optional E f2 = 2;",
           "  oneof u { S f3 = 3; };",
           "  message S { ",
           "    optional EE f1 = 1;",
           "    enum EE { aa=0; }",
           "    };",
           "  enum E { a=0; };",
           "}"]}],
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="main.proto",
          package=undefined,
          message_type =
              [#'DescriptorProto'{
                  name="M", % name, not a ref
                  field=[#'FieldDescriptorProto'{type_name=".M.S"},
                         #'FieldDescriptorProto'{type_name=".M.E"},
                         #'FieldDescriptorProto'{type_name=".M.S"}],
                  nested_type =
                      [#'DescriptorProto'{
                          name="S",
                          field=[#'FieldDescriptorProto'{type_name=".M.S.EE"}],
                          enum_type=[#'EnumDescriptorProto'{name="EE"}]}],
                  enum_type = [#'EnumDescriptorProto'{name="E"}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []).

proto3_optional_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';",
           "message M {",
           "  uint32 f1 = 1;",
           "  optional uint32 f2 = 2;",
           "  oneof c { uint32 f3 = 3; }",
           "}"]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          message_type =
              [#'DescriptorProto'{
                  name="M",
                  field=[#'FieldDescriptorProto'{name="f1"},
                         #'FieldDescriptorProto'{name="f2", oneof_index=1},
                         #'FieldDescriptorProto'{name="f3", oneof_index=0}],
                  %% Synthetic names must come after any non-synthetic ones
                  oneof_decl=[#'OneofDescriptorProto'{name="c"},
                              #'OneofDescriptorProto'{}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []).

%% --helpers----------

compile_descriptors(IoLists, GpbCompileOpts) ->
    {ok, Defs, []=_Warns} = compile_files_as_iolists(IoLists, GpbCompileOpts),
    {Bin, PBins} = gpb_compile_descr:encode_defs_to_descriptors(Defs,
                                                                GpbCompileOpts),
    {gpb_descriptor:decode_msg(Bin, 'FileDescriptorSet'),
     [{ProtoName,gpb_descriptor:decode_msg(ProtoBin, 'FileDescriptorProto')}
      || {ProtoName, ProtoBin} <- PBins]}.

compile_files_as_iolists([{FName, _IoList} | _Rest]=IoLists, GpbCompileOpts) ->
    ReadFile = fun(F) ->
                       B = filename:basename(F),
                       case lists:keyfind(B, 1, IoLists) of
                           {B, Contents} ->
                               {ok, iolist_to_binary([Contents])};
                           _ ->
                               file:read_file(F)
                       end
               end,
    ReadFileInfo = fun(F) ->
                           B = filename:basename(F),
                           case lists:keyfind(B, 1, IoLists) of
                               {B, _Contents} ->
                                   {ok, #file_info{access=read}};
                               _ ->
                                   file:read_file_info(F)
                           end
                   end,
    LatestDefsVsn = lists:max(gpb_defs:supported_defs_versions()),
    gpb_compile:file(
      FName,
      [{file_op, [{read_file, ReadFile},
                  {read_file_info, ReadFileInfo},
                  {write_file, fun(_,_) -> ok end}]},
       {i,"."},
       to_proto_defs, {proto_defs_version, LatestDefsVsn},
       return_errors, return_warnings
       | GpbCompileOpts]).
