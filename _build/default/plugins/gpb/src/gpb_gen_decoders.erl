%%% Copyright (C) 2017  Tomas Abrahamsson
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

%%% @doc Generation of msg-decoding functions.
%%%
%%% Merging is an integral part of decoding: optional and required
%%% messages that occur multiple times on the wire are merged
%%% recursively. Scalar optional or required fields a merged by
%%% overwriting. Repeated fields are merged by appending.
%%%
%%% @private

-module(gpb_gen_decoders).

-export([format_exports/2]).
-export([format_decoders_top_function/3]).
-export([format_msg_decoders/3]).
-export([format_map_decoders/3]).
-export([format_aux_decoders/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").
-include("gpb_decoders_lib.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

%% -- exports -----------------------------------------------------

format_exports(Defs, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    NoNif = not DoNif,
    [?f("-export([decode_msg/2"),[", decode_msg/3" || NoNif], ?f("]).~n"),
     [[?f("-export([decode/2]). %% epb compatibility~n"),
       [?f("-export([~p/1]).~n", [gpb_lib:mk_fn(decode_, MsgName)])
        || {{msg,MsgName}, _Fields} <- Defs],
       "\n"]
      || gpb_lib:get_epb_functions_by_opts(Opts)],
     [[[begin
            NoWrapperFnName = gpb_lib:mk_fn(decode_msg_, MsgName),
            if DoNif ->
                    ?f("-export([~p/1]).~n", [NoWrapperFnName]);
               not DoNif ->
                    ?f("-export([~p/1, ~p/2]).~n",
                       [NoWrapperFnName, NoWrapperFnName])
            end
        end
        || {{msg,MsgName}, _Fields} <- Defs],
       "\n"]
      || gpb_lib:get_bypass_wrappers_by_opts(Opts)]].

%% -- decoders -----------------------------------------------------

%% Varints are processed 7 bits at a time.
%% We can expect that we have processed this number of bits before
%% we expect to see the last varint byte, which must have msb==0.
%% 64 - 7 = 57.
-define(NB, 57).
-define(is_msg_or_group(X),
        is_tuple(X)
        andalso tuple_size(X) =:= 2
        andalso (element(1, X) =:= msg orelse element(1, X) =:= group)).

format_decoders_top_function(Defs, AnRes, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_decoders_top_function_msgs(Defs, AnRes, Opts);
        false -> format_decoders_top_function_no_msgs(Opts)
    end.

format_decoders_top_function_no_msgs(Opts) ->
    ["-spec decode_msg(binary(), atom()) -> no_return().\n",
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, _MsgName) when is_binary(Bin) ->
               erlang:error({gpb_error, no_messages})
       end),
     "-spec decode_msg(binary(), atom(), list()) -> no_return().\n",
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, _MsgName, _Opts) when is_binary(Bin) ->
               erlang:error({gpb_error, no_messages})
       end),
     "\n",
     [["%% epb compatibility\n",
       ?f("-spec decode(atom(), binary()) -> no_return().\n"),
       gpb_codegen:format_fn(
         decode,
         fun(MsgName, Bin) when is_atom(MsgName), is_binary(Bin) ->
                 erlang:error({gpb_error, no_messages})
         end)]
      || gpb_lib:get_epb_functions_by_opts(Opts)]].

format_decoders_top_function_msgs(Defs, AnRes, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    Error = ("error({gpb_error," ++
             ""     "{decoding_failure," ++
             ""     " {Bin, MsgName, {Class, Reason, StackTrace}}}})"),
    DecodeMsg1Catch_GetStackTraceAsPattern =
        ?f("decode_msg_1_catch(Bin, MsgName, TrUserData) ->~n"
           "    try decode_msg_2_doit(MsgName, Bin, TrUserData)~n"
           "    catch Class:Reason:StackTrace -> ~s~n"
           "    end.~n", [Error]),
    DecodeMsg1Catch_GetStackTraceAsCall =
        ?f("decode_msg_1_catch(Bin, MsgName, TrUserData) ->~n"
           "    try decode_msg_2_doit(MsgName, Bin, TrUserData)~n"
           "    catch Class:Reason ->~n"
           "        StackTrace = erlang:get_stacktrace(),~n"
           "        ~s~n"
           "    end.~n", [Error]),
    [gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, MsgName) when is_binary(Bin) ->
               call_self(Bin, MsgName, [])
       end),
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, MsgName, Opts) when is_binary(Bin) ->
               TrUserData = proplists:get_value(user_data, Opts),
               decode_msg_1_catch(Bin, MsgName, TrUserData)
       end),
     ["-ifdef('OTP_RELEASE').\n", % This macro appeared in Erlang 21
      DecodeMsg1Catch_GetStackTraceAsPattern,
      "-else.\n",
      DecodeMsg1Catch_GetStackTraceAsCall,
      "-endif.\n\n"],
     gpb_codegen:format_fn(
       decode_msg_2_doit,
       fun('MsgName', Bin, TrUserData) ->
               'Tr'('decode-fn'(Bin, 'TrUserData'), TrUserData)
       end,
       [repeat_clauses(
          'MsgName',
          [begin
               ElemPath = [MsgName],
               Transl = gpb_gen_translators:find_translation(
                          ElemPath, decode, AnRes),
               [replace_term('MsgName', MsgName),
                replace_term('Tr', Transl),
                replace_term('decode-fn', gpb_lib:mk_fn(decode_msg_, MsgName))]
           end
           || {{msg, MsgName}, _Fields} <- Defs]),
        splice_trees('TrUserData', if DoNif -> [];
                                      true  -> [?expr(TrUserData)]
                                   end)]),
     [["\n",
       "%% epb compatibility\n",
       gpb_codegen:format_fn(
         decode,
         fun(MsgName, Bin) when is_atom(MsgName), is_binary(Bin) ->
                 decode_msg(Bin, MsgName)
         end),
       [gpb_codegen:format_fn(
          gpb_lib:mk_fn(decode_, MsgName),
          fun(Bin) when is_binary(Bin) ->
                  decode_msg(Bin, 'MsgName')
          end,
          [replace_term('MsgName', MsgName)])
        || {{msg,MsgName}, _Fields} <- Defs]]
      || gpb_lib:get_epb_functions_by_opts(Opts)]].

format_aux_decoders(Defs, AnRes, _Opts) ->
    [format_enum_decoders(Defs, AnRes),
     [format_read_group_fn() || gpb_lib:contains_messages(Defs)]].

format_enum_decoders(Defs, #anres{used_types=UsedTypes}) ->
    %% FIXME: enum values can be negative, but "raw" varints are positive
    %%        insert a 2-complement in the mapping in order to move computations
    %%        from run-time to compile-time??
    [gpb_codegen:format_fn(
       gpb_lib:mk_fn(d_enum_, EnumName),
       fun('<EnumValue>') -> '<EnumSym>';
          (V) -> V % for yet unknown enums
       end,
       [repeat_clauses('<EnumValue>',
                       [[replace_term('<EnumValue>', EnumValue),
                         replace_term('<EnumSym>', EnumSym)]
                        || {EnumSym, EnumValue} <- gpb_lib:unalias_enum(
                                                     EnumDef)])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        gpb_lib:smember({enum,EnumName}, UsedTypes)].

format_map_decoders(Defs, AnRes, Opts0) ->
    Opts1 = case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts0)
            of
                '2tuples' -> [{msgs_as_maps, false} | Opts0];
                maps      -> [{msgs_as_maps, true} | Opts0]
            end,
    format_msg_decoders(Defs, AnRes, Opts1).

format_msg_decoders(Defs, AnRes, Opts) ->
    [[[gpb_codegen:format_fn(
         gpb_lib:mk_fn(decode_msg_, MsgName),
         fun(Bin) ->
                 %% The undefined is the default TrUserData
                 call_self(Bin, undefined)
         end)
       || {{msg,MsgName},_Fields} <- Defs]
      || gpb_lib:get_bypass_wrappers_by_opts(Opts)],
     [format_msg_decoder(Name, MsgDef, Defs, AnRes, Opts)
      || {_Type, Name, MsgDef} <- gpb_lib:msgs_or_groups(Defs)]].

format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    %% The general idea here is:
    %%
    %% - The code layout function can assume pass_as_records
    %%   and only generate code for records.
    %% - Code generation for maps is done post-generation
    %%   as additional steps using the code morpher functionality
    %% - The pass_as_params feature (improves performance),
    %%   is also done post-generation using the code-morpher.
    %%
    TrUserDataVar = ?expr(TrUserData),
    InitExprs = gpb_decoders_lib:init_exprs(MsgName, MsgDef, Defs,
                                            TrUserDataVar, AnRes, Opts),
    Fns = lists:flatten(
            [format_msg_decoder_read_field(MsgName, MsgDef, InitExprs,
                                           AnRes),
             format_field_decoders(MsgName, MsgDef, AnRes, Opts),
             format_field_skippers(MsgName)]),
    FieldPass = gpb_lib:get_field_pass(MsgName, AnRes),
    MappingUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    FNames = [FName || {FName, _InitExpr} <- InitExprs],
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    FieldInfos = gpb_decoders_lib:calc_field_infos(MsgDef, IsProto3, Opts),
    %% Compute extra post-generation operations needed for maps
    %% and pass_as_params
    Ops = case {MappingUnset, FieldPass} of
              {records, pass_as_record} ->
                  [gpb_decoders_lib:underscore_unused_vars()];
              {records, pass_as_params} ->
                  [gpb_decoders_lib:explode_param_init(MsgName, InitExprs, 4),
                   gpb_decoders_lib:explode_param_pass(MsgName, FNames, 4),
                   gpb_decoders_lib:underscore_unused_vars()];
              {#maps{unset_optional=present_undefined},pass_as_record} ->
                  [gpb_decoders_lib:rework_records_to_maps(4, FieldInfos,
                                                           undefined),
                   gpb_decoders_lib:underscore_unused_vars(),
                   gpb_decoders_lib:finalize_marked_map_exprs(Opts)];
              {#maps{unset_optional=present_undefined},pass_as_params} ->
                  [gpb_decoders_lib:explode_param_init(MsgName, InitExprs, 4),
                   gpb_decoders_lib:explode_param_pass(MsgName, FNames, 4),
                   gpb_decoders_lib:implode_to_map_exprs_all_mandatory(),
                   gpb_decoders_lib:underscore_unused_vars(),
                   gpb_decoders_lib:finalize_marked_map_exprs(Opts)];
              {#maps{unset_optional=omitted}, pass_as_record} ->
                  [gpb_decoders_lib:change_undef_marker_in_clauses('$undef'),
                   gpb_decoders_lib:rework_records_to_maps(4, FieldInfos,
                                                           '$undef'),
                   gpb_decoders_lib:underscore_unused_vars(),
                   gpb_decoders_lib:finalize_marked_map_exprs(Opts)];
              {#maps{unset_optional=omitted}, pass_as_params} ->
                  [gpb_decoders_lib:change_undef_marker_in_clauses('$undef'),
                   gpb_decoders_lib:explode_param_init(MsgName, InitExprs, 4),
                   gpb_decoders_lib:explode_param_pass(MsgName, FNames, 4),
                   gpb_decoders_lib:implode_to_map_exprs(4, FieldInfos,
                                                         '$undef'),
                   gpb_decoders_lib:underscore_unused_vars(),
                   gpb_decoders_lib:finalize_marked_map_exprs(Opts)]
          end,
    gpb_decoders_lib:run_morph_ops(Ops, Fns).

format_msg_decoder_read_field(MsgName, MsgDef, InitExprs, AnRes) ->
    Key = ?expr(Key),
    Rest = ?expr(Rest),
    {Param, FParam, FParamBinds} =
        gpb_decoders_lib:decoder_read_field_param(MsgName, MsgDef),
    Bindings = new_bindings([{'<Param>', Param},
                             {'<FParam>', FParam},
                             {'<FFields>', FParamBinds},
                             {'<Key>', Key},
                             {'<Rest>', Rest},
                             {'<TrUserData>', ?expr(TrUserData)}]),
    [format_msg_init_decoder(MsgName, InitExprs),
     format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes),
     format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes)].

format_msg_init_decoder(MsgName, InitExprs) ->
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(decode_msg_, MsgName),
          fun(Bin, TrUserData) ->
                  '<decode-field-fp>'(Bin, 0, 0, '<init>', TrUserData)
          end,
          [replace_term('<decode-field-fp>',
                        gpb_lib:mk_fn(dfp_read_field_def_, MsgName)),
           replace_tree('<init>',
                        gpb_lib:record_create(MsgName, InitExprs))]),
    #fn{name = boot,
        initializes_fields = true,
        tree = T}.

format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes) ->
    %% The fast-path decoder directly matches the minimal varint form
    %% of the field-number combined with the wiretype.
    %% Unrecognized fields fall back to the more generic decoder-loop
    Param = fetch_binding('<Param>', Bindings),
    FParam = fetch_binding('<FParam>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
          fun('<precomputed-binary-match>', Z1, Z2, '<Param>', TrUserData) ->
                  '<calls-to-field-decoding>';
             (<<>>, 0, 0, '<FParam>', TrUserData) ->
                  '<finalize-result>';
             (Other, Z1, Z2, '<Param>', TrUserData) ->
                  '<decode-general>'(Other, Z1, Z2, '<Param>', TrUserData)
          end,
          [replace_tree('<Param>', Param),
           replace_tree('<FParam>', FParam),
           repeat_clauses(
             '<precomputed-binary-match>',
             [[replace_tree('<precomputed-binary-match>', BinMatch),
               replace_tree('<calls-to-field-decoding>', FnCall)]
              || {BinMatch, FnCall} <- decoder_fp(Bindings, MsgName, MsgDef)]),
           replace_tree('<finalize-result>',
                        gpb_decoders_lib:decoder_finalize_result(
                          Param, FFields,
                          MsgName, ?expr(TrUserData),
                          AnRes)),
           replace_term('<decode-general>',
                        gpb_lib:mk_fn(dg_read_field_def_, MsgName))]),
    #fn{name = fastpath,
        has_finalizer = true,
        passes_msg = true,
        tree = T}.

format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes) ->
    %% The more general field selecting decoder
    %% Stuff that ends up here: non-minimal varint forms and field to skip
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    FParam = fetch_binding('<FParam>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(dg_read_field_def_, MsgName),
          fun(<<1:1, X:7, '<Rest>'/binary>>, N, Acc, '<Param>', TrUserData)
                when N < (32-7) ->
                  call_self('<Rest>', N+7, X bsl N + Acc, '<Param>',
                            TrUserData);
             (<<0:1, X:7, '<Rest>'/binary>>, N, Acc, '<Param>', TrUserData) ->
                  '<Key>' = X bsl N + Acc,
                  '<calls-to-field-decoding-or-skip>';
             (<<>>, 0, 0, '<FParam>', TrUserData) ->
                  '<finalize-result>'
          end,
          [replace_tree('<Key>', Key),
           replace_tree('<Rest>', Rest),
           replace_tree('<Param>', Param),
           replace_tree('<FParam>', FParam),
           replace_tree('<calls-to-field-decoding-or-skip>',
                        decoder_field_calls(Bindings, MsgName, MsgDef, AnRes)),
           replace_tree('<finalize-result>',
                        gpb_decoders_lib:decoder_finalize_result(
                          Param, FFields,
                          MsgName, ?expr(TrUserData),
                          AnRes))]),
    #fn{name = generic,
        has_finalizer = true,
        passes_msg = true,
        tree = T}.

%% compute info for the fast-path field recognition/decoding-call
decoder_fp(Bindings, MsgName, MsgDef) ->
    Rest = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    [begin
         BMatch = ?expr(<<'<field-and-wiretype-bytes>', '<Rest>'/binary>>,
                        [splice_trees('<field-and-wiretype-bytes>',
                                      gpb_lib:varint_to_binary_fields(
                                        Selector)),
                         replace_tree('<Rest>', Rest)]),
         FnCall = ?expr('decode_field'('<Rest>', Z1, Z2, '<Param>',
                                       'TrUserData'),
                        [replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         replace_tree('<Param>', Param),
                         replace_tree('TrUserData', TrUserDataVar)]),
         {BMatch, FnCall}
     end
     || {Selector, DecodeFn} <- decoder_field_selectors(MsgName, MsgDef)].

decoder_field_calls(Bindings, MsgName, []=_MsgDef, _AnRes) ->
    Key = fetch_binding('<Key>', Bindings),
    WiretypeExpr = ?expr('<Key>' band 7, [replace_tree('<Key>', Key)]),
    Bindings1 = add_binding({'<wiretype-expr>', WiretypeExpr}, Bindings),
    decoder_skip_calls(Bindings1, MsgName);
decoder_field_calls(Bindings, MsgName, MsgDef, AnRes) ->
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    SkipCalls = decoder_field_calls(Bindings, MsgName, [], AnRes),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    FieldSelects = decoder_field_selectors(MsgName, MsgDef),
    ?expr(case '<Key>' of
              '<selector>' -> 'decode_field'('<Rest>', 0, 0, '<Param>',
                                             'TrUserData');
              _            -> '<skip-calls>'
       end,
       [replace_tree('<Key>', Key),
        repeat_clauses('<selector>',
                       [[replace_term('<selector>', Selector),
                         replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         replace_tree('<Param>', Param)]
                        || {Selector, DecodeFn} <- FieldSelects]),
        replace_tree('<skip-calls>', SkipCalls),
        replace_tree('TrUserData', TrUserDataVar)]).

decoder_skip_calls(Bindings, MsgName) ->
    KeyExpr = fetch_binding('<Key>', Bindings),
    FieldNumExpr = ?expr('<Key>' bsr 3, [replace_tree('<Key>', KeyExpr)]),
    WiretypeExpr = fetch_binding('<wiretype-expr>', Bindings),
    RestExpr = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    ?expr(case '<wiretype-expr>' of
              0 -> skip_vi('<Rest>', 0, 0, '<Param>', 'TrUserData');
              1 -> skip_64('<Rest>', 0, 0, '<Param>', 'TrUserData');
              2 -> skip_ld('<Rest>', 0, 0, '<Param>', 'TrUserData');
              3 -> skip_gr('<Rest>', 'FNum', 0, '<Param>', 'TrUserData');
              5 -> skip_32('<Rest>', 0, 0, '<Param>', 'TrUserData')
          end,
          [replace_tree('<wiretype-expr>', WiretypeExpr),
           replace_tree('<Rest>', RestExpr),
           replace_tree('<Param>', Param),
           replace_tree('TrUserData', TrUserDataVar),
           replace_term(skip_vi, gpb_lib:mk_fn(skip_varint_, MsgName)),
           replace_term(skip_64, gpb_lib:mk_fn(skip_64_, MsgName)),
           replace_term(skip_ld, gpb_lib:mk_fn(skip_length_delimited_,
                                               MsgName)),
           replace_term(skip_gr, gpb_lib:mk_fn(skip_group_, MsgName)),
           replace_tree('FNum', FieldNumExpr),
           replace_term(skip_32, gpb_lib:mk_fn(skip_32_, MsgName))]).

decoder_field_selectors(MsgName, MsgDef) ->
    lists:append(
      map_msgdef_fields_o(
        fun(#?gpb_field{name=FName, fnum=FNum, type=Type, occurrence=Occ},
            _IsOneof) ->
                case Occ == repeated andalso gpb:is_type_packable(Type) of
                    true ->
                        %% "Protocol buffer parsers must be able to parse
                        %% repeated fields that were compiled as packed
                        %% as if they were not packed, and vice versa."
                        %%
                        %% So generate selectors for recognizing both
                        %% the packed and unpacked case.
                        PWiretype = gpb:encode_wiretype(bytes),
                        UWiretype = gpb:encode_wiretype(Type),
                        [begin
                             Selector = (FNum bsl 3) bor Wiretype,
                             DecodeFn = gpb_lib:mk_fn(Prefix, MsgName, FName),
                             {Selector, DecodeFn}
                         end
                         || {Wiretype, Prefix} <- [{PWiretype, d_pfield_},
                                                   {UWiretype, d_field_}]];
                    false ->
                        Wiretype = case Type of
                                       {group, _} ->
                                           gpb:encode_wiretype(group_start);
                                       _ ->
                                           gpb:encode_wiretype(Type)
                                   end,
                        Selector = (FNum bsl 3) bor Wiretype,
                        DecodeFn = gpb_lib:mk_fn(d_field_, MsgName, FName),
                        [{Selector, DecodeFn}]
                end
        end,
        MsgDef)).

format_field_decoders(MsgName, MsgDef, AnRes, Opts) ->
    map_msgdef_fields_o(
      fun(Field, IsOneof) ->
              format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts)
      end,
      MsgDef).

format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts) ->
    XField = {Field, IsOneof},
    case Field of
        #?gpb_field{occurrence=repeated, type=Type} ->
            case gpb:is_type_packable(Type) of
                true ->
                    %% Generate decoder functions for both the packed
                    %% and unpacked case
                    [format_non_packed_field_decoder(MsgName, XField, AnRes,
                                                     Opts),
                     %% A packed field can never be one of a `oneof' fields
                     %% So pass Field and not XField
                     format_packed_field_decoder(MsgName, Field, AnRes, Opts)];
                false ->
                    format_non_packed_field_decoder(MsgName, XField, AnRes,
                                                    Opts)
            end;
        _ ->
            format_non_packed_field_decoder(MsgName, XField, AnRes, Opts)
    end.

format_non_packed_field_decoder(MsgName, XField, AnRes, Opts) ->
    {#?gpb_field{type=Type}, _IsOneof} = XField,
    case Type of
        sint32   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        sint64   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        int32    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        int64    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        uint32   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        uint64   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bool     -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {enum,_} -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        fixed32  -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        sfixed32 -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        float    -> format_floating_point_field_decoder(MsgName, XField,
                                                        float, AnRes);
        fixed64  -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        sfixed64 -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        double   -> format_floating_point_field_decoder(MsgName, XField,
                                                        double, AnRes);
        string   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {map,_,_}-> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {group,_}-> format_group_field_decoder(MsgName, XField, AnRes)
    end.

format_packed_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #?gpb_field{name=FName} = FieldDef,
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_pfield_, MsgName, FName),
          fun(<<1:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData)
                when N < ?NB ->
                  call_self(Rest, N + 7, X bsl N + Acc, Msg, TrUserData);
             (<<0:1, X:7, Rest/binary>>, N, Acc, #'MsgName'{field=E}=Msg,
              TrUserData) ->
                  Len = X bsl N + Acc,
                  <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
                  NewSeq = decode_packed(PackedBytes, 0, 0, E, TrUserData),
                  '<call-read-field>'(Rest2, 0, 0,
                                      Msg#'MsgName'{field=NewSeq},
                                      TrUserData)
          end,
          [replace_term(decode_packed,
                        gpb_lib:mk_fn(d_packed_field_, MsgName, FName)),
           replace_term('<call-read-field>',
                        gpb_lib:mk_fn(dfp_read_field_def_, MsgName)),
           replace_term('MsgName', MsgName),
           replace_term(field, FName)]),
    [#fn{name = packed_field,
         passes_msg = true,
         tree = T},
     format_packed_field_seq_decoder(MsgName, FieldDef, AnRes, Opts)].

format_packed_field_seq_decoder(MsgName, #?gpb_field{type=Type}=Field,
                                AnRes, Opts) ->
    case Type of
        fixed32  -> format_dpacked_nonvi(MsgName, Field, 32, [little],
                                         AnRes);
        sfixed32 -> format_dpacked_nonvi(MsgName, Field, 32, [little,signed],
                                         AnRes);
        float    -> format_dpacked_nonvi(MsgName, Field, 32, float,
                                         AnRes);
        fixed64  -> format_dpacked_nonvi(MsgName, Field, 64, [little],
                                         AnRes);
        sfixed64 -> format_dpacked_nonvi(MsgName, Field, 64, [little,signed],
                                         AnRes);
        double   -> format_dpacked_nonvi(MsgName, Field, 64, double,
                                         AnRes);
        _        -> format_dpacked_vi(MsgName, Field, AnRes, Opts)
    end.

format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 32, float, AnRes) ->
    ElemPath = [MsgName, FName, []],
    TranslFn = gpb_gen_translators:find_translation(ElemPath, decode, AnRes),
    Cons = gpb_gen_translators:find_translation(
             ElemPath, decode_repeated_add_elem, AnRes),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, AccSeq, TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(infinity, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<0:16,128,255, Rest/binary>>, Z1, Z2, AccSeq, TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'('-infinity', TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2, AccSeq,
              TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(nan, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<Value:32/little-float, Rest/binary>>, Z1, Z2, AccSeq,
              TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(Value, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<>>, _, _, AccSeq, _TrUserData) ->
                  AccSeq
          end,
          [replace_term('Tr', TranslFn),
           replace_term('[New|Acc]', Cons)]),
    #fn{name = packed_float,
        tree = T};
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 64, double, AnRes) ->
    ElemPath = [MsgName, FName, []],
    TranslFn = gpb_gen_translators:find_translation(ElemPath, decode, AnRes),
    Cons = gpb_gen_translators:find_translation(
             ElemPath, decode_repeated_add_elem, AnRes),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, AccSeq, TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(infinity, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<0:48,240,255, Rest/binary>>, Z1, Z2, AccSeq, TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'('-infinity', TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2, AccSeq,
              TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(nan, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<Value:64/little-float, Rest/binary>>, Z1, Z2, AccSeq,
              TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(Value, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<>>, _, _, AccSeq, _TrUserData) ->
                  AccSeq
          end,
          [replace_term('Tr', TranslFn),
           replace_term('[New|Acc]', Cons)]),
    #fn{name = packed_double,
        tree = T};
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, BitLen, BitTypes,
                     AnRes) ->
    ElemPath = [MsgName, FName, []],
    TranslFn = gpb_gen_translators:find_translation(ElemPath, decode, AnRes),
    Cons = gpb_gen_translators:find_translation(
             ElemPath, decode_repeated_add_elem, AnRes),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, AccSeq, TrUserData) ->
                  call_self(Rest, Z1, Z2,
                            '[New|Acc]'('Tr'(Value, TrUserData), AccSeq,
                                        TrUserData),
                            TrUserData);
             (<<>>, _, _, AccSeq, _TrUserData) ->
                  AccSeq
          end,
          [replace_term('<N>', BitLen),
           splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes]),
           replace_term('Tr', TranslFn),
           replace_term('[New|Acc]', Cons)]),
    #fn{name = packed_fixint,
        tree = T}.

format_dpacked_vi(MsgName, #?gpb_field{name=FName}=FieldDef, AnRes, Opts) ->
    ExtValue = ?expr(X bsl N + Acc),
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, false,
                                                AnRes),
    DExpr = decode_int_value(ExtValue, Rest, TrUserDataVar, FieldDef, Tr, Opts),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<1:1, X:7, Rest/binary>>, N, Acc, AccSeq, TrUserData)
                when N < ?NB ->
                  call_self(Rest, N + 7, X bsl N + Acc, AccSeq, TrUserData);
             (<<0:1, X:7, Rest/binary>>, N, Acc, AccSeq, TrUserData) ->
                  {NewFValue, RestF} = '<decode-expr>',
                  call_self(RestF, 0, 0, [NewFValue | AccSeq], TrUserData);
             (<<>>, 0, 0, AccSeq, TrUserData) ->
                  AccSeq
          end,
          [replace_tree('<decode-expr>', DExpr)]),
    #fn{name = packed_vi_based,
        tree = T}.

format_vi_based_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName}=FieldDef, IsOneof}=XFieldDef,
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    MsgVar =?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    ReadFieldDefFn = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    OutParam = updated_merged_param(MsgName, XFieldDef, AnRes,
                                    FVar, PrevValue, MsgVar,
                                    TrUserDataVar),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof,
                                                AnRes),
    DExpr = decode_int_value(ExtValue, Rest, TrUserDataVar, FieldDef, Tr, Opts),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_field_, MsgName, FName),
          fun(<<1:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData)
                when N < ?NB ->
                  call_self(Rest, N + 7, X bsl N + Acc, Msg, TrUserData);
             (<<0:1, X:7, Rest/binary>>, N, Acc, 'InParam', TrUserData) ->
                  {'FVar', RestF} = '<decode-expr>',
                  '<call-read-field>'(RestF, 0, 0, 'OutParam', 'TrUserData')
          end,
          [replace_tree('InParam', InParam),
           replace_term('<call-read-field>', ReadFieldDefFn),
           replace_tree('FVar', FVar),
           replace_tree('<decode-expr>', DExpr),
           replace_tree('OutParam', OutParam),
           replace_tree('TrUserData', TrUserDataVar)]),
    #fn{name = vi_based,
        passes_msg = true,
        tree = T}.

decode_int_value(ExtValueExpr, Rest, TrUserDataVar, FieldDef, Tr, Opts) ->
    #?gpb_field{type=Type}=FieldDef,
    StringsAsBinaries = gpb_lib:get_strings_as_binaries_by_opts(Opts),
    TrReplacements = [replace_tree('TrUserData', TrUserDataVar),
                      replace_term('Tr', Tr(decode))],
    case Type of
        sint32 ->
            tuplify(decode_zigzag(ExtValueExpr, Tr, TrUserDataVar), Rest);
        sint64 ->
            tuplify(decode_zigzag(ExtValueExpr, Tr, TrUserDataVar), Rest);
        int32 ->
            tuplify(decode_uint_to_int(ExtValueExpr, 32, Tr, TrUserDataVar),
                    Rest);
        int64 ->
            tuplify(decode_uint_to_int(ExtValueExpr, 64, Tr, TrUserDataVar),
                    Rest);
        uint32 ->
            tuplify(?expr('Tr'('ExtValueExpr', 'TrUserData'),
                          [replace_tree('ExtValueExpr', ExtValueExpr)
                           | TrReplacements]),
                    Rest);
        uint64 ->
            tuplify(?expr('Tr'('ExtValueExpr', 'TrUserData'),
                          [replace_tree('ExtValueExpr', ExtValueExpr)
                           | TrReplacements]),
                    Rest);
        bool ->
            tuplify(?expr('Tr'(('ExtValueExpr') =/= 0, 'TrUserData'),
                          [replace_tree('ExtValueExpr', ExtValueExpr)
                           | TrReplacements]),
                    Rest);
        {enum, EnumName} ->
            EnumDecodeFn = gpb_lib:mk_fn(d_enum_, EnumName),
            Tr2 = fun(decode) -> id end,
            UintToIntExpr = decode_uint_to_int(ExtValueExpr, 32,
                                               Tr2, TrUserDataVar),
            ToSym = ?expr('Tr'('decode-enum'('decode-uint-to-int'),
                               'TrUserData'),
                          [replace_term('decode-enum', EnumDecodeFn),
                           replace_tree('decode-uint-to-int', UintToIntExpr)
                           | TrReplacements]),
            tuplify(ToSym, Rest);
        string when StringsAsBinaries ->
            unpack_bytes(ExtValueExpr, Rest, Tr, TrUserDataVar, Opts);
        string when not StringsAsBinaries ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Utf8:Len/binary, Rest2/binary>> = 'Rest',
                      {'Tr'(unicode:characters_to_list(Utf8, unicode),
                            'TrUserData'), Rest2}
                  end,
                  [replace_tree('ExtValueExpr', ExtValueExpr),
                   replace_tree('Rest', Rest)
                   | TrReplacements]);
        bytes ->
            unpack_bytes(ExtValueExpr, Rest, Tr, TrUserDataVar, Opts);
        {msg, Msg2Name} ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bs:Len/binary, Rest2/binary>> = 'Rest',
                      {'Tr'('d-msg-X'(Bs, 'TrUserData'), 'TrUserData'), Rest2}
                  end,
                  [replace_tree('ExtValueExpr', ExtValueExpr),
                   replace_tree('Rest', Rest),
                   replace_term('d-msg-X',
                                gpb_lib:mk_fn(decode_msg_, Msg2Name))
                   | TrReplacements]);
        {map, KeyType, ValueType} ->
            MapAsMsgMame = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
            F2 = FieldDef#?gpb_field{type={msg,MapAsMsgMame}},
            decode_int_value(ExtValueExpr, Rest, TrUserDataVar, F2, Tr, Opts)
    end.

unpack_bytes(ExtValueExpr, Rest, Tr, TrUserDataVar, Opts) ->
    CompilerHasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    Copy = case proplists:get_value(copy_bytes, Opts, auto) of
               auto when not CompilerHasBinary -> false;
               auto when CompilerHasBinary     -> true;
               true                            -> true;
               false                           -> false;
               N when is_integer(N)            -> N;
               N when is_float(N)              -> N
           end,
    Transforms = [replace_tree('ExtValueExpr', ExtValueExpr),
                  replace_tree('Rest', Rest),
                  replace_term('Tr', Tr(decode)),
                  replace_tree('TrUserData', TrUserDataVar)],
    if Copy == false ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bytes:Len/binary, Rest2/binary>> = 'Rest',
                      {'Tr'(Bytes, 'TrUserData'), Rest2}
                  end,
                  Transforms);
       Copy == true ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bytes:Len/binary, Rest2/binary>> = 'Rest',
                      {'Tr'(binary:copy(Bytes), 'TrUserData'), Rest2}
                  end,
                  Transforms);
       is_integer(Copy); is_float(Copy) ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bytes:Len/binary, Rest2/binary>> = 'Rest',
                      Res = case binary:referenced_byte_size(Bytes) of
                                LB when LB >= byte_size(Bytes) * 'Copy' ->
                                    'Tr'(binary:copy(Bytes), 'TrUserData');
                                _ ->
                                    'Tr'(Bytes, 'TrUserData')
                            end,
                      {Res, Rest2}
                  end,
                  [replace_term('Copy', Copy) | Transforms])
       end.

format_group_field_decoder(MsgName, XFieldDef, AnRes) ->
    {#?gpb_field{name=FName, fnum=FNum, type={group,GroupName}}=FieldDef,
     IsOneof}=XFieldDef,
    ResVar = ?expr(NewFValue), %% result is to be put in this variable
    TrUserDataVar = ?expr(TrUserData),
    MsgVar = ?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    OutParam = updated_merged_param(MsgName, XFieldDef, AnRes,
                                    ResVar, PrevValue, MsgVar,
                                    TrUserDataVar),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof,
                                                AnRes),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_field_, MsgName, FName),
          fun(Bin, _, _, 'InParam', TrUserData) ->
                  {GroupBin, Rest} = read_group(Bin, 'FieldNum'),
                  'Res' = 'Tr'('d_msg_X'(GroupBin, TrUserData), TrUserData),
                  'call-read-field'(Rest, 0, 0, 'OutParam', TrUserData)
          end,
          [replace_tree('InParam', InParam),
           replace_term('call-read-field', gpb_lib:mk_fn(dfp_read_field_def_,
                                                         MsgName)),
           replace_term('Tr', Tr(decode)),
           replace_tree('Res', ResVar),
           replace_tree('FieldNum', erl_syntax:integer(FNum)),
           replace_term('d_msg_X', gpb_lib:mk_fn(decode_msg_, GroupName)),
           replace_tree('OutParam', OutParam)]),
    #fn{name = group,
        passes_msg = true,
        tree = T}.

updated_merged_param(MsgName, XFieldDef, AnRes, NewValue, PrevValue,
                     MsgVar, TrUserDataVar) ->
    Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(MsgName, XFieldDef,
                                                           AnRes),
    MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                   MsgName, Tr, TrUserDataVar,
                                   AnRes),
    case XFieldDef of
        {#?gpb_field{name=FName}, false} ->
            gpb_lib:record_update(MsgVar, MsgName, [{FName, MergedValue}]);
        {_OField, {true, CFName}} ->
            gpb_lib:record_update(MsgVar, MsgName, [{CFName, MergedValue}])
    end.

merge_field_expr({FieldDef, false}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes) ->
    case gpb_lib:classify_field_merge_action(FieldDef) of
        overwrite ->
            NewValue;
        seqadd ->
            ElemPath = [MsgName, gpb_lib:get_field_name(FieldDef)],
            Cons = gpb_gen_translators:find_translation(
                     ElemPath,
                     decode_repeated_add_elem,
                     AnRes),
            ?expr('[New|Acc]'('<New>', '<Acc>', 'TrUserData'),
                  [replace_term('[New|Acc]', Cons),
                   replace_tree('<New>', NewValue),
                   replace_tree('<Acc>', PrevValue),
                   replace_tree('TrUserData', TrUserDataVar)]);
        msgmerge ->
            FMsgName = case FieldDef of
                           #?gpb_field{type={msg,Nm}} -> Nm;
                           #?gpb_field{type={group,Nm}} -> Nm
                       end,
            MergeFn = gpb_lib:mk_fn(merge_msg_, FMsgName),
            ?expr(if 'Prev' == undefined -> 'New';
                     true -> 'merge_msg_X'('Prev', 'New', 'TrUserData')
                  end,
                  [replace_term('merge_msg_X', Tr(merge, MergeFn)),
                   replace_tree('Prev', PrevValue),
                   replace_tree('New', NewValue),
                   replace_tree('TrUserData', TrUserDataVar)])
    end;
merge_field_expr({FieldDef, {true, CFName}}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes)->
    CfElemPath = [MsgName, CFName],
    CfTransl = gpb_gen_translators:find_translation(CfElemPath, decode, AnRes),
    #?gpb_field{name=FName, type=Type} = FieldDef,
    if ?is_msg_or_group(Type) ->
            {_, FMsgName} = Type,
            MergeFn = gpb_lib:mk_fn(merge_msg_, FMsgName),
            MVPrev = gpb_lib:prefix_var("MV", PrevValue),
            ?expr(case 'Prev' of
                      undefined ->
                          'Tr'({'tag', 'New'}, 'TrUserData');
                      {'tag', 'MVPrev'} ->
                          'Tr'({'tag', 'merge_msg_X'('MVPrev', 'New',
                                                'TrUserData')},
                               'TrUserData');
                      _ ->
                          'Tr'({'tag', 'New'}, 'TrUserData')
                  end,
                  [replace_tree('Prev', PrevValue),
                   replace_term('tag', FName),
                   replace_tree('New', NewValue),
                   replace_term('merge_msg_X', Tr(merge, MergeFn)),
                   replace_tree('MVPrev', MVPrev),
                   replace_term('Tr', CfTransl),
                   replace_tree('TrUserData', TrUserDataVar)]);
       true ->
            %% Replace
            ?expr('Tr'({'tag', '<expr>'}, 'TrUserData'),
                  [replace_term('tag', FName),
                   replace_tree('<expr>', NewValue),
                   replace_term('Tr', CfTransl),
                   replace_tree('TrUserData', TrUserDataVar)])
    end.

decoder_in_param(MsgVar, MsgName, {FieldDef, false}) ->
    #?gpb_field{name=FName}=FieldDef,
    Prev = erl_syntax:variable('Prev'),
    InParam = gpb_lib:match_bind_var(
                gpb_lib:record_match(MsgName, [{FName, Prev}]),
                MsgVar),
    {InParam, Prev};
decoder_in_param(MsgVar, MsgName, {FieldDef, {true, CFName}}) ->
    #?gpb_field{type=Type} = FieldDef,
    if ?is_msg_or_group(Type) ->
            %% oneof fields that of message type may need merging
            Prev = erl_syntax:variable('Prev'),
            InParam = gpb_lib:match_bind_var(
                        gpb_lib:record_match(MsgName, [{CFName, Prev}]),
                        MsgVar),
            {InParam, Prev};
       true ->
            {MsgVar, erl_syntax:variable('Prev')}
    end.

format_fixlen_field_decoder(MsgName, XFieldDef, AnRes) ->
    {#?gpb_field{name=FName, type=Type}=Field, IsOneof} = XFieldDef,
    ElemPath = gpb_gen_translators:mk_elempath_elem(MsgName, Field, IsOneof),
    TranslFn = gpb_gen_translators:find_translation(ElemPath, decode, AnRes),
    {BitLen, BitTypes} = case Type of
                             fixed32  -> {32, [little]};
                             sfixed32 -> {32, [little,signed]};
                             float    -> {32, [little,float]};
                             fixed64  -> {64, [little]};
                             sfixed64 -> {64, [little,signed]};
                             double   -> {64, [little,float]}
                         end,
    MsgVar = ?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    TrUserDataVar = ?expr(TrUserData),
    TrValue = ?expr('Tr'(Value, TrUserData), [replace_term('Tr', TranslFn)]),
    Param2 = updated_merged_param(MsgName, XFieldDef, AnRes,
                                  TrValue, PrevValue, MsgVar,
                                  TrUserDataVar),
    ReadFieldDefFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_field_, MsgName, FName),
          fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, '<InParam>',
              'TrUserData') ->
                  '<call-read-field>'(Rest, Z1, Z2, '<OutParam>', 'TrUserData')
          end,
          [replace_term('<N>', BitLen),
           splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes]),
           replace_tree('<InParam>', InParam),
           replace_term('<call-read-field>', ReadFieldDefFnName),
           replace_tree('<OutParam>', Param2),
           replace_term('Tr', TranslFn),
           replace_tree('TrUserData', TrUserDataVar)]),
    #fn{name = fixlen,
        passes_msg = true,
        tree = T}.

format_floating_point_field_decoder(MsgName, XFieldDef, Type, AnRes) ->
    {#?gpb_field{name=FName}=Field, IsOneof} = XFieldDef,
    ElemPath = gpb_gen_translators:mk_elempath_elem(MsgName, Field, IsOneof),
    TranslFn = gpb_gen_translators:find_translation(ElemPath, decode, AnRes),
    TrUserDataVar = ?expr(TrUserData),
    MsgVar = ?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    OutParamReplacements =
        [begin
             TrOutExpr = ?expr('Tr'('OutExpr', TrUserData),
                               [replace_term('Tr', TranslFn),
                                replace_tree('OutExpr', OutExpr)]),
             UpdatedOutExpr = updated_merged_param(MsgName, XFieldDef, AnRes,
                                                   TrOutExpr, PrevValue, MsgVar,
                                                   TrUserDataVar),
             replace_tree(Marker, UpdatedOutExpr)
         end
         || {Marker, OutExpr} <- [{'OutParam', ?expr(Value)},
                                  {'InfinityOutParam', ?expr(infinity)},
                                  {'-InfinityOutParam', ?expr('-infinity')},
                                  {'NanOutParam', ?expr(nan)}]],
    ReadFieldDefFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    Replacements =
        [replace_tree('InParam', InParam),
         replace_term('<call-read-field>', ReadFieldDefFnName),
         replace_tree('TrUserData', TrUserDataVar),
         replace_term('Tr', TranslFn)] ++
        OutParamReplacements,
    T = case Type of
            float ->
                gpb_codegen:mk_fn(
                  gpb_lib:mk_fn(d_field_, MsgName, FName),
                  fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'InfinityOutParam',
                                              'TrUserData');
                     (<<0:16,128,255, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              '-InfinityOutParam',
                                              'TrUserData');
                     (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'NanOutParam',
                                              'TrUserData');
                     (<<Value:32/little-float, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'OutParam',
                                              'TrUserData')
                  end,
                  Replacements);
            double ->
                gpb_codegen:mk_fn(
                  gpb_lib:mk_fn(d_field_, MsgName, FName),
                  fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'InfinityOutParam',
                                              'TrUserData');
                     (<<0:48,240,255, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              '-InfinityOutParam',
                                              'TrUserData');
                     (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'NanOutParam',
                                              'TrUserData');
                     (<<Value:64/little-float, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'OutParam',
                                              'TrUserData')
                  end,
                  Replacements)
        end,
    #fn{name = float,
        passes_msg = true,
        tree = T}.

decode_zigzag(ExtValueExpr, Tr, TrUserDataVar) ->
    ?expr(begin
              ZValue = 'ExtValueExpr',
              if ZValue band 1 =:= 0 -> 'Tr'(ZValue bsr 1, 'TrUserData');
                 true                -> 'Tr'(-((ZValue + 1) bsr 1),
                                             'TrUserData')
              end
          end,
          [replace_tree('ExtValueExpr', ExtValueExpr),
           replace_term('Tr', Tr(decode)),
           replace_tree('TrUserData', TrUserDataVar)]).

decode_uint_to_int(ExtValueExpr, NumBits, Tr, TrUserDataVar) ->
    %% Contrary to the 64 bit encoding done for int32 (and enum),
    %% decode the value as 32 bits, so we decode negatives
    %% given both as 32 bits and as 64 bits wire encodings
    %% to the same integer.
    ?expr(begin
              <<Res:'N'/signed-native>> =
                  <<('ExtValueExpr'):'N'/unsigned-native>>,
              'Tr'(Res, 'TrUserData')
          end,
          [replace_term('N', NumBits),
           replace_tree('ExtValueExpr', ExtValueExpr),
           replace_term('Tr', Tr(decode)),
           replace_tree('TrUserData', TrUserDataVar)]).

format_read_group_fn() ->
    ["read_group(Bin, FieldNum) ->\n"
     "    {NumBytes, EndTagLen} = read_gr_b(Bin, 0, 0, 0, 0, FieldNum),\n"
     "    <<Group:NumBytes/binary, _:EndTagLen/binary, Rest/binary>> = Bin,\n"
     "    {Group, Rest}.\n"
     "\n"
     "%% Like skipping over fields, but record the total length,\n"
     "%% Each field is <(FieldNum bsl 3) bor FieldType> ++ <FieldValue>\n"
     "%% Record the length because varints may be non-optimally encoded.\n"
     "%%\n"
     "%% Groups can be nested, but assume the same FieldNum cannot be nested\n"
     "%% because group field numbers are shared with the rest of the fields\n"
     "%% numbers. Thus we can search just for an group-end with the same\n"
     "%% field number.\n"
     "%%\n"
     "%% (The only time the same group field number could occur would\n"
     "%% be in a nested sub message, but then it would be inside a\n"
     "%% length-delimited entry, which we skip-read by length.)\n"
     "read_gr_b(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum)\n"
     "  when N < (32-7) ->\n"
     "    read_gr_b(Tl, N+7, X bsl N + Acc, NumBytes, TagLen+1, FieldNum);\n"
     "read_gr_b(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen,\n"
     "          FieldNum) ->\n"
     "    Key = X bsl N + Acc,\n"
     "    TagLen1 = TagLen + 1,\n"
     "    case {Key bsr 3, Key band 7} of\n"
     "        {FieldNum, 4} -> % 4 = group_end\n"
     "            {NumBytes, TagLen1};\n"
     "        {_, 0} -> % 0 = varint\n"
     "            read_gr_vi(Tl, 0, NumBytes + TagLen1, FieldNum);\n"
     "        {_, 1} -> % 1 = bits64\n"
     "            <<_:64, Tl2/binary>> = Tl,\n"
     "            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 8, 0, FieldNum);\n"
     "        {_, 2} -> % 2 = length_delimited\n"
     "            read_gr_ld(Tl, 0, 0, NumBytes + TagLen1, FieldNum);\n"
     "        {_, 3} -> % 3 = group_start\n"
     "            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);\n"
     "        {_, 4} -> % 4 = group_end\n"
     "            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);\n"
     "        {_, 5} -> % 5 = bits32\n"
     "            <<_:32, Tl2/binary>> = Tl,\n"
     "            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 4, 0, FieldNum)\n"
     "    end.\n"
     "\n"
     "read_gr_vi(<<1:1, _:7, Tl/binary>>, N, NumBytes, FieldNum)\n"
     "  when N < (64-7) ->\n"
     "    read_gr_vi(Tl, N+7, NumBytes+1, FieldNum);\n"
     "read_gr_vi(<<0:1, _:7, Tl/binary>>, _, NumBytes, FieldNum) ->\n"
     "    read_gr_b(Tl, 0, 0, NumBytes+1, 0, FieldNum).\n"
     "\n"
     "read_gr_ld(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum)\n"
     "  when N < (64-7) ->\n"
     "    read_gr_ld(Tl, N+7, X bsl N + Acc, NumBytes+1, FieldNum);\n"
     "read_gr_ld(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum) ->\n"
     "    Len = X bsl N + Acc,\n"
     "    NumBytes1 = NumBytes + 1,\n"
     "    <<_:Len/binary, Tl2/binary>> = Tl,\n"
     "    read_gr_b(Tl2, 0, 0, NumBytes1 + Len, 0, FieldNum).\n"].

format_field_skippers(MsgName) ->
    SkipVarintFnName = gpb_lib:mk_fn(skip_varint_, MsgName),
    SkipLenDelimFnName = gpb_lib:mk_fn(skip_length_delimited_, MsgName),
    SkipGroupFnName = gpb_lib:mk_fn(skip_group_, MsgName),
    ReadFieldFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    Ts = [%% skip_varint_<MsgName>/2,4
          gpb_codegen:mk_fn(
            SkipVarintFnName,
            fun(<<1:1, _:7, Rest/binary>>, Z1, Z2, Msg, TrUserData) ->
                    call_self(Rest, Z1, Z2, Msg, TrUserData);
               (<<0:1, _:7, Rest/binary>>, Z1, Z2, Msg, TrUserData) ->
                    '<call-read-field>'(Rest, Z1,Z2, Msg, TrUserData)
            end,
            [replace_term('<call-read-field>', ReadFieldFnName)]),
          %% skip_length_delimited_<MsgName>/4
          gpb_codegen:mk_fn(
            SkipLenDelimFnName,
            fun(<<1:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData)
                  when N < ?NB ->
                    call_self(Rest, N+7, X bsl N + Acc, Msg,
                              TrUserData);
               (<<0:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData) ->
                    Length = X bsl N + Acc,
                    <<_:Length/binary, Rest2/binary>> = Rest,
                    '<call-read-field>'(Rest2, 0, 0, Msg, TrUserData)
            end,
            [replace_term('<call-read-field>', ReadFieldFnName)]),
          %% skip_group_<MsgName>/4
          gpb_codegen:mk_fn(
            SkipGroupFnName,
            fun(Bin, FNum, Z2, Msg, TrUserData) ->
                    {_, Rest} = read_group(Bin, FNum),
                    '<call-read-field>'(Rest, 0, Z2, Msg, TrUserData)
            end,
            [replace_term('<call-read-field>', ReadFieldFnName)]),
          %% skip_32_<MsgName>/2,4
          %% skip_64_<MsgName>/2,4
          [gpb_codegen:mk_fn(
             gpb_lib:mk_fn(skip_, NumBits, MsgName),
             fun(<<_:'<NumBits>', Rest/binary>>, Z1, Z2, Msg, TrUserData) ->
                     '<call-read-field>'(Rest, Z1, Z2, Msg, TrUserData)
             end,
             [replace_term('<call-read-field>', ReadFieldFnName),
              replace_term('<NumBits>', NumBits)])
           || NumBits <- [32, 64]]],
    [#fn{name = skipper,
         passes_msg = true,
         tree=T}
     || T <- lists:flatten(Ts)].

new_bindings(Tuples) ->
    lists:foldl(fun add_binding/2, new_bindings(), Tuples).

new_bindings() ->
    dict:new().

add_binding({Key, Value}, Bindings) ->
    dict:store(Key, Value, Bindings).

fetch_binding(Key, Bindings) ->
    dict:fetch(Key, Bindings).

%% The fun takes two args: Fun(#?gpb_field{}, IsOneofField) -> term()
map_msgdef_fields_o(Fun, Fields) ->
    lists:reverse(
      lists:foldl(
        fun(#?gpb_field{}=Field, Acc) ->
                [Fun(Field, false) | Acc];
           (#gpb_oneof{name=CFName, fields=OFields}, Acc) ->
                IsOneOf = {true, CFName},
                lists:foldl(fun(OField, OAcc) -> [Fun(OField, IsOneOf) | OAcc]
                            end,
                            Acc,
                            OFields)
        end,
        [],
        Fields)).

tuplify(Expr, Rest) ->
    ?expr({'Expr', 'Rest'},
          [replace_tree('Expr', Expr),
           replace_tree('Rest', Rest)]).
