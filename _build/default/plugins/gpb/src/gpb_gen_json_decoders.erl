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

%%% @private
%%% @doc Generation of routines for converting JSON to internal format

-module(gpb_gen_json_decoders).

-export([format_exports/2]).
-export([format_top_function/3]).
-export([format_decoders/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").
-include("gpb_decoders_lib.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

format_exports(Defs, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    NoNif = not DoNif,
    [?f("-export([from_json/2"),[", from_json/3" || NoNif], ?f("]).~n"),
     [[[begin
            NoWrapperFnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
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

format_top_function(Defs, AnRes, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_top_function_msgs(Defs, AnRes, Opts);
        false -> format_top_function_no_msgs(Opts)
    end.

format_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoders(Defs, AnRes, Opts)].

format_top_function_no_msgs(_Opts) ->
    ["-spec from_json(term(), atom()) -> no_return().\n",
     gpb_codegen:format_fn(
       from_json,
       fun(_Json, _MsgName) ->
               erlang:error({gpb_error, no_messages})
       end),
     "-spec from_json(term(), atom(), list()) -> no_return().\n",
     gpb_codegen:format_fn(
       from_json,
       fun(_Json, _MsgName, _Opts) ->
               erlang:error({gpb_error, no_messages})
       end)].

format_top_function_msgs(Defs, AnRes, Opts) ->
    Error = ("error({gpb_error," ++
             ""     "{from_json_failure," ++
             ""     " {Json, MsgName, {Class, Reason, StackTrace}}}})"),
    FromJsonMsg1Catch_GetStackTraceAsPattern =
        ?f("from_json_1_catch(Json, MsgName, TrUserData) ->~n"
           "    try from_json_2_doit(MsgName, Json, TrUserData)~n"
           "    catch Class:Reason:StackTrace -> ~s~n"
           "    end.~n", [Error]),
    FromJsonMsg1Catch_GetStackTraceAsCall =
        ?f("from_json_1_catch(Json, MsgName, TrUserData) ->~n"
           "    try from_json_2_doit(MsgName, Json, TrUserData)~n"
           "    catch Class:Reason ->~n"
           "        StackTrace = erlang:get_stacktrace(),~n"
           "        ~s~n"
           "    end.~n", [Error]),
    DoNif = proplists:get_bool(nif, Opts),
    [gpb_codegen:format_fn(
       from_json,
       fun(Json, MsgName) ->
               call_self(Json, MsgName, [])
       end),
     gpb_codegen:format_fn(
       from_json,
       fun(Json, MsgName, Opts) ->
               TrUserData = proplists:get_value(user_data, Opts),
               from_json_1_catch(Json, MsgName, TrUserData)
       end),
     ["-ifdef('OTP_RELEASE').\n", % This macro appeared in Erlang 21
      FromJsonMsg1Catch_GetStackTraceAsPattern,
      "-else.\n",
      FromJsonMsg1Catch_GetStackTraceAsCall,
      "-endif.\n\n"],
     gpb_codegen:format_fn(
       from_json_2_doit,
       fun('MsgName', Json, TrUserData) ->
               'Tr'('from-json-fn'(Json, 'TrUserData'), TrUserData)
       end,
       [repeat_clauses(
          'MsgName',
          [begin
               ElemPath = [MsgName],
               Transl = gpb_gen_translators:find_translation(
                          ElemPath, decode, AnRes),
               [replace_term('MsgName', MsgName),
                replace_term('Tr', Transl),
                replace_term('from-json-fn',
                             gpb_lib:mk_fn(from_json_msg_, MsgName)),
                splice_trees('TrUserData', if DoNif -> [];
                                              true  -> [?expr(TrUserData)]
                                           end)]
           end
           || {{msg, MsgName}, _Fields} <- Defs])])].

format_msg_decoders(Defs, AnRes, Opts) ->
    [[[gpb_codegen:format_fn(
         gpb_lib:mk_fn(from_json_msg_, MsgName),
         fun(Bin) ->
                 %% The undefined is the default TrUserData
                 call_self(Bin, undefined)
         end)
       || {{msg,MsgName},_Fields} <- Defs]
      || gpb_lib:get_bypass_wrappers_by_opts(Opts)],
     [case {Type, test_proto3_wellknown(MsgName, MsgDef)} of
          {msg, {true, FormatterFn}} ->
              FormatterFn(MsgName, MsgDef, Defs, AnRes, Opts);
          _ ->
              format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts)
      end
      || {Type, MsgName, MsgDef} <- gpb_lib:msgs_or_groups(Defs)],
     format_helpers(Defs, AnRes, Opts)].

format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    %% Compared to decoding from protobuf wire-format (gpb_gen_decoders),
    %% We do not need to do any merging here intertwined with decoding.
    %% In Json, a key can occur only once.
    %%
    %% (In contrast, the protobuf wire-format is a stream of fields,
    %% if a field occurs more than once, the value is to be merged:
    %% - overwritten if it is an an optional or required scalar field,
    %% - appended to the sequence if the field is a repeated,
    %% - recursively merged if the field is another sub message.
    %% )
    %%

    %% Writing TrUserDataVar = ?expr(TrUserData) ought to be the same,
    %% but dialyzer complains down in the mk_call function.
    TrUserDataVar = erl_syntax:variable("TrUserData"),

    TmpAnRes = set_field_pass_as_params_for_all_msgs(AnRes),
    InitExprs1 = gpb_decoders_lib:init_exprs(MsgName, MsgDef, Defs,
                                             TrUserDataVar, TmpAnRes, Opts),
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    FieldInfos1 = gpb_decoders_lib:calc_field_infos(MsgDef, IsProto3, Opts),
    {InitExprs2, FieldInfos2} = defaultify_p3wellknowns(InitExprs1, MsgDef,
                                                        FieldInfos1,
                                                        TrUserDataVar),
    if MsgDef == [] ->
            format_msg_decoder_no_fields(MsgName, Opts);
       MsgDef /= [] ->
            [format_msg_init_decoder(MsgName, InitExprs2, FieldInfos2,
                                     TrUserDataVar, Opts),
             format_msg_decoder_loop(MsgName, MsgDef,
                                     TrUserDataVar, AnRes, Opts)]
    end.

set_field_pass_as_params_for_all_msgs(#anres{d_field_pass_method=FP}=AnRes) ->
    FP1 = dict:fold(fun(K, _Pass, D) -> dict:store(K, pass_as_params, D) end,
                    dict:new(),
                    FP),
    AnRes#anres{d_field_pass_method = FP1}.

format_msg_decoder_no_fields(MsgName, Opts) ->
    InitExprs1 = calc_init_msg_expr(MsgName, [], [], Opts),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(from_json_msg_, MsgName),
      fun(_Json, _TrUserData) ->
              '<init>'
      end,
      [replace_tree('<init>', InitExprs1)]).

format_msg_init_decoder(MsgName, InitExprs, FieldInfos, TrUserDataVar, Opts) ->
    InitMsg = calc_init_msg_expr(MsgName, InitExprs, FieldInfos, Opts),
    FromJAuxFn = gpb_lib:mk_fn(fj_msg_, MsgName),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(from_json_msg_, MsgName),
      fun(Json, 'TrUserData') ->
              '<fj_msg_MsgName>'(fj_next(fj_iter(Json)), '<init>', 'TrUserData')
      end,
      [replace_term('<fj_msg_MsgName>', FromJAuxFn),
       replace_tree('<init>', InitMsg),
       replace_tree('TrUserData', TrUserDataVar)]).

calc_init_msg_expr(MsgName, InitExprs, FieldInfos, Opts) ->
    MappingUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    case MappingUnset of
        records ->
            gpb_lib:record_create(MsgName, InitExprs);
        #maps{unset_optional=present_undefined} ->
            gpb_lib:map_create(InitExprs, Opts);
        #maps{unset_optional=omitted, oneof=tuples} ->
            InitExprs2 = repeated_or_required_or_needed_init_exprs(
                           InitExprs, FieldInfos),
            gpb_lib:map_create(InitExprs2, Opts);
        #maps{unset_optional=omitted, oneof=flat} ->
            InitExprs2 = repeated_or_required_or_needed_init_exprs(
                           InitExprs, FieldInfos),
            gpb_lib:map_create(InitExprs2, Opts)
    end.

repeated_or_required_or_needed_init_exprs(InitExprs, FieldInfos) ->
    [{FName, InitExpr}
     || {FName, InitExpr, Occ} <- kzip(InitExprs, FieldInfos),
        Occ == required orelse Occ == repeated orelse Occ == needed].

kunzip(L12) ->
    lists:unzip([{{K, V1}, {K, V2}} || {K, V1, V2} <- L12]).

kzip(L1, L2) ->
    lists:map(fun({{K, V1}, {K, V2}}) -> {K, V1, V2} end,
              lists:zip(L1, L2)).

defaultify_p3wellknowns(InitExprs, MsgDef, FieldInfos, TrUserDataVar) ->
    %% Given this proto
    %%   message Msg {
    %%      SubMsg                      f1 = 1;
    %%      uint32                      f2 = 2;
    %%      google.protobuf.UInt32Value f3 = 3;
    %%   }
    %% and either of these JSONs to decode:
    %%   {}
    %%   {"f3": null}
    %% Then the decoding is to be:
    %%   #{f3 => #{value => 0}}
    %% Ie for wrappers (at least), special handling for
    %% omitted values is needed.
    kunzip(
      lists:map(
        fun({{FName, InitExpr, Occ}, #?gpb_field{type={msg,MsgName}}}) ->
                case lists:member(MsgName, p3wellknown_wrappers()) of
                    true ->
                        FnNameDefault = gpb_lib:mk_fn(fj_default_, MsgName),
                        InitExpr2 = mk_call(FnNameDefault, [TrUserDataVar]),
                        {FName, InitExpr2, needed};
                    false ->
                        {FName, InitExpr, Occ}
                end;
           ({{FName, InitExpr, Occ}, _Field}) ->
                {FName, InitExpr, Occ}
        end,
        lists:zip(kzip(InitExprs, FieldInfos), MsgDef))).

mk_call(FnName, Args) ->
    erl_syntax:application(erl_syntax:atom(FnName), Args).

format_msg_decoder_loop(MsgName, MsgDef, TrUserDataVar, AnRes, Opts) ->
    JValueExpr = ?expr(JValue),
    EMsgVar = ?expr(EMsg),
    Decodings = calc_decodings(MsgName, MsgDef,
                               JValueExpr, EMsgVar, TrUserDataVar,
                               AnRes, Opts),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(fj_msg_, MsgName),
      fun({JKey, JValue, JRest}, EMsg, TrUserData) ->
              EMsg2 = case JKey of
                          '<FieldNamePattern>' ->
                              '<decode-value-update-EMsg>';
                          _ ->
                              EMsg
                      end,
              call_self(fj_next(JRest), EMsg2, TrUserData);
         (none, EMsg, _TrUserData) ->
              EMsg
      end,
      [repeat_clauses(
         '<FieldNamePattern>',
         [[replace_tree('<FieldNamePattern>', KeyPattern),
           replace_tree('<decode-value-update-EMsg>', DecodeExpr)]
          || {KeyPattern, DecodeExpr} <- Decodings])]).

calc_decodings(MsgName, MsgDef, JValueExpr, EMsgVar, TrUserDataVar,
               AnRes, Opts) ->
    %% In Json, the oneof fields are like flat oneofs:
    %%
    %% message Msg {
    %%   oneof c {
    %%     uint32 alt_one = 1;
    %%     string alt_two = 2;
    %%   }
    %%   uint32 outside_oneof = 10;
    %% }
    %%
    %% -> {"alt_one": 17}
    %% or {"alt_two": "x"} in Json
    %%
    %% The Erlang-internal format (if maps) may be either
    %%    #{c => {alt_one, 18}} if option {maps_oneof,flat} is _not_ set, or
    %%    #{alt_one => 18}      if option {maps_oneof,flat} is set.
    %%
    %% Additionally, in Json, "[p]arsers accept both the lowerCamelCase name
    %% (or the one specified by the json_name option) and the original proto
    %% field name."
    %% (src: https://developers.google.com/protocol-buffers/docs/proto3#json)
    %%
    %% So build a mapping from Json field name (binary, atom or string)
    %% to decoding-and-update expressions:
    %%
    KeyFormat = gpb_lib:json_key_format_by_opts(Opts),
    lists:reverse(
      gpb_lib:fold_msgdef_fields_o(
        fun(#?gpb_field{}=Field, IsOneof, Acc)->
                KeyPatterns = key_patterns(Field, KeyFormat),
                DecExpr = mk_check_null_decode_value_update_field_expr(
                            MsgName, Field, IsOneof,
                            JValueExpr, EMsgVar, TrUserDataVar,
                            AnRes, Opts),
                [{KP, DecExpr} || KP <- KeyPatterns] ++ Acc
        end,
        [],
        MsgDef)).

key_patterns(#?gpb_field{name=FName}=Field, KeyFormat) ->
    FNameS = atom_to_list(FName),
    LowerCamelCase = gpb_lib:get_field_json_name(Field),
    Strs = lists:usort([FNameS, LowerCamelCase]),
    case KeyFormat of
        atom ->
            [erl_syntax:atom(JFName) || JFName <- Strs];
        binary ->
            %% Want <<"fname">> and not <<102,110,97,109,101>>
            %% so make a text node
            [erl_syntax:text(?ff("<<~p>>", [JFName])) || JFName <- Strs];
        string ->
            [erl_syntax:string(JFName) || JFName <- Strs]
    end.

mk_check_null_decode_value_update_field_expr(
  MsgName, #?gpb_field{type=Type, occurrence=Occurrence}=Field, IsOneof,
  JValueExpr, EMsgVar, TrUserDataVar,
  AnRes, Opts) ->
    JNull = gpb_lib:json_null(Opts),
    DecExpr =
        case occurrence_or_mapfield(Occurrence, Type) of
            optional -> type_decode_expr(Type, JValueExpr, TrUserDataVar);
            required -> type_decode_expr(Type, JValueExpr, TrUserDataVar);
            repeated -> repeated_field_decode_expr(MsgName, Field, JValueExpr,
                                                   TrUserDataVar, AnRes);
            mapfield -> mapfield_decode_expr(MsgName, Field, JValueExpr,
                                             TrUserDataVar, AnRes)
        end,
    if Type =:= {enum,'google.protobuf.NullValue'} ->
            %% libprotobuf decodes to the enum for null, 0, 0.0 and for
            %% any string, but neither for booleans nor arrays nor the empty
            %% object. For other numbers, it is treated as if the field
            %% is omitted, for other values (arrays, booleans, objects)
            %% it is a decoding failure.
            IsJStringGuard = case gpb_lib:json_string_format_by_opts(Opts) of
                                 binary -> is_binary;
                                 list   -> is_list
                             end,
            ?expr(if 'JValueExpr' =:= null;
                     'JValueExpr' =:= 0;
                     'JValueExpr' =:= 0.0;
                     is_jstring('JValueExpr') ->
                          '<update-field-expr>';
                     is_number('JValueExpr') ->
                          'EMsgVar'
                  end,
                  [replace_tree('JValueExpr', JValueExpr),
                   replace_term(null, JNull),
                   replace_term('is_jstring', IsJStringGuard),
                   replace_tree('EMsgVar', EMsgVar),
                   replace_tree('<update-field-expr>',
                                mk_decode_value_update_field_expr(
                                  MsgName, Field, IsOneof,
                                  EMsgVar, DecExpr, TrUserDataVar,
                                  AnRes, Opts))]);
       true ->
            ?expr(if 'JValueExpr' =:= null ->
                          'EMsgVar';
                     true ->
                          '<update-field-expr>'
                  end,
                  [replace_tree('JValueExpr', JValueExpr),
                   replace_term(null, JNull),
                   replace_tree('EMsgVar', EMsgVar),
                   replace_tree('<update-field-expr>',
                                mk_decode_value_update_field_expr(
                                  MsgName, Field, IsOneof,
                                  EMsgVar, DecExpr, TrUserDataVar,
                                  AnRes, Opts))])
    end.

mk_decode_value_update_field_expr(MsgName, #?gpb_field{name=FName}, IsOneof,
                                  EMsgVar, DecExpr, TrUserDataVar,
                                  AnRes, Opts) ->
    MappingUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    case {MappingUnset, IsOneof} of
        {records, false} ->
            ElemPath = [MsgName, FName],
            FValue = tr_wrap({ElemPath, decode, AnRes},
                             DecExpr,
                             TrUserDataVar),
            ?expr('<EMsg>'#'<MsgName>'{'<field-name>' = '<field-value>'},
                  [replace_tree('<EMsg>', EMsgVar),
                   replace_term('<MsgName>', MsgName),
                   replace_term('<field-name>', FName),
                   replace_tree('<field-value>', FValue)]);
        {records, {true, CFName}} ->
            %% Create CTr({tag, OTr(<decode-expr>, TrUserData)}, TrUserData)
            CElemPath = [MsgName, CFName],
            OElemPath = [MsgName, CFName, FName],
            FValue = tr_wrap({CElemPath, decode, AnRes},
                             tag_wrap(FName,
                                      tr_wrap({OElemPath, decode, AnRes},
                                              DecExpr,
                                              TrUserDataVar)),
                             TrUserDataVar),
            ?expr('<EMsg>'#'<MsgName>'{'<field-name>' = '<field-value>'},
                  [replace_tree('<EMsg>', EMsgVar),
                   replace_term('<MsgName>', MsgName),
                   replace_term('<field-name>', CFName),
                   replace_tree('<field-value>', FValue)]);
        {#maps{oneof=tuples}, false} ->
            ElemPath = [MsgName, FName],
            FValue = tr_wrap({ElemPath, decode, AnRes},
                             DecExpr,
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{FName, FValue}], Opts);
        {#maps{oneof=tuples}, {true, CFName}} ->
            %% Create CTr({tag, OTr(<decode-expr>, TrUserData)}, TrUserData)
            CElemPath = [MsgName, CFName],
            OElemPath = [MsgName, CFName, FName],
            FValue = tr_wrap({CElemPath, decode, AnRes},
                             tag_wrap(FName,
                                      tr_wrap({OElemPath, decode, AnRes},
                                              DecExpr,
                                              TrUserDataVar)),
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{CFName, FValue}], Opts);
        {#maps{oneof=flat}, false} ->
            ElemPath = [MsgName, FName],
            FValue = tr_wrap({ElemPath, decode, AnRes},
                             DecExpr,
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{FName, FValue}], Opts);
        {#maps{oneof=flat}, {true, CFName}} ->
            %% Generate maps:without(...)?  To guarantee at most one oneof
            %% field, also in case the json would be "malformed" containing
            %% for instance both alt_one and alt_two?
            CElemPath = [MsgName, CFName],
            OElemPath = [MsgName, CFName, FName],
            FValue = tr_wrap({CElemPath, decode, AnRes},
                             tr_wrap({OElemPath, decode, AnRes},
                                     DecExpr,
                                     TrUserDataVar),
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{FName, FValue}], Opts)
    end.

repeated_field_decode_expr(MsgName, #?gpb_field{name=FName, type=Type},
                           JValueExpr, TrUserDataVar, AnRes) ->
    [TrEmptySeq, TrAddElem, TrFinalize] =
        [gpb_gen_translators:find_translation([MsgName, FName], Op, AnRes)
         || Op <- [decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize]],
    ElemPath = [MsgName, FName, []],
    JElemVar = gpb_lib:var("JElem@~s", [FName]),
    ElemTrDecExpr = tr_wrap({ElemPath, decode, AnRes},
                            type_decode_expr(Type, JElemVar, TrUserDataVar),
                            TrUserDataVar),
    ?expr('lists:reverse'(
            lists:foldl(
              fun('JElem@FVar', Acc) ->
                      '[New|Acc]'('<decoded-elem>', Acc, 'TrUserData')
              end,
              '[]'([], 'TrUserData'),
              fj_array('JValue')),
            'TrUserData'),
          [replace_tree('JValue', JValueExpr),
           replace_tree('JElem@FVar', JElemVar),
           replace_tree('<decoded-elem>', ElemTrDecExpr),
           replace_term('lists:reverse', TrFinalize),
           replace_term('[New|Acc]', TrAddElem),
           %% Get it from EMsg instead to avoid translator called twice?
           replace_term('[]', TrEmptySeq),
           replace_tree('TrUserData', TrUserDataVar)]).

mapfield_decode_expr(MsgName, #?gpb_field{name=FName, type={map, KT, VT}=FType},
                     JValueExpr, TrUserDataVar, AnRes) ->
    MapMsgName = gpb_lib:map_type_to_msg_name(KT, VT),
    ElemPath = [MsgName, FName, []],
    [TrEmptySeq, TrAddElem, TrFinalize] =
        [gpb_gen_translators:find_translation([MsgName, FName], Op, AnRes)
         || Op <- [decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize]],
    KElemDecExpr = tr_wrap(
                     {ElemPath, decode, AnRes},
                     map_key_type_decode_expr(KT, ?expr(Key), TrUserDataVar),
                     TrUserDataVar),
    VElemDecExpr = tr_wrap(
                     {ElemPath, decode, AnRes},
                     type_decode_expr(VT, ?expr(Value), TrUserDataVar),
                     TrUserDataVar),
    ElemDecExpr = tr_wrap(
                    {ElemPath, decode, AnRes},
                    type_decode_expr(FType, JValueExpr, TrUserDataVar),
                    TrUserDataVar),
    ?expr(fj_mapfield_fold(
            '<map-name-as-msg>',
            fun 'new-[]'/2,
            fun 'add-elem-[New|Acc]'/3,
            fun 'lists:reverse'/2,
            fun(Key) -> '<ElemDecExpr(Key)>' end,
            fun(Value) -> '<ElemDecExpr(Value)>' end,
            'Tr(JValueExpr,TrUserData)',
            'TrUserData'),
          [replace_term('<map-name-as-msg>', MapMsgName),
           %%  to avoid translator called twice?
           replace_term('new-[]', TrEmptySeq), % Get it from EMsg instead?
           replace_term('add-elem-[New|Acc]', TrAddElem),
           replace_term('lists:reverse', TrFinalize),
           replace_tree('<ElemDecExpr(Key)>', KElemDecExpr),
           replace_tree('<ElemDecExpr(Value)>', VElemDecExpr),
           replace_tree('Tr(JValueExpr,TrUserData)', ElemDecExpr),
           replace_tree('TrUserData', TrUserDataVar)]).

tr_wrap({ElemPath, Op, AnRes}, Expr, TrUserDataVar) ->
    Tr = gpb_gen_translators:find_translation(ElemPath, Op, AnRes),
    ?expr('Tr'('<expr>', 'TrUserData'),
          [replace_term('Tr', Tr),
           replace_tree('<expr>', Expr),
           replace_tree('TrUserData', TrUserDataVar)]).

tag_wrap(Tag, Expr) ->
    ?expr({'<tag>', '<expr>'},
          [replace_term('<tag>', Tag),
           replace_tree('<expr>', Expr)]).

map_key_type_decode_expr(Type, JValueExpr, TrUserDataVar) when Type /= bool ->
    type_decode_expr(Type, JValueExpr, TrUserDataVar);
map_key_type_decode_expr(bool, JValueExpr, TrUserDataVar) ->
    %% All types that can appear as map keys are acceptable
    %% as string on JSON->internal. Except booleans, so
    %% need to make an extra pass for them
    JValueExpr2 = ?expr(case '<JValueExpr>' of
                            <<"true">> -> true;
                            <<"false">> -> false
                        end,
                        [replace_tree('<JValueExpr>', JValueExpr)]),
    type_decode_expr(bool, JValueExpr2, TrUserDataVar).

type_decode_expr(Type, JValueExpr, TrUserDataVar) ->
    case Type of
        Int32 when Int32 == sint32;
                   Int32 == int32;
                   Int32 == uint32 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        Int64 when Int64 == sint64;
                   Int64 == int64;
                   Int64 == uint64 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        bool ->
            ?expr(fj_bool('Var'),
                  [replace_tree('Var', JValueExpr)]);
        {enum,EnumName} ->
            DecodeEnumFn = gpb_lib:mk_fn(fj_enum_, EnumName),
            ?expr('fj_enum_<EName>'('Var'),
                  [replace_tree('Var', JValueExpr),
                   replace_term('fj_enum_<EName>', DecodeEnumFn)]);
        Int32 when Int32 == fixed32;
                   Int32 == sfixed32 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        Int64 when Int64 == fixed64;
                   Int64 == sfixed64 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        Float when Float == float;
                   Float == double ->
            ?expr(fj_float('Var'),
                  [replace_tree('Var', JValueExpr)]);
        string ->
            ?expr(fj_string('Var'),
                  [replace_tree('Var', JValueExpr)]);
        bytes ->
            ?expr(fj_bytes('Var'),
                  [replace_tree('Var', JValueExpr)]);
        {msg,MsgName} ->
            FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
            ?expr('from_json_msg_<MsgName>'('Var', 'TrUserData'),
                  [replace_term('from_json_msg_<MsgName>', FnName),
                   replace_tree('Var', JValueExpr),
                   replace_tree('TrUserData', TrUserDataVar)]);
        {map,_KT,_VT} ->
            JValueExpr; % handled on a higher level
        {group,GName} ->
            FnName = gpb_lib:mk_fn(from_json_msg_, GName),
            ?expr('from_json_msg_<GName>'('Var', 'TrUserData'),
                  [replace_term('from_json_msg_<GName>', FnName),
                   replace_tree('Var', JValueExpr),
                   replace_tree('TrUserData', TrUserDataVar)])
    end.

format_helpers(Defs, AnRes, Opts) ->
    [format_json_msg_iterator_helpers(Defs, Opts),
     format_json_array_helpers(Defs, AnRes, Opts),
     format_mapfield_helper(AnRes, Opts),
     format_json_type_helpers(Defs, AnRes, Opts),
     format_json_p3wellknown_helpers(Defs, AnRes, Opts)].

format_json_msg_iterator_helpers(Defs, Opts) ->
    HaveNonemptyMsgs = have_nonempty_msg(Defs),
    HaveMapIterators = gpb_lib:target_has_map_iterators(Opts),
    [[case gpb_lib:json_object_format_by_opts(Opts) of
          eep18 ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun([{}]) -> [];
                    (Proplist) -> Proplist
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)];
          {proplist} ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun({Proplist}) -> Proplist
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)];
          {Tag, proplist} ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun({struct, Proplist}) -> Proplist
                 end,
                 [replace_term(struct, Tag)]),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)];
          map when HaveMapIterators ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun(Map) -> maps:iterator(Map)
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun(Iter) -> maps:next(Iter)
                 end)];
          map when not HaveMapIterators ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun(Map) -> maps:to_list(Map)
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)]
      end] || HaveNonemptyMsgs].

mk_is_jobject_guard(Var, Opts) ->
    case gpb_lib:json_object_format_by_opts(Opts) of
        eep18 ->
            ?expr(is_list('X') andalso is_tuple(hd('X')),
                  [replace_tree('X', Var)]);
        {proplist} ->
            ?expr((tuple_size('X') =:= 1 andalso is_list(element(1,'X'))),
                  [replace_tree('X', Var)]);
        {Tag, proplist} ->
            ?expr((tuple_size('X') =:= 2
                   andalso (element(1,'X') =:= 'Tag')
                   andalso is_list(element(2,'X'))),
                  [replace_tree('X', Var),
                   replace_term('Tag', Tag)]);
        map ->
            ?expr(is_map('X'),
                  [replace_tree('X', Var)])
    end.

format_json_array_helpers(Defs, #anres{map_types=MapTypes}, Opts) ->
    HaveMapfields = sets:size(MapTypes) > 0,
    HaveRepeated = have_repeated_fields(Defs) orelse HaveMapfields,
    [[case gpb_lib:json_array_format_by_opts(Opts) of
          list ->
              [gpb_lib:nowarn_unused_function(fj_array, 1),
               gpb_codegen:format_fn(
                 fj_array,
                 fun(L) -> L end)];
          {Tag, list} ->
              [gpb_lib:nowarn_unused_function(fj_array, 1),
               gpb_codegen:format_fn(
                 fj_array,
                 fun({array, L}) -> L end,
                 [replace_term(array, Tag)])]
      end] || HaveRepeated].


mk_is_jarray_guard(Var, Opts) ->
    case gpb_lib:json_array_format_by_opts(Opts)of
        list ->
            ?expr(is_list('X'),
                  [replace_tree('X', Var)]);
        {Tag, list} ->
            ?expr((tuple_size('X') =:= 2 andalso
                   element(1,'X') =:= 'Tag' andalso is_list(2,'X')),
                  [replace_tree('X', Var),
                   replace_term('Tag', Tag)])
    end.

format_mapfield_helper(#anres{map_types=MapTypes}, Opts) ->
    HaveMapfields = sets:size(MapTypes) > 0,
    [[gpb_codegen:format_fn(
        fj_mapfield_fold,
        fun(MapMsgName, New, AddElem, Finalize, DecodeKey, DecodeValue,
            JMapfield, TrUserData) ->
                Finalize(
                  fj_mapfield_fold_aux(fj_iter(JMapfield),
                                       MapMsgName, DecodeKey, DecodeValue,
                                       AddElem,
                                       New([], TrUserData),
                                       TrUserData),
                  TrUserData)
        end),
      case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
          '2tuples' ->
              gpb_codegen:format_fn(
                fj_mapfield_fold_aux,
                fun(JIter, MapMsgName, DecodeKey, DecodeValue,
                    AddElem, Acc, TrUserData) ->
                        case fj_next(JIter) of
                            {JKey, JValue, JRest} ->
                                EKey = DecodeKey(JKey),
                                EValue = DecodeValue(JValue),
                                TmpMsg = {MapMsgName, EKey, EValue},
                                Acc1 = AddElem(TmpMsg, Acc, TrUserData),
                                call_self(JRest,
                                          MapMsgName, DecodeKey, DecodeValue,
                                          AddElem, Acc1, TrUserData);
                            none ->
                                Acc
                        end
                end);
          maps ->
              {EKey, EValue} = {?expr(EKey), ?expr(EValue)},
              MkMapExpr = gpb_lib:map_create([{key, EKey}, {value, EValue}],
                                             Opts),
              gpb_codegen:format_fn(
                fj_mapfield_fold_aux,
                fun(JIter, MapMsgName, DecodeKey, DecodeValue,
                    AddElem, Acc, TrUserData) ->
                        case fj_next(JIter) of
                            {JKey, JValue, JRest} ->
                                EKey = DecodeKey(JKey),
                                EValue = DecodeValue(JValue),
                                TmpMsg = '#{key => EKey, value => EValue}',
                                Acc1 = AddElem(TmpMsg, Acc, TrUserData),
                                call_self(JRest,
                                          MapMsgName, DecodeKey, DecodeValue,
                                          AddElem, Acc1, TrUserData);
                            none ->
                                Acc
                        end
                end,
                [replace_tree('#{key => EKey, value => EValue}', MkMapExpr)])
      end] || HaveMapfields].

%%% --- proto3 wellknowns ---
test_proto3_wellknown(MsgName, _MsgDef) ->
    case MsgName of
        'google.protobuf.Duration' ->
            {true, fun format_p3wellknown_duration_decoder/5};
        'google.protobuf.Timestamp' ->
            {true, fun format_p3wellknown_timestamp_decoder/5};
        'google.protobuf.DoubleValue' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.FloatValue' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.Int64Value' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.UInt64Value' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.Int32Value' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.UInt32Value' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.BoolValue' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.StringValue' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.BytesValue' ->
            {true, fun format_p3wellknown_wrapper_decoder/5};
        'google.protobuf.Struct' ->
            {true, fun format_p3wellknown_struct_decoder/5};
        'google.protobuf.Value' ->
            {true, fun format_p3wellknown_value_decoder/5};
        'google.protobuf.ListValue' ->
            {true, fun format_p3wellknown_list_value_decoder/5};
        'google.protobuf.Empty' ->
            {true, fun format_p3wellknown_empty_decoder/5};
        'google.protobuf.FieldMask' ->
            {true, fun format_p3wellknown_field_mask_decoder/5};
        _ ->
            false
    end.

format_p3wellknown_duration_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    %% Examples: "1.000340012s", "1s"
    %%
    %% 'Generated output always contains 0, 3, 6, or 9 fractional digits,
    %% depending on required precision, followed by the suffix "s".'
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    FieldInfos = field_info_trees(MsgName, MsgDef, Defs, AnRes, Opts),
    [gpb_codegen:format_fn(
       FnName,
       fun(Str, TrUserData) ->
               {Seconds, Nanos} = fj_parse_duration_s1(fj_ensure_list(Str)),
               fj_mk_msg([Seconds, Nanos],
                         'google.protobuf.Duration',
                         'field-infos',
                         TrUserData)
       end,
       [replace_tree('field-infos', FieldInfos)]),
     gpb_codegen:format_fn(
       fj_parse_duration_s1,
       fun("-" ++ Rest) -> fj_parse_duration_s2(Rest, neg, "");
          (Rest)        -> fj_parse_duration_s2(Rest, pos, "")
       end,
       []),
     gpb_codegen:format_fn(
       fj_parse_duration_s2,
       fun("." ++ Rest, Sign, Acc) ->
               Seconds = list_to_integer(lists:reverse(Acc)),
               fj_parse_duration_n(Rest, Sign, Seconds, "");
          ("s", Sign, Acc) ->
               Seconds = list_to_integer(lists:reverse(Acc)),
               if Sign == pos -> {Seconds, 0};
                  Sign == neg -> {-Seconds, 0}
               end;
          ([D|Rest], Sign, Acc) when $0 =< D,D =< $9 ->
               call_self(Rest, Sign, [D | Acc])
       end,
       []),
     gpb_codegen:format_fn(
       fj_parse_duration_n,
       fun("s", Sign, Seconds, Acc) ->
               %% With Google protobuf, "1.s" seems valid (but ".1s" does not)
               Acc1 = if Acc =:= "" -> "0";
                         true -> Acc
                      end,
               LFactor = case length(Acc1) of
                             9 -> 1;
                             8 -> 10;
                             7 -> 100;
                             6 -> 1000;
                             5 -> 10000;
                             4 -> 100000;
                             3 -> 1000000;
                             2 -> 10000000;
                             1 -> 100000000;
                             _ -> error({badnanos, lists:reverse(Acc)})
                         end,
               Nanos = list_to_integer(lists:reverse(Acc1)),
               if Sign == pos -> {Seconds, Nanos * LFactor};
                  Sign == neg -> {-Seconds, -Nanos * LFactor}
               end;
          ([D|Rest], Sign, Seconds, Acc) when $0 =< D,D =< $9 ->
               call_self(Rest, Sign, Seconds, [D | Acc])
       end,
       []),
     ""].

format_p3wellknown_timestamp_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    %% Example: "1972-01-01T10:00:20.021Z"
    %%
    %% '0, 3, 6, or 9 fractional digits.', 'Offsets other than "Z" are also
    %% accepted.'
    %%
    %% The Google protobuf seems to only accept upper case T and Z.  Seems to
    %% assume 2-digit years are actually 2-digit years ie in the first
    %% century AD, ie century is not guessed.
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    FieldInfos = field_info_trees(MsgName, MsgDef, Defs, AnRes, Opts),
    [gpb_codegen:format_fn(
       FnName,
       fun(Str, TrUserData) ->
               {YYYY, M, D, HH, MM, SS, Nanos, Offset} =
                   fj_parse_timestamp(fj_ensure_list(Str)),
               S1 = calendar:datetime_to_gregorian_seconds(
                      {{YYYY, M, D}, {HH, MM, SS}}),
               %% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
               %% -> 62167219200
               Gs1970 = 62167219200, % the epoch
               Seconds = S1 - Gs1970 - Offset,
               fj_mk_msg([Seconds, Nanos],
                         'google.protobuf.Timestamp',
                         'field-infos',
                         TrUserData)
       end,
       [replace_tree('field-infos', FieldInfos)]),
     gpb_codegen:format_fn(
       fj_parse_timestamp,
       fun(S) ->
               fj_parse_timestamp_1(S)
       end),
     gpb_codegen:format_fn(
       fj_parse_timestamp_1,
       fun(S) ->
               {YYYY, S1} = fj_read_int_until(S, $-),
               {M, S2} = fj_read_int_until(S1, $-),
               {D, S3} = fj_read_int_until(S2, $T),
               {HH, S4} = fj_read_int_until(S3, $:),
               {MM, S5} = fj_read_int_until(S4, $:),
               {SS, S6} = fj_read_int_until_any_of(S5, "Z.+-"),
               case S6 of
                   "Z" ->
                       {YYYY,M,D, HH,MM,SS,0, 0};
                   "."++S7 ->
                       {Nanos, S8} = fj_read_nanos_until_any_of(S7, "Z+-"),
                       case S8 of
                           "Z" ->
                               {YYYY,M,D, HH,MM,SS,Nanos, 0};
                           "+"++S9 ->
                               Offset = fj_read_timestamp_offs(S9),
                               {YYYY,M,D, HH,MM,SS,Nanos, Offset};
                           "-"++S9 ->
                               Offset = fj_read_timestamp_offs(S9),
                               {YYYY,M,D, HH,MM,SS,Nanos, -Offset}
                       end;
                   "+"++S7 ->
                       Offset = fj_read_timestamp_offs(S7),
                       {YYYY,M,D, HH,MM,SS,0, Offset};
                   "-"++S7 ->
                       Offset = fj_read_timestamp_offs(S7),
                       {YYYY,M,D, HH,MM,SS,0, -Offset}
               end
       end),
     gpb_codegen:format_fn(
       fj_read_timestamp_offs,
       fun(S) ->
               {OffsHH, OffsMMStr} = fj_read_int_until(S, $:),
               OffsMM = list_to_integer(OffsMMStr),
               OffsHH * 3600 + OffsMM
       end),
     gpb_codegen:format_fn(
       fj_read_int_until,
       fun(S, Delim) ->
               {S1, Rest} = fj_read_str_until(S, Delim, ""),
               {list_to_integer(S1), Rest}
       end),
     gpb_codegen:format_fn(
       fj_read_int_until_any_of,
       fun(S, Delims) ->
               {S1, Rest} = fj_read_str_until_any_of(S, Delims, ""),
               {list_to_integer(S1), Rest}
       end),
     gpb_codegen:format_fn(
       fj_read_nanos_until_any_of,
       fun(S, Delims) ->
               {NanosStr, Rest} = fj_read_str_until_any_of(S, Delims, ""),
               LFactor = case length(NanosStr) of
                             9 -> 1;
                             8 -> 10;
                             7 -> 100;
                             6 -> 1000;
                             5 -> 10000;
                             4 -> 100000;
                             3 -> 1000000;
                             2 -> 10000000;
                             1 -> 100000000;
                             _ -> error({badnanos, NanosStr})
                         end,
               Nanos = list_to_integer(NanosStr) * LFactor,
               {Nanos, Rest}
       end),
     gpb_codegen:format_fn(
       fj_read_str_until,
       fun([Delim | Rest], Delim, Acc) -> {lists:reverse(Acc), Rest};
          ([C | Rest], Delim, Acc) -> call_self(Rest, Delim, [C | Acc])
       end),
     gpb_codegen:format_fn(
       fj_read_str_until_any_of,
       fun([C | Rest]=S, Delims, Acc) ->
               case lists:member(C, Delims) of
                   true  -> {lists:reverse(Acc), S};
                   false -> call_self(Rest, Delims, [C | Acc])
               end
       end),
     ""].

format_p3wellknown_wrapper_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    [#?gpb_field{type=Type}] = MsgDef,
    DecodeJExpr = type_decode_expr(Type, ?expr(Value), ?expr(_TrUserData)),
    FieldInfos = field_info_trees(MsgName, MsgDef, Defs, AnRes, Opts),
    [gpb_codegen:format_fn(
       FnName,
       fun(Value, TrUserData) ->
               fj_mk_msg(['decode-json-expr'],
                         'MsgName',
                         'field-infos',
                         TrUserData)
       end,
       [replace_tree('decode-json-expr', DecodeJExpr),
        replace_term('MsgName', MsgName),
        replace_tree('field-infos', FieldInfos)])].

format_p3wellknown_struct_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    [#?gpb_field{type={map,_,_}}=Field] = MsgDef,
    %% Decoding of map fields is special in that it automatically
    %% gets assigned translation functions, that are already called,
    %% mapfield_decode_expr makes sure of that.
    %% So use a field-infos with no translation.
    DecodeJExpr = mapfield_decode_expr(MsgName, Field, ?expr(JValue),
                                       ?expr(TrUserData), AnRes),
    FieldInfos = field_info_trees_no_tr(MsgDef, Defs, Opts),
    [gpb_codegen:format_fn(
       FnName,
       fun(JValue, TrUserData) ->
               fj_mk_msg(['decode-json-expr'],
                         'MsgName',
                         'field-infos',
                         TrUserData)
       end,
       [replace_tree('decode-json-expr', DecodeJExpr),
        replace_term('MsgName', MsgName),
        replace_tree('field-infos', FieldInfos)])].

format_p3wellknown_value_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    TypeNull = {enum, 'google.protobuf.NullValue'},
    TypeStruct = {msg, 'google.protobuf.Struct'},
    TypeList = {msg, 'google.protobuf.ListValue'},
    %% Be quite explcit in the matching below, so we won'd accidentally
    %% mis-decode a field at run-time, if the wellknown would change
    %% in some future import or update.
    [#gpb_oneof{name=kind=FName, rnum=RNum, fields=OFields}] = MsgDef,
    [#?gpb_field{name=null_value,   type=TypeNull},
     #?gpb_field{name=number_value, type=double},
     #?gpb_field{name=string_value, type=string},
     #?gpb_field{name=bool_value,   type=bool},
     #?gpb_field{name=struct_value, type=TypeStruct},
     #?gpb_field{name=list_value,   type=TypeList}] = OFields,
    OFDecInfos =
        [begin
             MkGuard = value_guard_maker(OFName, OFType, Opts),
             ElemPath = [MsgName, FName, OFName],
             Transl = gpb_gen_translators:find_translation(
                        ElemPath, decode, AnRes),
             MkDecodeExpr =
                 fun(JVar) ->
                         type_decode_expr(OFType, JVar, ?expr(TrUserData))
                 end,
             {OFName, MkGuard, Transl, MkDecodeExpr}
         end
         || #?gpb_field{name=OFName, type=OFType} <- OFields],
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        #maps{unset_optional=omitted, oneof=flat} ->
            JValue = ?expr(JValue),
            gpb_codegen:format_fn(
              FnName,
              fun(JValue, TrUserData) ->
                      if 'is_x(JValue)' ->
                              fj_mk_msg(['<decode-expr>'], 'MsgName',
                                        [{'OFName', 0, undefined, fun 'Tr'/2}],
                                        TrUserData)
                      end
              end,
              [repeat_clauses(
                 'is_x(JValue)',
                 [begin
                      [replace_tree('is_x(JValue)', MkGuard(JValue)),
                       replace_tree('OFName', map_key(OFName, Opts)),
                       replace_term('Tr', Transl),
                       replace_tree('<decode-expr>', MkDecodeExpr(JValue)),
                       replace_term('MsgName', MsgName)]
                  end
                  || {OFName, MkGuard, Transl, MkDecodeExpr} <- OFDecInfos])]);
        _ ->
            DummyType = {msg,MsgName},
            PF = #?gpb_field{name=FName, type=DummyType, rnum=RNum,
                             occurrence=required},
            PFieldInfos = field_info_trees(MsgName, [PF], Defs, AnRes, Opts),
            JValue = ?expr(JValue),
            gpb_codegen:format_fn(
              FnName,
              fun(JValue, TrUserData) ->
                      if 'is_x(JValue)' ->
                              X = {'Tag', 'Tr'('<decode-expr>', TrUserData)},
                              fj_mk_msg([X], 'MsgName',
                                        'pseudo-field-infos',
                                        TrUserData)
                      end
              end,
              [repeat_clauses(
                 'is_x(JValue)',
                 [[replace_tree('is_x(JValue)', MkGuard(JValue)),
                   replace_term('Tr', Transl),
                   replace_term('Tag', OFName),
                   replace_tree('<decode-expr>', MkDecodeExpr(JValue)),
                   replace_term('MsgName', MsgName),
                   replace_tree('pseudo-field-infos', PFieldInfos)]
                  || {OFName, MkGuard, Transl, MkDecodeExpr} <- OFDecInfos])])
    end.

value_guard_maker(null_value, {enum,'google.protobuf.NullValue'}, Opts) ->
    JNull = gpb_lib:json_null(Opts),
    fun(JVar) ->
            ?expr('JVar' =:= null,
                  [replace_tree('JVar', JVar),
                   replace_term(null, JNull)])
    end;
value_guard_maker(number_value, double, _Opts) ->
    fun(JVar) -> ?expr(is_number('JVar'), [replace_tree('JVar', JVar)]) end;
value_guard_maker(string_value, string, Opts) ->
    fun(JVar) -> mk_is_jstring_guard(JVar, Opts) end;
value_guard_maker(bool_value, bool, _Opts) ->
    fun(JVar) -> ?expr(is_boolean('JVar'), [replace_tree('JVar', JVar)]) end;
value_guard_maker(struct_value, {msg,'google.protobuf.Struct'}, Opts) ->
    fun(JVar) -> mk_is_jobject_guard(JVar, Opts) end;
value_guard_maker(list_value, {msg,'google.protobuf.ListValue'}, Opts) ->
    fun(JVar) -> mk_is_jarray_guard(JVar, Opts) end.

format_p3wellknown_list_value_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    [#?gpb_field{type=Type}] = MsgDef,
    DecodeJExpr = type_decode_expr(Type, ?expr(Elem), ?expr(TrUserData)),
    FieldInfos = field_info_trees(MsgName, MsgDef, Defs, AnRes, Opts),
    [gpb_codegen:format_fn(
       FnName,
       fun(JList, TrUserData) ->
               fj_mk_msg([['decode-json-expr' || Elem <- JList]],
                         'MsgName',
                         'field-infos',
                         TrUserData)
       end,
       [replace_tree('decode-json-expr', DecodeJExpr),
        replace_term('MsgName', MsgName),
        replace_tree('field-infos', FieldInfos)])].

format_p3wellknown_empty_decoder(MsgName, _MsgDef, _Defs, _AnRes, _Opts) ->
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    [gpb_codegen:format_fn(
       FnName,
       fun(_JMsg, _TrUserData) ->
               fj_mk_msg([], 'MsgName', [], _TrUserData)
       end,
       [replace_term('MsgName', MsgName)])].

format_p3wellknown_field_mask_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
    FieldInfos = field_info_trees(MsgName, MsgDef, Defs, AnRes, Opts),
    [gpb_codegen:format_fn(
       FnName,
       fun(JStr, TrUserData) ->
               fj_mk_msg([fj_field_names(JStr)],
                         'MsgName',
                         'field-infos',
                         TrUserData)
       end,
       [replace_term('MsgName', MsgName),
        replace_tree('field-infos', FieldInfos)]),

     gpb_codegen:format_fn(
       %% "user.displayName,photo"  -> ["user.display_name", "photo"]
       %% "user.displayName,,photo" -> ["user.display_name", "photo"]
       %% "user.displayName,"       -> ["user.display_name"]
       %% ",user.displayName,"      -> ["user.display_name"]
       fj_field_names,
       fun(JStr) ->
               Str = fj_ensure_list(JStr),
               Paths = fj_strsplit_nonempty($,, Str),
               [fj_string(fj_unlower_camel_case_path(Path)) || Path <- Paths]
       end),

     gpb_codegen:format_fn(
       %% "user.displayNName"       -> ["user.display_n_name"]
       %% "user.display3Name"       -> ["user.display3_name"]
       %% "user.display33name"      -> ["user.display33name"]
       %% "user.DISPALYNAME"        -> ["user.dispalyname"]
       %% "user.displayNAME"        -> ["user.display_name"]
       %% "user.displayABCName"     -> ["user.display_abc_name"]
       %%
       %% Algorithm: (a) lowercase each uppercase letter
       %% (b) non-uppercase followed by uppercase: precede by underscore
       %% (c) uppercase followed by non-uppercase: precede by underscore
       fj_unlower_camel_case_path,
       fun(S) ->
               fj_strjoin($., [fj_unlower_camel_case(Comp)
                               || Comp <- fj_strsplit($., S)])
       end),
     gpb_codegen:format_fn(
       fj_strjoin,
       fun(_Sep, []) -> "";
          (Sep, [Hd | Tl]) -> [Hd | [[Sep, Elem] || Elem <- Tl]]
       end),
     gpb_codegen:format_fn(
       fj_strsplit,
       fun(Sep, Str) ->
               fj_strsplit_2(Sep, Str, "", [])
       end),
     gpb_codegen:format_fn(
       fj_strsplit_2,
       fun(Sep, [Sep | Tl], Curr, Acc) ->
               call_self(Sep, Tl, "", [lists:reverse(Curr) | Acc]);
          (Sep, [C | Tl], Curr, Acc) ->
               call_self(Sep, Tl, [C | Curr], Acc);
          (_Sep, [], Curr, Acc) ->
               if Curr =:= "" -> lists:reverse(Acc);
                  Curr =/= "" -> lists:reverse([lists:reverse(Curr) | Acc])
               end
       end),
     gpb_codegen:format_fn(
       fj_strsplit_nonempty,
       fun(Sep, Str) ->
               fj_strsplit_nonempty_2(Sep, Str, none, [])
       end),
     gpb_codegen:format_fn(
       fj_strsplit_nonempty_2,
       fun(Sep, [Sep | Tl], Curr, Acc) ->
               call_self(Sep, Tl, none, fj_strsplit_add_to_acc(Curr, Acc));
          (Sep, [C | Tl], Curr, Acc) ->
               call_self(Sep, Tl, fj_strsplit_add_to_curr(C, Curr), Acc);
          (_Sep, [], Curr, Acc) ->
               lists:reverse(fj_strsplit_add_to_acc(Curr, Acc))
       end),
     gpb_codegen:format_fn(
       fj_strsplit_add_to_acc,
       fun(none, Acc) -> Acc;
          (Curr, Acc) -> [lists:reverse(Curr) | Acc]
       end),
     gpb_codegen:format_fn(
       fj_strsplit_add_to_curr,
       fun(C, none) -> [C];
          (C, Curr) -> [C | Curr]
       end),
     gpb_codegen:format_fn(
       fj_unlower_camel_case,
       fun(S) ->
               fj_unlcc(S, i, "")
       end),
     gpb_codegen:format_fn(
       fj_unlcc,
       fun([U | Tl], State, Acc) when $A =< U, U =< $Z ->
               if State =:= l, hd(Acc) =/= $_ ->
                       call_self(Tl, u, [fj_lowercase(U), $_ | Acc]);
                  true ->
                       call_self(Tl, u, [fj_lowercase(U) | Acc])
               end;
          ([NonU | Tl], State, Acc) ->
               if State =:= u, tl(Acc) =/= "", hd(tl(Acc)) =/= $_ ->
                       [C | Acc1] = Acc,
                       call_self(Tl, l, [NonU, C, $_ | Acc1]);
                  true ->
                       call_self(Tl, l, [NonU | Acc])
               end;
          ("", _State, Acc) ->
               lists:reverse(Acc)
       end),
     gpb_codegen:format_fn(
       fj_lowercase,
       fun(C) when $A =< C, C =< $Z -> C - ($A - $a);
          (C) -> C
       end),
     ""].

field_info_trees(MsgName, Fields, Defs, AnRes, Opts) ->
    erl_syntax:list(
      [begin
           Default = gpb_lib:proto3_type_default(Type, Defs, Opts),
           erl_syntax:tuple(
             [map_key(FName, Opts),
              erl_syntax:integer(RNum),
              erl_syntax:abstract(Default),
              calc_transl_info(MsgName, Field, AnRes)])
       end
       || #?gpb_field{name=FName, rnum=RNum, type=Type}=Field <- Fields]).

field_info_trees_no_tr(Fields, Defs, Opts) ->
    erl_syntax:list(
      [begin
           Default = gpb_lib:proto3_type_default(Type, Defs, Opts),
           erl_syntax:tuple(
             [map_key(FName, Opts),
              erl_syntax:integer(RNum),
              erl_syntax:abstract(Default),
              ?expr(id)])
       end
       || #?gpb_field{name=FName, rnum=RNum, type=Type} <- Fields]).

map_key(FName, Opts) ->
    KeyType = gpb_lib:get_maps_key_type_by_opts(Opts),
    case KeyType of
        atom ->
            erl_syntax:atom(FName);
        binary ->
            erl_syntax:binary(
              [erl_syntax:binary_field(
                 erl_syntax:string(atom_to_list(FName)))])
    end.

calc_transl_info(MsgName, #?gpb_field{occurrence=Occurrence}=Field, AnRes) ->
    case Occurrence of
        required ->
            calc_non_repeated_transl_info(MsgName, Field, AnRes);
        optional ->
            calc_non_repeated_transl_info(MsgName, Field, AnRes);
        repeated ->
            calc_repeated_transl_info(MsgName, Field, AnRes)
    end.

calc_non_repeated_transl_info(MsgName, #?gpb_field{name=FName}, AnRes) ->
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_translation(ElemPath, decode, AnRes) of
        {true, Transl} ->
            ?expr(fun 'Tr'/2,
                  [replace_term('Tr', Transl)]);
        false ->
            ?expr(id)
    end.

calc_repeated_transl_info(MsgName, #?gpb_field{name=FName}, AnRes) ->
    ElemPathsOps = [{[MsgName, FName], decode_init_default, 2},
                    {[MsgName, FName], decode_repeated_add_elem, 3},
                    {[MsgName, FName], decode_repeated_finalize, 2},
                    {[MsgName, FName, []], decode, 2}],
    Arities = [Arity || {_ElemPath, _Op, Arity} <- ElemPathsOps],
    %% If at least one has a translation, find the translation function
    %% for all, use it, else if none has a translation, use id.
    HasTransls = [gpb_gen_translators:has_translation(ElemPath, Op, AnRes)
                  || {ElemPath, Op, _Arity} <- ElemPathsOps],
    case lists:any(fun has_transl/1, HasTransls) of
        true ->
            Transls = find_transls_for_all(ElemPathsOps, HasTransls, AnRes),
            erl_syntax:tuple(
              [erl_syntax:atom(r_tr)
               | [fun_expr(Transl, Arity)
                  || {Transl, Arity} <- lists:zip(Transls, Arities)]]);
        false ->
            ?expr(id)
    end.

has_transl({true, _Tr}) -> true;
has_transl(false) -> false.

find_transls_for_all([{ElemPath, Op, _Arity} | RestEOs],
                     [HasTransl | RestHasTransls],
                     AnRes) ->
    case HasTransl of
        {true, Transl} ->
            [Transl | find_transls_for_all(RestEOs, RestHasTransls, AnRes)];
        false ->
            Transl = gpb_gen_translators:find_translation(ElemPath, Op, AnRes),
            [Transl | find_transls_for_all(RestEOs, RestHasTransls, AnRes)]
    end;
find_transls_for_all([], [], _AnRes) ->
    [].

fun_expr(FnName, Arity) ->
    erl_syntax:implicit_fun(erl_syntax:atom(FnName),
                            erl_syntax:integer(Arity)).

%%% -- other helpers --
format_json_type_helpers(Defs, #anres{used_types=UsedTypes}, Opts) ->
    StrBin = gpb_lib:get_strings_as_binaries_by_opts(Opts),
    NeedIntType = lists:any(fun(T) -> gpb_lib:smember(T, UsedTypes) end,
                            [sint32, int32, uint32,
                             sint64, int64, uint64,
                             fixed32, sfixed32,
                             fixed64, sfixed64]),
    NeedBoolType = gpb_lib:smember(bool, UsedTypes),
    NeedEnumType = lists:any(fun({enum,_}) -> true;
                                (_) -> false
                             end,
                             sets:to_list(UsedTypes)),
    NeedFloatType = lists:any(fun(T) -> gpb_lib:smember(T, UsedTypes) end,
                              [float, double]),
    NeedStringType = gpb_lib:smember(string, UsedTypes),
    NeedBytesType = gpb_lib:smember(bytes, UsedTypes),
    [[[%% If the only integer would be in eg google.protobuf.Duration,
       %% which has a specialized string format, then this function may
       %% turn out unused.
       gpb_lib:nowarn_unused_function(fj_int, 1),
       gpb_codegen:format_fn(
        fj_int,
        %% Leading zeros are not allowed according to RFC7159,
        %% so no octal representation decoding or anything such is needed.
        fun(N) when is_integer(N) ->
                N;
           (S) when is_binary(S) ->
                list_to_integer(binary_to_list(S));
           (S) when is_list(S) ->
                list_to_integer(S)
        end)] || NeedIntType],
     [[%% Avoid warning when messages contain only eg google.protobuf.Value,
       %% which is decoded specially.
       gpb_lib:nowarn_unused_function(fj_bool,1),
       %% The protobuf also accepts both boolean values as well as
       %% string representation of the same. It seems to also accepts
       %% string representations of 0 and 1, but oddly enough
       %% not the integers 0 and 1.  (protobuf 3.8.0-rc1)
       gpb_codegen:format_fn(
         fj_bool,
         fun(B) when is_boolean(B) -> B;
            (B) when is_binary(B) ->
                 case fj_bool_bin_casecanon(B, <<>>) of
                     (<<"true">>)           -> true;
                     (<<"false">>)          -> false;
                     (<<"1">>)              -> true;
                     (<<"0">>)              -> false
                 end;
            (S) when is_list(S) ->
                 call_self(list_to_binary(S))
         end),
       gpb_lib:nowarn_unused_function(fj_bool_bin_casecanon,2),
       gpb_codegen:format_fn(
         fj_bool_bin_casecanon, % to lowercase
         fun(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
                 call_self(Rest, <<Acc/binary, (C + 32)>>); % $a - $A == 32
            (<<C, Rest/binary>>, Acc) ->
                 call_self(Rest, <<Acc/binary, C>>);
            (<<>>, Acc) ->
                 Acc
         end)]
      || NeedBoolType],
     [if EnumName =:= 'google.protobuf.NullValue' ->
              FnName = gpb_lib:mk_fn(fj_enum_, EnumName),
              [gpb_lib:nowarn_unused_function(FnName,1),
               gpb_codegen:format_fn(
                 FnName,
                 fun(_) -> 'NULL_VALUE' end)];
         true ->
              JSymStrsToSyms = canonify_enum_jstrs(unalias_enum_syms(Enums),
                                                   Opts),
              IntStrsToSyms = enum_ints_to_syms(Enums),
              [{_, Sym1} | _] = JSymStrsToSyms,
              StrSyms = JSymStrsToSyms ++ IntStrsToSyms,
              FnName = gpb_lib:mk_fn(fj_enum_, EnumName),
              EnumDecoder = gpb_lib:mk_fn(d_enum_, EnumName),
              [gpb_lib:nowarn_unused_function(FnName,1),
               gpb_codegen:format_fn(
                 FnName,
                 fun(S) when is_binary(S) ->
                         case  fj_casecanon_enum(S, <<>>) of
                             '<<"Str">>' -> '<EnumSym>';
                             _ -> 'FirstSym'
                         end;
                    (N) when is_integer(N) ->
                         'd_enum_<EName>'(N);
                    (S) when is_list(S) ->
                         call_self(list_to_binary(S))
                 end,
                 [replace_term('d_enum_<EName>', EnumDecoder),
                  replace_term('FirstSym', Sym1),
                  repeat_clauses(
                    '<<"Str">>',
                    [[replace_tree('<<"Str">>', bstr(Str)),
                      replace_term('<EnumSym>', Sym)]
                     || {Str, Sym} <- StrSyms])])]
      end
      || {{enum,EnumName}, Enums} <- Defs,
         NeedEnumType],
     [%% Extra enum helper(s)
      case proplists:get_bool(json_case_insensitive_enum_parsing, Opts) of
          true ->
              [gpb_lib:nowarn_unused_function(fj_casecanon_enum,2),
               gpb_codegen:format_fn(
                 fj_casecanon_enum,
                 fun(<<C, Tl/binary>>, Acc) ->
                         call_self(Tl, <<Acc/binary, (fj_casecanon_char(C))>>);
                    (<<>>, Acc) ->
                         Acc
                 end),
               gpb_lib:nowarn_unused_function(fj_casecanon_char,1),
               gpb_codegen:format_fn(
                 fj_casecanon_char, % to uppercase
                 fun(C) when $a =< C, C =< $z -> C - 32; % $a - $A == 32
                    ($-) -> $_;
                    (C) -> C
                 end)];
          false ->
              [gpb_lib:nowarn_unused_function(fj_casecanon_enum,2),
               gpb_codegen:format_fn(
                 fj_casecanon_enum,
                 fun(B, _Acc) ->
                         B
                 end)]
      end
      || NeedEnumType],
     [[%% float with helper
       gpb_lib:nowarn_unused_function(fj_float, 1),
       gpb_codegen:format_fn(
         fj_float,
         fun(N) when is_integer(N) -> float(N);
            (N) when is_float(N) -> N;
            (S) when is_binary(S) -> call_self(binary_to_list(S));
            ("NaN") -> nan;
            ("Infinity") -> infinity;
            ("-Infinity") -> '-infinity';
            (S) when is_list(S) ->
                 case fj_d_num2e(S, integer, "") of
                     {integer, S2} -> float(list_to_integer(S2));
                     {float, S2}   -> list_to_float(S2)
                 end
         end),
       gpb_lib:nowarn_unused_function(fj_d_num2e, 3),
       gpb_codegen:format_fn(
         fj_d_num2e,
         fun("-"++Rest, St, Acc) ->
                 call_self(Rest, St, "-" ++ Acc);
            ("+"++Rest, St, ""=Acc) ->
                 %% Ignore leading plus
                 call_self(Rest, St, Acc);
            ([D | Rest], St, Acc) when $0 =< D, D =< $9 ->
                 call_self(Rest, St, [D | Acc]);
            ("."++_ = S, _, Acc) when Acc =:= ""; Acc =:= "-" ->
                 %% Initial leading decimal point: prepend a leading zero
                 %% and continue processing
                 call_self(S, float, "0" ++ Acc);
            ("."++[D|_] = S, _, Acc) when $0 =< D, D =< $9 ->
                 %% A decimal point: the rest (if wellformed as per rfc 7159)
                 %% will be parseable as an erlang float
                 {float, lists:reverse(Acc, S)};
            ("."++Rest, _, Acc) ->
                 %% A decimal point followed by non-digit (exponent or
                 %% or end  of string): Turn "." into ".0" to make
                 %% it parseable as an Erlang float
                 {float, lists:reverse(Acc, ".0" ++ Rest)};
            ([E | _]=Rest, integer, Acc) when E == $e; E == $E ->
                 %% Exponent: not preceded by a decimal point:
                 %% add ".0" before the exponent, and it will (if wellformed)
                 %% be parseable as an erlang float
                 {float, lists:reverse(Acc, ".0"++Rest)};
            ("", St, Acc) ->
                 {St, lists:reverse(Acc)}
         end)] || NeedFloatType],
     [[%% String
       case StrBin of
           true ->
               gpb_codegen:format_fn(
                 fj_string,
                 fun(S) when is_binary(S); is_list(S) ->
                         unicode:characters_to_binary(S)
                 end);
           false ->
               gpb_codegen:format_fn(
                 fj_string,
                 fun(S) when is_binary(S); is_list(S) ->
                         unicode:characters_to_list(S)
                 end)
       end || NeedStringType]],
     [gpb_codegen:format_fn(
        fj_bytes,
        fun(S) when is_binary(S); is_list(S) ->
                %% Convert any url-safe encoding to normal base64 encoding
                B64 = <<<<if C =:= $- -> $+;
                             C =:= $_ -> $/;
                             true -> C
                          end>>
                        || <<C>> <= iolist_to_binary(S)>>,
                base64:decode(B64)
        end) || NeedBytesType]].


mk_is_jstring_guard(Var, Opts) ->
    case gpb_lib:json_string_format_by_opts(Opts) of
        binary ->
            ?expr(is_binary('X'),
                  [replace_tree('X', Var)]);
        list ->
            ?expr(is_list('X'),
                  [replace_tree('X', Var)])
    end.

format_json_p3wellknown_helpers(Defs, AnRes, Opts) ->
    UsesP3Duration = uses_msg('google.protobuf.Duration', AnRes),
    UsesP3Timestamp = uses_msg('google.protobuf.Timestamp', AnRes),
    UsesP3Wrapper = lists:any(fun(W) -> uses_msg(W, AnRes) end,
                              p3wellknown_wrappers()),
    UsesP3Struct = uses_msg('google.protobuf.Struct', AnRes),
    UsesP3Value = uses_msg('google.protobuf.Value', AnRes),
    UsesP3ListValue = uses_msg('google.protobuf.ListValue', AnRes),
    UsesP3Empty = uses_msg('google.protobuf.Empty', AnRes),
    UsesP3FieldMask = uses_msg('google.protobuf.FieldMask', AnRes),

    FlatMaps = case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                   #maps{unset_optional=omitted, oneof=flat} ->
                       true;
                   _ ->
                       false
               end,
    NonFlatMaps = not FlatMaps,
    NeedsMkMsg = UsesP3Duration or UsesP3Timestamp or UsesP3Wrapper
        or UsesP3Struct or (UsesP3Value and NonFlatMaps) or UsesP3ListValue
        or UsesP3Empty or UsesP3FieldMask,
    NeedsEnsureList = UsesP3Duration or UsesP3Timestamp or UsesP3FieldMask,
    [if not NeedsMkMsg ->
             "";
        NeedsMkMsg ->
             format_json_p3wellknown_mk_msg(Defs, AnRes, Opts)
     end,
     [gpb_codegen:format_fn(
        fj_ensure_list,
        fun(B) when is_binary(B) -> binary_to_list(B);
           (S) when is_list(S) -> S
        end) || NeedsEnsureList],
     [begin
          FieldInfos = field_info_trees(MsgName, MsgDef, Defs, AnRes, Opts),
          gpb_codegen:format_fn(
            gpb_lib:mk_fn(fj_default_, MsgName),
            fun(TrUserData) ->
                    fj_mk_msg(['value,...'], 'MsgName', 'field-infos',
                              TrUserData)
            end,
            [splice_trees(
               'value,...',
               [begin
                    Default = gpb_lib:proto3_type_default(Type, Defs, Opts),
                    erl_syntax:abstract(Default)
                end
                || #?gpb_field{type=Type} <- MsgDef]),
             replace_term('MsgName', MsgName),
             replace_tree('field-infos', FieldInfos)])
      end
      || {{msg,MsgName},MsgDef} <- Defs,
         lists:member(MsgName, p3wellknown_wrappers())],
     ""].

p3wellknown_wrappers() ->
    ['google.protobuf.FloatValue',
     'google.protobuf.DoubleValue',
     'google.protobuf.Int64Value',
     'google.protobuf.UInt64Value',
     'google.protobuf.Int32Value',
     'google.protobuf.UInt32Value',
     'google.protobuf.BoolValue',
     'google.protobuf.StringValue',
     'google.protobuf.BytesValue'].

format_json_p3wellknown_mk_msg(_Defs, _AnRes, Opts) ->
    CanDoVarKeyUpdate = gpb_lib:target_has_variable_key_map_update(Opts),
    [case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
         records ->
             [gpb_codegen:format_fn(
                fj_mk_msg,
                fun(Values, MsgName, FieldInfos, TrUserData) ->
                        Defaults = [D || {_Key, _RNum, D, _Tr} <- FieldInfos],
                        Msg0 = list_to_tuple([MsgName | Defaults]),
                        fj_mk_msg2(Values, FieldInfos, Msg0, TrUserData)
                end),
              gpb_codegen:format_fn(
                fj_mk_msg2,
                fun([V | VRest], [{_Key, RNum, _D, Tr} | FRest], Msg0,
                    TrUserData) ->
                        V1 = if Tr =:= id -> V;
                                true -> fj_tr(Tr, V, TrUserData)
                             end,
                        Msg1 = setelement(RNum, Msg0, V1),
                        call_self(VRest, FRest, Msg1, TrUserData);
                   ([], [{_Key, RNum, Default, Tr} | FRest], Msg0,
                    TrUserData) -> % future update
                        Default1 = if Tr =:= id -> Default;
                                      true -> fj_tr(Tr, Default, TrUserData)
                                   end,
                        Msg1 = setelement(RNum, Msg0, Default1),
                        call_self([], FRest, Msg1, TrUserData);
                   ([], [], Msg, _TrUserData) ->
                        Msg
                end)];
         #maps{unset_optional=present_undefined} when CanDoVarKeyUpdate ->
             MapPutV1 = gpb_lib:map_set(?expr(Msg0),
                                        [{?expr(Key), ?expr(V1)}],
                                        Opts),
             MapPutDefault1 = gpb_lib:map_set(?expr(Msg0),
                                              [{?expr(Key), ?expr(Default1)}],
                                              Opts),
             [gpb_codegen:format_fn(
                fj_mk_msg,
                fun(Values, _MsgName, FieldInfos, TrUserData) ->
                        fj_mk_msg2(Values, FieldInfos, '#{}', TrUserData)
                end,
                [replace_tree('#{}', gpb_lib:map_create([], Opts))]),
              gpb_codegen:format_fn(
                fj_mk_msg2,
                fun([V | VRest], [{Key, _RNum, _D, Tr} | FRest], Msg0,
                    TrUserData) ->
                        V1 = if Tr =:= id -> V;
                                true -> fj_tr(Tr, V, TrUserData)
                             end,
                        Msg1 = 'Msg0#{Key => V1}',
                        call_self(VRest, FRest, Msg1, TrUserData);
                   ([], [{Key, _RNum, Default, Tr} | FRest], Msg0,
                    TrUserData) -> %future update
                        Default1 = if Tr =:= id -> Default;
                                      true -> fj_tr(Tr, Default, TrUserData)
                                   end,
                        Msg1 = 'Msg0#{Key => Default1}',
                        call_self([], FRest, Msg1, TrUserData);
                   ([], [], Msg, _TrUserData) ->
                        Msg
                end,
                [replace_tree('Msg0#{Key => V1}', MapPutV1),
                 replace_tree('Msg0#{Key => Default1}', MapPutDefault1)])];
         #maps{unset_optional=omitted} when CanDoVarKeyUpdate ->
             MapPut = gpb_lib:map_set(?expr(Msg0),
                                      [{?expr(Key), ?expr(V1)}],
                                      Opts),
             [gpb_codegen:format_fn(
                fj_mk_msg,
                fun(Values, _MsgName, FieldInfos, TrUserData) ->
                        fj_mk_msg2(Values, FieldInfos, '#{}', TrUserData)
                end,
                [replace_tree('#{}', gpb_lib:map_create([], Opts))]),
              gpb_codegen:format_fn(
                fj_mk_msg2,
                fun([V | VRest], [{Key, _RNum, _D, Tr} | FRest], Msg0,
                    TrUserData) ->
                        V1 = if Tr =:= id -> V;
                                true -> fj_tr(Tr, V, TrUserData)
                             end,
                        Msg1 = 'Msg0#{Key => V1}',
                        call_self(VRest, FRest, Msg1, TrUserData);
                   ([], _FRest, Msg, _TrUserData) ->
                        Msg
                end,
                [replace_tree('Msg0#{Key => V1}', MapPut)])];
         #maps{unset_optional=present_undefined} when not CanDoVarKeyUpdate ->
             [gpb_codegen:format_fn(
                fj_mk_msg,
                fun(Values, _MsgName, FieldInfos, TrUserData) ->
                        fj_mk_msg2(Values, FieldInfos, '#{}', TrUserData)
                end,
                [replace_tree('#{}', gpb_lib:map_create([], Opts))]),
              gpb_codegen:format_fn(
                fj_mk_msg2,
                fun([V | VRest], [{Key, _RNum, _D, Tr} | FRest], Msg0,
                    TrUserData) ->
                        V1 = if Tr =:= id -> V;
                                true -> fj_tr(Tr, V, TrUserData)
                             end,
                        Msg1 = maps:put(Key, V1, Msg0),
                        call_self(VRest, FRest, Msg1, TrUserData);
                   ([], [{Key, _RNum, Default, Tr} | FRest], Msg,
                    TrUserData) -> % future update
                        Default1 = if Tr =:= id -> Default;
                                      true -> fj_tr(Tr, Default, TrUserData)
                                   end,
                        Msg1 = maps:put(Key, Default1, Msg),
                        call_self([], FRest, Msg1, TrUserData);
                   ([], [], Msg, _TrUserData) ->
                        Msg
                end)];
         #maps{unset_optional=omitted} when not CanDoVarKeyUpdate ->
             [gpb_codegen:format_fn(
                fj_mk_msg,
                fun(Values, _MsgName, FieldInfos, TrUserData) ->
                        fj_mk_msg2(Values, FieldInfos, '#{}', TrUserData)
                end,
                [replace_tree('#{}', gpb_lib:map_create([], Opts))]),
              gpb_codegen:format_fn(
                fj_mk_msg2,
                fun([V | VRest], [{Key, _RNum, _Default, Tr} | FRest], Msg0,
                    TrUserData) ->
                        V1 = if Tr =:= id -> V;
                                true -> fj_tr(Tr, V, TrUserData)
                             end,
                        Msg1 = maps:put(Key, V1, Msg0),
                        call_self(VRest, FRest, Msg1, TrUserData);
                   ([], _FRest, Msg, _TrUserData) ->
                        Msg
                end)]
     end,
     gpb_codegen:format_fn(
       fj_tr,
       fun(Tr, V, TrUserData) when is_function(Tr) ->
               Tr(V, TrUserData);
          ({r_tr, InitTr, AddElemTr, FinalizeTr, ElemTr}, L, TrUserData) ->
               InitAcc = InitTr([], TrUserData),
               FinalizeTr(fj_r_tr(L, InitAcc, AddElemTr, ElemTr, TrUserData),
                          TrUserData)
       end),
     gpb_codegen:format_fn(
       fj_r_tr,
       fun([Elem | Rest], Acc, AddElemTr, ElemTr, TrUserData) ->
               Elem1 = ElemTr(Elem, TrUserData),
               Acc1 = AddElemTr(Elem1, Acc, TrUserData),
               fj_r_tr(Rest, Acc1, AddElemTr, ElemTr, TrUserData);
          ([], Acc, _AddElemTr, _ElemTr, _TrUserData) ->
               Acc
       end),
     ""].

uses_msg(MsgName, #anres{used_types=UsedTypes}) ->
    sets:is_element({msg,MsgName}, UsedTypes).

occurrence_or_mapfield(repeated, {map, _, _}) -> mapfield;
occurrence_or_mapfield(Occurrence, _)         -> Occurrence.

have_nonempty_msg([{{msg,_MsgName}, Fields} | Rest]) ->
    if Fields /= [] -> true;
       Fields == [] -> have_nonempty_msg(Rest)
    end;
have_nonempty_msg([_ | Rest]) ->
    have_nonempty_msg(Rest);
have_nonempty_msg([]) ->
    false.

have_repeated_fields([{{msg,_Msg}, Fields} | Rest]) ->
    case have_repeated_aux(Fields) of
        true  -> true;
        false -> have_repeated_fields(Rest)
    end;
have_repeated_fields([_ | Rest]) ->
    have_repeated_fields(Rest);
have_repeated_fields([]) ->
    false.

have_repeated_aux([#?gpb_field{occurrence = repeated} | _]) ->
    true;
have_repeated_aux([_ | Rest]) ->
    have_repeated_aux(Rest);
have_repeated_aux([]) ->
    false.

bstr(S) when is_list(S) ->
    %% Want <<"str">> and not <<102,110,97,109,101>>
    %% so make a text node
    erl_syntax:text(?ff("<<~p>>", [S])).

canonify_enum_jstrs(JStrsToSyms, _Opts) ->
    [{gpb_lib:uppercase(JStr), Sym} || {JStr, Sym} <- JStrsToSyms].

unalias_enum_syms(Enums) ->
    %% Enum can also have {option, allow_alias, true} elements.
    Enums1 = [Enum || {_Sym,_Num}=Enum <- Enums],
    %% In case of aliases: make a mapping:
    %%   If .proto is:           Then resulting mapping is:
    %%   enum E { E_0 = 0;       [{"E_0", 'E_0'},
    %%            E_1a = 1;       {"E_1a", 'E_1a'},
    %%            E_1b = 1; }     {"E_1b", 'E_1a'}]
    [{atom_to_list(Sym), ensure_sym_unaliased(Sym, Enums1)}
     || {Sym, _Num} <- Enums1].

ensure_sym_unaliased(Sym, Enums) ->
    {Sym, Num} = lists:keyfind(Sym, 1, Enums),
    {Sym1, Num} = lists:keyfind(Num, 2, Enums),
    Sym1.

enum_ints_to_syms(Enums) ->
    [{integer_to_list(Num), Sym} || {Sym,Num} <- gpb_lib:unalias_enum(Enums)].
