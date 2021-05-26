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
%%% @doc Common functions for gpb_gen_decoders and gpb_gen_json_decoders

-module(gpb_decoders_lib).

-export([init_exprs/6]).
-export([calc_field_infos/3]).
-export([decoder_read_field_param/2]).
-export([decoder_finalize_result/5]).

-export([run_morph_ops/2]).
-export([underscore_unused_vars/0]).
-export([explode_param_init/3]).
-export([explode_param_pass/3]).
-export([change_undef_marker_in_clauses/1]).
-export([implode_to_map_exprs_all_mandatory/0]).
-export([implode_to_map_exprs/3]).
-export([rework_records_to_maps/3]).
-export([finalize_marked_map_exprs/1]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").
-include("gpb_decoders_lib.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2]).

%% @doc Generate field initialization expressions.
%% Such expressions include `[]' initially for repeated fields, or the
%% proto3 type-default value for optional fields (or `undefined' for
%% optional fields in proto2 messages). Also of concern is that there
%% may be translators that set other initial values.
init_exprs(MsgName, MsgDef, Defs, TrUserDataVar, AnRes, Opts)->
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    IsMapMsg = is_map_msg(MsgName, AnRes),
    UseDefaults = proplists:get_bool(defaults_for_omitted_optionals, Opts),
    UseTypeDefaults = proplists:get_bool(type_defaults_for_omitted_optionals,
                                         Opts),
    ExprInfos1 =
        [case Field of
             #?gpb_field{name=FName, occurrence=Occurrence0, type=Type,
                         opts=FOpts} ->
                 HasDefault = lists:keymember(default, 1, FOpts),
                 SubMsgType = is_msg_type(Type),
                 Occurrence = if IsMapMsg, not SubMsgType -> optional;
                                 true -> Occurrence0
                              end,
                 {Undefined, Undef, P} =
                     if IsProto3 ->
                             TD = gpb_lib:proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             if SubMsgType     -> {ATD, ?expr('$undef'), o};
                                not SubMsgType -> {ATD, ATD, o}
                             end;
                        IsProto3, not SubMsgType ->
                             TD = gpb_lib:proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, m};
                        IsMapMsg, not SubMsgType ->
                             TD = gpb_lib:proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, o};
                        UseDefaults, HasDefault ->
                             {default,D} = lists:keyfind(default, 1, FOpts),
                             AD = erl_syntax:abstract(D),
                             {AD, AD, m};
                        UseTypeDefaults ->
                             TD = gpb_lib:proto2_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, m};
                        true ->
                             Pr = if HasDefault -> d;
                                     true -> o
                                  end,
                             {?expr(undefined), ?expr('$undef'), Pr}
                     end,
                 case Occurrence of
                     repeated -> {FName, m, ?expr([]),        ?expr([])};
                     required -> {FName, o, ?expr(undefined), ?expr('$undef')};
                     optional -> {FName, P, Undefined,        Undef}
                 end;
             #gpb_oneof{name=FName} ->
                 {FName, o, ?expr(undefined), ?expr('$undef')}
         end
         || Field <- MsgDef],
    ExprInfos2 =
        [begin
             ElemPath = [MsgName, FName],
             TranslFn = gpb_gen_translators:find_translation(
                          ElemPath,
                          decode_init_default,
                          AnRes),
             TrInitExpr = ?expr('Tr'('InitExpr', 'TrUserData'),
                                [replace_tree('InitExpr', InitExpr),
                                 replace_term('Tr', TranslFn),
                                 replace_tree('TrUserData', TrUserDataVar)]),
             TrMOExpr = ?expr('Tr'('MOExpr', 'TrUserData'),
                              [replace_tree('MOExpr', MOExpr),
                               replace_term('Tr', TranslFn),
                               replace_tree('TrUserData', TrUserDataVar)]),
             {FName, Presence, TrInitExpr, TrMOExpr}
         end
         || {FName, Presence, InitExpr, MOExpr} <- ExprInfos1],
    case gpb_lib:get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    [{FName, Expr} || {FName, _, Expr, _MOExpr} <- ExprInfos2];
                #maps{unset_optional=present_undefined} ->
                    [{FName, Expr} || {FName, _, Expr, _MOExpr} <- ExprInfos2];
                #maps{unset_optional=omitted} ->
                    [{FName, MapsOmittedExpr}
                     || {FName, _, _Expr, MapsOmittedExpr} <- ExprInfos2]
            end;
        pass_as_record ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    [{FName, Expr} || {FName, P, Expr, _} <- ExprInfos2,
                                      P == m orelse P == d];
                #maps{unset_optional=present_undefined} ->
                    [{FName, Expr} || {FName, _, Expr, _} <- ExprInfos2];
                #maps{unset_optional=omitted} ->
                    [{FName, Expr} || {FName, m, Expr, _} <- ExprInfos2]
            end
    end.

is_map_msg(MsgName, #anres{maps_as_msgs=MapsAsMsgs}) ->
    lists:keymember({msg,MsgName}, 1, MapsAsMsgs).

is_msg_type({msg,_}) -> true;
is_msg_type(_)       -> false.

%% @doc Generate an expression `#record{field_1 = R1, ... field_n=RN} = Msg',
%% and return it and also its constituent parts:
%% ```
%%   {<the Msg var>, <that expr>, [{<name of field_1>, <the R_1 var>}, ...]}
%% '''
decoder_read_field_param(MsgName, MsgDef) ->
    MappingVar = ?expr(Msg),
    FFields = [{FName, gpb_lib:var_n("R", I)}
               || {I,FName} <- gpb_lib:index_seq(
                                 repeated_field_names(MsgDef))],
    FMatch = gpb_lib:record_match(MsgName, FFields),
    FParam = ?expr('#r{f1=R1..fn=RN}' = '<Msg>',
                   [replace_tree('#r{f1=R1..fn=RN}', FMatch),
                    replace_tree('<Msg>', MappingVar)]),
    {MappingVar, FParam, FFields}.

repeated_field_names(MsgDef) ->
    [FName || #?gpb_field{name=FName, occurrence=repeated} <- MsgDef].

%% @doc Create a finalization expression by generating calls to
%%      finalization translators. To give a notion what this is about,
%%      a default such translator for repeated fields is `lists:reverse'.
decoder_finalize_result(MsgVar, FFields, MsgName, TrUserDataVar, AnRes) ->
    gpb_lib:record_update(
      MsgVar,
      MsgName,
      [begin
           ElemPath = [MsgName, FName],
           Finalizer = gpb_gen_translators:find_translation(
                         ElemPath,
                         decode_repeated_finalize,
                         AnRes),
           FValueExpr = ?expr('lists:reverse'('<FVar>', 'TrUserData'),
                              [replace_term('lists:reverse',Finalizer),
                               replace_tree('<FVar>', FVar),
                               replace_tree('TrUserData',
                                            TrUserDataVar)]),
           {FName, FValueExpr}
       end
       || {FName, FVar} <- FFields]).

%% @doc Compute optionality information for each field of a message.
calc_field_infos(MsgDef, IsProto3, Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        #maps{unset_optional=omitted, oneof=flat} ->
            field_infos(MsgDef, true, IsProto3);
        _ ->
            field_infos(MsgDef, false, IsProto3)
    end.

field_infos(MsgDef, FlattenOnoef, IsProto3) ->
    [case Field of
         #?gpb_field{name=FName, type={msg,_}} ->
             {FName, optional};
         #?gpb_field{name=FName} ->
             if not IsProto3 ->
                     {FName, gpb_lib:get_field_occurrence(Field)};
                IsProto3 ->
                     %% On finalization, treat proto3 (scalar) fields as
                     %% requried, to avoid redundant if F == '$undef' -> ...
                     %% checks;  it can never be '$undef' since it has a
                     %% type-default. (except for sub messages and onoef)
                     {FName, required}
             end;
         #gpb_oneof{name=FName} ->
             if not FlattenOnoef ->
                     {FName, optional};
                FlattenOnoef ->
                     {FName, flatten_oneof}
             end
     end
     || Field <- MsgDef].

%% @doc Process a list of morping operation over a list of `#fn{}'s
%% (function with some associated meta info).
%%
%% A morphing operation is a fun that in some way or another change the
%% syntax tree of the function in the `#fn{}', but only for the `#fn{}'s
%% to which the morphing operation is applicable.
run_morph_ops([Op | Rest], Fns) ->
    run_morph_ops(Rest, Op(Fns));
run_morph_ops([], Fns) ->
    [gpb_codegen:erl_prettypr_format_nl(Tree) || #fn{tree=Tree} <- Fns].

%% @doc Replace unused function params and case clause patterns with
%% underscore.
%%
%% The intention is that code generation can issue match patterns for
%% all fields of a message, or all auxilliary working parameters and not
%% have to worry about underscoring unused ones to avoid warning messages
%% about unused variables.
underscore_unused_vars() ->
    fun(Fns) ->
            loop_fns(fun gpb_codemorpher:underscore_unused_vars/1,
                     process_all(),
                     Fns)
    end.

%% @doc Passing and updating message fields as decoding-function
%% parameters instead of as a record/map. This often generates faster code.
%%
%% Applies to decoding-initialization functions only.
explode_param_init(MsgName, InitExprs, ArgPos) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:explode_record_fields_to_params_init(
                        FnTree, ArgPos, {MsgName, InitExprs})
              end,
              process_initializer(),
              Fns)
    end.

%% @doc Passing and updating message fields as decoding-function
%% parameters instead of as a record/map. This often generates faster code.
%%
%% Applies to field decoding/passing functions only.
explode_param_pass(MsgName, FNames, ArgPos) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:explode_record_fields_to_params(
                        FnTree, ArgPos, {MsgName, FNames})
              end,
              process_msg_passers(),
              Fns)
    end.

%% @doc For records, `undefined' is used to indicate an omitted optional value.
%% However, that atom is also a valid enum. For maps, we can do better,
%% so offer a way to change it to some other atom, such as `$undef', which is
%% not a valid protobuf enum.
change_undef_marker_in_clauses(Undef) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:change_undef_marker_in_clauses(
                        FnTree, Undef)
              end,
              process_all(),
              Fns)
    end.

%% @doc The opposite of the {@link explode_param_init/3}, when a map is
%% constructed from all field parameters. Useful for maps with unset
%% optional = preset_undefined.
%%
%% Applies to finalization functions only.
implode_to_map_exprs_all_mandatory() ->
    fun(Fns) ->
            loop_fns(fun gpb_codemorpher:implode_to_map_expr/1,
                     process_finalizers(),
                     Fns)
    end.

%% @doc The opposite of the {@link explode_param_init/3}, when a map is
%% constructed from all field parameters. Useful for maps with omitted
%% unset optionals (which is the default)
%%
%% Applies to finalization functions only.
implode_to_map_exprs(F1Pos, FieldInfos, Undef) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:implode_to_map_exprs(
                        FnTree, F1Pos, FieldInfos, Undef)
              end,
              process_finalizers(),
              Fns)
    end.

%% @doc Change record expressions to map expressions. Useful when passing
%% messages as maps/records.
rework_records_to_maps(RecordParamPos, FieldInfos, Undef) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:rework_records_to_maps(
                        FnTree, RecordParamPos, FieldInfos, Undef)
              end,
              process_initializers_finalizers_and_msg_passers(),
              Fns)
    end.

%% @doc Finalize expressions that have been marked as map expressions.
%%
%% Initially of importance but now of continuously diminising value, gpb
%% can generate code for maps also when hosted on a pre Erlang-17
%% systems where maps does not exist. It does this by rendering the map
%% expressions to string format using erl_syntax:text() nodes, which are
%% rendered in verbatim by the erl_prettypr:format machinery.
%%
%% However, the code morpher must still be able to manipulate
%% expressions.  To make this possible, it uses marked record
%% expressions. The markup is specially tagged tuples.  As the last
%% step, these tagged record expressions are converted to map
%% expressions, possibly by using erl_syntax text nodes if on old
%% systems, and otherwise map syntax nodes.
%%
%% That's what this morping step performs.
finalize_marked_map_exprs(Opts) ->
    F = fun(MarkedExpr) ->
                gpb_codemorpher:marked_map_expr_to_map_expr(MarkedExpr, Opts)
        end,
    fun(Fns) ->
            loop_fns(F, process_initializers_finalizers_and_msg_passers(), Fns)
    end.

loop_fns(MapFun, Filter, Fns) ->
    [case matches_filter(Fn, Filter) of
         true  -> Fn#fn{tree = MapFun(FnTree)};
         false -> Fn
     end
     || #fn{tree=FnTree}=Fn <- Fns].

matches_filter(#fn{}=Fn, Filter) ->
    Filter(Fn).

process_all() -> fun(#fn{}) -> true end.

process_initializer() -> fun(#fn{initializes_fields=Bool}) -> Bool end.

process_finalizers() -> fun(#fn{has_finalizer=Bool}) -> Bool end.

process_msg_passers() -> fun(#fn{passes_msg=Bool}) -> Bool end.

process_initializers_finalizers_and_msg_passers() ->
    fun(#fn{initializes_fields=B1,
            has_finalizer=B2,
            passes_msg=B3}) ->
            B1 or B2 or B3
    end.
