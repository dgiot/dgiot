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

%% @doc This is the parser
%% @private

-module(gpb_parse).
-export([parse/1]).
-export([format_error/1]).

-export_type([error/0]).

-type error() :: {Line::pos_integer(), module(), Reason::term()}.

%% -- bwd compat -- do not use these ----------------------------------
%% Deprecated!
%% Instead, to retrieve proto definitions,
%% use the option to_proto_defs
%% with gpb_compile:file or gpb_compile:string.
%% For exprotobuf, see also
%% https://github.com/bitwalker/exprotobuf/issues/114
-export([post_process_one_file/3]). % use opt to_proto_defs instead
-export([post_process_all_files/2]). % use opt to_proto_defs instead
-export([format_post_process_error/1]). % use gpb_compile:format_error/1
-export([fetch_imports/1]). % use gpb_defs:fetch_imports
%% --^^--- bwd compat -- do not use these ----------------------------

-include("../include/gpb.hrl").

%% @doc Parse a list of tokens as returned from {@link gpb_parse2:binary/1}
%%
%% Please, do not use this from outside of gpb. Instead, use the option
%% `to_proto_defs' with gpb_compile:file/1,2 or gob_string/2,3, to get
%% a parsed .proto file.
%%
%% @hidden
-spec parse([gpb_scan2:token()]) -> {ok, gpb_defs:defs()} | {error, [error()]}.
parse(Tokens) ->
    {ParseTree, Errors} = p_top(Tokens, [], []),
    if Errors == [] -> {ok, ParseTree};
       Errors /= [] -> {error, Errors}
    end.

-define(f(Fmt, Args), io_lib:format(Fmt, Args)).

%% @doc Format an error. Note that the {@link parse/1} can return a list of
%% errors. This function formats one element in such a list.
format_error({syntax_error, {before, Tokens}}) ->
    ?f("syntax error at: ~s", [tokens_to_str(Tokens)]);
format_error({syntax_error, {before, Tokens}, Why}) ->
    ?f("syntax error at: ~s: ~s", [tokens_to_str(Tokens), ensure_str(Why)]).

-define(t(Token), {Token, _, _}).         % a token
-define(w(N),   ?t(<<N>>)).               % a word, as a binary
-define(s(S),   ?t({str_lit, S})).        % a string literal
-define(i(I),   ?t({int_lit, I})).        % an integer literal
-define(fl(F),  ?t({float_lit, F})).      % a float literal
-define(syntax_error(Where), throw({syntax_error, line(Where), Where})).
-define(syntax_error(Where, Why),
        throw({syntax_error, line(Where), Where, Why})).

-ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R:St ->).
-else. % -ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R -> St = erlang:get_stacktrace(),).
-endif. % -ifdef(OTP_RELEASE).

%% Principles that most of the recursive descent parser below follow:
%%
%% * To parse each an item, there is a corresponding function p_item.
%% * Each p_item either returns {Item, Rest} or fail with a syntax error.
%% * Each p_item parses an entire <item>, including the first token,
%%   even when some caller peeked to know which p_<item> to call.
%%   This is to make sub routines more reusable.

p_top(Tokens, Acc, Errors) when Tokens /= [] ->
    try
        case hd(Tokens) of
            ?w("syntax") ->
                {Syntax, Rest} = p_syntax(Tokens),
                Acc1 = [Syntax | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("package") ->
                {Package, Rest} = p_package(Tokens),
                Acc1 = [Package | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("import") ->
                {Import, Rest} = p_import(Tokens),
                Acc1 = [Import | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("enum") ->
                {Enum, Rest} = p_enum(Tokens),
                Acc1 = [Enum | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("message") ->
                {Msg, Rest} = p_message(Tokens),
                Acc1 = [Msg | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("extend") ->
                {Extend, Rest} = p_extend(Tokens),
                Acc1 = [Extend | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("option") ->
                {Option, Rest} = p_option(Tokens),
                Acc1 = [Option | Acc],
                p_top(Rest, Acc1, Errors);
            ?w("service") ->
                {Service, Rest} = p_service(Tokens),
                Acc1 = [Service | Acc],
                p_top(Rest, Acc1, Errors);
            ?t(';') ->
                p_top(tl(Tokens), Acc, Errors);
            {'$end', _Line} ->
                %% bwd compat with the old parser
                p_top([], Acc, Errors);
            _ ->
                ?syntax_error(Tokens)
        end
    catch ?STACKTRACE(throw, {syntax_error, Line, FollowingTokens}, St)
            maybe_debug_syntax_error(Line, FollowingTokens, St, undefined),
            ParenStack = [],
            Rest1 = try_recover(ParenStack, safe_tl(Tokens)),
            Rest2 = skip_semicolon(Rest1),
            Where = safe_max_n_on_same_line(FollowingTokens, 3),
            Error = {Line, ?MODULE, {syntax_error, {before, Where}}},
            p_top(Rest2, Acc, [Error | Errors]);
          ?STACKTRACE(throw, {syntax_error, Line, FollowingTokens, Why}, St)
            maybe_debug_syntax_error(Line, FollowingTokens, St, Why),
            ParenStack = [],
            Rest1 = try_recover(ParenStack, safe_tl(Tokens)),
            Rest2 = skip_semicolon(Rest1),
            Where = safe_max_n_on_same_line(FollowingTokens, 3),
            Error = {Line, ?MODULE, {syntax_error, {before, Where}, Why}},
            p_top(Rest2, Acc, [Error | Errors])
    end;
p_top([], Acc, Errors) ->
    {lists:reverse(Acc), lists:reverse(Errors)}.

maybe_debug_syntax_error(Line, FollowingTokens, StackTrace, Why) ->
    case os:getenv("GPB_DEBUG_PARSER") of
        Yes when Yes == "1"; Yes == "true" ->
            io:format("Syntax error on line ~p~n   ~P~n  ~p~n   ~p~n",
                      [Line, FollowingTokens, 10, Why, StackTrace]);
        _ ->
            ok
    end.

safe_tl([_ | Rest]) -> Rest;
safe_tl([]) -> [].

line([{_Token, Line, _Orig} | _]) -> Line;
line([]) -> 'at end-of-file'.

safe_max_n_on_same_line([], _Max) -> [];
safe_max_n_on_same_line([{_, Line, _Orig} | _]=Tokens, Max) ->
    safe_max_aux(Tokens, Max, Line).

safe_max_aux([{_, Line, _Orig}=Token | Rest], Max, Line) when Max >= 1 ->
    [Token | safe_max_aux(Rest, Max-1, Line)];
safe_max_aux(_, _Max, _Line) ->
    [].

try_recover([], [?t(';') | Rest])           -> Rest;
%% Top-level items
try_recover([], [?w("package") | _]=Rest)   -> Rest;
try_recover([], [?w("message") | _]=Rest)   -> Rest;
try_recover([], [?w("extend") | _]=Rest)    -> Rest;
try_recover([], [?w("option") | _]=Rest)    -> Rest;
try_recover([], [?w("service") | _]=Rest)   -> Rest;
%% Push parentheses
try_recover(PStk, [?t('(') | Rest])         -> try_recover(['(' | PStk], Rest);
try_recover(PStk, [?t('{') | Rest])         -> try_recover(['{' | PStk], Rest);
try_recover(PStk, [?t('[') | Rest])         -> try_recover(['[' | PStk], Rest);
%% Pop parentheses
try_recover(['{'], [?t('}') | Rest])        -> Rest; % top-level
try_recover(['(' | PStk], [?t(')') | Rest]) -> try_recover(PStk, Rest);
try_recover(['{' | PStk], [?t('}') | Rest]) -> try_recover(PStk, Rest);
try_recover(['[' | PStk], [?t(']') | Rest]) -> try_recover(PStk, Rest);
try_recover(PStk, [_ | Rest])               -> try_recover(PStk, Rest);
try_recover(_PStk, [])                      -> [].

%% --------------------------------------------
%% syntax

%% syntax_def -> syntax '=' str_lit ';'
p_syntax(Tokens) ->
    case Tokens of
        [?w("syntax"), ?t('=') | Rest] ->
            {Value, Rest2} = p_const(Rest),
            Rest3 = skip_semicolon(Rest2),
            case verify_syntax(Value) of
                ok ->
                    {{syntax, Value}, Rest3};
                {error, Why} ->
                    ?syntax_error(Rest, Why)
            end;
        _ ->
            ?syntax_error(Tokens)
    end.

%% --------------------------------------------
%% package

%% package_def -> package dotted_name ';'
p_package([?w("package") | Rest]) ->
    {Name, Rest2} = p_dotted_name(Rest),
    Rest3 = skip_semicolon(Rest2),
    {{package, Name}, Rest3}.

%% --------------------------------------------
%% import

%% import_def -> import str_lit ';'
p_import([?w("import") | Rest]) ->
    case Rest of
        [?s(Import) | Rest2] ->
            Rest3 = skip_semicolon(Rest2),
            {{import, str_value(Import)}, Rest3};
        [?w("public"), ?s(Import) | Rest2] ->
            Rest3 = skip_semicolon(Rest2),
            {{import, str_value(Import)}, Rest3};
        [?w("weak"), ?s(Import) | Rest2] ->
            Rest3 = skip_semicolon(Rest2),
            {{import, str_value(Import)}, Rest3};
        _ ->
            ?syntax_error(Rest)
    end;
p_import(Tokens) ->
    ?syntax_error(Tokens, "expected import <string>;").

%% --------------------------------------------
%% enum

%% enum_def -> enum name '{' enum_fields '}'
%%
%% enum_fields -> enum_field enum_fields
%% enum_fields -> option_def enum_fields
%% enum_fields -> ';' enum_fields
%% enum_fields -> '$empty'
%%
%% enum_field -> identifier '=' integer ';'
%% enum_field -> identifier '=' integer '[' opt_list ']' ';'
%%
p_enum([?w("enum"), ?w(Name/binary), ?t('{') | Rest]) ->
    Rest2 = skip_semicolon(Rest),
    {EnumItems, [?t('}') | Rest3]} = p_enum_fields(Rest2, []),
    Rest4 = skip_semicolon(Rest3),
    {{{enum, word_value(Name)}, EnumItems}, Rest4};
p_enum(Tokens) ->
    ?syntax_error(Tokens, "expected enum <name> { <symbol> = <value>; ... }").

p_enum_fields(Tokens, Acc) ->
    case Tokens of
        [?w("option") | _] ->
            {Option, Rest} = p_option(Tokens),
            p_enum_fields(Rest, [{Option} | Acc]);
        [?w("reserved") | _] ->
            {Reserved, Rest} = p_reserved(Tokens),
            p_enum_fields(Rest, [Reserved | Acc]);
        [?w(Name/binary), ?t('=') | Rest] ->
            {Value, Rest2} = p_integer_const(Rest),
            {EOpts, Rest3} = p_maybe_opt_list(Rest2),
            EnumField = {word_value(Name), Value, EOpts},
            Rest4 = skip_semicolon(Rest3),
            Acc1 = [EnumField | Acc],
            p_enum_fields(Rest4, Acc1);
        [?t('}') | _] ->
            {lists:reverse(Acc), Tokens};
        _ ->
            ?syntax_error(Tokens, "expected <symbol> = <value>;")
    end.

%% --------------------------------------------
%% message

%% message_def -> message fidentifier '{' msg_elems '}'
%%
%% msg_elems -> msg_elem msg_elems
%% msg_elems -> ';' msg_elems
%% msg_elems -> '$empty'
%%
%% msg_elem -> occurrence type identifier '=' dec_lit ';'
%% msg_elem -> occurrence type identifier '=' dec_lit '[' opt_list ']' ';'
%% msg_elem -> type identifier '=' dec_lit ';'                   % proto3
%% msg_elem -> type identifier '=' dec_lit '[' opt_list ']' ';'  % proto3
%% msg_elem -> map_type identifier '=' dec_lit ';'
%% msg_elem -> map_type identifier '=' dec_lit '[' opt_list ']' ';'
%% msg_elem -> message_def
%% msg_elem -> enum_def
%% msg_elem -> extensions_def
%% msg_elem -> oneof_def
%% msg_elem -> extend_def
%% msg_elem -> reserved_def
%% msg_elem -> group_def
%% msg_elem -> option_def
%%
%% occurrence -> required
%% occurrence -> optional
%% occurrence -> repeated
%%
%% type -> double
%% type -> float
%% type -> int32
%% type -> int64
%% type -> uint32
%% type -> uint64
%% type -> sint32
%% type -> sint64
%% type -> fixed32
%% type -> fixed64
%% type -> sfixed32
%% type -> sfixed64
%% type -> bool
%% type -> string
%% type -> bytes
%% type -> name
%%
p_message([?w("message"), ?w(Name/binary), ?t('{') | Rest]) ->
    Rest2 = skip_semicolon(Rest),
    {MsgElems, Rest3} = p_msg_elems(Rest2, []),
    Msg = {{msg, word_value(Name)}, MsgElems},
    Rest4 = skip_semicolon(Rest3),
    {Msg, Rest4};
p_message(Tokens) ->
    ExpectedWhat = "expected message <name> { <fields, messages or enums> }",
    ?syntax_error(Tokens, ExpectedWhat).

p_msg_elems(Tokens, Acc) ->
    case Tokens of
        [?t(';') | Rest] ->
            p_msg_elems(Rest, Acc);
        [?t('}') | Rest] ->
            {lists:reverse(Acc), Rest};
        _ ->
            {MsgElem, Rest} = p_msg_elem(Tokens),
            Rest2 = skip_semicolon(Rest),
            p_msg_elems(Rest2, [MsgElem | Acc])
    end.

p_msg_elem(Tokens) ->
    case hd(Tokens) of
        ?w("message") ->
            p_message(Tokens);
        ?w("enum") ->
            p_enum(Tokens);
        ?w("extensions") ->
            p_extensions(Tokens);
        ?w("oneof") ->
            p_oneof(Tokens);
        ?w("extend") ->
            p_extend(Tokens);
        ?w("reserved") ->
            p_reserved(Tokens);
        ?w("option") ->
            {Option, Rest} = p_option(Tokens),
            {{Option}, Rest};
        ?w("map") ->
            p_map(Tokens);
        ?w("required") ->
            p_field_or_group(required, tl(Tokens));
        ?w("optional") ->
            p_field_or_group(optional, tl(Tokens));
        ?w("repeated") ->
            p_field_or_group(repeated, tl(Tokens));
        _ ->
            p_field(undefined, Tokens)
    end.

%% group_def -> occurrence group identifier '=' dec_lit '{' msg_elems '}':
%%
p_field_or_group(Occurrence, [?w("group"), ?w(Name/binary), ?t('=') | Rest]) ->
    {FNum, Rest2} = p_integer_const(Rest),
    {_Opts, Rest3} = p_maybe_opt_list(Rest2),
    case Rest3  of
        [?t('{') | Rest4] ->
            Rest5 = skip_semicolon(Rest4),
            {MsgElems, Rest6} = p_msg_elems(Rest5, []),
            TmpGName = word_value(Name),
            Field = #?gpb_field{occurrence = Occurrence,
                                type       = {ref,['...expanded-later']},
                                name       = TmpGName,
                                fnum       = FNum,
                                opts       = []},
            Group = {group1, TmpGName, MsgElems, Field},
            Rest7 = skip_semicolon(Rest6),
            {Group, Rest7};
        _ ->
            ?syntax_error(Rest2)
    end;
p_field_or_group(Occurrence, Tokens) ->
    p_field(Occurrence, Tokens).

p_field(Occurrence, Tokens) ->
    {Type, Rest} = p_field_type(Tokens),
    case Rest of
        [?w(FName/binary), ?t('=') | Rest2] ->
            {FNum, Rest3} = p_integer_const(Rest2),
            {FOpts, Rest4} = p_field_opts(Rest3),
            Field = #?gpb_field{name       = word_value(FName),
                                type       = Type,
                                occurrence = Occurrence,
                                fnum       = FNum,
                                opts       = FOpts},
            Rest5 = skip_semicolon(Rest4),
            {Field, Rest5};
        _ ->
            ?syntax_error(Rest, "expected <field> = <num>")
    end.

p_field_type([?w("double") | Rest])   -> {double, Rest};
p_field_type([?w("float") | Rest])    -> {float, Rest};
p_field_type([?w("int32") | Rest])    -> {int32, Rest};
p_field_type([?w("int64") | Rest])    -> {int64, Rest};
p_field_type([?w("uint32") | Rest])   -> {uint32, Rest};
p_field_type([?w("uint64") | Rest])   -> {uint64, Rest};
p_field_type([?w("sint32") | Rest])   -> {sint32, Rest};
p_field_type([?w("sint64") | Rest])   -> {sint64, Rest};
p_field_type([?w("fixed32") | Rest])  -> {fixed32, Rest};
p_field_type([?w("fixed64") | Rest])  -> {fixed64, Rest};
p_field_type([?w("sfixed32") | Rest]) -> {sfixed32, Rest};
p_field_type([?w("sfixed64") | Rest]) -> {sfixed64, Rest};
p_field_type([?w("bool") | Rest])     -> {bool, Rest};
p_field_type([?w("string") | Rest])   -> {string, Rest};
p_field_type([?w("bytes") | Rest])    -> {bytes, Rest};
p_field_type(Tokens) ->
    {Name, Rest} = p_dotted_name(Tokens),
    {{ref, Name}, Rest}.

p_field_opts(Tokens) ->
    {Opts, Rest} = p_maybe_opt_list(Tokens),
    {[normalize_field_opt(Opt) || Opt <- Opts], Rest}.

normalize_field_opt({_,_}=Opt)  -> Opt;
normalize_field_opt(Opt)        -> {Opt, true}.

%% map_type -> map '<' map_key_type ',' type '>'
%%
%% map_key_type -> int32
%% map_key_type -> int64
%% map_key_type -> uint32
%% map_key_type -> uint64
%% map_key_type -> sint32
%% map_key_type -> sint64
%% map_key_type -> fixed32
%% map_key_type -> fixed64
%% map_key_type -> sfixed32
%% map_key_type -> sfixed64
%% map_key_type -> bool
%% map_key_type -> string
%% %% missing from type: double | float | bytes | message name | enum name
%%
p_map([?w("map"), ?t('<') | Rest]) ->
    {KeyType, Rest2} = p_map_key_type(Rest),
    case Rest2 of
        [?t(',') | Rest3] ->
            {ValueType, Rest4} = p_field_type(Rest3),
            case Rest4 of
                [?t('>'), ?w(FName/binary), ?t('=') | Rest5] ->
                    {FNum, Rest6} = p_integer_const(Rest5),
                    Type = {map, KeyType, ValueType},
                    {FOpts, Rest7} = p_field_opts(Rest6),
                    Field = #?gpb_field{name       = word_value(FName),
                                        type       = Type,
                                        occurrence = repeated,
                                        fnum       = FNum,
                                        opts       = FOpts},
                    Rest8 = skip_semicolon(Rest7),
                    {Field, Rest8};
                _ ->
                    ?syntax_error(Rest4, expected_mapfield_tokens())
            end;
        _->
            ?syntax_error(Rest2, expected_mapfield_tokens())
    end;
p_map(Tokens) ->
    ?syntax_error(Tokens, expected_mapfield_tokens()).

expected_mapfield_tokens() ->
    "expected map< <key type>, <value type> > = <num>;".

p_map_key_type([?w("int32") | Rest])    -> {int32, Rest};
p_map_key_type([?w("int64") | Rest])    -> {int64, Rest};
p_map_key_type([?w("uint32") | Rest])   -> {uint32, Rest};
p_map_key_type([?w("uint64") | Rest])   -> {uint64, Rest};
p_map_key_type([?w("sint32") | Rest])   -> {sint32, Rest};
p_map_key_type([?w("sint64") | Rest])   -> {sint64, Rest};
p_map_key_type([?w("fixed32") | Rest])  -> {fixed32, Rest};
p_map_key_type([?w("fixed64") | Rest])  -> {fixed64, Rest};
p_map_key_type([?w("sfixed32") | Rest]) -> {sfixed32, Rest};
p_map_key_type([?w("sfixed64") | Rest]) -> {sfixed64, Rest};
p_map_key_type([?w("bool") | Rest])     -> {bool, Rest};
p_map_key_type([?w("string") | Rest])   -> {string, Rest}.

%% oneof_def -> 'oneof' identifier '{' oneof_elems '}'
%%
%% oneof_elems -> oneof_elem oneof_elems
%% oneof_elems -> oneof_elem
%%
%% oneof_elem -> type fidentifier '=' dec_lit ';'
%% oneof_elem -> type fidentifier '=' dec_lit '[' opt_list ']' ';'
p_oneof([?w("oneof"), ?w(Name/binary), ?t('{') | Rest]) ->
    Rest2 = skip_semicolon(Rest),
    {Elems, [?t('}') | Rest3]} = p_oneof_elems(Rest2, []),
    {Opts, OFields} = lists:partition(
                        fun({{option, _OptName, _OptValue}}) -> true;
                           (_Other) -> false
                        end,
                        Elems),
    Opts1 = [{OptName,OptVal} || {{option, OptName, OptVal}} <- Opts],
    Field = #gpb_oneof{name = word_value(Name),
                       fields = OFields,
                       opts = Opts1},
    Rest4 = skip_semicolon(Rest3),
    {Field, Rest4};
p_oneof(Tokens) ->
    ?syntax_error(Tokens, "expected oneof <name> { <fields> }").

p_oneof_elems(Tokens, Acc) ->
    case Tokens of
        [?w("option") | _] ->
            {Opt, Rest} = p_option(Tokens),
            Rest2 = skip_semicolon(Rest),
            Acc1 = [{Opt} | Acc],
            p_oneof_elems(Rest2, Acc1);
        _ ->
            {Field, Rest} = p_field_or_group(optional, Tokens),
            Rest2 = skip_semicolon(Rest),
            Acc1 = [Field | Acc],
            case Rest2 of
                [?t('}') | _] ->
                    {lists:reverse(Acc1), Rest2};
                _ ->
                    p_oneof_elems(Rest2, Acc1)
            end
    end.

%% extensions_def -> extensions exts ';'
%%
%% exts -> ext ',' exts
%% exts -> ext
%%
%% ext -> integer
%% ext -> integer to integer
%% ext -> integer to max
p_extensions([?w("extensions") | Rest]) ->
    {Exts, Rest2} = p_exts(Rest, []),
    {_Opts, Rest3} = p_maybe_opt_list(Rest2),
    Rest4 = skip_semicolon(Rest3),
    {{extensions, lists:sort(Exts)}, Rest4}.

p_exts(Tokens, Acc) ->
    {Ext, Rest} = p_ext(Tokens),
    Acc1 = [Ext | Acc],
    case Rest of
        [?t(',') | Rest2] ->
            p_exts(Rest2, Acc1);
        _ ->
            {lists:reverse(Acc1), Rest}
    end.

p_ext([?i(Min), ?w("to"), ?i(Max) | Rest]) ->
    {{int_value(Min), int_value(Max)}, Rest};
p_ext([?i(Min), ?w("to"), ?w("max") | Rest]) ->
    {{int_value(Min), max}, Rest};
p_ext([?i(Int) | Rest]) ->
    I = int_value(Int),
    {{I, I}, Rest};
p_ext(Tokens) ->
    ?syntax_error(Tokens, "expected <n>, <n> to <m>, or <n> to max").

%% reserved_def -> reserved res_numbers
%% reserved_def -> reserved res_names
%%
%% res_numbers -> res_number ',' res_numbers
%% res_numbers -> res_number
%%
%% res_number -> integer
%% res_number -> integer to integer
%%
%% res_names -> string_expr ',' res_names
%% res_names -> string_expr
%%
p_reserved([?w("reserved") | Rest]) ->
    case hd(Rest) of
        ?i(_) ->
            {Numbers, Rest2} = p_reserved_numbers_or_ranges(Rest, []),
            Rest3 = skip_semicolon(Rest2),
            {{reserved_numbers, Numbers}, Rest3};
        ?t('-') ->
            {Numbers, Rest2} = p_reserved_numbers_or_ranges(Rest, []),
            Rest3 = skip_semicolon(Rest2),
            {{reserved_numbers, Numbers}, Rest3};
        ?s(_) ->
            {Names, Rest2} = p_reserved_names(Rest, []),
            Rest3 = skip_semicolon(Rest2),
            {{reserved_names, Names}, Rest3};
        _ ->
            ?syntax_error(Rest, "expected reserved numbers or ranges or names")
    end.

p_reserved_numbers_or_ranges(Tokens, Acc) ->
    {Reserved, Rest} = p_reserved_number_or_range(Tokens),
    Acc1 = [Reserved | Acc],
    case Rest of
        [?t(',') | Rest2] ->
            p_reserved_numbers_or_ranges(Rest2, Acc1);
        _ ->
            {lists:reverse(Acc1), Rest}
    end.

p_reserved_number_or_range(Tokens) ->
    {Min, Rest} = p_integer_const(Tokens),
    case Rest of
        [?w("to"), ?w("max") | Rest2] ->
            {{Min, max}, Rest2};
        [?w("to") | Rest2] ->
            {Max, Rest3} = p_integer_const(Rest2),
            {{Min, Max}, Rest3};
        _ ->
            {Min, Rest}
    end.

p_reserved_names(Tokens, Acc) ->
    {Reserved, Rest} = p_reserved_name(Tokens),
    Acc1 = [Reserved | Acc],
    case Rest of
        [?t(',') | Rest2] ->
            p_reserved_names(Rest2, Acc1);
        _ ->
            {lists:reverse(Acc1), Rest}
    end.

p_reserved_name(Tokens) ->
    p_str_const(Tokens, []).

%% --------------------------------------------
%% extend

%% extend_def -> extend name '{' msg_elems '}':
p_extend([?w("extend") | Rest]) ->
    {Name, Rest2} = p_dotted_name(Rest),
    case Rest2 of
        [?t('{') | Rest3] ->
            Rest4 = skip_semicolon(Rest3),
            {MsgElems, Rest5} = p_msg_elems(Rest4, []),
            Extend = {{extend, {eref1,Name}}, MsgElems},
            Rest6 = skip_semicolon(Rest5),
            {Extend, Rest6};
        _ ->
            ?syntax_error(Rest2)
    end.

%% --------------------------------------------
%% option

%% option_def -> option option_name '=' constant
p_option([?w("option") | Rest]=Tokens) ->
    case p_option_name(Rest) of
        {OptName, [?t('=') | Rest2]} ->
            {Value, Rest3} = p_option_value(Rest2),
            Rest4 = skip_semicolon(Rest3),
            {{option, OptName, Value}, Rest4};
        _ ->
            ?syntax_error(Tokens, "expected option <name> = <value>")
    end;
p_option(Tokens) ->
    ?syntax_error(Tokens).

p_option_value([?t('{')=T | Rest]) ->
    p_uninterpreted_block(Rest, 1, [T]);
p_option_value(Tokens) ->
    p_const(Tokens).

p_uninterpreted_block([Token | Rest], Depth, Acc) ->
    %% Just count curly braces until we find a matching one.
    %% This seems to be what the protobuf does.
    case Token of
        ?t('}') ->
            if Depth =:= 1 ->
                    AccTokens = lists:reverse([Token | Acc]),
                    S = lists:flatten(tokens_to_str(AccTokens)),
                    V = {uninterpreted, S},
                    {V, Rest};
               Depth > 1 ->
                    p_uninterpreted_block(Rest, Depth-1, [Token | Acc])
            end;
        ?t('{') ->
            p_uninterpreted_block(Rest, Depth+1, [Token | Acc]);
        _ ->
            p_uninterpreted_block(Rest, Depth, [Token | Acc])
    end;
p_uninterpreted_block([]=Tokens, _Depth, Acc) ->
    L0Str = integer_to_list(line(lists:last(Acc))),
    Why = "unexpected end of input in option block starting at line " ++ L0Str,
    ?syntax_error(Tokens, Why).

%% --------------------------------------------
%% service

%% service_def -> service fidentifier '{' rpc_defs '}'
%%
%% rpc_defs -> rpc_def rpc_defs
%% rpc_defs -> ';' rpc_defs
%% rpc_defs -> '$empty'
%%
%% rpc_def -> rpc fidentifier rpc_arg returns rpc_ret ';':
%% rpc_def -> rpc fidentifier rpc_arg returns rpc_ret '{' m_opts '}':
%%
%% rpc_arg -> '(' name ')'
%% rpc_arg -> '(' stream name ')'
%%
%% rpc_ret -> '(' name ')'
%% rpc_ret -> '(' stream name ')'
%%
%% m_opts -> option_def ';' m_opts
%% m_opts -> ';' m_opts
%% m_opts -> '$empty'
p_service([?w("service"), ?w(Name/binary), ?t('{') | Rest]) ->
    Rest2 = skip_semicolon(Rest),
    {RpcDefs, [?t('}') | Rest3]} = p_rpc_defs(Rest2, []),
    Service = {{service, word_value(Name)}, RpcDefs},
    Rest4 = skip_semicolon(Rest3),
    {Service, Rest4}.

p_rpc_defs(Tokens, Acc) ->
    case Tokens of
        [?t('}') | _] ->
            {lists:reverse(Acc), Tokens};
        [?w("option") | _] ->
            {Opt, Rest} = p_option(Tokens),
            Acc1 = [{Opt} | Acc],
            p_rpc_defs(Rest, Acc1);
        [?w("rpc") | _] ->
            {Rpc, Rest} = p_rpc_def(Tokens),
            Acc1 = [Rpc | Acc],
            Rest2 = skip_semicolon(Rest),
            p_rpc_defs(Rest2, Acc1);
        _ ->
            ?syntax_error(Tokens, "expected rpc definitions")
    end.

p_rpc_def([?w("rpc"), ?w(Name/binary) | Rest]) ->
    case p_rpc_arg(Rest) of
        {RpcArg, [?w("returns") | Rest2]}  ->
            {RpcRet, Rest3} = p_rpc_ret(Rest2),
            {MOpts, Rest4} = p_maybe_rpc_method_opts(Rest3),
            Rpc = {word_value(Name), RpcArg, RpcRet, MOpts},
            Rest5 = skip_semicolon(Rest4),
            {Rpc, Rest5};
        _ ->
            ?syntax_error(Rest, "expected returns")
    end.

p_rpc_arg(Tokens) -> p_rpc_ar(Tokens).

p_rpc_ret(Tokens) -> p_rpc_ar(Tokens).

p_rpc_ar([?t('(') | Rest]) ->
    {IsStream, Rest3} =
        case Rest of
            [?w("stream") | Rest2] ->
                {true, Rest2};
            _ ->
                {false, Rest}
        end,
    case p_dotted_name(Rest3) of
        {Name, [?t(')') | Rest4]} ->
            {{Name, IsStream}, Rest4};
        _ ->
            ?syntax_error(Rest3)
    end;
p_rpc_ar(Tokens) ->
    ?syntax_error(Tokens).

p_maybe_rpc_method_opts([?t('{') | Rest]) ->
    Rest2 = skip_semicolon(Rest),
    {Opts, [?t('}') | Rest3]} = p_rpc_method_opts(Rest2, []),
    {Opts, Rest3};
p_maybe_rpc_method_opts(Tokens) ->
    {[], Tokens}.

p_rpc_method_opts(Tokens, Acc) ->
    case Tokens of
        [?t('}') | _] ->
            {lists:reverse(Acc), Tokens};
        [?w("option") | _] ->
            {Option, Rest} = p_option(Tokens),
            Acc1 = [Option | Acc],
            Rest2 = skip_semicolon(Rest),
            p_rpc_method_opts(Rest2, Acc1);
        _ ->
            ?syntax_error(Tokens, "expected rpc method options")
    end.

%% --------------------------------------------
%% misc common

%% dotted_name -> '.' name_parts
%% dotted_name -> name_parts
%%
%% name_parts -> name '.' name_parts
%% name_parts -> name
p_dotted_name([?t('.') | Rest]) -> p_dn2(Rest, ['.']);
p_dotted_name(Tokens)           -> p_dn2(Tokens, []).

p_dn2([?w(Name/binary) | Rest], Acc) ->
    p_dn3(Rest, [word_value(Name) | Acc]);
p_dn2(Tokens, _Acc) ->
    ?syntax_error(Tokens, "expected dotted name").

p_dn3([?t('.'), ?w(Name/binary) | Tl], NAcc) ->
    p_dn3(Tl, [word_value(Name), '.' | NAcc]);
p_dn3(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

%% option_name -> option_name_part { '.' option_name_part }*
%%
%% option_name_part -> identifier
%% option_name_part -> '(' dotted_name ')'
%%
%% Return: an atom if the option_name is a single identifier
%%         a flat list if the option is any form of dotted name
p_option_name(Tokens) ->
    {NamePart, Rest} = p_option_name_part(Tokens),
    p_opt_nm2(Rest, opt_name_acc_new(NamePart)).

p_opt_nm2([?t('.') | Rest], Acc) ->
    {NamePart, Rest2} = p_option_name_part(Rest),
    Acc1 = opt_name_acc_add(NamePart, Acc),
    p_opt_nm2(Rest2, Acc1);
p_opt_nm2(Tokens, Acc) ->
    Res = opt_name_acc_finalize(Acc),
    {Res, Tokens}.

opt_name_acc_new(Name) when is_atom(Name) ->
    Name;
opt_name_acc_new(Name) when is_tuple(Name) ->
    [Name].

opt_name_acc_add(Name, Acc) when is_atom(Name) ->
    Acc1 = ensure_atom_list(Acc),
    [Name | Acc1];
opt_name_acc_add(Name, Acc) when is_tuple(Name) ->
    Acc1 = ensure_atom_list(Acc),
    [Name | Acc1].

opt_name_acc_finalize(A) when is_atom(A)  -> A;
opt_name_acc_finalize(L) when is_list(L)  -> lists:reverse(L).

ensure_atom_list(A) when is_atom(A)  -> [A];
ensure_atom_list(L) when is_list(L)  -> L.

p_option_name_part(Tokens) ->
    case Tokens of
        [?t('(') | Rest] ->
            case p_dotted_name(Rest) of
                {DottedName, [?t(')') | Rest2]} ->
                    {list_to_tuple(undot_but_first(DottedName)), Rest2};
                _ ->
                    ?syntax_error(Rest, "expected option name")
            end;
        [?w(Ident/binary) | Rest] ->
            {word_value(Ident), Rest};
        _ ->
            ?syntax_error(Tokens, "expected option name")
    end.

undot_but_first(['.' | Rest]) -> ['.' | undot2(Rest)];
undot_but_first(Components)   -> undot2(Components).

undot2(Components) ->
    lists:filter(fun(C) -> C /= '.' end, Components).

%% opt_list -> opt ',' opt_list
%% opt_list -> opt
%%
%% opt -> option_name '=' constant
%% opt -> option_name
p_opt_list(Tokens, Acc) ->
    case Tokens of
        [?t(']') | Rest] ->
            {lists:reverse(Acc), Rest};
        [?t(',') | Rest] ->
            p_opt_list(Rest, Acc);
        _ ->
            {OptionName, Rest} = p_option_name(Tokens),
            {Option, Rest4} =
                case Rest of
                    [?t('=') | Rest2] ->
                        {Value, Rest3} = p_option_value(Rest2),
                        {{OptionName, Value}, Rest3};
                    _ ->
                        {OptionName, Rest}
                end,
            Acc1 = [Option | Acc],
            p_opt_list(Rest4, Acc1)
    end.

p_maybe_opt_list([?t('[') | Rest]) ->
    {Opts, Rest2} = p_opt_list(Rest, []),
    {Opts, Rest2};
p_maybe_opt_list(Tokens) ->
    {[], Tokens}.

p_integer_const([?i(Int) | Rest])          -> {int_value(Int), Rest};
p_integer_const([?t('-'), ?i(Int) | Rest]) -> {-int_value(Int), Rest};
p_integer_const([?t('+'), ?i(Int) | Rest]) -> {int_value(Int), Rest};
p_integer_const(Tokens) ->
    ?syntax_error(Tokens).

%% const -> integer_or_float
%% const -> '+' integer_or_float
%% const -> '-' integer_or_float
%% const -> pstring_expr
%% const -> word
%%
%% integer_or_float -> float_lit
%% integer_or_float -> inf
%% integer_or_float -> nan
%% integer_or_float -> int_lit
p_const([?s(Str) | Rest]) ->
    p_str_const(Rest, [str_value(Str)]);
p_const([?i(Int) | Rest]) ->
    {int_value(Int), Rest};
p_const([?fl(Float) | Rest]) ->
    {float_value(Float), Rest};
p_const([?w("inf") | Rest]) ->
    {infinity, Rest};
p_const([?w("nan") | Rest]) ->
    {nan, Rest};
p_const([?t('-'), ?i(Int) | Rest]) ->
    {-int_value(Int), Rest};
p_const([?t('-'), ?fl(Float) | Rest]) ->
    {-float_value(Float), Rest};
p_const([?t('-'), ?w("inf") | Rest]) ->
    {'-infinity', Rest};
p_const([?t('+'), ?i(Int) | Rest]) ->
    {int_value(Int), Rest};
p_const([?t('+'), ?fl(Float) | Rest]) ->
    {float_value(Float), Rest};
p_const([?t('+'), ?w("inf") | Rest]) ->
    {infinity, Rest};
p_const([?w(Word/binary) | Rest]) ->
    {word_value(Word), Rest};
p_const(Tokens) ->
    ?syntax_error(Tokens, "expected constant").

p_str_const([?s(Str) | Rest], Acc) ->
    Acc1 = [str_value(Str) | Acc],
    p_str_const(Rest, Acc1);
p_str_const(Rest, Acc) ->
    S =  lists:concat(lists:reverse(Acc)),
    {S, Rest}.

skip_semicolon([?t(';') | Rest]) -> skip_semicolon(Rest);
skip_semicolon(Tokens)           -> Tokens.

verify_syntax("proto2") ->
    ok;
verify_syntax("proto3") ->
    ok;
verify_syntax("proto"++_ = Unsupported) ->
    {error, "Unsupported proto version: " ++ Unsupported};
verify_syntax(Unsupported) ->
    {error, ?f("Unsupported proto syntax: ~p", [Unsupported])}.

str_value(S) ->
    S.

word_value(Bin) ->
    list_to_atom(binary_to_list(Bin)).

int_value({dec, N}) -> N;
int_value({hex, N}) -> N;
int_value({oct, N}) -> N.

float_value(Float) ->
    Float.

tokens_to_str(Tokens) ->
    space_join([token_to_str(T) || T <- Tokens]).

token_to_str({_X, _Loc, Orig}) ->
    if is_binary(Orig) ->
            unicode:characters_to_list(Orig);
       is_atom(Orig) ->
            atom_to_list(Orig)
    end.

space_join([]) -> "";
space_join([H | Tl]) -> [H | [[$\s, Elem] || Elem <- Tl]].

ensure_str(S) ->
    case io_lib:printable_list(S) of
        true ->
            S;
        false ->
            ?f("~p", [S])
    end.

%% -- bwd compat -- do not use these ----------------------------------
%% Deprecated!
%% Instead, to retrieve proto definitions,
%% use the option to_proto_defs
%% with gpb_compile:file or gpb_compile:string
%% For exprotobuf, see also
%% https://github.com/bitwalker/exprotobuf/issues/114

%% Use the option to_proto_defs with gpb_compile:file/string instead.
post_process_one_file(FileName, Defs, Opts) ->
    gpb_defs:post_process_one_file(FileName, Defs, Opts).

%% Use the option to_proto_defs with gpb_compile:file/string instead.
post_process_all_files(Defs, _Opts) ->
    case gpb_defs:post_process_all_files(Defs, _Opts) of
        {ok, Defs1} ->
            gpb_defs:convert_defs_from_latest_version(Defs1, 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% Use the option to_proto_defs with gpb_compile:file/string instead.
format_post_process_error({error, Reasons}) ->
    gpb_defs:format_post_process_error({error, Reasons}).

%% Use gpb_defs:fetch_imports instead.
fetch_imports(Defs) ->
    gpb_defs:fetch_imports(Defs).
%% --^^--- bwd compat -- do not use these ----------------------------
