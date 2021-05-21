%% This line tells emacs to use -*- erlang -*- mode for this file

%%% Copyright (C) 2010-2013  Tomas Abrahamsson
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

Nonterminals
        proto
        syntax_def
        elements element
        enum_def enum_fields enum_field
        opt_enum_opts enum_opts enum_opt
        message_def msg_elems msg_elem
        opt_field_opts field_opts field_opt occurrence type
        map_type map_key_type
        package_def
        import_def
        identifiers
        option_name_ident
        extend_def extensions_def exts ext
        reserved_def res_numbers res_number res_names
        oneof_def oneof_elems oneof_elem
        option_def
        group_def
        service_def rpc_defs rpc_def rpc_arg rpc_ret m_opts
        name
        option_name
        constant
        integer
        string_expr
        fidentifier
        .

Terminals
        package
        message enum
        required optional repeated
        double float int32 int64 uint32
        uint64 sint32 sint64 fixed32 fixed64
        sfixed32 sfixed64 bool string bytes map
        identifier str_lit dec_lit oct_lit hex_lit float_lit bool_lit
        default
        import
        option
        extensions extend max to reserved
        oneof
        group
        service rpc returns stream
        packed deprecated
        syntax
        '.' ';' '(' ')' '{' '}' '[' ']' '=' ',' '<' '>'
        .

Rootsymbol
        proto.

Endsymbol
        '$end'.


%% TODO: implement verification of references
%% TODO: implement (custom) options: allowed everywhere

proto -> elements:                      '$1'.
proto -> syntax_def elements:           ['$1' | '$2'].

syntax_def -> syntax '=' str_lit ';':   verify_syntax('$3').

elements -> element elements:           ['$1' | '$2'].
elements -> ';' elements:               '$2'.
elements -> '$empty':                   [].

element -> package_def:                 '$1'.
element -> import_def:                  '$1'.
element -> enum_def:                    '$1'.
element -> message_def:                 '$1'.
element -> extend_def:                  '$1'.
element -> option_def:                  '$1'.
element -> service_def:                 '$1'.

package_def -> package name ';':        {package, '$2'}.

name -> '.' identifiers:                ['.' | '$2'].
name -> identifiers:                    '$1'.

option_name_ident -> identifier:        identifier_name('$1').
option_name_ident -> '(' name ')':      ensure_list('$2').

option_name -> option_name_ident:           '$1'.
option_name -> option_name_ident '.' name:  ensure_list('$1') ++ '$3'.

identifiers -> identifier '.' identifiers:      [identifier_name('$1'), '.'
                                                 | '$3'].
identifiers -> identifier:                      [identifier_name('$1')].

import_def -> import str_lit ';':       {import, literal_value('$2')}.

option_def -> option option_name '=' constant: {option, '$2', '$4'}.

enum_def -> enum fidentifier '{' enum_fields '}':
                                        {{enum,identifier_name('$2')},'$4'}.

enum_fields -> enum_field enum_fields:  ['$1' | '$2'].
enum_fields -> option_def enum_fields:  ['$1' | '$2'].
enum_fields -> ';' enum_fields:         '$2'.
enum_fields -> '$empty':                [].

enum_field -> fidentifier '=' integer ';':
                                        {identifier_name('$1'), '$3'}.
enum_field -> fidentifier '=' integer '[' opt_enum_opts ']' ';':
                                        {identifier_name('$1'), '$3'}.

opt_enum_opts -> enum_opts:             '$1'.
opt_enum_opts -> '$empty':              [].

enum_opts -> enum_opt ',' enum_opts:    ['$1' | '$2'].
enum_opts -> enum_opt:                  ['$1'].

enum_opt -> deprecated:                 {deprecated, true}.
enum_opt -> deprecated '=' bool_lit:    {deprecated, literal_value('$3')}.
enum_opt -> option_name '=' constant:   {'$1', '$3'}.
enum_opt -> option_name:                '$1'.


message_def -> message fidentifier '{' msg_elems '}':
                                        {{msg,identifier_name('$2')},'$4'}.

msg_elems -> msg_elem msg_elems:        ['$1' | '$2'].
msg_elems -> ';' msg_elems:             '$2'.
msg_elems -> '$empty':                  [].

msg_elem -> occurrence type fidentifier '=' dec_lit ';':
                                        #?gpb_field{occurrence='$1',
                                                    type='$2',
                                                    name=identifier_name('$3'),
                                                    fnum=literal_value('$5'),
                                                    opts=[]}.
msg_elem -> occurrence type fidentifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #?gpb_field{occurrence='$1',
                                                    type='$2',
                                                    name=identifier_name('$3'),
                                                    fnum=literal_value('$5'),
                                                    opts='$7'}.
msg_elem -> type fidentifier '=' dec_lit ';': % proto3
                                        #?gpb_field{occurrence=undefined,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts=[]}.
msg_elem -> type fidentifier '=' dec_lit '[' opt_field_opts ']' ';': % proto3
                                        #?gpb_field{occurrence=undefined,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts='$6'}.
msg_elem -> map_type fidentifier '=' dec_lit ';':
                                        #?gpb_field{occurrence=repeated,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4')}.
msg_elem -> map_type fidentifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #?gpb_field{occurrence=repeated,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts='$6'}.

msg_elem -> message_def:                '$1'.
msg_elem -> enum_def:                   '$1'.
msg_elem -> extensions_def:             {extensions,lists:sort('$1')}.
msg_elem -> oneof_def:                  '$1'.
msg_elem -> extend_def:                 '$1'.
msg_elem -> reserved_def:               '$1'.
msg_elem -> group_def:                  '$1'.
msg_elem -> option_def:                 '$1'.

fidentifier -> identifier:              '$1'.
fidentifier -> package:                 kw_to_identifier('$1').
fidentifier -> service:                 kw_to_identifier('$1').
fidentifier -> enum:                    kw_to_identifier('$1').
fidentifier -> message:                 kw_to_identifier('$1').
fidentifier -> required:                kw_to_identifier('$1').
fidentifier -> optional:                kw_to_identifier('$1').
fidentifier -> repeated:                kw_to_identifier('$1').
fidentifier -> double:                  kw_to_identifier('$1').
fidentifier -> 'float':                 kw_to_identifier('$1').
fidentifier -> int32:                   kw_to_identifier('$1').
fidentifier -> int64:                   kw_to_identifier('$1').
fidentifier -> uint32:                  kw_to_identifier('$1').
fidentifier -> uint64:                  kw_to_identifier('$1').
fidentifier -> sint32:                  kw_to_identifier('$1').
fidentifier -> sint64:                  kw_to_identifier('$1').
fidentifier -> fixed32:                 kw_to_identifier('$1').
fidentifier -> fixed64:                 kw_to_identifier('$1').
fidentifier -> sfixed32:                kw_to_identifier('$1').
fidentifier -> sfixed64:                kw_to_identifier('$1').
fidentifier -> bool:                    kw_to_identifier('$1').
fidentifier -> string:                  kw_to_identifier('$1').
fidentifier -> bytes:                   kw_to_identifier('$1').
fidentifier -> bool_lit:                kw_to_identifier(literal_value('$1')).
fidentifier -> default:                 kw_to_identifier('$1').
fidentifier -> import:                  kw_to_identifier('$1').
fidentifier -> option:                  kw_to_identifier('$1').
fidentifier -> extensions:              kw_to_identifier('$1').
fidentifier -> extend:                  kw_to_identifier('$1').
fidentifier -> max:                     kw_to_identifier('$1').
fidentifier -> to:                      kw_to_identifier('$1').
fidentifier -> rpc:                     kw_to_identifier('$1').
fidentifier -> returns:                 kw_to_identifier('$1').
fidentifier -> stream:                  kw_to_identifier('$1').
fidentifier -> packed:                  kw_to_identifier('$1').
fidentifier -> deprecated:              kw_to_identifier('$1').
fidentifier -> syntax:                  kw_to_identifier('$1').
fidentifier -> map:                     kw_to_identifier('$1').
fidentifier -> reserved:                kw_to_identifier('$1').
fidentifier -> group:                   kw_to_identifier('$1').

opt_field_opts -> field_opts:           '$1'.
opt_field_opts -> '$empty':             [].


field_opts -> field_opt ',' field_opts: ['$1' | '$3'].
field_opts -> field_opt:                ['$1'].


field_opt -> default '=' constant:      {default, '$3'}.
field_opt -> packed:                    {packed, true}.
field_opt -> packed '=' bool_lit:       {packed, literal_value('$3')}.
field_opt -> deprecated:                {deprecated, true}.
field_opt -> deprecated '=' bool_lit:   {deprecated, literal_value('$3')}.
field_opt -> option_name:               {'$1', true}.
field_opt -> option_name '=' constant:  {'$1', '$3'}.

occurrence -> required:                 required.
occurrence -> optional:                 optional.
occurrence -> repeated:                 repeated.

type -> double:                         double.
type -> float:                          float.
type -> int32:                          int32.
type -> int64:                          int64.
type -> uint32:                         uint32.
type -> uint64:                         uint64.
type -> sint32:                         sint32.
type -> sint64:                         sint64.
type -> fixed32:                        fixed32.
type -> fixed64:                        fixed64.
type -> sfixed32:                       sfixed32.
type -> sfixed64:                       sfixed64.
type -> bool:                           bool.
type -> string:                         string.
type -> bytes:                          bytes.
type -> name:                           {ref, '$1'}.

map_type -> map '<' map_key_type ',' type '>': {map,'$3','$5'}.

map_key_type -> int32:                  int32.
map_key_type -> int64:                  int64.
map_key_type -> uint32:                 uint32.
map_key_type -> uint64:                 uint64.
map_key_type -> sint32:                 sint32.
map_key_type -> sint64:                 sint64.
map_key_type -> fixed32:                fixed32.
map_key_type -> fixed64:                fixed64.
map_key_type -> sfixed32:               sfixed32.
map_key_type -> sfixed64:               sfixed64.
map_key_type -> bool:                   bool.
map_key_type -> string:                 string.
%% missing from type: double | float | bytes | message name | enum name

group_def -> occurrence group fidentifier '=' dec_lit '{' msg_elems '}':
                 begin
                     TmpGName = identifier_name('$3'),
                     {group1,TmpGName,'$7',
                      #?gpb_field{occurrence='$1',
                                  type={ref,['...expanded-later']},
                                  name=identifier_name('$3'),
                                  fnum=literal_value('$5'),
                                  opts=[]}}
                 end.

constant -> identifier:                 identifier_name('$1').
constant -> integer:                    '$1'.
constant -> float_lit:                  literal_value('$1').
constant -> string_expr:                '$1'.
constant -> bool_lit:                   literal_value('$1').

integer -> dec_lit:                     literal_value('$1').
integer -> oct_lit:                     literal_value('$1').
integer -> hex_lit:                     literal_value('$1').

%% the protoc parser sports a c[++] style string concatenation feature
string_expr -> str_lit string_expr:     literal_value('$1') ++ '$2'.
string_expr -> str_lit:                 literal_value('$1').

extensions_def -> extensions exts ';':  '$2'.

exts -> ext ',' exts:                   ['$1' | '$3'].
exts -> ext:                            ['$1'].

ext -> integer:                         {'$1','$1'}.
ext -> integer to integer:              {'$1','$3'}.
ext -> integer to max:                  {'$1',max}.

reserved_def -> reserved res_numbers:   {reserved_numbers,'$2'}.
reserved_def -> reserved res_names:     {reserved_names,'$2'}.

res_numbers -> res_number ',' res_numbers: ['$1' | '$3'].
res_numbers -> res_number:                 ['$1'].

res_number -> integer:                  '$1'.
res_number -> integer to integer:       {'$1','$3'}.

res_names -> string_expr ',' res_names: ['$1' | '$3'].
res_names -> string_expr:               ['$1'].

oneof_def -> 'oneof' fidentifier '{' oneof_elems '}':
                                        #gpb_oneof{name=identifier_name('$2'),
                                                   fields='$4'}.

oneof_elems -> oneof_elem oneof_elems:  ['$1' | '$2'].
oneof_elems -> oneof_elem:              ['$1'].

oneof_elem -> type fidentifier '=' dec_lit ';':
                                        #?gpb_field{occurrence=optional,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts=[]}.
oneof_elem -> type fidentifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #?gpb_field{occurrence=optional,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts='$6'}.

extend_def -> extend name '{' msg_elems '}':
                                        {{extend,{eref1,'$2'}},'$4'}.


service_def -> service fidentifier '{' rpc_defs '}':
                                        {{service,identifier_name('$2')},'$4'}.

rpc_defs -> rpc_def rpc_defs:           ['$1' | '$2'].
rpc_defs -> ';' rpc_defs:               '$2'.
rpc_defs -> '$empty':                   [].

rpc_def -> rpc fidentifier rpc_arg returns rpc_ret ';':
                                        {identifier_name('$2'), '$3','$5',[]}.
rpc_def -> rpc fidentifier rpc_arg returns rpc_ret '{' m_opts '}':
                                        {identifier_name('$2'), '$3','$5','$7'}.

rpc_arg -> '(' name ')':                {'$2', false}.
rpc_arg -> '(' stream name ')':         {'$3', true}.

rpc_ret -> '(' name ')':                {'$2', false}.
rpc_ret -> '(' stream name ')':         {'$3', true}.

m_opts -> option_def ';' m_opts:        ['$1' | '$3'].
m_opts -> ';' m_opts:                   '$2'.
m_opts -> '$empty':                     [].



Header
"%%% @doc The yecc grammar for the protobuf language,"
"%%% both for syntax = proto2 and for proto3."
"%%% @private"
"".

Erlang code.

-include("../include/gpb.hrl").

%% ------------------------------------------------------------
%% Redirect some funcions and types for backwards compatibility
%% (some external libs currently use some of them)
-export([post_process_one_file/3]).
-export([post_process_all_files/2]).
-export([format_post_process_error/1]).
-export([fetch_imports/1]).

-export_type([defs/0, def/0]).
-export_type([field/0]).
-type defs() :: gpb_defs:defs().
-type def() :: gpb_defs:def().
-type field() :: gpb_defs:field().

post_process_one_file(FileName, Defs, Opts) ->
    gpb_defs:post_process_one_file(FileName, Defs, Opts).

post_process_all_files(Defs, _Opts) ->
    gpb_defs:post_process_all_files(Defs, _Opts).

format_post_process_error({error, Reasons}) ->
    gpb_defs:format_post_process_error({error, Reasons}).

fetch_imports(Defs) ->
    gpb_defs:fetch_imports(Defs).
%% --------------------------------------------------------


verify_syntax({str_lit, _Line, "proto2"}) ->
    {syntax, "proto2"};
verify_syntax({str_lit, _Line, "proto3"}) ->
    {syntax, "proto3"};
verify_syntax({str_lit, Line, "proto"++_ = Unsupported}) ->
    return_error(Line, "Unsupported proto version: " ++ Unsupported);
verify_syntax({str_lit, Line, Unsupported}) ->
    return_error(Line, "Unsupported proto syntax: " ++ Unsupported).

identifier_name({identifier, _Line, Name}) -> list_to_atom(Name).

kw_to_identifier({Kw, Line}) ->
    {identifier, Line, atom_to_list(Kw)}.

literal_value({_TokenType, _Line, Value}) -> Value.

ensure_list(L) when is_list(L) -> L;
ensure_list(Elem) -> [Elem].
