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

%%% ------------------------------------------------------------------
%%% @doc
%%% Compile protobuf definition files to a module that can encode and decode
%%% values to and from binaries.
%%%
%%% The Erlang types for the values are as follows
%%%
%%% <a id="protobuf-to-erlang-types"/>
%%% <table rules="all" frame="border">
%%% <thead><tr><th align="left">Protobuf type</th>
%%%            <th align="left">Erlang type</th></tr></thead>
%%% <tbody>
%%% <!-- = = = = = = = = = = = = = = = = = = = = = = = = = = = -->
%%% <tr><td>double, float</td>
%%%     <td>float() | infinity | '-infinity' | nan<br/>
%%%         When encoding, integers, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>   int32,    int64<br/>
%%%           uint32,   uint64<br/>
%%%           sint32,   sint64<br/>
%%%          fixed32,  fixed64<br/>
%%%         sfixed32, sfixed64</td>
%%%     <td>integer()</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>bool</td>
%%%     <td>true | false<br/>
%%%         When encoding, the integers 1 and 0, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>enum</td>
%%%     <td>atom()<br/>
%%%         unknown enums decode to integer()</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>message</td>
%%%     <td>record (thus tuple())<br/>
%%%         or map() if the maps (-maps) option is specified</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>string</td>
%%%     <td>unicode string, thus list of integers<br/>
%%%         or binary() if the strings_as_binaries (-strbin) option is
%%%         specified<br/>
%%%         When encoding, iolists, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>bytes</td>
%%%     <td>binary()<br/>
%%%         When encoding, iolists, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>oneof</td>
%%%     <td><tt>{ChosenFieldName, Value}</tt><br/>
%%%         or <tt>ChosenFieldName => Value</tt> if the {maps_oneof,flat}
%%%         (-maps_oneof flat) option is specified</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td><![CDATA[map<_,_>]]></td>
%%%     <td>An unordered list of 2-tuples, <tt>[{Key,Value}]</tt><br/>
%%%         or  a map(), if the maps (-maps) option is specified</td></tr>
%%% </tbody></table>
%%%
%%% Repeated fields are represented as lists.
%%%
%%% Optional fields are represented as either the value or `undefined' if
%%% not set. However, for maps, if the option `maps_unset_optional' is set
%%% to `omitted', then unset optional values are omitted from the map,
%%% instead of being set to `undefined' when encoding messages. When
%%% decoding messages, even with `maps_unset_optional' set to `omitted',
%%% the default value will be set in the decoded map.
%%%
%%% @end
%%% ------------------------------------------------------------------


-module(gpb_compile).
%-compile(export_all).
-export([file/1, file/2]).
-export([string/2, string/3]).
-export([proto_defs/2, proto_defs/3, proto_defs/5]).
-export([msg_defs/2, msg_defs/3]).
-export([format_error/1, format_warning/1]).
-export([c/0, c/1, c/2]). % Cmd line interface, halts vm---don't use from shell!
-export([parse_opts_and_args/1]).
-export([show_args/0]).
-export([show_version/0]).
-export([locate_import/2]).
-export([read_import/2]).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, repeat_clauses/2]).

%% -- Types -----------------------------------------------------

%% Options
-type boolean_opt(X) :: X | {X, boolean()}.% Just an option `X' means `{X,true}'
-type directory() :: string().

-type opts() :: [opt()].
-type opt() :: type_specs | {type_specs, boolean()} |
               {verify, optionally | always | never} |
               {copy_bytes, true | false | auto | integer() | float()} |
               {strings_as_binaries, boolean()} | strings_as_binaries |
               boolean_opt(defs_as_proplists) |
               boolean_opt(descriptor) |
               boolean_opt(maps) |
               boolean_opt(msgs_as_maps) |
               boolean_opt(mapfields_as_maps) |
               boolean_opt(defs_as_maps) |
               {maps_unset_optional, omitted | present_undefined} |
               {maps_oneof, tuples | flat} |
               {maps_key_type, atom | binary} |
               boolean_opt(nif) |
               {load_nif, string()} |
               {i, directory()} |
               {o, directory()} |
               {o_erl, directory()} | {o_hrl, directory()} |
               {o_nif_cc, directory()} |
               binary | to_proto_defs | to_msg_defs |
               return |
               boolean_opt(return_warnings) | boolean_opt(return_errors) |
               report |
               boolean_opt(report_warnings) | boolean_opt(report_errors) |
               boolean_opt(warnings_as_errors) |
               boolean_opt(include_as_lib) |
               boolean_opt(use_packages) |
               {erlc_compile_options,string()} |
               {rename, renaming()} |
               {msg_name_prefix,
                string() | atom() |
                {by_proto, prefix_by_proto()}} |
               {msg_name_suffix, string() | atom()} |
               boolean_opt(msg_name_to_snake_case) |
               boolean_opt(msg_name_to_lower) |
               {module_name_prefix, string() | atom()} |
               {module_name_suffix, string() | atom()} |
               {module_name, string() | atom()} |
               {translate_type, {gpb_field_type(), [translation()]}} |
               {translate_field, {field_path(), [translation()]}} |
               {any_translate, [translation()]} |
               boolean_opt(epb_compatibility) |
               boolean_opt(epb_functions) |
               boolean_opt(defaults_for_omitted_optionals) |
               boolean_opt(type_defaults_for_omitted_optionals) |
               {import_fetcher, import_fetcher_fun()} |
               {target_erlang_version, integer() | current} |
               boolean_opt(bypass_wrappers) |
               boolean_opt(json) |
               boolean_opt(json_always_print_primitive_fields) |
               boolean_opt(json_preserve_proto_field_names) |
               boolean_opt(json_case_insensitive_enum_parsing) |
               {json_format, json_format()} |
               {json_object_format, json_object_format()} |
               {json_key_format, json_key_format()} |
               {json_array_format, json_array_format()} |
               {json_string_format, json_string_format()} |
               {json_null, atom()} |
               boolean_opt(gen_mergers) |
               boolean_opt(gen_introspect) |
               boolean_opt(ignore_wellknown_types_directory) |
               term().

-type renaming() :: {pkg_name, name_change()} |
                    {msg_name, msg_name_change()} |
                    {msg_fqname, msg_name_change()} |
                    {group_name, name_change()} |
                    {group_fqname, name_change()} |
                    {service_name, name_change()} |
                    {service_fqname, name_change()} |
                    {rpc_name, name_change()} |
                    {msg_typename, name_change()} |
                    {enum_typename, name_change()}.

-type name_change() :: {prefix, string() | atom()} |
                       {suffix, string() | atom()} |
                       lowercase |
                       snake_case |
                       dots_to_underscores |
                       base_name.

-type msg_name_change() :: name_change() |
                           {prefix, {by_proto, prefix_by_proto()}}.

-type prefix_by_proto() :: [{ProtoName::atom(), Prefix::string() | atom()}].


-type field_path() :: [atom() | []].
-type translation() :: {encode, mod_fn_argtemplate()} |
                       {decode, mod_fn_argtemplate()} |
                       {decode_init_default, mod_fn_argtemplate()} |
                       {decode_repeated_add_elem, mod_fn_argtemplate()} |
                       {decode_repeated_finalize, mod_fn_argtemplate()} |
                       {merge,  mod_fn_argtemplate()} |
                       {verify, mod_fn_argtemplate()} |
                       {type_spec, string()}.
-type fn_name() :: atom().
-type mod_fn_argtemplate() :: {module(), fn_name(), arg_template()}.
-type arg_template() :: [arg()].
-type arg() :: term() | named_arg().
-type named_arg() :: '$1' | '$2' | '$errorf' | '$user_data' | '$op'.

-type fetcher_ret() :: from_file | {ok, string()} | {error, term()}.
-type import_fetcher_fun() :: fun((string()) -> fetcher_ret()).

-type json_format() :: jsx | mochijson2 | jiffy | maps.
%% Convenience shorthand to specify object, key, array and string and null
%% format.
-type json_object_format() :: eep18 | {proplist} | {atom(), proplist} | map.
%% <ul>
%%   <li>`eep18' means objects on format `[{}] | proplist()', such as
%%       for instance for jsx.</li>
%%   <li>`{proplist}' means a `proplist()' in a tuple.</li>
%%   <li>`{atom(),proplist}' means a `proplist()' in a tagged tuple,
%%       such as `{struct, proplist()}' for instance for mochijson2.</li>
%%   <li>`map' means as a map</li>
%% </ul>
-type json_key_format() :: atom | binary | string.
-type json_array_format() :: list | {atom(), list}.
%% A list or a list in a tagged tuple.
-type json_string_format() :: binary | list.

%% Compilation return values
-type comp_ret() :: mod_ret() | bin_ret() | defs_ret() | error_ret().
-type mod_ret() :: ok | {ok, [warning()]}.
-type bin_ret() :: {ok, module(), code()} |
                   {ok, module(), code(), [warning()]}.
-type defs_ret() :: {ok, gpb_defs:defs()} |
                    {ok, gpb_defs:defs(), [warning()]}.
-type error_ret() :: error | {error, reason()} | {error, reason(), [warning()]}.
-type warning() :: term().
-type reason() :: term().
-type code() :: binary() | gpb_defs:defs() | [code_item()].
-type code_item() :: {erl, ErlCode :: binary()} |
                     {nif, NifCcText :: string()}.
-export_type([opts/0, opt/0]).
-export_type([comp_ret/0]).

-ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R:St ->).
-else. % -ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R -> St = erlang:get_stacktrace(),).
-endif. % -ifdef(OTP_RELEASE).

%% @equiv file(File, [])
-spec file(string()) -> comp_ret().
file(File) ->
    file(File, []).

%% @doc
%% Compile a .proto file to a .erl file and to a .hrl file.
%%
%% The `File' argument must not include path to the .proto file. Example:
%% "SomeDefinitions.proto" is ok, while "/path/to/SomeDefinitions.proto"
%% is not ok.
%%
%% <a id="option-i"/>
%% The .proto file is expected to be found in a directories specified by an
%% `{i,directory()}' option. It is possible to specify `{i,directory()}'
%% several times, they will be searched in the order specified.
%%
%% <a id="option-type_specs"/>
%% The `type_specs' option enables or disables `::Type()' annotations
%% in the generated .hrl file. Default is `true'. The default changed
%% in gpb version 4.0.0. Previously, the default was `false'.
%% If you have messages referencing other messages cyclically, and get into
%% troubles when compiling the generated files, set this to `false'.
%%
%% <a id="option-verify"/>
%% The `verify' option specifies whether or not to generate code
%% that verifies, during encoding, that values are of correct type and
%% within range.  The `verify' option can have the following values:
%% <dl>
%%    <dt>`always'</dt><dd>Generate code that unconditionally
%%        verifies values.</dd>
%%    <dt>`never'</dt><dd>Generate code that never verifies
%%        values time. Encoding will fail if a value of the wrong
%%        type is supplied. This includes forgetting to set a required
%%        message field. Encoding may silently truncate values out of
%%        range for some types.</dd>
%%    <dt>`optionally'</dt><dd>Generate an `encode_msg/2' that accepts
%%        the run-time option `verify' or `{verify,boolean()}' for specifying
%%        whether or not to verify values.</dd>
%% </dl>
%%
%% Erlang value verification either succeeds or crashes with the `error'
%% `{gpb_type_error,Reason}'. Regardless of the `verify' option,
%% a function, `verify_msg/1' is always generated.
%%
%% <a id="option-copy_bytes"/>
%% The `copy_bytes' option specifies whether when decoding data of
%% type `bytes' (or strings if the `strings_as_binaries' is set), the
%% decoded bytes should be copied or not.  Copying requires the
%% `binary' module, which first appeared in Erlang R14A. When not
%% copying decoded bytes, they will become sub binaries of the larger
%% input message binary. This may tie up the memory in the input
%% message binary longer than necessary after it has been
%% decoded. Copying the decoded bytes will avoid creating sub
%% binaries, which will in turn make it possible to free the input message
%% binary earlier. The `copy_bytes' option can have the following values:
%% <dl>
%%   <dt>`false'</dt><dd>Never copy bytes/(sub-)binaries.</dd>
%%   <dt>`true'</dt><dd>Always copy bytes/(sub-)binaries.</dd>
%%   <dt>`auto'</dt><dd>Copy bytes/(sub-)binaries if the beam vm,
%%           on which the compiler (this module) is running,
%%           has the `binary:copy/1' function. (This is the default)</dd>
%%   <dt>integer() | float()</dt><dd>Copy the bytes/(sub-)binaries if the
%%           message this many times or more larger than the size of the
%%           bytes/(sub-)binary.</dd>
%% </dl>
%%
%% <a id="option-strings_as_binaries"/>
%% The `strings_as_binaries' option specifies whether strings should
%% be returned from decoding as strings (list of Unicode code points),
%% or as binaries (UTF-8 encoded). The `copy_bytes' option applies
%% to strings as well, when the `strings_as_binaries' option is set.
%% Upon encoding, both binaries and iolists are accepted.
%%
%% <a id="option-defs_as_proplists"/>
%% The `defs_as_proplists' option changes the generated introspection
%% functions `find_msg_def' and `get_msg_defs' to return the description
%% of each message field as a proplist, instead of as a `#field{}' record.
%% The purpose is to make the generated code completely independent
%% of gpb, at compile-time (it is already independent at run-time).
%% The keys of the proplist are the names of the record fields in the
%% `#field{}' record.  See also {@link gpb:proplists_to_field_records()}
%% and related functions for conversion functions between these two
%% formats.
%%
%% <a id="option-descriptor"/>
%% The `descriptor' option specifies whether or not to generate a
%% function, descriptor/0, which returns a binary that describes the
%% proto file(s) contents according to the protobuf's `descriptor.proto'.
%% The default is to not generate such a description.  The generated
%% description binary is most likely not identical to what `protoc'
%% would generate, but the contents is roughly equivalent.
%%
%% <a id="option-o"/>
%% The `{o,directory()}' option specifies directory to use for storing
%% the generated `.erl' and `.hrl' files. Default is the same
%% directory as for the proto `File'.
%%
%% <a id="option-o_erl"/>
%% <a id="option-o_hrl"/>
%% <a id="option-o_nif_cc"/>
%% The `{o_erl,directory()}', `{o_hrl,directory()}', `{o_nif_cc,directory()}',
%% options specify output directories for where to generate the `.erl'
%% and `.hrl' files respectively, and for the NIF C++ file,
%% if the `nif' option is specified. The `{o_erl,directory()}' option
%% overrides any `{o,directory()}' option, and similarly for the
%% other file-type specific output options.
%%
%% <a id="option-maps"/>
%% <a id="option-msgs_as_maps"/>
%% <a id="option-defs_as_maps"/>
%% <a id="option-mapfields_as_maps"/>
%% The `maps' option will generate a protobuf encoder/decoder that
%% uses maps instead of records. This option expands to the following
%% options:
%% <dl>
%%    <dt>`msgs_as_maps'</dt>
%%    <dd>No `.hrl' file will be generated, and the functions
%%        `encode_msg', `merge_msgs' and `verify_msg' will take the
%%        message name as an additional parameter.</dd>
%%    <dt>`mapfields_as_maps'</dt>
%%    <dd>The value for fields of type `map<_,_>' will be a map
%%        instead of a list of 2-tuples.</dd>
%%    <dt>`defs_as_maps'</dt>
%%    <dd>The introspection will generate message field descriptions
%%        as maps instead of as `#field{}' records, unless, of course
%%        `defs_as_proplists' is specified, in which case they will be
%%        proplists instead.</dd>
%% </dl>
%%
%% <a id="option-maps_unset_optional"/>
%% For messages as maps, for optional fields, if not set, the
%% `maps_unset_optional' option specifies the Erlang-internal
%% representation; both how it is expected to be found at encoding,
%% and how decoding will return it, for `proto2' syntax:
%% <dl>
%%   <dt>`omitted'</dt>
%%   <dd>This means it is not included in the map.
%%       This is the default. (since gpb version 4.0.0)
%%   </dd>
%%   <dt>`present_undefined'</dt>
%%   <dd>This means it is present and has the value `undefined'.
%%       This <em>was</em> the default before gpb version 4.0.0.
%%   </dd>
%% </dl>
%% For `proto3' syntax, the scene is a bit different. In proto3 all
%% fields are kind-of optional, but omitted scalar fields, strings and
%% bytes decode to their type-default. On encoding with proto3, a field
%% that has its type-default value is not included in the encoded
%% binary, so one could say that even though all fields are optional,
%% in a conceptual way, all scalars, strings and bytes always have a value.
%% The exceptions are sub-messages and `oneof' fields, and for these
%% fields, this option has the meaning as indicated above.
%%
%% <a id="option-maps_oneof"/>
%% The `maps_oneof' option can be used for messages as maps, and can only
%% take effect if `maps_unset_optional' is `omitted' (default since 4.0.0).
%% It changes the representation of oneof fields as described below, if
%% we would have a oneof-field, `xf' with two alternatives `a1' and `a2':
%% <dl>
%%   <dt>`{maps_oneof,tuples}'</dt>
%%   <dd>`#{xf => {a1, Value}}' or `#{xf => {a2, Value}}'</dd>
%%   <dt>`{maps_oneof,flat}'</dt>
%%   <dd>`#{a1 => Value}' or `#{a2 => Value}'</dd>
%% </dl>
%%
%% <a id="option-maps_key_type"/>
%% For messages as maps, the `maps_key_type' option makes it possible
%% to control whether keys should be atoms (default) or binaries.
%%
%% <a id="option-nif"/>
%% The `nif' option will cause the compiler to generate nif C++ code
%% for encoding and decoding. The generated nif C++ code can be linked
%% with the Google protobuf C++ library.  Read the file
%% `README.nif-cc' for more info.
%%
%% <a id="option-binary"/>
%% The `binary' option will cause the generated and compiled code to be
%% returned as a binary. No files will be written. The return value
%% will be on the form `{ok,Mod,Code}' or `{ok,Mod,Code,Warnings}'
%% if the compilation is successful. This option may be useful
%% e.g. when generating test cases. In case the `nif' option is set,
%% the `Code' will be a list of tuples: `{erl,binary()}' which
%% contains the Erlang object byte code, and `{nif,binary()}' which
%% contains the C++ code. You will have to compile the C++ code with a
%% C++ compiler, before you can use the Erlang code.
%%
%% <a id="option-to_proto_defs"/>
%% The `to_proto_defs' option will result in `{ok,Defs}' or
%% `{ok,Defs,Warns}' being returned if the compilation is successful.
%% The returned message definitions can be used with the
%% {@link proto_defs/2} or {@link proto_defs/3} functions.
%%
%% <a id="option-to_msg_defs"/>
%% The `to_msg_defs' option is a deprecated alias for `to_proto_defs'.
%%
%% <a id="option-report"/>
%% <a id="option-report_errors"/>
%% <a id="option-report_warnings"/>
%% <a id="option-return"/>
%% <a id="option-return_errors"/>
%% <a id="option-return_warnings"/>
%% <dl>
%%   <dt>`report_errors'/`report_warnings'</dt>
%%   <dd>Causes errors/warnings to be printed as they occur.</dd>
%%   <dt>`report'</dt>
%%   <dd>This is a short form for both `report_errors' and
%%       `report_warnings'.</dd>
%%   <dt>`return_errors'</dt>
%%   <dd>If this flag is set, then  `{error,ErrorList,WarningList}' is
%%       returned when there are errors.</dd>
%%   <dt>`return_warnings'</dt>
%%   <dd>If  this  flag  is set, then an extra field containing `WarningList'
%%       is added to the tuples returned on success.</dd>
%%   <dt>`return'</dt>
%%   <dd>This is a short form for both `return_errors' and
%%       `return_warnings'.</dd>
%% </dl>
%%
%% <a id="option-warnings_as_errors"/>
%% Setting the `warnings_as_errors' option will cause warnings to be
%% treated as errors.  If there are warnings but no errors, and
%% `return_warnings' is not specified, then `error' will be returned.
%%
%% See {@link format_error/1} for a way to turn an error <i>Reason</i> to
%% plain text.
%%
%% <a id="option-include_as_lib"/>
%% If the `include_as_lib' option is set, the generated code will include
%% gpb.hrl as a library, which is necessary if dependencies are managed with
%% Rebar. Otherwise, the header file is included directly and must be located
%% in the path, which is default behavior.
%%
%% <a id="option-use_packages"/>
%% The `use_packages' option instructs gpb to prepend the name of a package
%% to every message it contains. If no package is defined, nothing will be
%% prepended. This enables the reference of messages in other packages which
%% would otherwise not be possible. However, for reasons of backward
%% compatibility, this option is disabled by default.
%%
%% <a id="option-erlc_compile_options"/>
%% If the the `{erlc_compile_options,string()}' option is set,
%% then the genereted code will contain a directive `-compile([String]).'
%%
%% <a id="option-rename"/>
%% The `{rename,{What,How}}' can transform message names, package names,
%% service and rpc names in various ways. This option supersedes the
%% options `{msg_name_prefix,Prefix}', `{msg_name_suffix,Suffix}',
%% `msg_name_to_lower' and `msg_name_to_snake_case', while at the same
%% time giving more fine-grained control. It is for example possible to
%% apply snake_casing only to the message name, while keeping the
%% package name, the service name and the rpc name intact. This can be
%% useful with grpc, where these name components are exposed. The
%% `msg_fqname' refers to the fully qualified message name, as in
%% `Package.MsgName', while the `msg_name' refers to just the message
%% name without package. The `service_fqname' and `service_name' specifiers
%% work analogously. The `enum_typename' and `msg_typename' operate on
%% any enum or msg renamings already applied.
%%
%% It is possible to stack `rename' options, and they will be applied in
%% the order they are specified. So it is for example possible to
%% snake_case a name, and then also prefix it.
%%
%% <a id="option-msg_name_prefix"/>
%% <a id="option-msg_name_suffix"/>
%% The `{msg_name_prefix,Prefix}' will add `Prefix' (a string or an atom)
%% to each message. This might be useful for resolving colliding names,
%% when incorporating several protocol buffer definitions into the same
%% project. The `{msg_name_suffix,Suffix}' works correspondingly.
%%
%% The `{msg_name_prefix,Prefix}' option expands
%% to `[{rename,{pkg_name,Prefix}},{rename,{msg_fqname,{prefix,Prefix}}},
%% {rename,{group_fqname,{prefix,Prefix}}}}]',
%% and ditto for suffixes.
%%
%% For backwards compatibility, the `{msg_name_prefix,{by_proto,PrefixList}}'
%% expands to just `[{rename,{msg_fqname,{prefix,PrefixList}}}]'.
%%
%% <a id="option-msg_name_to_lower"/>
%% <a id="option-msg_name_to_snake_case"/>
%% The `msg_name_to_lower' and `msg_name_to_snake_case' options expands
%% to `[{rename,{pkg_name,X}},{rename,{service_fqname,X}},
%% {rename,{rpc_name,X}},{rename,{msg_fqname,X}},
%% {rename,{rpc_name,X}},{rename,{group_fqname,X}}]' where `X' is
%% `lowercase' or `snake_case' respectively.
%%
%% <a id="option-module_name_prefix"/>
%% <a id="option-module_name_suffix"/>
%% The `{module_name_prefix,Prefix}' will add `Prefix' (a string or an atom)
%% to the generated code and definition files. The `{module_name_suffix,Suffix}'
%% works correspondingly. For the case of compatibility with Erlang Protobuffs,
%% the `epb_compatibility' option implies `{module_name_suffix,"_pb"}'
%%
%% <a id="option-module_name"/>
%% The `{module_name,Name}' can be used to specify the module name of the
%% generated code freely, instead of basing it on the proto file name.
%% The name specified with `module_name' can be prefixed and suffixed with
%% the `module_name_prefix' and `module_name_suffix' options.
%%
%% <a id="option-translate_type"/>
%% The `translate_type' option can be used to provide packer and unpacker
%% functions for message fields of a certain type.
%% For messages, the `MsgName' refers to a name <em>after</em>
%% renaming has taken place.
%% The merge translator is optional, and is called either via the `merge_msgs'
%% function in the generated code, or when the decoder sees another
%% field of the same type. The default merge operation is to let the second
%% element overwrite previous elements. The verify translator is
%% optional too, since verification can be disabled.
%% The translation calls are specified as `{Mod,Fn,ArgTemplate}' where
%% `Mod',`Fn' is a module and function to call, `ArgTemplate' is a list
%% of terms, containing markers, such as `$1', `$2' and so on, for where
%% to place the actual args. This makes it possible to specify additional
%% static argument terms, for instance.
%% The translator functions are called as follows:
%% <dl>
%%   <dt>Encode (Packing)</dt>
%%   <dd>Call `Mod:Fn(Term)' to pack the `Term' (`$1') to
%%       a value of the suitable for normal gpb encoding.</dd>
%%   <dt>Decode (Unpacking)</dt>
%%   <dd>Call `Mod:Fn(Any)' to unpack the `Any' (`$1') to
%%       unpack a normal gpb decoded value to a term.</dd>
%%   <dt>Merge</dt>
%%   <dd>Call `Mod:Fn(Term1, Term2) -> Term3' to merge two
%%       unpacked terms to a resulting Term3. The `$1' is the
%%       previously seen term (during decoding, on encountering a
%%       second field of the same type), or the first argument to the
%%       `merge_msgs' function. The `$2' is the lastly seen term, or
%%       the second argument to the `merge_msgs' function.</dd>
%%   <dt>Verify</dt>
%%   <dd>Call `Mod:Fn(Term) -> _' to verify an unpacked `Term'.
%%       If `Term' (`$1') is valid, the function is expected to just return
%%       any value, which is ignored and discarded.
%%       If `Term' is invalid, the function is exptected to not
%%       return anything, but instead either crash, call
%%       `erlang:error/1', or `throw/1' or `exit/1'.  with the
%%       reason for error.
%%       (For backwards compatibility, it is also possible
%%       to have an error function as argument, using `$errorf',
%%       but this is deprecated.)</dd>
%% </dl>
%% There are additional translator argument markers:
%% <dl>
%%   <dt>`$user_data'</dt>
%%   <dd>This will be replaced by the `user_data' option to the
%%     generated `encode_msg', `decode_msg', `merge_msgs' and
%%     `verify_msg' functions. If that option is not specified, the
%%     value `undefined' is used substituted for `$user_data'.</dd>
%%   <dt>`$op'</dt>
%%   <dd>This will be replaced by `encode', `decode', `merge',
%%   `verify', `decode_init_default', `decode_repeated_add_elem' or
%%   `decode_repeated_finalize', depending on from which context it
%%   is actually called. This can be useful because if the message is
%%   to be verified on encoding (see the `verify' option), then the
%%   same options, and thus the same user-data, are used for both
%%   `encode_msg' and for `verify_msg'. The `$op' marker makes it
%%   possible to tell these two call sites apart, if needed.</dd>
%% </dl>
%%
%% <a id="option-any_translate"/>
%% The option `{any_translate,Translations}' is retained for backwards
%% compatibility, and expands to
%% <code>{translate_type,{'google.protobuf.Any',Translations}}</code>.
%%
%% <a id="option-translate_field"/>
%% The `translate_field' option can be used to translate individual fields.
%% The option format is `{translate_field,{FieldPath,Translations}}' where
%% each `Translation' consists of `{Op,{Mod,Fn,ArgTemplate}}' elements,
%% just as for `translate_type'. The `FieldPath' is a list on the
%% following format:
%% <ul>
%%   <li>`[MsgName]' for the message itself on the top-level</li>
%%   <li>`[MsgName,FieldName]' for fields, generally</li>
%%   <li>`[MsgName,FieldName,[]]' for elements of repeated fields</li>
%%   <li>`[MsgName,OneofFieldName,FieldName]' for elements of oneof
%%     fields.</li>
%% </ul>
%% For repeated fields, the additional operations `decode_init_default',
%% `decode_repeated_add_elem' and `decode_repeated_finalize' also exist
%% and must all be specified.
%%
%% For translated proto3 message fields -- ie fields for messages in files
%% with `syntax="proto3";' -- the `decode' callback will be invoked also
%% initially when the decoding of the message binary is about to start.
%% This holds for non-repeated non-oneof fields of integer types, enums,
%% strings and bytes. The `decode' callback will be invoked with the type's
%% default value. This is because in proto3, a field with the type's default
%% value is never included in the resulting wire octets, so on decoding,
%% gpb initially assumes such fields have the type's default value,
%% and the translator needs to be invoked accordingly.
%%
%% <a id="option-epb_compatibility"/>
%% <a id="option-epb_functions"/>
%% The `epb_compatibility' option is an umbrella-option for
%% compatibility with the Erlang protobuffs library. It will expand to
%% the options below. It will expand in-place, meaning any of these
%% can be overridden if specified before the `epb_compatibility'
%% option.
%% <ul>
%%   <li>`epb_functions'</li>
%%   <li>`defaults_for_omitted_optionals'</li>
%%   <li>`{module_name_suffix,"_pb"}'</li>
%%   <li>`{msg_name_to_lower,true}'</li>
%% </ul>
%%
%% If the `epb_functions' option is specified, then for compatibility
%% with Erlang protobuffs, the following functions will be generated:
%% <ul>
%%   <li>`encode/1'</li>
%%   <li>`encode_<MsgName>/1'</li>
%%   <li>`decode/2'</li>
%%   <li>`decode_<MsgName>/1'</li>
%% </ul>
%%
%% <a id="option-defaults_for_omitted_optionals"/>
%% <a id="option-type_defaults_for_omitted_optionals"/>
%% The `defaults_for_omitted_optionals' and
%% `type_defaults_for_omitted_optionals' options generates code that
%% set default values or type-defaults respectively, on decoding, if
%% an optional field is not present in the binary to decode. Normally
%% it would otherwise have been set to `undefined'. Note that with
%% these options it is not possible to determine after decoding
%% whether a field contained data in the binary message. Also note
%% that these options are only applicable for proto2 syntax messages,
%% and are ignored for proto3 syntax messages. (For proto3, it
%% effectively <em>must</em> be ignored, since, on the wire, a field
%% set to its type-default value is indistinguishable from an omitted
%% value.)
%%
%% <a id="option-import_fetcher"/>
%% The `import_fetcher' option can be used to catch imports. The
%% option value must be a function taking one argument, the name of
%% the file to import. It must return either `from_file', letting this
%% file pass through the normal file import, or `{ok,string()}' if it
%% has fetched the file itself, or `{error,term()}'.
%%
%% <a id="option-target_erlang_version"/>
%% The `target_erlang_version' can be used to specify another major
%% version of Erlang/OTP to generate code for. The default, `current'
%% means that the generated code is expected to be compiled and run
%% on the same major version as gpb runs on.
%%
%% <a id="option-bypass_wrappers"/>
%% The `bypass_wrappers' option exposes the more-or-less internal
%% top-level encode and decode functions without wrappers. The list
%% below describe what functionality the wrappers provide. The main
%% purpose of being able to bypass the wrappers is performance,
%% especially when combined with the `nif' option. This option causes the following extra functions to be exported:
%% <ul>
%%   <li><code>encode_msg_<i>MsgName</i>/1</code></li>
%%   <li><code>encode_msg_<i>MsgName</i>/2</code> unless `nif'</li>
%%   <li><code>decode_msg_<i>MsgName</i>/1</code></li>
%%   <li><code>decode_msg_<i>MsgName</i>/2</code> unless `nif'</li>
%% </ul>
%% <dl>
%%   <dt>For encode, the wrapper takes care of:</dt>
%%   <dd><ul>
%%     <li>Any calling of verifiers.</li>
%%     <li>Reading of options for any translation `user_data'.</li>
%%   </ul></dd>
%%   <dt>For decode, the wrapper takes care of:</dt>
%%   <dd><ul>
%%     <li>Wrapping the decode in a try catch to provide a uniform error
%%       format when the binary to be decoded is invalid.</li>
%%     <li>Reading of options for any translation `user_data'.</li>
%%   </ul></dd>
%% </dl>
%%
%% <a id="option-json"/>
%% The `json' option will cause gpb to also generate functions for
%% converting between internal format and a JSON representation.
%%
%% Note that gpb will not encode to an actual JSON text.
%% Instead, it returns an erlang structure that can be used with some other
%% JSON library to turn it into actual JSON text. Ditto for decoding.
%% It is possible to flexibly specify details of the JSON representation,
%% with shorthand presets for some common libraries.
%%
%% However, with the `nif' option, the generated code uses the Google
%% C++ protobuf library, which produces already-formatted JSON text, as
%% binaries, with no further processing required. When `nif' is
%% specified, the various JSON format options are thus not used.
%% The `json_always_print_primitive_fields', the
%% `json_preserve_proto_field_names' and the
%% `json_case_insensitive_enum_parsing' options are honoured with `nif',
%% though.
%%
%% <a id="option-json_always_print_primitive_fields"/>
%% The `json_always_print_primitive_fields' makes the generated
%% `to_json' function always emit json key-value items also when the
%% value is the type's default value.  The default is to omit such
%% values, as per the language guide.  This holds for messages in files
%% with proto3 syntax.
%%
%% <a id="option-json_preserve_proto_field_names"/>
%% The `json_preserve_proto_field_names' makes the generated `to_json'
%% function always use the field name in the `.proto' file. The default
%% is to use lowerCamelCase, as per the language guide.
%%
%% <a id="option-json_case_insensitive_enum_parsing"/>
%% If the the `json_case_insensitive_enum_parsing' option is specified,
%% case is not significant when parsing json enums. Also, dashes instead
%% of underscores are allowed. Default is that that case <em>is</em>
%% significant.
%%
%% <a id="option-json_format"/>
%% The `{json_format,Format}' option is a convenience shorthand, and will expand
%% as indicated below. If the json_format is not specified, it defaults to
%% `map' if the `maps' option is specified, and otherwise to `eep18' when
%% generating code for records.
%% <dl>
%%   <dt>jsx</dt>
%%   <dd><code>[{json_object_format, eep18},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%%   <dt>mochijson2</dt>
%%   <dd><code>[{json_object_format, {struct, proplist}},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%%   <dt>jiffy</dt>
%%   <dd><code>[{json_object_format, {proplist}},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%%   <dt>map</dt>
%%   <dd><code>[{json_object_format, map},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%% </dl>
%%
%% <a id="option-json_object_format"/>
%% The `{json_object_format,Format}' option specifies the format
%% of json object, as indicated below. (Note that the format of the keys
%% is specified by the `json_object_key' option, see further below.)
%% <dl>
%%   <dt>`eep18'</dt>
%%   <dd>The empty json object is represented as `[{}]'.<br/>
%%       Non-empty json objects are represented as proplists.</dd>
%%   <dt>`{proplist}'</dt>
%%   <dd>A json object is represented as a proplist in a tuple.</dd>
%%   <dt>`{atom(), proplist}'</dt>
%%   <dd>A json object is represented as a proplist in a tagged tuple,
%%       with the possibility to specify the tag.</dd>
%%   <dt>`map'</dt>
%%   <dd>The json object is represented as an Erlang map.</dd>
%% </dl>
%%
%% <a id="option-json_key_format"/>
%% The `{json_key_format,Format}' option specifies the format
%% of json object keys, as follows:
%% <ul>
%%   <li>`atom'</li>
%%   <li>`binary' (default)</li>
%%   <li>`string'</li>
%% </ul>
%%
%% <a id="option-json_array_format"/>
%% The `{json_array_format,Format}' option specifies the format
%% of json arrays, as follows:
%% <ul>
%%   <li>`list' (default)</li>
%%   <li>`{atom(), list}' A list in a tagged tuple, with the possibility
%%       to specify the tag.</li>
%% </ul>
%%
%% <a id="option-json_string_format"/>
%% The `{json_string_format,Format}' option specifies the format
%% of json arrays, as follows:
%% <ul>
%%   <li>`list'</li>
%%   <li>`binary' (default)</li>
%% </ul>
%%
%% <a id="option-json_null"/>
%% The `{json_null,atom()}' option specifies the atom to use
%% for the JSON `null' value. The default is to use the atom `null'.
%%
%% <a id="option-gen_mergers"/>
%% The `{gen_mergers,false}' option will cause gpb to not generate code for
%% merging of messages. This is only useful with the option `nif'. One
%% rationale for this is option is to reduce the size of the generated code.
%%
%% <a id="option-gen_introspect"/>
%% The `{gen_introspect,false}' option will cause gpb to not generate code
%% for introspection. One rationale for this is option is to reduce the size of
%% the generated code.
%%
%% <a id="option-ignore_wellknown_types_directory"/>
%% The `{ignore_wellknown_types_directory, true}' option will stop gpb from
%% looking rom a well known types directory by trying to locate the `priv'
%% directory of the `gpb' application. This can be used either when this
%% directory is not available or to provide a custom set of well known types.

-spec file(string(), opts()) -> comp_ret().
file(File, Opts) ->
    do_file_or_string(File, Opts).

%% @equiv string(Mod, Str, [])
-spec string(module(), string()) -> comp_ret().
string(Mod, Str) ->
    string(Mod, Str, []).

%% @doc
%% Compile a `.proto' file as string. See {@link file/2} for information
%% on options and return values.
-spec string(module(), string(), opts()) -> comp_ret().
string(Mod, Str, Opts) ->
    do_file_or_string({Mod, Str}, Opts).

do_file_or_string(In, Opts0) ->
    Opts1 = normalize_opts(Opts0),
    case parse_file_or_string(In, Opts1) of
        {ok, Defs, Sources} ->
            case gpb_names:compute_renamings(Defs, Opts1) of
                {ok, Renamings} ->
                    Defs1 = gpb_names:apply_renamings(Defs, Renamings),
                    Mod = find_out_mod(In, Opts1),
                    DefaultOutDir = find_default_out_dir(In),
                    Opts2 = Opts1 ++ [{o,DefaultOutDir}],
                    do_proto_defs_aux1(Mod, Defs1, Defs, Sources, Renamings,
                                       Opts2);
                {error, Reason} = Error ->
                    possibly_report_error(Error, Opts1),
                    case proplists:get_bool(return_warnings, Opts1) of
                        true  -> {error, Reason, []};
                        false -> Error
                    end
            end;
        {error, Reason} = Error ->
            possibly_report_error(Error, Opts1),
            case proplists:get_bool(return_warnings, Opts1) of
                true  -> {error, Reason, []};
                false -> Error
            end
    end.

normalize_opts(Opts0) ->
    normalize_return_report_opts(
      normalize_alias_opts(Opts0)).

normalize_alias_opts(Opts) ->
    lists:foldl(fun(F, OptsAcc) -> F(OptsAcc) end,
                Opts,
                [fun norm_opt_alias_to_msg_proto_defs/1,
                 fun norm_opt_epb_compat_opt/1,
                 fun norm_opt_map_opts/1,
                 fun norm_opt_any_translate/1,
                 fun norm_opt_json_format/1]).

norm_opt_alias_to_msg_proto_defs(Opts) ->
    lists:map(fun(to_msg_defs)         -> to_proto_defs;
                 ({to_msg_defs, Bool}) -> {to_proto_defs, Bool};
                 (Opt)                 -> Opt
              end,
              Opts).

norm_opt_epb_compat_opt(Opts) ->
    proplists:expand(
      [{epb_compatibility, [epb_functions,
                            defaults_for_omitted_optionals,
                            {module_name_suffix,"_pb"},
                            {msg_name_to_lower, true}]},
       {{epb_compatibility,false}, [{epb_functions,false},
                                    {defaults_for_omitted_optionals,false}]}],
      Opts).

norm_opt_map_opts(Opts) ->
    proplists:expand(
      [{maps, [msgs_as_maps,
               mapfields_as_maps,
               defs_as_maps]},
       {{maps,false}, [{msgs_as_maps, false},
                       {mapfields_as_maps, false},
                       {defs_as_maps, false}]}],
      Opts).

norm_opt_any_translate(Opts) ->
    AnyType = {msg, 'google.protobuf.Any'},
    lists:map(fun({any_translate, Transls}) ->
                      {translate_type, {AnyType, Transls}};
                 (Opt) ->
                      Opt
              end,
              Opts).

norm_opt_json_format(Opts) ->
    proplists:expand(
      [{{json_format, jsx},        [{json_object_format, eep18},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, mochijson2}, [{json_object_format, {struct, proplist}},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, jiffy},      [{json_object_format, {proplist}},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, maps},       [{json_object_format, map},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]}],
      Opts).

normalize_return_report_opts(Opts1) ->
    Opts2 = expand_opt(return, [return_warnings, return_errors], Opts1),
    Opts3 = expand_opt(report, [report_warnings, report_errors], Opts2),
    Opts4 = unless_defined_set(return_warnings, report_warnings, Opts3),
    Opts5 = unless_defined_set(return_errors,   report_errors, Opts4),
    Opts5.

expand_opt(OptionToTestFor, OptionsToExpandTo, Opts) ->
    lists:append(
      lists:map(fun(Opt) when Opt == OptionToTestFor -> OptionsToExpandTo;
                   (Opt) -> [Opt]
                end,
                Opts)).

unless_defined_set(OptionToTestFor, Default, Opts) ->
    case is_option_defined(OptionToTestFor, Opts) of
        true  -> Opts;
        false -> Opts ++ [Default]
    end.

is_option_defined(Key, Opts) ->
    lists:any(fun({K, _V}) -> K =:= Key;
                 (K)       -> K =:= Key
              end,
              Opts).

find_out_mod({Mod, _S}, _Opts) ->
    Mod;
find_out_mod(File, Opts) ->
    gpb_names:file_name_to_module_name(File, Opts).

find_default_out_dir({_Mod, _S}) -> ".";
find_default_out_dir(File) -> filename:dirname(File).

%% @equiv proto_defs(Mod, Defs, [])
-spec proto_defs(module(), gpb_defs:defs()) -> comp_ret().
proto_defs(Mod, Defs) ->
    proto_defs(Mod, Defs, []).

%% @doc
%% Compile a list of pre-parsed definitions to file or to a binary.
%% See {@link file/2} for information on options and return values.
-spec proto_defs(module(), gpb_defs:defs(), opts()) -> comp_ret().
proto_defs(Mod, Defs, Opts) ->
    proto_defs(Mod, Defs, Defs, no_renamings, Opts).

%% @doc
%% Compile a list of pre-parsed definitions to file or to a binary.
%% This is useful when there are renamings, and one nifs or descriptors
%% are to be generated, since these need the original definitions before
%% any renamings. The renaming must be applied separately, see
%% {@link gpb_names:compute_renamings/2} and
%% {@link gpb_names:apply_renamings/2}. See
%% {@link gpb_names:is_renaming_opt/1} for how to filter options.
%% See {@link file/2} for information on options and return values.
proto_defs(Mod, Defs, DefsNoRenamings, Renamings, Opts) ->
    Sources = [lists:concat([Mod, ".proto"])],
    Opts1 = normalize_opts(Opts),
    do_proto_defs_aux1(Mod, Defs, DefsNoRenamings, Sources, Renamings, Opts1).

do_proto_defs_aux1(Mod, Defs, DefsNoRenamings, Sources, Renamings, Opts) ->
    possibly_probe_defs(Defs, Opts),
    Warns0 = check_unpackables_marked_as_packed(Defs),
    Warns1 = check_maps_flat_oneof_may_fail_on_compilation(Opts),
    Warns = Warns0 ++ Warns1,
    AnRes = gpb_analyzer:analyze_defs(Defs, Sources, Renamings, Opts),
    case verify_opts(Defs, Opts) of
        ok ->
            Res1 = do_proto_defs_aux2(Defs, DefsNoRenamings,
                                      clean_module_name(Mod), AnRes, Opts),
            return_or_report_warnings_or_errors(Res1, Warns, Opts,
                                                get_output_format(Opts));
        {error, OptError} ->
            return_or_report_warnings_or_errors({error, OptError}, [], Opts,
                                                get_output_format(Opts))
    end.

verify_opts(Defs, Opts) ->
    while_ok([fun() -> verify_opts_translation_and_nif(Opts) end,
              fun() -> verify_opts_epb_compat(Defs, Opts) end,
              fun() -> verify_opts_flat_oneof(Opts) end,
              fun() -> verify_opts_no_gen_mergers(Opts) end]).

while_ok(Funs) ->
    lists:foldl(fun(F, ok) -> F();
                   (_, Err) -> Err
                end,
                ok,
                Funs).

verify_opts_translation_and_nif(Opts) ->
    TranslType = lists:keymember(translate_type, 1, Opts),
    TranslField = lists:keymember(translate_field, 1, Opts),
    DoNif = proplists:get_bool(nif, Opts),
    if (TranslType or TranslField) and DoNif ->
            {error, {invalid_options, translation, nif}};
       true ->
            ok
    end.

verify_opts_epb_compat(Defs, Opts) ->
    while_ok(
      [fun() ->
               case {proplists:get_bool(epb_functions, Opts),
                     gpb_lib:get_records_or_maps_by_opts(Opts)} of
                   {true, maps} ->
                       {error, {invalid_options, epb_functions,maps}};
                   _ ->
                       ok
               end
       end,
       fun() ->
               case proplists:get_bool(epb_functions, Opts) of
                   true ->
                       case lists:member(msg, gpb_lib:msg_names(Defs)) of
                           true ->
                               {error, {epb_functions_impossible,
                                        {with_msg_named,msg}}};
                           false ->
                               ok
                       end;
                   false ->
                       ok
               end
       end]).

verify_opts_flat_oneof(Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        #maps{oneof=flat} ->
            case gpb_lib:target_can_do_flat_oneof_for_maps(Opts) of
                true ->
                    ok;
                false -> {error, maps_flat_oneof_not_supported_for_target_version}
            end;
        _ ->
            ok
    end.

check_maps_flat_oneof_may_fail_on_compilation(Opts) ->
    CanFlatOneof = gpb_lib:target_can_do_flat_oneof_for_maps(Opts),
    MayFail = gpb_lib:target_may_fail_compilation_for_flat_oneof_for_maps(Opts),
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        #maps{oneof=flat} ->
            if CanFlatOneof, MayFail ->
                    [maps_flat_oneof_generated_code_may_fail_to_compile];
               not CanFlatOneof ->
                    []; % a later check will signal an error
               true ->
                    []
            end;
        _ ->
            []
    end.

verify_opts_no_gen_mergers(Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    GenMergers = proplists:get_value(gen_mergers, Opts),
    case {DoNif, GenMergers} of
        {_,     undefined} -> ok; % default for gen_mergers is true
        {_,     true} -> ok;
        {true,  false} -> ok;
        {false, false} -> {error, {invalid_options, nif, {gen_mergers,false}}}
    end.

%% @equiv msg_defs(Mod, Defs, [])
%% @doc Deprecated, use proto_defs/2 instead.
-spec msg_defs(module(), gpb_defs:defs()) -> comp_ret().
msg_defs(Mod, Defs) ->
    msg_defs(Mod, Defs, []).

%% @spec msg_defs(Mod, Defs, Opts) -> CompRet
%% @equiv proto_defs(Mod, Defs, Opts)
%% @doc Deprecated, use proto_defs/2 instead.
-spec msg_defs(module(), gpb_defs:defs(), opts()) -> comp_ret().
msg_defs(Mod, Defs, Opts) ->
    proto_defs(Mod, Defs, Opts).

do_proto_defs_aux2(Defs, DefsNoRenamings, Mod, AnRes, Opts) ->
    case get_output_format(Opts) of
        proto_defs ->
            {ok, Defs};
        binary ->
            ErlTxt = format_erl(Mod, Defs, DefsNoRenamings, AnRes, Opts),
            HrlTxt = possibly_format_hrl(Mod, Defs, AnRes, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            compile_to_binary(Mod, HrlTxt, ErlTxt, NifTxt, Opts);
        file ->
            ErlTxt = format_erl(Mod, Defs, DefsNoRenamings, AnRes, Opts),
            HrlTxt = possibly_format_hrl(Mod, Defs, AnRes, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            ErlOutDir = get_erl_outdir(Opts),
            HrlOutDir = get_hrl_outdir(Opts),
            NifCcOutDir = get_nif_cc_outdir(Opts),
            Erl   = filename:join(ErlOutDir, atom_to_list(Mod) ++ ".erl"),
            Hrl   = filename:join(HrlOutDir, atom_to_list(Mod) ++ ".hrl"),
            NifCc = filename:join(NifCcOutDir, atom_to_list(Mod) ++ ".nif.cc"),
            case {file_write_file(Erl, ErlTxt, Opts),
                  possibly_write_file(Hrl, HrlTxt, Opts),
                  possibly_write_file(NifCc, NifTxt, Opts)} of
                {ok, ok, ok}       -> ok;
                {{error, R}, _, _} -> {error, {write_failed, Erl, R}};
                {_, {error, R}, _} -> {error, {write_failed, Erl, R}};
                {_, _, {error, R}} -> {error, {write_failed, NifCc,  R}}
            end
    end.

return_or_report_warnings_or_errors(Res, ExtraWarns, Opts, OutFormat) ->
    Res2 = merge_warns(Res, ExtraWarns, OutFormat),
    possibly_report_warnings(Res2, Opts),
    possibly_report_error(Res2, Opts),
    return_warnings_or_errors(Res2, Opts).

merge_warns(ok, Warns, _OutFmt)                  -> {ok, Warns};
merge_warns({ok, Warns1}, Warns2, file)          -> {ok, Warns2++Warns1};
merge_warns({ok, Defs}, Warns, proto_defs)       -> {ok, Defs, Warns};
merge_warns({ok, M, B}, Warns, binary)           -> {ok, M, B, Warns};
merge_warns({ok, M, B, Warns1}, Warns2, binary)  -> {ok, M, B, Warns2++Warns1};
merge_warns({error, R}, Warns, _OutFmt)          -> {error, R, Warns};
merge_warns({error, R, Warns1}, Warns2, _OutFmt) -> {error, R, Warns2++Warns1};
merge_warns(error, Warns, binary) ->
    erlang:error({internal_error, ?MODULE,
                  generated_code_failed_to_compile, Warns}).

possibly_report_warnings(Result, Opts) ->
    Warns = case Result of
                {error, _Reason, Ws} -> Ws;
                {ok, _M, _B, Ws}     -> Ws;
                {ok, _Defs, Ws}      -> Ws;
                {ok, Ws}             -> Ws
            end,
    case proplists:get_bool(report_warnings, Opts) of
        true  -> lists:foreach(fun report_warning/1, Warns);
        false -> ok
    end.

report_warning(Warn) ->
    io:format("~s~n", [format_warning(Warn)]).

possibly_report_error(Res, Opts) ->
    case {Res, proplists:get_bool(report_errors, Opts)} of
        {{error, _Reason, _Warns}, true} ->
            io:format("~s~n", [format_error(Res)]);
        {{error, _Reason}, true} ->
            io:format("~s~n", [format_error(Res)]);
        _ ->
            ok
    end.

return_warnings_or_errors(Res, Opts) ->
    case proplists:get_bool(return_warnings, Opts) of
        true ->
            case proplists:get_bool(warnings_as_errors, Opts) of
                true  -> turn_warnings_to_errors_keep(Res);
                false -> Res
            end;
        false ->
            case proplists:get_bool(warnings_as_errors, Opts) of
                true  -> turn_warnings_to_errors_remove(Res);
                false -> remove_warnings_from_res(Res)
            end
    end.

turn_warnings_to_errors_keep({ok, _Mod, _Bin, []}=Res) -> Res;
turn_warnings_to_errors_keep({ok, _MsgDefs, []}=Res)   -> Res;
turn_warnings_to_errors_keep({ok, []}=Res)             -> Res;
turn_warnings_to_errors_keep({ok, _Mod, _Bin, Warns})  -> {error, [], Warns};
turn_warnings_to_errors_keep({ok, _MsgDefs, Warns})    -> {error, [], Warns};
turn_warnings_to_errors_keep({ok, Warns})              -> {error, [], Warns};
turn_warnings_to_errors_keep({error, R, Warns})        -> {error, R, Warns}.

turn_warnings_to_errors_remove({ok, Mod, Bin, []})       -> {ok, Mod, Bin};
turn_warnings_to_errors_remove({ok, MsgDefs, []})        -> {ok, MsgDefs};
turn_warnings_to_errors_remove({ok, []})                 -> ok;
turn_warnings_to_errors_remove({ok, _Mod, _Bin, _Warns}) -> error;
turn_warnings_to_errors_remove({ok, _MsgDefs, _Warns})   -> error;
turn_warnings_to_errors_remove({ok, _Warns})             -> error;
turn_warnings_to_errors_remove({error, R, _Warns})       -> {error, R}.

remove_warnings_from_res({ok, Mod, Bin, _Warns}) -> {ok, Mod, Bin};
remove_warnings_from_res({ok, MsgDefs, _Warns})  -> {ok, MsgDefs};
remove_warnings_from_res({ok, _Warns})           -> ok;
remove_warnings_from_res({error, R, _Warns})     -> {error, R}.

get_output_format([binary | _])                -> binary;
get_output_format([{binary, true} | _])        -> binary;
get_output_format([to_proto_defs | _])         -> proto_defs;
get_output_format([{to_proto_defs, true} | _]) -> proto_defs;
get_output_format([_ | Rest])                  -> get_output_format(Rest);
get_output_format([])                          -> file.

get_erl_outdir(Opts) ->
    proplists:get_value(o_erl, Opts, get_outdir(Opts)).

get_hrl_outdir(Opts) ->
    proplists:get_value(o_hrl, Opts, get_outdir(Opts)).

get_nif_cc_outdir(Opts) ->
    proplists:get_value(o_nif_cc, Opts, get_outdir(Opts)).

get_outdir(Opts) ->
    proplists:get_value(o, Opts, ".").

clean_module_name(Mod) ->
    Clean = re:replace(atom_to_list(Mod), "[.]", "_", [global, {return,list}]),
    list_to_atom(Clean).

%% @spec format_error({error, Reason} | Reason) -> io_list()
%%           Reason = term()
%%
%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link proto_defs/2}.
-spec format_error(Err) -> iolist() when
      Err :: reason() | {error, reason()} | {error, reason(), [warning()]}.
format_error({error, Reason, _Warns}) -> fmt_err(Reason);
format_error({error, Reason})         -> fmt_err(Reason);
format_error(Reason)                  -> fmt_err(Reason).

%% Note: do NOT include trailing newline (\n or ~n)
fmt_err({option_error, {not_supported, maps_omitted_nif}}) ->
    ?f("Options maps, maps_unset_optional=omitted and nif is not supported");
fmt_err({parse_error, FileName, {Line, Module, ErrInfo}}) ->
    ?f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)]);
fmt_err({scan_error, FileName, {Line, Module, ErrInfo}}) ->
    ?f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)]);
fmt_err({import_not_found, Import, Tried}) ->
    PrettyTried = [begin
                       PrettyReason = file:format_error(Reason),
                       ?f("~n  ~ts (~s (~p))", [File,PrettyReason,Reason])
                   end
                   || {File,Reason} <- Tried],
    TriedTxt = if Tried == [] -> "";
                  true -> ", tried:"
               end,
    ?f("Could not find import file ~p~s~s", [Import, TriedTxt, PrettyTried]);
fmt_err({fetcher_issue, File, Reason}) ->
    ?f("Failed to import file ~p using fetcher, ~p", [File, Reason]);
fmt_err({read_failed, File, Reason}) ->
    ?f("failed to read ~p: ~s (~p)", [File, file:format_error(Reason), Reason]);
fmt_err({post_process, Reasons}) ->
    gpb_defs:format_post_process_error({error, Reasons});
fmt_err({write_failed, File, Reason}) ->
    ?f("failed to write ~s: ~s (~p)", [File, file:format_error(Reason),Reason]);
fmt_err({invalid_options, translation, nif}) ->
    "Option error: Not supported: both translation option and nif";
fmt_err({unsupported_translation, _Type, non_msg_type}) ->
    "Option to translate is supported only for message types, for now";
fmt_err({invalid_options, epb_functions, maps}) ->
    "Option error: Not supported: both epb_compatibility (or epb_functions) "
        "and maps";
fmt_err({invalid_options, nif, {gen_mergers, false}}) ->
    "Option error: It is only possible to omit mergers with nif";
fmt_err({epb_compatibility_impossible, {with_msg_named, msg}}) ->
    "Not possible to generate epb compatible functions when a message "
        "is named 'msg' because of collision with the standard gpb functions "
        "'encode_msg' and 'decode_msg'";
fmt_err(maps_flat_oneof_not_supported_for_target_version) ->
    "Flat oneof for maps is only supported on Erlang 18 and later";
fmt_err({rename_defs, Reason}) ->
    gpb_names:format_error(Reason);
fmt_err(X) ->
    ?f("Unexpected error ~p", [X]).

%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link proto_defs/2}.
%% @end
%% Note: do NOT include trailing newline (\n or ~n)
-spec format_warning(warning()) -> iolist().
format_warning({ignored_field_opt_packed_for_unpackable_type,
                MsgName, FName, Type, _Opts}) ->
    ?f("Warning: ignoring option packed for non-packable field ~s.~s "
       "of type ~w", [MsgName, FName, Type]);
format_warning(maps_flat_oneof_generated_code_may_fail_to_compile) ->
    "Warning: Generated code for flat oneof for maps may fail to compile "
        "on 18.3.4.6, or later Erlang 18 versions, due to a compiler issue";
format_warning(X) ->
    case io_lib:deep_char_list(X) of
        true  -> X;
        false -> ?f("Warning: Unknown warning: ~p", [X])
    end.

%% @doc Command line interface for the compiler.
%% With no proto file to compile, print a help message and exit.
-spec c() -> no_return().
c() ->
    io:format("No proto files specified.~n"),
    show_help(),
    halt(0).

%% @doc This function is intended as a command line interface for the compiler.
%% Call it from the command line as follows:
%% ```
%%    erl <erlargs> [gpb-opts] -s gpb_compile c File.proto ...
%%    erl <erlargs> -s gpb_compile c File.proto ... -extra [gpb-opts]
%% '''
%% The `<erlargs>' can be `-noshell -noinput +B -boot start_clean -pa SomeDir'
%%
%% The options below are supported. Dashes and underscores inside option names
%% are equivalent, ie `-o-erl' and `-o_erl' are the same option.
%% <dl>
%%   <dt><a id="cmdline-option-I"/>
%%       `-IDir' `-I Dir'</dt>
%%   <dd>Specify include directory.
%%       Option may be specified more than once to specify
%%       several include directories.</dd>
%%   <dt><a id="cmdline-option-o"/>
%%       `-o Dir'</dt>
%%   <dd>Specify output directory for where to generate
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl</dd>
%%   <dt><a id="cmdline-option-o-erl"/>
%%       <a id="cmdline-option-o-hrl"/>
%%       <a id="cmdline-option-o-nif-cc"/>
%%       `-o-erl Dir' | `-o-hrl Dir' | `-o-nif-cc Dir'</dt>
%%   <dd>Specify output directory for where to generate
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl respectively,
%%       and for the NIF C++ file, if the `-nif' option is specified.
%%       The `-o-erl Dir' option overrides any `-o Dir' option, and
%%       similarly for the other file-type specific output options.</dd>
%%   <dt><a id="cmdline-option-v"/>
%%       `-v optionally | always | never'</dt>
%%   <dd>Specify how the generated encoder should
%%       verify the message to be encoded.</dd>
%%   <dt><a id="cmdline-option-nif"/>
%%       `-nif'</dt>
%%   <dd>Generate nifs for linking with the protobuf C(++) library.</dd>
%%   <dt><a id="cmdline-option-load_nif"/>
%%       `-load_nif FunctionDefinition'</dt>
%%   <dd>Specify `FunctionDefinition' as the text that defines the
%%       function `load_nif/0'.  This is called as the `on_load'
%%       hook for loading the NIF.  See also the doc for the `load_nif'
%%       option in the {@link file/2} function.</dd>
%%   <dt><a id="cmdline-option-c"/>
%%       `-c true | false | auto | integer() | float()'</dt>
%%   <dd>Specify how or when the generated decoder should
%%       copy fields of type `bytes'. See the `copy_bytes' option
%%       for the function {@link file/2} for more info.</dd>
%%   <dt><a id="cmdline-option-strbin"/>
%%       `-strbin'</dt>
%%   <dd>Specify that decoded strings should be returned as binaries,
%%       instead of as strings (lists).</dd>
%%   <dt><a id="cmdline-option-pldefs"/>
%%       `-pldefs'</dt>
%%   <dd>Specify that introspection functions shall return proplists
%%       instead of `#field{}' records, to make the generated code
%%       completely free of even compile-time dependencies to gpb.</dd>
%%   <dt><a id="cmdline-option-pkgs"/>
%%       `-pkgs'</dt>
%%   <dd>Prepend the name of a package to every message it contains.
%%       If no package is defined, nothing will be prepended.
%%       Default is to not prepend package names for backwards
%%       compatibility, but it is needed for some proto files.</dd>
%%   <dt><a id="cmdline-option-translate_type"/>
%%       `-translate_type TMsFs'</dt>
%%   <dd>Call functions in `TMsFs' to pack, unpack, merge and verify
%%       for the specifed type. The `TMsFs' is a string on the
%%       following format: `type=Type,e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,V=Mod:Fn]'.
%%       The Type and specified modules and functions are called and used
%%       as follows:
%%       <dl>
%%         <dt>`type=Type'</dt>
%%         <dd>Specfies that the translations apply to fields of type.
%%             The `Type' may be either of:
%%             `msg:MsgName' (after any renaming operations),
%%             `enum:EnumName', `int32', `int64', `uint32', `uint64',
%%             `sint32', `sint64', `fixed32', `fixed64', `sfixed32',
%%             `sfixed64', `bool', `double', `string', `bytes' or
%%             `map<KeyType,ValueType>'. The last may need quoting in
%%             the shell.</dd>
%%         <dt>`e=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term)' to pack the `Term' to a value of type
%%             `Type', ie to a value that gpb knows how to wire-encode.</dd>
%%         <dt>`d=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Value)' to unpack the just wire-decoded `Value'
%%             of type `Type', to something of your choice.</dd>
%%         <dt>`m=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term1, Term2) -> Term3' to merge two
%%             unpacked terms to a resulting Term3. Note that this function
%%             is never called for scalar types.</dd>
%%         <dt>`V=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term) -> _' to verify an unpacked `Term'.
%%             If `Term' is valid, the function is expected to just return
%%             any value, which is ignored and discarded.
%%             If `Term' is invalid, the function is exptected to not
%%             return anything, but instead either crash, call
%%             `erlang:error/1', or `throw/1' or `exit/1' with the
%%             reason for error.
%%             If you want to use a verifier, this is the new preferred
%%             approach.</dd>
%%         <dt>`v=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term, ErrorF) -> _' to verify an unpacked `Term'.
%%             This exists for backwards compatibility, and its use
%%             is deprecated.</dd>
%%       </dl>
%%   </dd>
%%   <dt><a id="cmdline-option-translate_field"/>
%%       `-translate_field FMsFs'</dt>
%%   <dd>Call functions in FMsFs to pack, unpack, merge, and verify.
%%       This is similar to the `-translate_type' option, except that
%%       a message field is specified instead of a type. The `FMsFs'
%%       is a string on the following format:
%%       `field=Path,e=...,d=...,m=...,V=...[,i=Mod:Fn][,a=Mod:Fn][,f=Mod:Fn]'
%%       See the `-translate_type' option for info on `e=', `d=', `m=' and `V='
%%       items. Additionally for this `-translate_field' option, these exist:
%%       <dl>
%%         <dt>`field=Path'</dt>
%%         <dd>The `Path' indicates the element to translate as follows:
%%           <ul>
%%             <li>`MsgName' for the message itself. (This is actually
%%                  equivalent to `-translate_type type=msg:MsgName,...')</li>
%%             <li>`MsgName.FieldName' for fields generally</li>
%%             <li>`MsgName.OneofName.FieldName' for oneof fields</li>
%%             <li>`MsgName.FieldName.[]' for elements of repeated fields</li>
%%           </ul>
%%         </dd>
%%         <dt>`i=Mod:Fn'</dt>
%%         <dd>For repeated fields, call `Mod:Fn()' on decoding to initialize
%%             the field to some value</dd>
%%         <dt>`a=Mod:Fn'</dt>
%%         <dd>For repeated fields, call `Mod:Fn(Elem,S)' on decoding
%%             to add an item)</dd>
%%         <dt>`f=Mod:Fn'</dt>
%%         <dd>For repeated fields, call `Mod:Fn(S)' on decoding
%%             to finalize the field</dd>
%%       </dl>
%%   </dd>
%%   <dt><a id="cmdline-option-any_translate"/>
%%       `-any_translate MsFs'</dt>
%%   <dd>Call functions in `MsFs' to pack, unpack, merge and verify
%%       `google.protobuf.Any' messages. The `MsFs' is a string on the
%%       following format: `e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,V=Mod:Fn]'.
%%       See the translate option for details on the string components.</dd>
%%   <dt><a id="cmdline-option-msgprefix"/>
%%       `-msgprefix Prefix'</dt>
%%   <dd>Prefix each message with `Prefix'. This can be useful to
%%       when including different sub-projects that have colliding
%%       message names.</dd>
%%   <dt><a id="cmdline-option-modprefix"/>
%%       `-modprefix Prefix'</dt>
%%   <dd>Prefix each module with `Prefix'. Normally the module name of
%%       the generated code is based on the name of the `.proto' file.
%%       This option prepends a prefix to the module name, which can be
%%       useful when including different sub-projects that have
%%       colliding proto file names.</dd>
%%   <dt><a id="cmdline-option-msgsuffix"/>
%%       `-msgsuffix Suffix'</dt>
%%   <dd>Suffix each message name with `Suffix'.</dd>
%%   <dt><a id="cmdline-option-modsuffix"/>
%%       `-modsuffix Suffix'</dt>
%%   <dd>Suffix each module name with `Suffix'.</dd>
%%   <dt><a id="cmdline-option-modname"/>
%%       `-modname Name'</dt>
%%   <dd>Specify the name of the generated module.</dd>
%%   <dt><a id="cmdline-option-msgtolower"/>
%%       `-msgtolower'</dt>
%%   <dd>ToLower each message. Any prefixes/suffixes are added
%%       after case modification.</dd>
%%   <dt><a id="cmdline-option-rename"/>
%%       `-rename What:How'</dt>
%%   <dd>The following `What' values are available:
%%       <dl>
%%         <dt>`pkg_name'</dt>
%%         <dd>Modify the package name. Useful with the `-pkgs' option.</dd>
%%         <dt>`msg_name'</dt>
%%         <dd>Modify message names, but not the package part of them</dd>
%%         <dt>`msg_fqname'</dt>
%%         <dd>Modify message names, including their package parts.
%%             Useful with the `-pkgs' option.</dd>
%%         <dt>`group_name'</dt>
%%         <dd>Group names.</dd>
%%         <dt>`group_fqname'</dt>
%%         <dd>Group names including their package parts.</dd>
%%         <dt>`service_name'</dt>
%%         <dd>Service names.</dd>
%%         <dt>`service_fqname'</dt>
%%         <dd>Service names including their package parts.</dd>
%%         <dt>`rpc_name'</dt>
%%         <dd>The RPC name.</dd>
%%         <dt>`msg_typename'</dt>
%%         <dd>Erlang type names for messages and groups.</dd>
%%         <dt>`enum_typename'</dt>
%%         <dd>Erlang type names for enums.</dd>
%%       </dl>
%%       The following `How' values are available:
%%       <dl>
%%         <dt>`prefix=Prefix'</dt>
%%         <dd>Prepend the Prefix to the beginning of the name.</dd>
%%         <dt>`suffix=Suffix'</dt>
%%         <dd>Append the Suffix to the end of the name.</dd>
%%         <dt>`lower_case'</dt>
%%         <dd>Example: from `MsgName' to `msgname'</dd>
%%         <dt>`snake_case'</dt>
%%         <dd>Example: from `MsgName' to `msg_name'</dd>
%%         <dt>`dots_to_underscores'</dt>
%%         <dd>Example: from `Msg.Sub' to `Msg_Sub'</dd>
%%         <dt>`base_name'</dt>
%%         <dd>Example: from `Domain.Pkg.Msg' to `Msg'</dd>
%%         <dt>`proto=Proto:prefix=Prefix[,...]'</dt>
%%         <dd>For each message belonging the the .proto Proto, without the
%%             `.proto' suffix, prepend `Prefix' to the beginning of the name.
%%             This only works for `msg_name' and `msg_fqname'.</dd>
%%       </dl>
%%       It is possible to specify more than one -rename option,
%%       and they are applied in the order specified.</dd>
%%   <dt><a id="cmdline-option-il"/>
%%       `-il'</dt>
%%   <dd>Generate code that include gpb.hrl using `-include_lib'
%%       instead of `-include', which is the default.</dd>
%%   <dt><a id="cmdline-option-type"/>
%%       <a id="cmdline-option-notype"/>
%%       `-type'<br/>`-no_type'</dt>
%%   <dd>Enables or disables `::Type()' annotations in the generated code.
%%       Default is to enable if there are no cyclic dependencies.</dd>
%%   <dt><a id="cmdline-option-descr"/>
%%       `-descr'</dt>
%%   <dd>Generate self-description information.</dd>
%%   <dt><a id="cmdline-option-maps"/>
%%       `-maps'</dt>
%%   <dd>This option expands to the following options:
%%       <ul>
%%         <li>`-msgs-as-maps'</li>
%%         <li>`-mapfields-as-maps'</li>
%%         <li>`-defs-as-maps'</li>
%%       </ul>
%%       See the `maps' option for the function {@link file/2}
%%       for more info.</dd>
%%   <dt><a id="cmdline-option-maps_unser_optional"/>
%%       `-maps_unset_optional omitted | present_undefined'</dt>
%%   <dd>Specifies the internal format for optional fields that are unset.</dd>
%%   <dt><a id="cmdline-option-maps_oneof"/>
%%       `-maps_oneof tuples | flat'</dt>
%%   <dd>Specifies the internal format for oneof fields in maps. (Requires
%%       `-maps' and `-maps_unset_optional omitted', of which the latter
%%       is default since 4.0.0.)</dd>
%%   <dt><a id="cmdline-option-maps_key_type"/>
%%       `-maps_key_type atom | binary'</dt>
%%   <dd>Specifies the key type for maps.</dd>
%%   <dt><a id="cmdline-option-msgs-as-maps"/>
%%       `-msgs-as-maps'</dt>
%%   <dd>Specifies that messages should be maps. No `.hrl' file will
%%       be generated.
%%       Without this option, messages will be records.</dd>
%%   <dt><a id="cmdline-option-mapfields-as-maps"/>
%%       `-mapfields-as-maps'</dt>
%%   <dd>Specifies that fields of type `map<_,_>' should be maps.
%%       Otherwise, they will be 2-tuples.</dd>
%%   <dt><a id="cmdline-option-defs-as-maps"/>
%%       `-defs-as-maps'</dt>
%%   <dd>Specifies that proto defintions from the generated code
%%       are to be returned as maps. Otherwise, they will be lists
%%       of tuples and records (or proplists if the `-pldefs' option
%%       is specified)</dd>
%%   <dt><a id="cmdline-option-erlc_compile_options"/>
%%       `-erlc_compile_options Options'</dt>
%%   <dd>Specifies compilation options, in a comma separated string, to pass
%%       along to the `-compile(...)' directive on the generated code.</dd>
%%   <dt><a id="cmdline-option-epb"/>
%%       `-epb'</dt>
%%   <dd>Enable compatibility with the Erlang Protobuffs library:
%%       <ul>
%%         <li>Implies the `-epb-functions' option</li>
%%         <li>Implies the `-defaults-for-omitted-optionals' option</li>
%%         <li>Implies the `-modsuffix _pb' option</li>
%%         <li>Implies the `-msgtolower' option</li>
%%       </ul></dd>
%%   <dt><a id="cmdline-option-epb-functions"/>
%%       `-epb-functions'</dt>
%%   <dd>For compatibility with the Erlang Protobuffs library, generate also
%%       the following functions: `encode/1', `decode/2', `encode_MsgName/1'
%%       and `decode_MsgName/1'</dd>
%%   <dt><a id="cmdline-option-defaults-for-omitted-optionals"/>
%%       `-defaults-for-omitted-optionals'</dt>
%%   <dd>For optional fields not present on decoding, set the field to
%%       its default value, if any, instead of to `undefined'.</dd>
%%   <dt><a id="cmdline-option-type-defaults-for-omitted-optionals"/>
%%       `-type-defaults-for-omitted-optionals'</dt>
%%   <dd>For optional fields not present on decoding, set the field to
%%       its type-default, instead of to `undefined'.</dd>
%%   <dt><a id="cmdline-option-for-version"/>
%%       `-for-version N'</dt>
%%   <dd>Generate code for Erlang/OTP version N instead of current.</dd>
%%   <dt><a id="cmdline-option-bypass-wrappers"/>
%%       `-bypass-wrappers'</dt>
%%   <dd>Bypass wrappers.</dd>
%%   <dt><a id="cmdline-option-json"/>
%%       `-json'</dt>
%%   <dd>Generate functions for converting to and from a JSON
%%       representation.</dd>
%%   <dt><a id="cmdline-option-json-format"/>
%%       `-json-format jsx | mochijson2 | jiffy | maps'</dt>
%%   <dd>Specify format for the JSON representation.
%%       `maps' is default if the `-maps' option is specified,
%%       otherwise the jsx format is default.</dd>
%%   <dt><a id="cmdline-option-json-object-format"/>
%%       `-json-object-format eep18 | tpl | tpl:Tag | map'</dt>
%%   <dd>Specify JSON object format:
%%      <ul>
%%        <li>`eep18' means `[{}] | proplist()', this is the default.</li>
%%        <li>`tpl' means `{proplist()}'.</li>
%%        <li>`tpl:Tag' means `{Tag, proplist()}'.</li>
%%        <li>`map' means `map()'.</li>
%%      </ul>
%%   </dd>
%%   <dt><a id="cmdline-option-json-key-format"/>
%%       `-json-key-format binary | atom | string'</dt>
%%   <dd>Specify JSON object key format. `binary' is the default.</dd>
%%   <dt><a id="cmdline-option-json-array-format"/>
%%       `-json-array-format list | tl:Tag'</dt>
%%   <dd>Specify JSON object array format.
%%      <ul>
%%        <li>`list' means `list()', this is the default.</li>
%%        <li>`tl:Tag' means `{Tag, list()}'.</li>
%%      </ul>
%%   </dd>
%%   <dt><a id="cmdline-option-json-string-format"/>
%%       `-json-string-format binary | list'</dt>
%%   <dd>Specify JSON string format. `binary' is the default.</dd>
%%   <dt><a id="cmdline-option-json-null"/>
%%       `-json-null Null'</dt>
%%   <dd>Specify JSON null value, `null' is the default.</dd>
%%   <dt><a id="cmdline-option-json-always-print-primitive-fields"/>
%%       `-json-always-print-primitive-fields'</dt>
%%   <dd>Print also fields whose value is their type's default.</dd>
%%   <dt><a id="cmdline-option-json-preserve-proto-field-names"/>
%%       `-json-preserve-proto-field-names'</dt>
%%   <dd>Print the fields' names as in `.proto' file, not
%%       as lowerCamelCase.</dd>
%%   <dt><a id="cmdline-option-json-case-insensitive-enum-parsing"/>
%%       `-json-case-insensitive-enum-parsing'</dt>
%%   <dd>Make case insignificant when parsing enums in JSON. Also allow
%%       dash as alternative to undercore.
%%       Default is that case <em>is</em> significant when parsing enums.</dd>
%%   <dt><a id="cmdline-option-no-gen-mergers"/>
%%       `-no-gen-mergers'</dt>
%%   <dd>Do not generate code for merging of messages. This is only useful
%%       with the option `-nif'.</dd>
%%   <dt><a id="cmdline-option-no-gen-introspect"/>
%%       `-no-gen-introspect'</dt>
%%   <dd>Do not generate code for introspection.</dd>
%%   <dt><a id="cmdline-option-W"/>
%%       `-Werror', `-W1', `-W0', `-W', `-Wall'</dt>
%%   <dd>`-Werror' means treat warnings as errors<br></br>
%%       `-W1' enables warnings, `-W0' disables warnings.<br></br>
%%       `-W' and `-Wall' are the same as `-W1'</dd>
%%   <dt><a id="cmdline-option-help"/>
%%       <a id="cmdline-option-h"/>
%%       `--help' or `-h'</dt>
%%   <dd>Show help.</dd>
%%   <dt><a id="cmdline-option-version"/>
%%       `--version' or `-V'</dt>
%%   <dd>Show the version number of gpb.</dd>
%% </dl>
%% If several files are specified, each is compiled individually, no
%% checking is done for instance for multiply defined messages or
%% fields across files, such as the `protoc' does.
-spec c([string() | atom()]) -> no_return().
c([F | _]=Files) when is_atom(F); is_list(F) -> %% invoked with -s or -run
    erlang:system_flag(backtrace_depth, 32),
    FileNames = [if is_atom(File)     -> atom_to_list(File);
                    is_list(File)     -> File
                 end
                 || File <- Files],
    InitArgs = init_args_to_argv(init:get_arguments()),
    PlainArgs = init:get_plain_arguments(),
    Argv = InitArgs ++ PlainArgs ++ FileNames,
    case parse_opts_and_args(Argv) of
        {ok, {Opts, Args}} ->
            c(Opts, Args);
        {error, Reason} ->
            io:format("Error: ~s.~n", [Reason]),
            show_args(),
            halt(1)
    end.

init_args_to_argv(InitArgs) ->
    lists:append([["-"++atom_to_list(OptName) | OptArgs]
                  || {OptName, OptArgs} <- InitArgs,
                     is_gpb_opt(OptName)]).

%% Opts are expected to be on same format as accepted by file/2.
%% passed by parse_opts_and_args/2.
-spec c(opts(), [ProtoFileName::string()]) -> no_return().
c(Opts, Args) ->
    case determine_cmdline_op(Opts, Args) of
        error  ->
            show_help(),
            halt(1);
        show_help  ->
            show_help(),
            halt(0);
        show_version  ->
            show_version(),
            halt(0);
        compile ->
            Opts2 = Opts ++ [report_warnings, report_errors],
            Results = [file(FileName, Opts2) || FileName <- Args],
            case lists:usort(Results) of
                [ok]  -> halt(0);
                _Errs -> halt(1)
            end
    end.

-spec parse_opts_and_args([string()]) -> {ok, {opts(), Args::[string()]}} |
                                         {error, Reason::string()}.
parse_opts_and_args(Argv) ->
    do_parse_argv(Argv, [], []).

do_parse_argv(["-"++OptName=Opt | Rest], Opts, Files) ->
    case find_opt_spec(OptName) of
        {ok, OptSpec} ->
            case parse_opt(OptName, OptSpec, Rest) of
                {ok, {ParsedOpt, Rest2}} ->
                    do_parse_argv(Rest2, [ParsedOpt | Opts], Files);
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, "Unknown option " ++ Opt}
    end;
do_parse_argv([File | Rest], Opts, Files) ->
    do_parse_argv(Rest, Opts, [File | Files]);
do_parse_argv([], Opts, Files) ->
    {ok, {lists:reverse(Opts), lists:reverse(Files)}}.

is_gpb_opt(InitArgOptAtom) ->
    find_opt_spec(atom_to_list(InitArgOptAtom)) /= error.

find_opt_spec(OptName) ->
    case [OptSpec || OptSpec <- opt_specs(), opt_matches(OptName, OptSpec)] of
        [] ->
            error;
        [OptSpec] ->
            {ok, OptSpec}
    end.

opt_matches(Opt, {OptName, 'string_maybe_appended()', _OptTag, _Descr}) ->
    lists:prefix(norm_uscore_dash(OptName), norm_uscore_dash(Opt));
opt_matches(Opt, {OptName, _Type, _OptTag, _Descr}) ->
    norm_uscore_dash(Opt) == norm_uscore_dash(OptName).

norm_uscore_dash("_"++Tl)  -> "-" ++ norm_uscore_dash(Tl);
norm_uscore_dash([C | Tl]) -> [C | norm_uscore_dash(Tl)];
norm_uscore_dash("")       -> "".

parse_opt(Opt, {OptName, 'string_maybe_appended()', OptTag, _Descr}, Rest) ->
    case {Opt, Rest} of
        {OptName, [H | Rest2]} ->
            {ok, {{OptTag, H}, Rest2}};
        {OptName, []} ->
            {error, "Missing argument for option -" ++ OptName};
        _ ->
            true = lists:prefix(OptName, Opt),
            OptArg = gpb_lib:string_slice(Opt, length(OptName)),
            {ok, {{OptTag, OptArg}, Rest}}
    end;
parse_opt(_, {_OptName, undefined, OptTag, _Descr}, Rest) ->
    {ok, {OptTag, Rest}};
parse_opt(_, {_OptName, 'string()', OptTag, _Descr}, [OptArg | Rest]) ->
    {ok, {{OptTag, OptArg}, Rest}};
parse_opt(_, {_OptName, 'atom()', OptTag, _Descr}, [OptArg | Rest]) ->
    {ok, {{OptTag, list_to_atom(OptArg)}, Rest}};
parse_opt(_, {OptName, 'integer()', OptTag, _Descr}, [OptArg | Rest]) ->
    try list_to_integer(OptArg) of
        N -> {ok, {{OptTag, N}, Rest}}
    catch error:badarg ->
            {error, ?ff("Invalid version number (integer) for ~s: ~p",
                        [OptName, OptArg])}
    end;
parse_opt(_, {_OptName, F, OptTag, _Descr}, Rest) when is_function(F) ->
    F(OptTag, Rest);
parse_opt(_, {OptName, Alternatives, OptTag, _Descr}, [OptArg | Rest]) ->
    case parse_opt_alts(tuple_to_list(Alternatives), OptArg, OptTag) of
        {ok, Opt} -> {ok, {Opt, Rest}};
        error     -> {error, "Invalid argument for -" ++ OptName}
    end;
parse_opt(OptName, _OptSpec, []) ->
    {error, "Missing argument for option -" ++ OptName}.

parse_opt_alts(['number()' | Rest], OptArg, OptTag) ->
    case string_to_number(OptArg) of
        {ok, Value} -> {ok, {OptTag, Value}};
        error       -> parse_opt_alts(Rest, OptArg, OptTag)
    end;
parse_opt_alts([Value | Rest], OptArg, OptTag) ->
    case atom_to_list(Value) of
        OptArg -> {ok, {OptTag, Value}};
        _      -> parse_opt_alts(Rest, OptArg, OptTag)
    end;
parse_opt_alts([], _OptArg, _OptTag) ->
    error.

opt_specs() ->
    [
     {"I", 'string_maybe_appended()', i, "\n"
      "       Specify include directory.\n"
      "       Option may be specified more than once to specify\n"
      "       several include directories.\n"},
     {"o", 'string()', o, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the <ProtoFile>.erl and <ProtoFile>.hrl\n"},
     {"o-erl", 'string()', o_erl, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the <ProtoFile>.erl.\n"
      "       The -o-erl Dir option overrides any -o Dir option, and\n"
      "       similarly for the other file-type specific output options.\n"},
     {"o-hrl", 'string()', o_hrl, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the <ProtoFile>.hrl\n"},
     {"o-nif-cc", 'string()', o_nif_cc, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the NIF C++ file, if the -nif option is specified\n"},
     {"nif", undefined, nif, "\n"
      "       Generate nifs for linking with the protobuf C(++) library.\n"},
     {"load_nif", 'string()', load_nif, "FunctionDefinition\n"
      "       Specify FunctionDefinition as the text that defines the\n"
      "       function load_nif/0.  This is called as the -on_load.\n"
      "       hook for loading the NIF.\n"},
     {"v", {optionally, always, never}, verify, " optionally | always | never\n"
      "       Specify how the generated encoder should\n"
      "       verify the message to be encoded.\n"},
     {"c", {true, false, auto, 'number()'}, copy_bytes,
      " true | false | auto | number()\n"
      "       Specify how or when the generated decoder should\n"
      "       copy fields of type bytes.\n"},
     {"strbin", undefined, strings_as_binaries, "\n"
      "       Specify that decoded strings should be returned as binaries,\n"
      "       instead of as strings (lists).\n"},
     {"pldefs", undefined, defs_as_proplists, "\n"
      "       Specify that introspection functions shall return proplists\n"
      "       instead of #field{} records, to make the generated code\n"
      "       completely free of even compile-time dependencies to gpb.\n"},
     {"pkgs", undefined, use_packages, "\n"
      "       Prepend the name of a package to every message it contains.\n"
      "       If no package is defined, nothing will be prepended.\n"
      "       Default is to not prepend package names for backwards\n"
      "       compatibility, but it is needed for some proto files.\n"},
     {"translate_type", fun opt_translate_type/2, translate_type,
      " type=Type,e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]\n"
      "       For fields of the specified type, call Mod:Fn to:\n"
      "       - encode (calls Mod:Fn(Term) -> AnyMessage to pack)\n"
      "       - decode (calls Mod:Fn(AnyMessage) -> Term to unpack)\n"
      "       - merge  (calls Mod:Fn(Term,Term2) -> Term3 to merge unpacked)\n"
      "       - verify (calls Mod:Fn(Term) -> _ to verify unpacked)\n"
      "       Type can be any of msg:MsgName (after any renaming operations)\n"
      "       enum:EnumName, int32, int64, uint32, uint64, sint32 sint64,\n"
      "       fixed32, fixed64, sfixed32, sfixed64, bool, double, string,\n"
      "       bytes, map<KeyType,ValueType>. The last may need quoting in\n"
      "       the shell. No merge function is called for scalar fields.\n"},
     {"translate_field", fun opt_translate_field/2, translate_field,
      " field=Field,e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]"
      "[,i=Mod:Fn][,a=Mod:Fn][,f=Mod:Fn]\n"
      "       For the specified field, call Mod:Fn. Specify Field as one of:\n"
      "       - MsgName for the message itself\n"
      "       - MsgName.FieldName for fields generally\n"
      "       - MsgName.OneofName.FieldName for oneof fields\n"
      "       - MsgName.FieldName.[] for elements of repeated fields.\n"
      "       For repeated fields, ie for the field itself, not its elements,\n"
      "       the following extra translations are to be specified:\n"
      "       - i=Mod:Fn (calls Mod:Fn() on decoding to initialize the field)\n"
      "       - a=Mod:Fn (calls Mod:Fn(Elem,S) on decoding to add an item)\n"
      "       - f=Mod:Fn (calls Mod:Fn(S) on decoding to finalize the field)\n"
      ""},
     {"any_translate", fun opt_any_translate/2, any_translate,
      " e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]\n"
      "       For a google.protobuf.Any message, call Mod:Fn to:\n"
      "       - encode (calls Mod:Fn(Term) -> AnyMessage to pack)\n"
      "       - decode (calls Mod:Fn(AnyMessage) -> Term to unpack)\n"
      "       - merge  (calls Mod:Fn(Term,Term2) -> Term3 to merge unpacked)\n"
      "       - verify (calls Mod:Fn(Term) -> _ to verify unpacked)\n"},
     {"msgprefix", 'string()', msg_name_prefix, "Prefix\n"
      "       Prefix each message with Prefix.\n"},
     {"modprefix", 'string()', module_name_prefix, "Prefix\n"
      "       Prefix the module name with Prefix.\n"},
     {"msgsuffix", 'string()', msg_name_suffix, "Suffix\n"
      "       Suffix each message with Suffix.\n"},
     {"msgtolower", undefined, msg_name_to_lower, "ToLower\n"
      "       ToLower each message.  Any prefixes/suffixes are added\n"
      "       after case modification.\n"},
     {"rename", fun opt_rename/2, rename, " What:How\n"
      "       What:\n"
      "         pkg_name       Modify the package name. Useful\n"
      "                        with the -pkgs option.\n"
      "         msg_name       Modify message names, but not the package\n"
      "                        partof them\n"
      "         msg_fqname     Modify message names, including their\n"
      "                        package part. Useful with the -pkgs option.\n"
      "         group_name     Group names.\n"
      "         group_fqname   Group names including their package parts.\n"
      "         service_name   Service names.\n"
      "         service_fqname Service names including their package parts.\n"
      "         rpc_name       The RPC name.\n"
      "         msg_typename   Erlang type names for messages and groups.\n"
      "         enum_typename  Erlang type names for enums.\n"
      "       How:\n"
      "          prefix=Prefix        Prepend the Prefix.\n"
      "          suffix=Suffix        Append the Suffix.\n"
      "          lower_case           Example: from MsgName to msgname\n"
      "          snake_case           Example: from MsgName to msg_name\n"
      "          dots_to_underscores  Example: from Msg.Sub to Msg_Sub\n"
      "          base_name            Example: from Domain.Pkg.Msg to Msg\n"
      "          proto=Proto:prefix=Prefix[,...]\n"
      "                               For each message belonging the the\n"
      "                               proto Proto, without the `.proto'\n"
      "                               suffix, prepend Prefix. This only\n"
      "                               works for msg_name and msg_fqname.\n"
      "       It is possible to specify more than one -rename option,\n"
      "       and they are applied in the order specified.\n"},
     {"modsuffix", 'string()', module_name_suffix, "Suffix\n"
      "       Suffix the module name with Suffix.\n"},
     {"modname", 'string()', module_name, "Name\n"
      "       Specify the name of the generated module.\n"},
     {"il", undefined, include_as_lib, "\n"
      "       Generate code that includes gpb.hrl using -include_lib\n"
      "       instead of -include, which is the default.\n"},
     {"type", undefined, type_specs, "\n"
      "       Enables `::Type()' annotations in the generated code.\n"},
     {"no_type", fun opt_x_false/2, type_specs, "\n"
      "       Disbles `::Type()' annotations in the generated code.\n"},
     {"descr", undefined, descriptor, "\n"
      "       Generate self-description information.\n"},
     {"maps", undefined, maps, "\n"
      "       This will expand to the following options:\n"
      "         -msgs-as-maps\n"
      "         -msgfields-as-maps\n"
      "         -defs-as-maps\n"},
     {"maps_unset_optional", {omitted, present_undefined}, maps_unset_optional,
      "omitted | present_undefined\n"
      "       Specifies the internal format for optional fields\n"
      "       that are unset.\n"},
     {"maps_oneof", {tuples, flat}, maps_oneof,
      "tuples | flat\n"
      "       Specifies the representation for oneof fields in maps:\n"
      "       as tuples, #{..., OneofField => {Tag, Value}, ...}   (default)\n"
      "       or flat,   #{..., Tag => Value, ...}\n"},
     {"maps_key_type", {atom, binary}, maps_key_type,
      "atom | binary\n"
      "       Specifies the key type for maps.\n"},
     {"msgs-as-maps", undefined, msgs_as_maps, "\n"
      "        Specifies that messages should be maps.\n"
      "        Otherwise, they will be records.\n"},
     {"mapfields-as-maps", undefined, mapfields_as_maps, "\n"
      "        Specifies that fields of type map<_,_> should be maps.\n"
      "        Otherwise, they will be 2-tuples.\n"},
     {"defs-as-maps", undefined, defs_as_maps, "\n"
      "        Specifies that proto defintions from the generated code\n"
      "        are to be returned as maps. Otherwise, they will be lists\n"
      "        of tuples and records (or proplists if the -pldefs option\n"
      "        is specified)\n"},
     {"erlc_compile_options", 'string()', erlc_compile_options, "String\n"
      "       Specifies compilation options, in a comma separated string, to\n"
      "       pass along to the -compile() directive on the generated code.\n"},
     {"epb", undefined, epb_compatibility, "\n"
      "       Enable compatibility with the Erlang Protobuffs library:\n"
      "       * Implies the -epb-functions option\n"
      "       * Implies the -modsuffix _pb option\n"
      "       * Implies the -msgtolower option\n"},
     {"epb-functions", undefined, epb_functions, "\n"
      "       Generate some functions for API compatibility with the\n"
      "       Erlang protobuffs library:\n"
      "       * encode/1 and encode_MsgName/1\n"
      "       * decode/2 and decode_MsgName/1\n"},
     {"defaults-for-omitted-optionals", undefined,
      defaults_for_omitted_optionals, "\n"
      "       For optional fields not present on decoding, set the field\n"
      "       to its default value, if any, instead of to undefined.\n"},
     {"type-defaults-for-omitted-optionals", undefined,
      type_defaults_for_omitted_optionals, "\n"
      "       For optional fields not present on decoding, set the field\n"
      "       to its type-default, instead of to undefined.\n"},
     {"for-version", 'integer()', target_erlang_version, "N\n"
      "       Generate code for Erlang/OTP version N instead of current.\n"},
     {"bypass-wrappers", undefined, bypass_wrappers, "\n"
      "       Bypass wrappers.\n"},
     {"json", undefined, json, "\n"
      "       Generate functions for converting to and from\n"
      "       a JSON representation.\n"},
     {"json-format", {jsx,mochijson2,jiffy,maps}, json_format, "\n"
      "       Specify format for JSON representation:\n"
      "       * jsx          (default if -maps is not specified)\n"
      "       * mochihison2\n"
      "       * jiffy\n"
      "       * maps         (default if -maps is specified)\n"},
     {"json-object-format", fun opt_json_object_format/2,json_object_format,"\n"
      "       Specify format for JSON object representation:\n"
      "       * eep18        [{}] | proplist()  this is the default\n"
      "       * tpl          {proplist()}\n"
      "       * tpl:Tag      {Tag, proplist()}\n"
      "       * map          map()\n"},
     {"json-key-format", {binary,atom,string}, json_key_format, "\n"
      "       Specify format for JSON object keys:\n"
      "       * binary       (default)\n"
      "       * atom\n"
      "       * string\n"},
     {"json-array-format", fun opt_json_array_format/2, json_array_format, "\n"
      "       Specify format for JSON arrays:\n"
      "       * list       list() this is the default\n"
      "       * tl:Tag     {Tag,list()}\n"},
     {"json-string-format", {binary,list}, json_string_format, "\n"
      "       Specify format for JSON strings:\n"
      "       * binary       this is the default\n"
      "       * list\n"},
     {"json-null", 'atom()', json_null, "\n"
      "       Specify format for the JSON null value, null is the default.\n"},
     {"json-always-print-primitive-fields", undefined,
      json_always_print_primitive_fields, "\n"
      "       Print also fields whose value is their type's default.\n"},
     {"json-preserve-proto-field-names", undefined,
      json_preserve_proto_field_names, "\n"
      "       Print the fields' names as in the .proto file, not as\n"
      "       lowerCamelCase.\n"},
     {"json-case-insensitive-enum-parsing", undefined,
      json_case_insensitive_enum_parsing, "\n"
      "       Make case insignificant when parsing enums in JSON. Also allow\n"
      "       dash as alternative to undercore.\n"
      "       Default is that case _is_ significant when parsing enums.\n"},
     {"no-gen-mergers", fun opt_x_false/2, gen_mergers, "\n"
      "       Do not generate code for merging of messages. This is only\n"
      "       usefulwith the option -nif.\n"},
     {"no-gen-introspect", fun opt_x_false/2, gen_introspect, "\n"
      "       Do not generate code for introspection.\n"},
     {"Werror",undefined, warnings_as_errors, "\n"
      "       Treat warnings as errors\n"},
     {"W1", undefined, report_warnings, "\n"
      "       Report warnings\n"},
     {"W0", undefined, {report_warnings,false}, "\n"
      "       Do not report warnings\n"},
     {"Wall", undefined, report_warnings, "\n"
      "       Same as -W1\n"},
     {"W", undefined, report_warnings, "\n"
      "       Same as -W1\n"},
     {"h", undefined, help, "\n"
      "       Show help\n"},
     {"-help", undefined, help, "\n"
      "       Show help\n"},
     {"V", undefined, version, "\n"
      "       Show version\n"},
     {"-version", undefined, version, "\n"
      "       Show version\n"}
    ] ++
        case os:getenv("GPB_DEV_OPTS") of
            "true" ->
                [{"fp", {pass_as_params,pass_as_record}, field_pass_method,
                  "pass_as_params | pass_as_record\n"
                  "        Override whether message fields are to be passed\n"
                  "        as parameters or as a record (or map, depending\n"
                  "        on the -maps option).  This is purely internal,\n"
                  "        and has no impact neither on input nor output,\n"
                  "        but there may be a performance difference.\n"
                  "        Normally, it is calculated automatically for each\n"
                  "        message, but during development it may be useful\n"
                  "        to be able to force it.\n"}];
            _ ->
                []
        end.


opt_rename(OptTag, [S | Rest]) ->
    try
        {What, S2} =opt_rename_what(S),
        How = opt_rename_how(What, S2),
        Opt = {OptTag, {What, How}},
        {ok, {Opt, Rest}}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_rename_what(S) ->
    case S of
        "pkg_name:"++S2       -> {pkg_name, S2};
        "msg_name:"++S2       -> {msg_name, S2};
        "msg_fqname:"++S2     -> {msg_fqname, S2};
        "group_name:"++S2     -> {group_name, S2};
        "group_fqname:"++S2   -> {group_fqname, S2};
        "service_name:"++S2   -> {service_name, S2};
        "service_fqname:"++S2 -> {service_fqname, S2};
        "rpc_name:"++S2       -> {rpc_name, S2};
        "msg_typename:"++S2   -> {msg_typename, S2};
        "enum_typename:"++S2  -> {enum_typename, S2};
        _ -> throw({badopt, "Invalid thing to rename: "++S})
    end.

opt_rename_how(What, S) ->
    case S of
        "prefix="++Prefix -> {prefix, Prefix};
        "suffix="++Suffix -> {suffix, Suffix};
        "lower_case" -> lower_case;
        "snake_case" -> snake_case;
        "dots_to_underscores" -> dots_to_underscores;
        "base_name" -> base_name;
        "proto="++_ when What == msg_name;
                         What == msg_fqname ->
            {prefix, {by_proto, opt_rename_how_proto_prefix(S)}};
        "proto="++_ ->
            throw({badopt, "Expected proto= only allowed for msg_name and "
                   "msg_fqname: " ++ ?ff("~p", [What])});
        _ ->
            throw({badopt, "Invalid renaming " ++ S})
    end.

opt_rename_how_proto_prefix("proto="++S) ->
    case read_s(S, $:, "") of
        {Proto, "prefix="++S2} ->
            try read_s(S2, $,, "") of
                {Prefix, Rest} ->
                    [{Proto, Prefix} | opt_rename_how_proto_prefix(Rest)]
            catch throw:{badopt, _} -> % no comma, end of list
                    [{Proto, S2}]
            end;
        _ ->
            throw({badopt, "Expected prefix= following proto="})
    end.

opt_x_false(OptTag, Rest) ->
    Opt = {OptTag, false},
    {ok, {Opt, Rest}}.

opt_translate_type(OptTag, [S | Rest]) ->
    try S of
        "type="++S2 ->
            {Type,Rest2} = opt_translate_type(S2),
            Ts = gpb_lib:string_lexemes(Rest2, ","),
            Opt = {OptTag, {Type, [opt_translate_mfa(T) || T <- Ts]}},
            {ok, {Opt, Rest}};
        _ ->
            {error, "Translation is expected to begin with type="}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_translate_field(OptTag, [S | Rest]) ->
    try S of
        "field="++S2 ->
            {Path,Rest2} = opt_translate_elempath(S2),
            Ts = gpb_lib:string_lexemes(Rest2, ","),
            Opt = {OptTag, {Path, [opt_translate_mfa(T) || T <- Ts]}},
            {ok, {Opt, Rest}};
        _ ->
            {error, "Translation is expected to begin with field="}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_any_translate(OptTag, [S | Rest]) ->
    try
        Ts = gpb_lib:string_lexemes(S, ","),
        Opt = {OptTag, [opt_translate_mfa(T) || T <- Ts]},
        {ok, {Opt, Rest}}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_translate_type("msg:"++Rest)  -> opt_to_comma_with_tag(Rest, msg);
opt_translate_type("enum:"++Rest) -> opt_to_comma_with_tag(Rest, enum);
opt_translate_type("map<"++Rest)  -> opt_translate_map_type(Rest);
opt_translate_type(Other) ->
    {S, Rest} = read_s(Other, $,, ""),
    Type = s2a(S),
    Allowed = [int32, int64, uint32, uint64, sint32, sint64, fixed32, fixed64,
               sfixed32, sfixed64, bool, float, double, string, bytes],
    case lists:member(Type, Allowed) of
        true -> {Type, Rest};
        false -> throw({badopt,"Invalid translation type: "++S})
    end.

opt_translate_map_type(S) ->
    {KeyType, Rest} = opt_translate_type(S),
    case gpb:is_allowed_as_key_type(KeyType) of
        true ->
            {S2, Rest2} = read_s(Rest, $>, ""),
            case opt_translate_type(S2++",") of
                {ValueType, ""} ->
                    {{map,KeyType,ValueType}, Rest2};
                {_ValueType, _} ->
                    throw({badopt,"Trailing garbage text"})
            end;
        false ->
            throw({badopt,"Not allowed as map key type"})
    end.

opt_to_comma_with_tag(S, Tag) ->
    {S2, Rest} = read_s(S, $,, ""),
    {{Tag, s2a(S2)}, Rest}.

opt_translate_elempath(S) ->
    {S2, Rest} = read_s(S, $,, ""),
    case gpb_lib:string_lexemes(S2, ".") of
        [Msg]              -> {[s2a(Msg)], Rest};
        [Msg,Field]        -> {[s2a(Msg),s2a(Field)], Rest};
        [Msg,Field,"[]"]   -> {[s2a(Msg),s2a(Field),[]], Rest};
        [Msg,Field,OFName] -> {[s2a(Msg),s2a(Field),s2a(OFName)], Rest};
        _ -> throw({badopt, "Invalid element path"})
    end.

read_s([Delim|Rest], Delim, Acc) -> {lists:reverse(Acc), Rest};
read_s([C|Rest], Delim, Acc)     -> read_s(Rest, Delim, [C | Acc]);
read_s("", _Delim, _Acc)         -> throw({badopt, "Unexpected end of string"}).

opt_translate_mfa("e="++MF) -> {encode,opt_mf_str(MF, 1)};
opt_translate_mfa("d="++MF) -> {decode,opt_mf_str(MF, 1)};
opt_translate_mfa("m="++MF) -> {merge, opt_mf_str(MF, 2)};
opt_translate_mfa("V="++MF) -> {verify,opt_mf_str(MF, 1)};
opt_translate_mfa("v="++MF) -> {verify,opt_mf_str_verify(MF)};
opt_translate_mfa("i="++MF) -> {decode_init_default,opt_mf_str(MF, 0)};
opt_translate_mfa("a="++MF) -> {decode_repeated_add_elem, opt_mf_str(MF, 2)};
opt_translate_mfa("f="++MF) -> {decode_repeated_finalize, opt_mf_str(MF, 1)};
opt_translate_mfa(X) -> throw({badopt,"Invalid translation spec: "++X}).

opt_mf_str(S, Arity) ->
    case gpb_lib:string_lexemes(S, ":") of
        [M,F] -> {list_to_atom(M),list_to_atom(F),opt_arg_template(Arity)};
        _     -> throw({badopt,"Invalid Mod:Fn spec: "++S})
    end.

opt_mf_str_verify(S) ->
    {M,F,[A]} = opt_mf_str(S, 1),
    {M,F,[A,'$errorf']}.

opt_arg_template(Arity) ->
    [list_to_atom(?ff("$~w", [I])) || I <- lists:seq(1,Arity)].

opt_json_object_format(OptTag, [S | Rest]) ->
    case S of
        "eep18"     -> {ok, {{OptTag, eep18}, Rest}};
        "tpl"       -> {ok, {{OptTag, {proplist}}, Rest}};
        "tpl:"++Tag -> {ok, {{OptTag, {s2a(Tag), proplist}}, Rest}};
        "map"       -> {ok, {{OptTag, map}, Rest}};
        _           -> {error, "Invalid JSON object format: "++S}
    end;
opt_json_object_format(_OptTag, []) ->
    {error, "Missing JSON object format"}.


opt_json_array_format(OptTag, [S | Rest]) ->
    case S of
        "list"     -> {ok, {{OptTag, list}, Rest}};
        "tl:"++Tag -> {ok, {{OptTag, {s2a(Tag),list}}, Rest}};
        _          -> {error, "Invalid JSON array format: "++S}
    end;
opt_json_array_format(_OptTag, []) ->
    {error, "Missing JSON array format"}.

s2a(S) -> list_to_atom(S).

determine_cmdline_op(Opts, FileNames) ->
    case {lists:member(help, Opts), lists:member(version, Opts)} of
        {true, _} -> show_help;
        {_, true} -> show_version;
        _         -> if FileNames == [] -> error;
                        FileNames /= [] -> compile
                     end
    end.

show_help() ->
    io:format(
      "gpb version ~s~n"
      "Usage: erl <erlargs> [gpb-opts] -s ~p c <ProtoFile>.proto~n"
      "   or: erl <erlargs> -s ~p c <ProtoFile>.proto -extra [gpb-opts]~n"
      "Typical erlargs = -noshell -noinput +B -boot start_clean -pa SomeDir~n"
      "~n",
      [gpb:version_as_string(), ?MODULE, ?MODULE]),
    show_args().

show_arg({OptDef, 'string_maybe_appended()', _, OptDoc}) ->
    io:format("   -~s   -~sOption ~s", [OptDef, OptDef, OptDoc]);
show_arg({OptDef, _, _, OptDoc}) ->
    io:format("   -~s ~s", [OptDef, OptDoc]).

-spec show_args() -> _. % side effect is to print valid opts/args
show_args() ->
    io:format(
      "Recognized gpb-opts: (see the edoc for ~p for further details)~n",
      [?MODULE]),
    lists:foreach(fun show_arg/1, opt_specs()).

-spec show_version() -> _. % side effect is to print version
show_version() ->
    io:format("gpb version ~s~n", [gpb:version_as_string()]).

string_to_number(S) ->
    try {ok, list_to_integer(S)}
    catch error:badarg ->
            try {ok, list_to_float(S)}
            catch error:badarg -> error
            end
    end.

parse_file_or_string(In, Opts) ->
    Opts1 = add_curr_dir_as_include_if_needed(Opts),
    case parse_file_and_imports(In, Opts1) of
        {ok, {Defs1, AllImported}} ->
            case gpb_defs:post_process_all_files(Defs1, Opts1) of
                {ok, Defs2} ->
                    {ok, Defs2, AllImported};
                {error, Reasons} ->
                    {error, {post_process, Reasons}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

add_curr_dir_as_include_if_needed(Opts) ->
    ImportDirs = [Dir || {i,Dir} <- Opts],
    case lists:member(".", ImportDirs) of
        true  -> Opts;
        false -> Opts ++ [{i,"."}]
    end.

parse_file_and_imports(In, Opts) ->
    FName = file_name_from_input(In),
    parse_file_and_imports(In, [FName], Opts).

file_name_from_input({Mod,_S}) -> lists:concat([Mod, ".proto"]);
file_name_from_input(FName)    -> FName.

parse_file_and_imports(In, AlreadyImported, Opts) ->
    case locate_read_import_int(In, Opts) of
        {ok, Contents} ->
            %% Add to AlreadyImported to prevent trying to import it again: in
            %% case we get an error we don't want to try to reprocess it later
            %% (in case it is multiply imported) and get the error again.
            FName = file_name_from_input(In),
            AlreadyImported2 = append_unique(AlreadyImported, [FName]),
            case scan_and_parse_string(Contents, FName, Opts) of
                {ok, Defs} ->
                    Imports = gpb_defs:fetch_imports(Defs),
                    Opts2 = ensure_include_path_to_wellknown_types(Opts),
                    read_and_parse_imports(Imports, AlreadyImported2,
                                           Defs, Opts2);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

scan_and_parse_string(S, FName, Opts) ->
    case gpb_scan:string(S) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens++[{'$end', 999}]) of
                {ok, PTree} ->
                    case gpb_defs:post_process_one_file(FName, PTree, Opts) of
                        {ok, Result} ->
                            {ok, Result};
                        {error, Reason} ->
                            {error, {parse_error, FName, Reason}}
                    end;
                {error, {_Line, _Module, _ErrInfo}=Reason} ->
                    {error, {parse_error, FName, Reason}}
            end;
        {error, {_Line0, _Module, _ErrInfo}=Reason, _Line1} ->
            {error, {scan_error, FName, Reason}}
    end.

read_and_parse_imports([Import | Rest], AlreadyImported, Defs, Opts) ->
    case lists:member(Import, AlreadyImported) of
        true ->
            read_and_parse_imports(Rest, AlreadyImported, Defs, Opts);
        false ->
            case import_it(Import, AlreadyImported, Defs, Opts) of
                {ok, {Defs2, Imported2}} ->
                    read_and_parse_imports(Rest, Imported2, Defs2, Opts);
                {error, Reason} ->
                    {error, Reason}
            end
    end;
read_and_parse_imports([], Imported, Defs, _Opts) ->
    {ok, {Defs, Imported}}.

import_it(Import, AlreadyImported, Defs, Opts) ->
    %% FIXME: how do we handle scope of declarations,
    %%        e.g. options/package for imported files?
    case parse_file_and_imports(Import, AlreadyImported, Opts) of
        {ok, {MoreDefs, MoreImported}} ->
            Defs2 = Defs++MoreDefs,
            Imported2 = append_unique(AlreadyImported, MoreImported),
            {ok, {Defs2, Imported2}};
        {error, Reason} ->
            {error, Reason}
    end.

append_unique(Items, MoreItemsToAppend) ->
    lists:foldl(
      fun(NewItem, Acc) ->
              case lists:member(NewItem, Acc) of
                  true -> Acc;
                  false -> Acc ++ [NewItem]
              end
      end,
      Items,
      MoreItemsToAppend).


locate_read_import_int({_Mod, Str}, _Opts) ->
    {ok, Str};
locate_read_import_int(Import, Opts) ->
    case proplists:get_value(import_fetcher, Opts) of
        undefined ->
            locate_read_import_aux(Import, Opts);
        Importer when is_function(Importer, 1) ->
            case Importer(Import) of
                from_file ->
                    locate_read_import_aux(Import, Opts);
                {ok, Contents} when is_list(Contents) ->
                    case lists:all(fun is_integer/1, Contents) of
                        true ->
                            {ok, Contents};
                        false ->
                            error({bad_fetcher_return,
                                   {not_a_string, Contents},
                                   Import})
                    end;
                {error, Reason} ->
                    {error, {fetcher_issue, Import, Reason}};
                X ->
                    error({bad_fetcher_return, Import, X})
            end
    end.


locate_read_import_aux(Import, Opts) ->
    ImportPaths = [Path || {i, Path} <- Opts],
    case locate_import_aux(ImportPaths, Import, Opts, []) of
        {ok, File} ->
            read_import(File, Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc Locate an import target.  This function might be potentially
%% useful for instance in an intercepting `import_fetcher' fun that
%% just wants to record the accessed imports.
-spec locate_import(string(), opts()) -> {ok, File::string()} |
                                         {error, reason()}.
locate_import(ProtoFileName, Opts) ->
    Opts1 = ensure_include_path_to_wellknown_types(Opts),
    ImportPaths = [Path || {i, Path} <- Opts1],
    locate_import_aux(ImportPaths, ProtoFileName, Opts1, []).

locate_import_aux([Path | Rest], Import, Opts, Tried) ->
    File = filename:join(Path, Import),
    case file_read_file_info(File, Opts) of
        {ok, #file_info{access = A}} when A == read; A == read_write ->
            {ok, File};
        {ok, #file_info{}} ->
            locate_import_aux(Rest, Import, Opts, Tried);
        {error, Reason} ->
            locate_import_aux(Rest, Import, Opts, [{File,Reason} | Tried])
    end;
locate_import_aux([], Import, _Opts, Tried) ->
    {error, {import_not_found, Import, Tried}}.

%% @doc Read an import file.  This function might be potentially
%% useful for instance in an intercepting `import_fetcher' fun that
%% just wants to record the accessed imports.
-spec read_import(string(), opts()) -> {ok, string()} | {error, reason()}.
read_import(File, Opts) ->
    case file_read_file(File, Opts) of
        {ok,B} ->
            case utf8_decode(B) of
                {ok, {utf8, S}} ->
                    {ok, S};
                {ok, {latin1, S}} ->
                    {ok, S};
                {error, Reason} ->
                    {error, {utf8_decode_failed, Reason, File}}
            end;
        {error, Reason} ->
            {error, {read_failed, File, Reason}}
    end.

ensure_include_path_to_wellknown_types(Opts) ->
    case proplists:get_bool(ignore_wellknown_types_directory, Opts) of
        true ->
            Opts;
        false ->
            PrivDir = get_priv_dir(),
            Wellknown = filename:join(PrivDir, "proto3"),
            sanity_check_installation_wellknown_proto3(Wellknown),
            add_opt_unless_present({i,Wellknown}, Opts)
    end.

add_opt_unless_present(Opt, [Opt | Rest]) ->
    [Opt | Rest];
add_opt_unless_present(Opt, [H | Rest]) ->
    [H | add_opt_unless_present(Opt, Rest)];
add_opt_unless_present(Opt, []) ->
    [Opt].

get_priv_dir() ->
    case locate_app_dir() of
        {ok,CurrApp} ->
            code:priv_dir(CurrApp);
        undefined ->
            %% Not loaded as an application, just executing code;
            %% from an escript possibly? (or even from an ez archive?)
            case locate_priv_dir_by_module(?MODULE) of
                {ok, PrivDir} ->
                    PrivDir;
                {error, Reason} ->
                    error({failed_to_locate_privdir,Reason})
            end
    end.

locate_app_dir() ->
    case application:get_application(?MODULE) of
        {ok,CurrApp} ->
            %% Have seen situations where there is a code path (eg via
            %% $ERL_LIBS) to another app named gpb-addon or similar.
            %% This can fool the code loader functions into wrongly
            %% thinking that the `-addon' part indicates a version
            %% number of an app named `gpb'.
            %% So do an extra safety check: this module should be located
            %% in the app's ebin dir.
            SelfBeamBase = filename:basename(code:which(?MODULE)),
            AppEbinDir = filename:join(code:lib_dir(CurrApp), "ebin"),
            case filelib:is_file(filename:join(AppEbinDir, SelfBeamBase)) of
                true ->
                    {ok, CurrApp};
                false ->
                    %% todo: Maybe we should attempt to find the true `gpb'
                    %% by searching further towards the end of the code path
                    %% but for now keep it simple(ish): rely on the fallback.
                    undefined
            end;
        undefined ->
            undefined
    end.

locate_priv_dir_by_module(Mod) ->
    case calc_priv_dir_by_module_aux(Mod) of
        {ok, PrivDir} ->
            case filelib:is_dir(PrivDir) of
                true ->
                    {ok, PrivDir};
                false ->
                    {error, {candidate_for_mod_is_no_dir, Mod, PrivDir}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

calc_priv_dir_by_module_aux(Mod) ->
    MDir = filename:dirname(code:which(Mod)),
    case filename:basename(MDir) of
        "ebin" ->
            {ok, filename:join(filename:dirname(MDir), "priv")};
        ".eunit" -> % Probably rebar2: a .eunit dir in the app's top dir
            {ok, filename:join(filename:dirname(MDir), "priv")};
        _ ->
            case code:priv_dir(gpb) of % hard-wired app name...
                Dir when is_list(Dir) ->
                    {ok, Dir};
                {error,Reason} ->
                    {error, {priv_failed_for_fallback, Reason, MDir}}
            end
    end.

sanity_check_installation_wellknown_proto3(WellknownDir) ->
    case filelib:is_dir(WellknownDir) of
        true ->
            ok;
        false ->
            error({well_known_proto3_missing,
                   "Your installation is missing the priv/proto3 "
                   "directory, which is expected to house the "
                   "'proto3 well known types' such as "
                   "google/protobuf/timestamp.proto and "
                   "google/protobuf/duration.proto. "
                   "They were expected (calculated) to be found in "
                    ++ WellknownDir})
    end.


%% Input .proto file appears to be expected to be UTF-8 by Google's protobuf.
%% In 3.0.0, it accepts a byte order mark (BOM), but in 2.6.1 it does not.
%% It only accepts a BOM for for UTF-8. It does not accept UTF-16 nor UTF-32
%% input (tried both little and big endian for both, with proper BOMs).
utf8_decode(B) ->
    {Enc, Len} = unicode:bom_to_encoding(B),
    <<_Bom:Len/binary, B2/binary>> = B,
    if Enc == latin1;
       Enc == utf8 ->
            %% Enc == latin1 means just that no Byte order mark was seen,
            %% it might still be UTF-8 encoded, though, so try that first.
            case unicode:characters_to_list(B2) of
                S when is_list(S) ->
                    {ok, {utf8, S}};
                {error, _, _} ->
                    {ok, {latin1, binary_to_list(B2)}}
            end;
       true ->
            {error, {invalid_proto_byte_order_mark, Enc}}
    end.

check_unpackables_marked_as_packed(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, MsgName, #?gpb_field{name=FName, type=Type, opts=Opts}, Acc) ->
              case {lists:member(packed, Opts), gpb:is_type_packable(Type)} of
                  {true, false} ->
                      Warn = {ignored_field_opt_packed_for_unpackable_type,
                              MsgName, FName, Type, Opts},
                      [Warn | Acc];
                  _ ->
                      Acc
              end
      end,
      [],
      Defs).

%% -- generating code ----------------------------------------------

format_erl(Mod, Defs, DefsNoRenamings,
           #anres{maps_as_msgs=MapsAsMsgs}=AnRes, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    AsLib = proplists:get_bool(include_as_lib, Opts),
    DoJson = gpb_lib:json_by_opts(Opts),
    DoMergers = gpb_lib:get_gen_mergers(Opts),
    DoIntrospect = gpb_lib:get_gen_introspect(Opts),
    CompileOptsStr = get_erlc_compile_options_str(Opts),
    gpb_lib:iolist_to_utf8_or_escaped_binary(
      [?f("%% @private~n"
          "%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n",
          [?MODULE, gpb:version_as_string()]),
       ?f("-module(~w).~n", [Mod]),
       case CompileOptsStr of
           ""    -> "";
           [_|_] -> ?f("-compile([~ts]).~n", [CompileOptsStr])
       end,
       "\n",
       gpb_gen_encoders:format_exports(Defs, Opts),
       gpb_gen_decoders:format_exports(Defs, Opts),
       [gpb_gen_mergers:format_exports(Defs, Opts) || DoMergers],
       gpb_gen_verifiers:format_exports(Defs, Opts),
       [[gpb_gen_json_encoders:format_exports(Defs, Opts),
         gpb_gen_json_decoders:format_exports(Defs, Opts)]
        || DoJson],
       [gpb_gen_introspect:format_exports(Defs, AnRes, Opts) || DoIntrospect],
       [?f("-export([descriptor/0, descriptor/1]).~n")
        || gpb_lib:get_gen_descriptor_by_opts(Opts)],
       ?f("-export([gpb_version_as_string/0, gpb_version_as_list/0]).~n"),
       "\n",
       [["-on_load(load_nif/0).\n",
         "-export([load_nif/0]). %% for debugging of nif loading\n",
         "\n"]
        || DoNif],
       case gpb_lib:get_records_or_maps_by_opts(Opts) of
           records -> ?f("-include(\"~s.hrl\").~n", [Mod]);
           maps    -> ""
       end,
       case gpb_lib:get_defs_as_maps_or_records(Opts) of
           records ->
               [case gpb_lib:get_field_format_by_opts(Opts) of
                    fields_as_records ->
                        if AsLib ->
                                ?f("-include_lib(\"gpb/include/gpb.hrl\").~n");
                           not AsLib ->
                                ?f("-include(\"gpb.hrl\").~n")
                        end;
                    fields_as_proplists ->
                        "";
                    fields_as_maps ->
                        ""
                end];
           maps ->
               ""
       end,
       "\n",
       gpb_gen_types:format_export_types(Defs, AnRes, Opts),
       "\n",
       if not DoNif ->
               case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts)
               of
                   '2tuples' ->
                       gpb_gen_types:format_maps_as_msgs_record_defs(
                         MapsAsMsgs);
                   maps ->
                       ""
               end;
          DoNif ->
               ""
       end,
       [[?f("~s~n", [gpb_gen_nif:format_load_nif(Mod, Opts)]),
         "\n"]
        || DoNif],
       %% Enabling inlining seems to cause performance to drop drastically
       %% I've seen decoding performance go down from 76000 msgs/s
       %% to about 10000 msgs/s for a set of mixed message samples.
       %% f("-compile(inline).~n"),
       %%
       gpb_gen_encoders:format_encoders_top_function(Defs, AnRes, Opts),
       "\n",
       if DoNif ->
               ?f("~s~n", [gpb_gen_nif:format_nif_encoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [gpb_gen_encoders:format_msg_encoders(Defs, AnRes, Opts,
                                                     true),
                gpb_gen_encoders:format_map_encoders(MapsAsMsgs, AnRes, Opts,
                                                     false),
                gpb_gen_encoders:format_aux_encoders(Defs, AnRes, Opts),
                gpb_gen_encoders:format_aux_common_encoders(Defs, AnRes, Opts)]
       end,
       "\n",
       gpb_gen_decoders:format_decoders_top_function(Defs, AnRes, Opts),
       "\n\n",
       if DoNif ->
               [gpb_gen_nif:format_nif_decoder_error_wrappers(Defs,
                                                              AnRes, Opts)];
          not DoNif ->
               [gpb_gen_decoders:format_msg_decoders(Defs, AnRes, Opts),
                gpb_gen_decoders:format_map_decoders(MapsAsMsgs, AnRes, Opts),
                gpb_gen_decoders:format_aux_decoders(Defs, AnRes, Opts)]
       end,
       "\n",
       [gpb_gen_mergers:format_msg_merge_code(Defs, AnRes, Opts)
        || DoMergers],
       "\n",
       gpb_gen_verifiers:format_verifiers_top_function(Defs, AnRes, Opts),
       "\n",
       gpb_gen_verifiers:format_verifiers(Defs, AnRes, Opts),
       "\n",
       if not DoNif ->
               [gpb_gen_translators:format_aux_transl_helpers(),
                gpb_gen_translators:format_translators(Defs, AnRes, Opts)];
          DoNif ->
               [gpb_gen_translators:format_aux_transl_helpers(),
                gpb_gen_translators:format_merge_translators(Defs, AnRes,
                                                             Opts)]
       end,
       "\n",
       [[gpb_gen_json_encoders:format_top_function(Defs, AnRes, Opts),
         if DoNif ->
                [gpb_gen_nif:format_nif_to_json_error_wrappers(
                   Defs, AnRes, Opts)];
           not DoNif ->
                [gpb_gen_json_encoders:format_encoders(Defs, AnRes, Opts)]
         end,
         gpb_gen_json_decoders:format_top_function(Defs, AnRes, Opts),
         if DoNif ->
                 [gpb_gen_nif:format_nif_from_json_error_wrappers(
                    Defs, AnRes, Opts)];
            not DoNif ->
                 [gpb_gen_json_decoders:format_decoders(Defs, AnRes, Opts)]
         end]
        || DoJson],
       "\n",
       [gpb_gen_introspect:format_introspection(Defs, AnRes, Opts)
        || DoIntrospect],
       "\n",
       possibly_format_descriptor(DefsNoRenamings, Opts),
       "\n",
       ?f("gpb_version_as_string() ->~n"),
       ?f("    \"~s\".~n", [gpb:version_as_string()]),
       "\n",
       ?f("gpb_version_as_list() ->~n"),
       ?f("    ~s.~n", [gpb_version_as_list_pretty()])],
      Opts).

gpb_version_as_list_pretty() ->
    %% The version "2.2-60-gb0decf3" is rendered with ~w
    %% as: [2,2,0,0,60,[103,98,48,100,101,99,102,51]]
    %% this function renders it as [2,2,0,0,60,"gb0decf3"]
    %% which is exactly the same, but easier for humans to read.
    {V, SubStrs} =
        lists:mapfoldl(fun(N, Acc) when is_integer(N) -> {N, Acc};
                          (S, Acc) when is_list(S) -> {x, Acc++[S]}
                       end,
                       [],
                       gpb:version_as_list()),
    S2 = remove_whitespaces(?ff("~p~n", [V])),
    r_strs(S2, $x, SubStrs).

remove_whitespaces(S)  -> [C || C <- S, not is_whitespace_char(C)].
is_whitespace_char($\s) -> true;
is_whitespace_char($\t) -> true;
is_whitespace_char($\n) -> true;
is_whitespace_char(_)   -> false.

r_strs([M | Tl], M, [S|Rest]) -> ?ff("~p", [S]) ++ r_strs(Tl, M, Rest);
r_strs([C | Tl], M, SubStrs)  -> [C | r_strs(Tl, M, SubStrs)];
r_strs("", _M, [])            -> "".

get_erlc_compile_options_str(Opts) ->
    proplists:get_value(erlc_compile_options, Opts, "").

%% -- descr -----------------------------------------------------

possibly_format_descriptor(Defs, Opts) ->
    case gpb_lib:get_gen_descriptor_by_opts(Opts) of
        true ->
            try gpb_compile_descr:encode_defs_to_descriptors(Defs, Opts) of
                {Bin, PBins} when is_binary(Bin), is_list(PBins) ->
                    [gpb_codegen:format_fn(
                       descriptor, fun() -> 'bin' end,
                       [replace_term(bin, Bin)]),
                     ["-spec descriptor(_) -> no_return().\n" || PBins == []],
                     gpb_codegen:format_fn(
                       descriptor,
                       fun('"base"') -> '<<PBin>>';
                          (X) -> error({gpb_error, {badname, X}})
                       end,
                       [repeat_clauses(
                          '"base"',
                          [[replace_term('"base"', ProtoBase),
                            replace_term('<<PBin>>', PBin)]
                           || {ProtoBase, PBin} <- PBins])])]
            catch ?STACKTRACE(error,undef,ST) % ->
                    case {element(1,hd(ST)), element(2,hd(ST))} of
                        {gpb_compile_descr, encode_defs_to_descriptors} ->
                            ["-spec descriptor() -> no_return().\n",
                             gpb_codegen:format_fn(
                               descriptor,
                               fun() -> erlang:error(descr_not_avail) end),
                             "-spec descriptor(_) -> no_return().\n",
                             gpb_codegen:format_fn(
                               descriptor,
                               fun(_) -> erlang:error(descr_not_avail) end)];
                        _ ->
                            %% other error
                            erlang:raise(error, undef, ST)
                    end
            end;
        false ->
            ""
    end.

%% -- hrl -----------------------------------------------------

possibly_format_hrl(Mod, Defs, AnRes, Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records -> format_hrl(Mod, Defs, AnRes, Opts);
        maps    -> '$not_generated'
    end.

format_hrl(Mod, Defs, AnRes, Opts1) ->
    Opts = [{module, Mod}|Opts1],
    ModVsn = list_to_atom(atom_to_list(Mod) ++ "_gpb_version"),
    gpb_lib:iolist_to_utf8_or_escaped_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n",
          [?MODULE, gpb:version_as_string()]),
       "\n",
       ?f("-ifndef(~p).~n", [Mod]),
       ?f("-define(~p, true).~n", [Mod]),
       "\n",
       ?f("-define(~p, \"~s\").~n", [ModVsn, gpb:version_as_string()]),
       "\n",
       gpb_lib:nl_join(
         [gpb_gen_types:format_msg_record(Msg, Fields, AnRes, Opts, Defs)
          || {_,Msg,Fields} <- gpb_lib:msgs_or_groups(Defs)]),
       "\n",
       ?f("-endif.~n")],
      Opts).

%% -- nif c++ code -----------------------------------------------------

possibly_format_nif_cc(Mod, Defs, AnRes, Opts) ->
    case proplists:get_bool(nif, Opts) of
        true  -> gpb_gen_nif:format_nif_cc(Mod, Defs, AnRes, Opts);
        false -> '$not_generated'
    end.

%% -- compile to memory -----------------------------------------------------

compile_to_binary(Mod, HrlText, ErlCode, PossibleNifCode, Opts) ->
    ModAsStr = flatten_iolist(?f("~p", [Mod])),
    ErlCode2 = nano_epp(ErlCode, ModAsStr, HrlText, Opts),
    {ok, Toks, _EndLine} = erl_scan:string(ErlCode2),
    FormToks = split_toks_at_dot(Toks),
    Forms = [case erl_parse:parse_form(Ts) of
                 {ok, Form} ->
                     Form;
                 {error, Reason} ->
                     erlang:error(
                       {internal_error,?MODULE,Mod,Ts,Reason,
                        {more_info,[{full_erl,ErlCode2},{hrl,HrlText},
                                    {nif,PossibleNifCode},{opts,Opts}]}})
             end
             || Ts <- FormToks],
    combine_erl_and_possible_nif(compile:noenv_forms(Forms, Opts),
                                 PossibleNifCode).

-record(nepp, %% nano-epp state
        {depth, %% for ifdef/else/endif processing
         mod, %% ModAsStr,
         hrl,
         defs}).

nano_epp(Code, ModAsStr, HrlText, Opts) ->
    %% nepp = nano-erlang-preprocessor. Couldn't find a way to run
    %% the epp from a string, and don't want or need to use the file
    %% system when everything is already in memory.

    %% Setup a dictionary, mostly to handle -ifdef...-endif
    %% in hrls and in the decoders.
    %% The OTP_RELEASE first appeared in Erlang 21.
    D0 = dict:new(),
    OtpRelease = gpb_lib:current_otp_release(),
    TargetOtpRelease = proplists:get_value(target_erlang_version, Opts,
                                           OtpRelease),
    D1 = if TargetOtpRelease >= 21 ->
                 dict:store('OTP_RELEASE', OtpRelease, D0);
            TargetOtpRelease < 21 ->
                 D0
         end,
    NState = #nepp{depth=1, mod=ModAsStr, hrl=HrlText, defs=D1},
    {Txt, <<>>, _EndNState, _EndLine} = nepp1(Code, NState, _Line=1, []),
    Txt.

nepp1(<<"%% -*- coding:",_/binary>>=B, #nepp{mod=ModAsStr}=NState, N, Acc) ->
    %% First (non-coding) line must be a -file(...) directive,
    %% or else unused record definitions in included files will
    %% produce warnings: eg: {27,erl_lint,{unused_record,gpb_oneof}}.
    {CodingLine,Rest} = read_until(B, "\n", ""),
    Erl = (ModAsStr -- "''") ++ ".erl",
    CodingAndFileDirective = CodingLine ++ "\n" ++ file_directive(Erl, 1),
    Acc2 = lists:reverse(CodingAndFileDirective, Acc),
    nepp2_nl(Rest, NState, N, Acc2);
nepp1(Rest, #nepp{mod=ModAsStr}=NState, N, Acc) ->
    Erl = (ModAsStr -- "''") ++ ".erl",
    FileDirective = file_directive(Erl, 1),
    Acc2 = lists:reverse(FileDirective, Acc),
    nepp2_nl(Rest, NState, N, Acc2).

nepp2(<<"?MODULE", Rest/binary>>, #nepp{mod=ModAsStr}=NState, N, Acc) ->
    nepp2(Rest, NState, N, lists:reverse(ModAsStr, Acc));
nepp2(<<$\n, Rest/binary>>, NState, N, Acc) ->
    nepp2_nl(Rest, NState, N+1, [$\n | Acc]);
nepp2(<<C, Rest/binary>>, NState, N, Acc) ->
    nepp2(Rest, NState, N, [C | Acc]);
nepp2(<<>>, NState, N, Acc) ->
    {lists:reverse(Acc), <<>>, NState, N}.

%% collect and handle pre-processor directives
nepp2_nl(<<"-include", Rest/binary>>, NState, N, Acc) ->
    nepp2_inc(Rest,NState, N, Acc);
nepp2_nl(<<"-include_lib", Rest/binary>>, NState, N, Acc) ->
    nepp2_inc(Rest, NState, N, Acc);
nepp2_nl(<<"-define", Rest/binary>>, NState, N, Acc) ->
    nepp2_def(Rest, NState, N, Acc);
nepp2_nl(<<"-ifdef", Rest/binary>>, NState, N, Acc) ->
    nepp2_ifdef(Rest, ifdef, NState, N, Acc);
nepp2_nl(<<"-ifndef", Rest/binary>>, NState, N, Acc) ->
    nepp2_ifdef(Rest, ifndef, NState, N, Acc);
nepp2_nl(<<"-else.\n", Rest/binary>>, #nepp{depth=1}=NState, N, Acc) ->
    nepp2_skip(Rest, NState, N+1, Acc);
nepp2_nl(<<"-endif.\n", Rest/binary>>, #nepp{depth=1}=NState, N, Acc) ->
    {lists:reverse(Acc), Rest, NState, N+1};
nepp2_nl(X, NState, N, Acc) ->
    nepp2(X, NState, N, Acc).

nepp2_inc(Rest, #nepp{mod=ModAsStr, hrl=Hrl}=NState, N, Acc) ->
    {_,    Rest1} = read_until(Rest,  "(", ""),
    {Inc1, Rest2} = read_until(Rest1, ")", ""),
    {_,    Rest3} = read_until(Rest2, "\n", ""),
    Inc = parse_term(Inc1),
    Erl = (ModAsStr -- "''") ++ ".erl",
    case classify_inc(Inc) of
        gpb_hrl ->
            FieldDef = field_record_to_text(),
            OneofDef = oneof_record_to_text(),
            RpcDef   = rpc_record_to_text(),
            Txt = lists:flatten([file_directive(Inc, 1),
                                 FieldDef, OneofDef, RpcDef]),
            Acc2 = lists:reverse(Txt ++ file_directive(Erl, N+1), Acc),
            nepp2_nl(Rest3, NState, N+1, Acc2);
        mod_hrl when Hrl /= '$not_generated' ->
            {Txt1, <<>>, NState2, _EndLine} = nepp2_nl(Hrl, NState, 1, []),
            Txt2 = lists:flatten([file_directive(Inc, 1), Txt1]),
            Acc2 = lists:reverse(Txt2 ++ file_directive(Erl, N+1), Acc),
            nepp2_nl(Rest3, NState2, N+1, Acc2)
    end.

nepp2_def(Rest, #nepp{defs=Ds}=NState, N, Acc) ->
    {_,   Rest1} = read_until(Rest,  "(", ""),
    {Sym, Rest2} = read_until(Rest1, ",", ""),
    {Val, Rest3} = read_until(Rest2, ")", ""),
    {_,   Rest4} = read_until(Rest3, "\n", ""),
    Ds1 = dict:store(parse_term(Sym), parse_term(Val), Ds),
    nepp2_nl(Rest4, NState#nepp{defs=Ds1}, N+1, Acc).

nepp2_ifdef(Rest, SkipCond, #nepp{depth=Depth, defs=Ds}=NState, N, Acc) ->
    {_,   Rest1} = read_until(Rest,  "(", ""),
    {Sym, Rest2} = read_until(Rest1, ")", ""),
    {_,   Rest3} = read_until(Rest2, "\n", ""),
    {Txt, Rest4, NState2, N2} =
    case {dict:is_key(parse_term(Sym), Ds), SkipCond} of
        {true,  ifdef}  -> nepp2_nl(Rest3, NState#nepp{depth=1}, N+1, []);
        {false, ifndef} -> nepp2_nl(Rest3, NState#nepp{depth=1}, N+1, []);
        _ -> nepp2_skip(Rest3, NState#nepp{depth=1}, N+1, [])

    end,
    nepp2_nl(Rest4, NState2#nepp{depth=Depth}, N2, lists:reverse(Txt, Acc)).

nepp2_skip(<<"-else.\n", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    if Depth == 1 -> nepp2_nl(Rest, NState, N+1, Acc);
       Depth >  1 -> nepp2_skip(Rest, NState, N+1, Acc)
    end;
nepp2_skip(<<"-endif.\n", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    if Depth == 1 -> {lists:reverse(Acc), Rest, NState, N+1};
       Depth >  1 -> nepp2_skip(Rest, NState#nepp{depth=Depth-1}, N+1, Acc)
    end;
nepp2_skip(<<"-ifdef", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    {_, Rest2} = read_until(Rest, "\n", ""),
    nepp2_skip(Rest2, NState#nepp{depth=Depth+1}, N+1, Acc);
nepp2_skip(<<"-ifndef", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    {_, Rest2} = read_until(Rest, "\n", ""),
    nepp2_skip(Rest2, NState#nepp{depth=Depth+1}, N+1, Acc);
nepp2_skip(<<$\n, Rest/binary>>, NState, N, Acc) ->
    nepp2_skip(Rest, NState, N+1, Acc);
nepp2_skip(<<_, Rest/binary>>, NState, N, Acc) ->
    nepp2_skip(Rest, NState, N, Acc).

read_until(<<C, Rest/binary>>, Delims, Acc) ->
    case lists:member(C, Delims) of
        true  -> {lists:reverse(Acc), Rest};
        false -> read_until(Rest, Delims, [C | Acc])
    end.

parse_term(S) ->
    {ok, Tokens, _End} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens++[{dot,1}]),
    Term.

classify_inc(F) ->
    case lists:last(filename:split(F)) of
        "gpb.hrl" -> gpb_hrl;
        _         -> mod_hrl
    end.

file_directive(File, N) ->
    ?ff("-file(\"~s\", ~p).\n", [File, N]).

split_toks_at_dot(AllToks) ->
    case lists:splitwith(fun is_no_dot/1, AllToks) of
        {Toks, [{dot,_}=Dot]}      -> [Toks ++ [Dot]];
        {Toks, [{dot,_}=Dot | Tl]} -> [Toks ++ [Dot] | split_toks_at_dot(Tl)]
    end.

is_no_dot({dot,_}) -> false;
is_no_dot(_)       -> true.

field_record_to_text() ->
    record_to_text(?gpb_field, record_info(fields, ?gpb_field), #?gpb_field{}).

oneof_record_to_text() ->
    record_to_text(gpb_oneof, record_info(fields, gpb_oneof), #gpb_oneof{}).

rpc_record_to_text() ->
    record_to_text(?gpb_rpc, record_info(fields, ?gpb_rpc), #?gpb_rpc{}).

record_to_text(RecordName, Fields, DefaultR) ->
    FieldTexts =
        [if Default == undefined -> ?ff("~p", [FName]);
            Default /= undefined -> ?ff("~p = ~p", [FName, Default])
         end
         || {FName,Default} <- lists:zip(Fields, tl(tuple_to_list(DefaultR)))],
    ?f("-record(~p, {~s}).~n",
       [RecordName, gpb_lib:comma_join(FieldTexts)]).

combine_erl_and_possible_nif(ErlCompilationResult, '$not_generated'=_Nif) ->
    ErlCompilationResult;
combine_erl_and_possible_nif({ok, ModuleName, ErlCode}, NifTxt) ->
    {ok, ModuleName, combine_erlcode_with_niftxt(ErlCode, NifTxt)};
combine_erl_and_possible_nif({ok, ModuleName, ErlCode, Warnings}, NifTxt) ->
    {ok, ModuleName, combine_erlcode_with_niftxt(ErlCode, NifTxt), Warnings};
combine_erl_and_possible_nif(Error, _NifTxt) ->
    Error.

combine_erlcode_with_niftxt(ErlCode, NifTxt) ->
    [{erl, ErlCode},
     {nif, NifTxt}].

%% -- internal utilities -----------------------------------------------------

flatten_iolist(IoList) ->
    binary_to_list(iolist_to_binary(IoList)).

file_read_file(FileName, Opts) ->
    file_op(read_file, [FileName], Opts).

file_read_file_info(FileName, Opts) ->
    file_op(read_file_info, [FileName], Opts).

file_write_file(FileName, Bin, Opts) ->
    file_op(write_file, [FileName, Bin], Opts).

possibly_write_file(FileName, Bin, Opts) when is_binary(Bin) ->
    file_op(write_file, [FileName, Bin], Opts);
possibly_write_file(_FileName, '$not_generated', _Opts) ->
    ok.

file_op(FnName, Args, Opts) ->
    case proplists:get_value(file_op, Opts) of
        undefined ->
            apply(file, FnName, Args);
        Ops ->
            case proplists:get_value(FnName, Ops) of
                undefined ->
                    apply(file, FnName, Args);
                Fn ->
                    apply(Fn, Args)
            end
    end.

possibly_probe_defs(Defs, Opts) ->
    case proplists:get_value(probe_defs, Opts, '$no') of
        '$no' -> ok;
        Fn    -> Fn(Defs)
    end.
