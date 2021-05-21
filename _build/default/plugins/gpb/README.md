The gpb is a compiler for
[Google protocol buffer](https://developers.google.com/protocol-buffers/)
definitions files for Erlang.

Shortcuts: [API documentation](https://hexdocs.pm/gpb/) ~ [gpb on hex.pm](https://hex.pm/packages/gpb/) ~ [![Build Status](https://travis-ci.org/tomas-abrahamsson/gpb.svg?branch=master)](https://travis-ci.org/tomas-abrahamsson/gpb)

Basic example of using gpb
--------------------------

Let's say we have a protobuf file, `x.proto`
```protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
}
```
We can generate code for this definition in a number of different
ways. Here we use the command line tool. For info on integration with
rebar, see further down.
```
# .../gpb/bin/protoc-erl -I. x.proto
```
Now we've got `x.erl` and `x.hrl`. First we compile it and then we can
try it out in the Erlang shell:
```erlang
# erlc -I.../gpb/include x.erl
# erl
Erlang/OTP 19 [erts-8.0.3] [source] [64-bit] [smp:12:12] [async-threads:10] [kernel-poll:false]

Eshell V8.0.3  (abort with ^G)
1> rr("x.hrl").
['Person']
2> x:encode_msg(#'Person'{name="abc def", id=345, email="a@example.com"}).    
<<10,7,97,98,99,32,100,101,102,16,217,2,26,13,97,64,101,
  120,97,109,112,108,101,46,99,111,109>>
3> Bin = v(-1).
<<10,7,97,98,99,32,100,101,102,16,217,2,26,13,97,64,101,
  120,97,109,112,108,101,46,99,111,109>>
4> x:decode_msg(Bin, 'Person').
#'Person'{name = "abc def",id = 345,email = "a@example.com"}
```

In the Erlang shell, the `rr("x.hrl")` reads record definitions, and
the `v(-1)` references a value one step earlier in the history.

Mapping of protocol buffer datatypes to erlang
----------------------------------------------

<table>
<thead><tr><th>Protobuf type</th><th>Erlang type</th></tr></thead>
<tbody>
<!-- = = = = = = = = = = = = = = = = = = = = = = = = = = = -->
<tr><td>double, float</td>
    <td>float() | infinity | '-infinity' | nan<br/>
        When encoding, integers, too, are accepted</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>   int32,    int64<br/>
          uint32,   uint64<br/>
          sint32,   sint64<br/>
         fixed32,  fixed64<br/>
        sfixed32, sfixed64</td>
    <td>integer()</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>bool</td>
    <td>true | false<br/>
        When encoding, the integers 1 and 0, too, are accepted</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>enum</td>
    <td>atom()<br/>
        unknown enums decode to integer()</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>message</td>
    <td>record (thus tuple())<br/>
        or map() if the maps (-maps) option is specified</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>string</td>
    <td>unicode string, thus list of integers<br/>
        or binary() if the strings_as_binaries (-strbin) option is
        specified<br/>
        When encoding, iolists, too, are accepted</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>bytes</td>
    <td>binary()<br/>
        When encoding, iolists, too, are accepted</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>oneof</td>
    <td><tt>{ChosenFieldName, Value}</tt><br/>
        or <tt>ChosenFieldName => Value</tt> if the {maps_oneof,flat}
        (-maps_oneof flat) option is specified</td></tr>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<tr><td>map<_,_></td>
    <td>An unordered list of 2-tuples, <tt>[{Key,Value}]</tt><br/>
        or  a map(), if the maps (-maps) option is specified</td></tr>
</tbody></table>


Repeated fields are represented as lists.

Optional fields are represented as either the value or `undefined` if
not set. However, for maps, if the option `maps_unset_optional` is set
to `omitted`, then unset optional values are omitted from the map,
instead of being set to `undefined` when encoding messages. When
decoding messages, even with `maps_unset_optional` set to `omitted`,
the default value will be set in the decoded map.

Examples of Erlang format for protocol buffer messages
------------------------------------------------------

#### Repeated and required fields

```protobuf
   message m1 {
     repeated uint32 i   = 1;
     required bool   b   = 2;
     required eee    e   = 3;
     required submsg sub = 4;
   }
   message submsg {
     required string s = 1;
     required bytes  b = 2;
   }
   enum eee {
     INACTIVE = 0;
     ACTIVE   = 1;
   }
```
##### Corresponding Erlang
```erlang
   #m1{i   = [17, 4711],
       b   = true,
       e   = 'ACTIVE',
       sub = #submsg{s = "abc",
                     b = <<0,1,2,3,255>>}}

   %% If compiled to with the option maps:
   #{i   => [17, 4711],
     b   => true,
     e   => 'ACTIVE',
     sub => #{s => "abc",
              b => <<0,1,2,3,255>>}}
```

#### Optional fields
```protobuf
   message m2 {
     optional uint32 i1 = 1;
     optional uint32 i2 = 2;
   }
```
##### Corresponding Erlang
```erlang
   #m2{i1 = 17}    % i2 is implicitly set to undefined

   %% With the maps option
   #{i1 => 17,
     i2 => undefined}

   %% With the maps option and the maps_unset_optional set to omitted:
   #{i1 => 17}
```

#### Oneof fields
This construct first appeared in Google protobuf version 2.6.0.
```protobuf
   message m3 {
     oneof u {
       int32  a = 1;
       string b = 2;
     }
   }
```
##### Corresponding Erlang
A oneof field is automatically always optional.
```erlang
   #m3{u = {a, 17}}
   #m3{u = {b, "hello"}}
   #m3{}                 % u is implicitly set to undefined

   %% With the maps option
   #{u => {a, 17}}
   #{u => {b, "hello"}}
   #{}                   % If maps_unset_optional = omitted (default)
   #{u => undefined}     % With maps_unset_optional set to present_undefined

   %% With the {maps_oneof,flat} option (requires maps_unset_optional = omitted)
   #{a => 17}
   #{b => "hello"}
   #{}

```

#### Map fields
Not to be confused with Erlang maps.
This construct first appeared in Google protobuf version 3.0.0 (for
both the `proto2` and the `proto3` syntax)
```protobuf
   message m4 {
     map<uint32,string> f = 1;
   }
```
##### Corresponding Erlang
For records, the order of items is undefined when decoding.
```erlang
   #m4{f = []}
   #m4{f = [{1, "a"}, {2, "b"}, {13, "hello"}]}

   %% With the maps option
   #{f => #{}}
   #{f => #{1 => "a", 2 => "b", 13 => "hello"}}
```


Unset optionals and the `default` option
----------------------------------------

#### For proto2 syntax

This describes how decoding works for optional fields that are
not present in the binary-to-decode.

The documentation for Google protobuf says these decode to the default
value if specified, or else to the field's type-specific default. The
code generated by Google's protobuf compiler also contains
`has_<field>()` methods so one can examine whether a field was
actually present or not.

However, in Erlang, the natural way to set and read fields is to just
use the syntax for records (or maps), and this leaves no good way to
at the same time both convey whether a field was present or not and to
read the defaults.

So the approach in `gpb` is that you have to choose: either or.
Normally, it is possible to see whether an optional field is
present or not, eg by checking if the value is `undefined`. But there
are options to the compiler to instead decode to defaults, in which
case you lose the ability to see whether a field is present or not.
The options are `defaults_for_omitted_optionals` and
`type_defaults_for_omitted_optionals`, for decoding to `default=<x>`
values, or to type-specific defaults respectively.

It works this way:

```protobuf
message o1 {
  optional uint32 a = 1 [default=33];
  optional uint32 b = 2; // the type-specific default is 0
}
```

Given binary data `<<>>`, that is, neither field `a` nor `b` is present,
then the call `decode_msg(Input, o1)` results in:

```erlang
#o1{a=undefined, b=undefined} % None of the options

#o1{a=33, b=undefined}        % with option defaults_for_omitted_optionals

#o1{a=33, b=0}                % with both defaults_for_omitted_optionals
                              %       and type_defaults_for_omitted_optionals

#o1{a=0, b=0}                 % with only type_defaults_for_omitted_optionals
```
The last of the alternatives is perhaps not very useful, but still
possible, and implemented for completeness.

[Google's Reference](https://developers.google.com/protocol-buffers/docs/proto#optional)

#### For proto3 syntax

For proto3, there is neither `required` nor `optional` nor `default=<x>`
for fields. Instead all scalar fields, strings and bytes are implicitly
optional. On decoding, if such a field is missing in the binary to
decode, they always decode to the type-specific default value.
On encoding, such fields are only included in the resulting encoded
binary if they have a value different from the type-specific default
value. Even though all fields are implicitly optional, one could also
say that on a conceptual level, all such fields always have a value.
At decoding, it is not possible to determine whether at encoding,
a value was present---with a type-specific value---or not.

A recommendation I've seen for if you need detection of "missing" data,
is to define `has_<field>` boolean fields and set them appropriately.
Another alternative could be to use the well-known wrapper messages.

Fields that are sub-messages and oneof fields, do not have any
type-specific default. A sub-message field that was not set encodes
differently from a sub-message field set to the sub-message, and it
decodes differently. This holds even when the sub-message has no
fields. It works a bit similarly for oneof fields. Either none of the
alternative oneof fields is set, or one of them is. The encoded format
is different, and on decoding it is possible to tell a difference.

Features of gpb
---------------

*  Parses protocol buffer definition files and can generate:
   - record definitions, one record for each message
   - erlang code for encoding/decoding the messages to/from binaries

*  Features of the protocol buffer definition files:
   gpb supports:
   - message definitions (also messages in messages)
   - scalar types
   - importing other proto files
   - nested types
   - message extensions
   - the `packed` and `default` options for fields
   - the `allow_alias` enum option (treated as if it is always set true)
   - generating metadata information
   - package namespacing (optional)
   - `oneof` (introduced in protobuf 2.6.0)
   - `map<_,_>` (introduced in protobuf 3.0.0)
   - proto3 support:
     - syntax and general semantics
     - import of well-known types
     - Callback functions can be specified for automatically translating
       google.protobuf.Any messages
   - groups
   - JSON mapping is supported, see the json (-json) option(s)

   gpb reads but ignores or throws away:
   - options other than `packed` or `default`
   - custom options

   gpb does not support:
   - aggregate custom options introduced in protobuf 2.4.0
   - rpc
   - JSON limitations:
     - does not handle the special JSON mapping of
       the google.protobuf.Any wellknown

*  Characteristics of gpb:
   - Skipping over unknown message fields or groups, when decoding,
     is supported
   - Merging of messages, also recursive merging, is supported
   - Gpb can optionally generate code for verification of values during
     encoding this makes it easy to catch e.g integers out of range,
     or values of the wrong type.
   - Gpb can optionally or conditionally copy the contents of `bytes`
     fields, in order to let the runtime system free the larger message
     binary.
   - Gpb can optionally make use of the `package` attribute by prepending
     the name of the package to every contained message type (if defined),
     which is useful to avoid name clashes of message types across packages.
     See the [`use_packages` option](https://hexdocs.pm/gpb/gpb_compile.html#option-use_packages)
     or the [`-pkgs` command line option](https://hexdocs.pm/gpb/gpb_compile.html#cmdline-option-pkgs).
   - The generated encode/decoder has no run-time dependency to gpb,
     but there is normally a compile-time dependency for the generated
     code: to the `#field{}` record in gpb.hrl for the `get_msg_defs`
     function, but it is possible to avoid this dependency by using
     the also the `defs_as_proplists` or `-pldefs` option.
   - Gpb can generate code both to files and to binaries.
   - Proto input files are expected to be UTF-8, but the file reader
     will fall back to decode the files as latin1 in UTF-8 decode errors,
     for backwards compatibility and behaviour that most closely
     emulates what Google protobuf does.

*  Introspection

   gpb generates some functions for examining messages, enums and services:
   - `get_msg_defs()`, `get_msg_names()`, `get_enum_names()`
   - `find_msg_def(MsgName)` and `fetch_msg_def(MsgName)`
   - `find_enum_def(MsgName)` and `fetch_enum_def(MsgName)`
   - `enum_symbol_by_value(EnumName, Value)`,
   - `enum_symbol_by_value_<EnumName>(Value)`,
     `enum_value_by_symbol(EnumName, Enum)` and
     `enum_value_by_symbol_<EnumName>(Enum)`
   - `get_service_names()`, `get_service_def(ServiceName)`, `get_rpc_names(ServiceName)`
   - `find_rpc_def(ServiceName, RpcName)`, `fetch_rpc_def(ServiceName, RpcName)`

   There are also some functions for translating between fully qualified
   names and internal names. These take any renaming options into
   consideration. They may be useful for instance with grpc reflection.

   - `fqbin_to_service_name(<<"Package.ServiceName">>)`
     and `service_name_to_fqbin('ServiceName')`
   - `fqbins_to_service_and_rpc_name(<<"Package.ServiceName">>, <<"RpcName">>)`
     and `service_and_rpc_name_to_fqbins('ServiceName', 'RpcName')`
   - `fqbin_to_msg_name(<<"Package.MsgName">>)` and
     `msg_name_to_fqbin('MsgName')`
   - `fqbin_to_enum_name(<<"Package.EnumName">>)` and
     `enum_name_to_fqbin('EnumName')`

   There are also some functions for querying what proto a type belongs
   to. Each type belongs to some `"name"` which is a string, usually the
   file name, sans extension, for example `"name"` if the proto file was
   `"name.proto"`.

   - `get_all_proto_names() -> ["name1", ...]`
   - `get_msg_containment("name") -> ['MsgName1', ...]`
   - `get_pkg_containment("name") -> 'Package'`
   - `get_service_containment("name") -> ['Service1', ...]`
   - `get_rpc_containment("name") -> [{'Service1', 'RpcName1}, ...]`
   - `get_proto_by_msg_name_as_fqbin(<<"Package.MsgName">>) -> "name"`
   - `get_proto_by_enum_name_as_fqbin(<<"Package.EnumName">>) -> "name"`
   - `get_protos_by_pkg_name_as_fqbin(<<"Package">>) -> ["name1", ...]`

   There are also some version information functions:

   - `gpb:version_as_string()` and `gpb:version_as_list()`
   - `GeneratedCode:version_as_string()` and `GeneratedCode:version_as_list()`
   - `?gpb_version`  (in gpb_version.hrl)
   - `?'GeneratedCode_gpb_version'`  (in GeneratedCode.hrl)

   The gpb can also generate a self-description of the proto file.
   The self-description is a description of the proto file, encoded to
   a binary using the descriptor.proto that comes with the Google
   protocol buffers library. Note that such an encoded self-descriptions
   won't be byte-by-byte identical to what the Google protocol buffers
   compiler will generate for the same proto, but should be roughly
   equivalent.

*  Erroneously encoded protobuf messages and fields will generally
   cause the decoder to crash.  Examples of such erroneous encodings are:
   - varints with too many bits
   - strings, bytes, sub messages or packed repeated fields,
     where the encoded length is longer than the remaining binary

*  Maps

   Gpb can generate encoders/decoders for maps.

   The option `maps_unset_optional` can be used to specify behavior
   for non-present optional fields: whether they are omitted from
   maps, or whether they are present, but have the value `undefined`
   like for records.

*  Reporting of errors in .proto files

   Gpb is not very good at error reporting, especially referencing
   errors, such as references to messages that are not defined.
   You might want to first verify with `protoc` that the .proto files
   are valid before feeding them to gpb.

*  Caveats

   The gpb does accept reserved words as names for fields (just like
   protoc does), but not as names for messages. To correct this, one
   would have to either rewrite the grammar, or stop using yecc.
   (maybe rewrite it all as a protoc plugin?)

Interaction with rebar
----------------------

For info on how to use gpb with rebar3, see
https://www.rebar3.org/docs/using-available-plugins#section-protocol-buffers

In rebar there is support for gpb since version 2.6.0. See the
proto compiler section of rebar.sample.config file at
https://github.com/rebar/rebar/blob/master/rebar.config.sample

For older versions of rebar---prior to 2.6.0---the text below outlines
how to proceed:

Place the .proto files for instance in a `proto/` subdirectory.
Any subdirectory, other than src/, is fine, since rebar will try to
use another protobuf compiler for any .proto it finds in the src/
subdirectory.  Here are some some lines for the `rebar.config` file:

    %% -*- erlang -*-
    {pre_hooks,
     [{compile, "mkdir -p include"}, %% ensure the include dir exists
      {compile,
       "/path/to/gpb/bin/protoc-erl -I`pwd`/proto"
       "-o-erl src -o-hrl include `pwd`/proto/*.proto"
      }]}.

    {post_hooks,
     [{clean,
       "bash -c 'for f in proto/*.proto; "
       "do "
       "  rm -f src/$(basename $f .proto).erl; "
       "  rm -f include/$(basename $f .proto).hrl; "
       "done'"}
     ]}.

    {erl_opts, [{i, "/path/to/gpb/include"}]}.


Performance
-----------

Here is a comparison between gpb (interpreted by the erlang vm) and
the C++, Python and Java serializers/deserializers of protobuf-2.6.1rc1

    [MB/s]        | gpb   |pb/c++ |pb/c++ | pb/c++ | pb/py |pb/java| pb/java|
                  |       |(speed)|(size) | (lite) |       |(size) | (speed)|
    --------------+-------+-------+-------+--------+-------+-------+--------+
    small msgs    |       |       |       |        |       |       |        |
      serialize   |   52  | 1240  |   85  |   750  |  6.5  |   68  |  1290  |
      deserialize |   63  |  880  |   85  |   950  |  5.5  |   90  |   450  |
    --------------+-------+-------+-------+--------+-------+-------+--------+
    large msgs    |       |       |       |        |       |       |        |
      serialize   |   36  |  950  |   72  |   670  |  4.5  |   55  |   670  |
      deserialize |   54  |  620  |   71  |   480  |  4.0  |   60  |   360  |
    --------------+-------+-------+-------+--------+-------+-------+--------+

The performances are measured as number of processed MB/s,
serialized form.  Higher values means better performance.

The benchmarks are run with small and large messages (228 and 84584
bytes, respectively, in serialized form)

The Java benchmark is run with optimization both for code size and for
speed. The Python implementation cannot optimize for speed.

    SW: Python 2.7.11, Java 1.8.0_77 (Oracle JDK), Erlang/OTP 18.3, g++ 5.3.1
        Linux kernel 4.4, Debian (in 64 bit mode), protobuf-2.6.1rc1
    HW: Intel Core i7 5820k, 3.3GHz, 6x256 kB L2 cache, 15MB L3 cache
        (CPU frequency pinned to 3.3 GHz)

*NB:* Optimizations landed in Erlang 22, but the performance figures
have not yet been updated.  The way the benchmarks are implemented is
causing more GC than normal, and just running the benchmarks will
produce pessimistic values.  I have yet to figure out a better way
of benchmarking, before I can update the figures..
See https://github.com/erlang/otp/commit/7d941c529d#commitcomment-31091771
for more info.

The benchmarks are all done with the exact same messages files and
proto files.  The source of the benchmarks was found in the Google
protobuf's svn repository.  The gpb originally did not support groups,
and the benchmarks in the protobuf used groups, so I converted the
google_message*.dat to use sub message structures instead.
For protobuf, that change was only barely noticeable.

For performance, the generated Erlang code avoids creating sub
binaries as far as possible. It has to for sub messages, strings and
bytes, but for the rest of the types, it avoids creating sub binaries,
both during encoding and decoding (for info, compile with the
`bin_opt_info` option)

The Erlang code ran in the smp emulator, though only one CPU core
was utilized.

The generated C++ core was compiled with -O3.


Version numbering
-----------------

The gpb version number is fetched from the git latest git tag
matching N.M where N and M are integers.  This version is
inserted into the gpb.app file as well as into the
include/gpb_version.hrl.  The version is the result of the command

    git describe --always --tags --match '[0-9]*.[0-9]*'

Thus, to create a new version of gpb, the single source from where
this version is fetched, is the git tag.   (If you are importing
gpb into another version control system than git, or using another
build tool than rebar, you might have to adapt rebar.config and
src/gpb.app.src accordingly. See also the section below about
[building outside of a git work tree](README.md#building-outside-of-a-git-work-tree) for info on
exporting gpb from git.)

The version number on the master branch of the gpb on github is
intended to always be only integers with dots, in order to be
compatible with reltool.  In other words, each push to github is
considered a release, and the version number is bumped.  To ensure
this, there is a `pre-push` git hook and two scripts,
`install-git-hooks` and `tag-next-minor-vsn`, in the helpers
subdirectory. The ChangeLog file will not necessarily reflect all
minor version bumps, only important updates.

Places to update when making a new version:
* Write about the changes in the ChangeLog file,
  if it is a non-minor version bump.
* tag it in git


Building outside of a git work tree
-----------------------------------

The gpb build process requires a git work tree, with tags, to get the
version numbering right, as described in the
[Version numbering section](#version-numbering). To export gpb
for building outside of a git work tree, run the
`helpers/export-from-git` script from a git work tree. The export script
will create a tar file with the version number already substituted.

In particular, the requirement on a git work tree (to get the
version number right) unfortunately also means that it does not work
to build github's automatically created release tar balls.


Related projects
----------------
* [rebar3_plugin_gpb](https://github.com/lrascao/rebar3_gpb_plugin) for
  using gpb with rebar3
* [exprotobuf](https://github.com/bitwalker/exprotobuf) for using gpb from
  [Elixir](http://elixir-lang.org)
* [enif_protobuf](https://github.com/jg513/enif_protobuf) for a NIF
  encoder/decoder
* [grpcbox](https://github.com/tsloughter/grpcbox), for creating
  grpc services (client and server)
* [grpc](https://github.com/Bluehouse-Technology/grpc) and
  [grpc_client](https://github.com/Bluehouse-Technology/grpc_client)
  for a grpc server and client


Contributing
------------

Contributions are welcome, preferably as pull requests or git patches
or git fetch requests.  Here are some guide lines:

* Use only spaces for indentation, no tabs. Indentation is 4 spaces.
* The code must fit 80 columns
* Verify that the code and documentation compiles and that tests are ok:<br/>
  `rebar clean; rebar eunit && rebar doc`<br/>
  (if you are still on rebar2, you will need to run rebar compile before eunit)
* If you add a feature, test cases are most welcome,
  so that the feature won't get lost in any future refactorization
* Use a git branch for your feature. This way, the git history will
  look better in case there is need to refetch.
* By submitting patches to this project, you agree to allow them to be
  redistributed under the project's license according to the normal
  forms and usages of the open-source community.


Version history
---------------

##### Major change in version 4.0.0: #####

The default value for the `maps_unset_optional` option has changed
to `omitted`, from `present_undefined` This concerns only code generated
with the maps (-maps) options. Projects that already set this option
explicitly are not impacted. Projects that relied on the default to be
`present_undefined` will need to set the option explicitly in order to
upgrade to 4.0.0.

For type specs, the default has changed to generate them when possible. The
option `{type_specs,false}` (-no_type) can be used to avoid generating type
specs.
