# jsx (v2.9.0) #


an erlang application for consuming, producing and manipulating [json][json]. 
inspired by [yajl][yajl]

**jsx** is built via [rebar3][rebar3], [rebar][rebar] or [mix][mix] and continuous integration testing provided courtesy [travis-ci][travis]

current status: [![Build Status](https://secure.travis-ci.org/talentdeficit/jsx.png?branch=develop)](http://travis-ci.org/talentdeficit/jsx)

**jsx** is released under the terms of the [MIT][MIT] license

copyright 2010-2016 alisdair sullivan

## really important note ##

there are a few changes for users upgrading from 1.x. see [CHANGES.md](CHANGES.md)
for the overview or [migrating from 1.x](#migrating) for the details


## index ##

* [quickstart](#quickstart)
* [description](#description)
  - [migrating from 1.x](#migrating)
  - [json <-> erlang mapping](#json---erlang-mapping)
  - [incomplete input](#incomplete-input)
* [data types](#data-types)
  - [`json_term()`](#json_term)
  - [`json_text()`](#json_text)
  - [`event()`](#event)
  - [`option()`](#option)
* [exports](#exports)
  - [`encoder/3`, `decoder/3` & `parser/3`](#encoder3-decoder3--parser3)
  - [`decode/1,2`](#decode12)
  - [`encode/1,2`](#encode12)
  - [`format/1,2`](#format12)
  - [`minify/1`](#minify1)
  - [`prettify/1`](#prettify1)
  - [`is_json/1,2`](#is_json12)
  - [`is_term/1,2`](#is_term12)
  - [`maps_support/0`](#maps_support0)
* [callback exports](#callback_exports)
  - [`Module:init/1`](#moduleinit1)
  - [`Module:handle_event/2`](#modulehandle_event2)
* [acknowledgements](#acknowledgements)


## quickstart ##

#### to add to a rebar3 project ####
Add to `rebar.config`
```erlang
...
{erl_opts, [debug_info]}.
{deps, [
       ...
       {jsx, {git, "https://hub.fastgit.org/talentdeficit/jsx.git", {branch, "v2.8.0"}}}
]}.
...
```

#### to build the library and run tests ####

```bash
$ rebar3 compile
$ rebar3 eunit
$ rebar compile
$ rebar eunit
$ mix compile
$ mix eunit
```

#### to convert a utf8 binary containing a json string into an erlang term ####

```erlang
1> jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>).
[{<<"library">>,<<"jsx">>},{<<"awesome">>,true}]
2> jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>, [return_maps]).
#{<<"awesome">> => true,<<"library">> => <<"jsx">>}
3> jsx:decode(<<"[\"a\",\"list\",\"of\",\"words\"]">>).
[<<"a">>, <<"list">>, <<"of">>, <<"words">>]
```

#### to convert an erlang term into a utf8 binary containing a json string ####

```erlang
1> jsx:encode([{<<"library">>,<<"jsx">>},{<<"awesome">>,true}]).
<<"{\"library\": \"jsx\", \"awesome\": true}">>
2> jsx:encode(#{<<"library">> => <<"jsx">>, <<"awesome">> => true}).
<<"{\"awesome\":true,\"library\":\"jsx\"}">>
3> jsx:encode([<<"a">>, <<"list">>, <<"of">>, <<"words">>]).
<<"[\"a\",\"list\",\"of\",\"words\"]">>
```

#### to check if a binary or a term is valid json ####

```erlang
1> jsx:is_json(<<"[\"this is json\"]">>).
true
2> jsx:is_json("[\"this is not\"]").
false
3> jsx:is_term([<<"this is a term">>]).
true
4> jsx:is_term([this, is, not]).
false
```

#### to minify some json ####

```erlang
1> jsx:minify(<<"{
  \"a list\": [
    1,
    2,
    3
  ]
}">>).
<<"{\"a list\":[1,2,3]}">>
```

#### to prettify some json ####

```erlang
1> jsx:prettify(<<"{\"a list\":[1,2,3]}">>).
<<"{
  \"a list\": [
    1,
    2,
    3
  ]
}">>
```

#### to compile **jsx** so that it always decodes json objects to maps ####

```bash
$ JSX_FORCE_MAPS rebar3 compile
$ JSX_FORCE_MAPS mix compile
```

## description ##


**jsx** is an erlang application for consuming, producing and manipulating 
[json][json]

**jsx** follows the json [spec][rfc4627] as closely as possible with allowances for
real world usage

**jsx** is pragmatic. the json spec allows extensions so **jsx** extends the spec in a
number of ways. see the section on `strict` in [options](#option) below though

json has no official comments but this parser allows c/c++ style comments. 
anywhere whitespace is allowed you can insert comments (both `// ...` and `/* ... */`)

some particularly irresponsible json emitters leave trailing commas at the end of
objects or arrays. **jsx** allows a single trailing comma in input. multiple commas
in any posistion or a preceding comma are still errors

all **jsx** decoder input should be `utf8` encoded binaries. sometimes you get binaries
that are almost but not quite valid utf8 whether due to improper escaping or poor
encoding. **jsx** replaces invalid codepoints and poorly formed sequences with the 
unicode replacement character (`u+FFFD`) but does it's best to return something
comprehensible

json only allows keys and strings to be delimited by double quotes (`u+0022`) but
javascript allows them to be delimited by single quotes (`u+0027`) as well. **jsx**
follows javascript in this. strings that start with single quotes can contain double
quotes but must end with single quotes and must escape any single quotes they contain

json and **jsx** only recognize escape sequences as outlined in the json spec. it just
ignores bad escape sequences leaving them in strings unaltered


### migrating from 1.x ###

if you're migrating from jsx v1.x to v2.x in most cases you won't need to
make any changes to your code

support for otp 17.0's new map type is now enabled by default when compiling
via rebar for any release that supports them. jsx should still compile cleanly for
earlier releases without any user intervention

if you used any of `replaced_bad_utf8`, `single_quoted_strings`, `comments`,
`ignored_bad_escapes` or `relax` you can simply omit them from your calls to jsx,
they are all enabled by default now. if you want stricter parsing see the new
[`strict` options](#option) available

if you were using jsx to parse partial json using it's streaming features it is now
disabled by default. you'll need to pass the `stream` option to calls to jsx functions
to reenable it

support for `pre_encode` and `post_decode` has been removed. they were fragile and hard
to understand and they prevented evolution of the encoding and decoding code


### json &lt;-> erlang mapping ###

**json**                        | **erlang**
--------------------------------|--------------------------------
`number`                        | `integer()` and `float()`
`string`                        | `binary()` and `atom()`
`true`, `false` and `null`      | `true`, `false` and `null`
`array`                         | `[]` and `[JSON]`
`object`                        | `#{}`, `[{}]` and `[{binary() OR atom() OR integer(), JSON}]`
see below                       | `datetime()`

*   numbers

    javascript and thus json represent all numeric values with floats. there's no
    reason for erlang -- a language that supports arbitrarily large integers -- to
    restrict all numbers to the ieee754 range
    
    whenever possible, **jsx** will interpret json numbers that look like integers as 
    integers. other numbers will be converted  to erlang's floating point type, which
    is nearly but not quite iee754. negative zero is not representable in erlang (zero
    is unsigned in erlang and `0` is equivalent to `-0`) and will be interpreted as
    regular zero. numbers not representable are beyond the concern of this implementation,
    and will result in parsing errors

    when converting from erlang to json, floats are represented with their 
    shortest representation that will round trip without loss of precision. this 
    means that some floats may be superficially dissimilar (although 
    functionally equivalent). for example, `1.0000000000000001` will be 
    represented by `1.0`

*   strings

    json strings must be unicode encoded binaries or erlang atoms. in practice,
    because **jsx** only accepts `utf8` binaries all binary strings must be `utf8`.
    in addition to being unicode json strings restrict a number of codepoints and
    define a number of escape sequences

    json string escapes of the form `\uXXXX` will be converted to their 
    equivalent codepoints during parsing. this means control characters and 
    other codepoints disallowed by the json spec may be encountered in resulting 
    strings. the utf8 restriction means the surrogates are explicitly disallowed.
    if a string contains escaped surrogates (`u+d800` to `u+dfff`) they are
    interpreted but only when they form valid surrogate pairs. surrogates
    encountered otherwise are replaced with the replacement codepoint (`u+fffd`)

    all erlang strings are represented by **valid** `utf8` encoded binaries. the 
    encoder will check strings for conformance. badly formed `utf8` sequences may
    be replaced with the replacement codepoint (`u+fffd`) according to the unicode
    spec

    this implementation performs no normalization on strings beyond that 
    detailed here. be careful when comparing strings as equivalent strings 
    may have different `utf8` encodings

*   true, false and null

    the json primitives `true`, `false` and `null` are represented by the 
    erlang atoms `true`, `false` and `null`. surprise

*   arrays

    json arrays are represented with erlang lists of json values as described 
    in this section

*   objects

    json objects are represented by erlang proplists. json maps may also be
    encoded to json and optionally decoded to maps (via the `return_maps`
    option)
    
    the empty object has the special representation `[{}]` to differentiate it
    from the empty list. ambiguities like `[true, false]` prevent the use of
    the shorthand form of property lists using atoms as properties so all
    properties must be tuples. all keys must be encoded as in `string` or as
    atoms or integers (which will be escaped and converted to binaries for
    presentation to handlers). values should be valid json values. repeated
    keys are tolerated in json text decoded to erlang terms but are not allowed
    in erlang terms encoded to json

*   datetime

    erlang datetime tuples (`{{Year, Month, Day}, {Hour, Min, Sec}}`) as returned
    from `erlang:localtime/0` are automatically encoded as [iso8601][iso8601]
    strings and are assumed to be UTC time. no conversion is attempted of json [iso8601][iso8601] strings in decoded json


### incomplete input ###

**jsx** can handle incomplete json texts. if the option `stream` is passed to the decoder
or parser and if a partial json text is parsed, rather than returning a term from
your callback handler, **jsx** returns `{incomplete, F}` where  `F` is a function with 
an identical API to the anonymous fun returned from `decoder/3`, `encoder/3` or 
`parser/3`. it retains the internal state of the  parser at the point where input
was exhausted. this allows you to parse as you stream json over a socket or file 
descriptor, or to parse large json texts without needing to keep them entirely in
memory

however, it is important to recognize that **jsx** is conservative by default. **jsx** will 
not consider the parsing complete even when input is exhausted and the json text is
unambiguously incomplete. to end parsing call the `incomplete` function with the
argument `end_stream` (or `end_json`) like:

```erlang
1> {incomplete, F} = jsx:decode(<<"[">>, [stream]).
{incomplete,#Fun<jsx_decoder.1.122947756>}
2> F(end_stream).  % can also be `F(end_json)`
** exception error: bad argument
3> {incomplete, G} = F(<<"]">>).
{incomplete,#Fun<jsx_decoder.1.122947756>}
4> G(end_stream).  % can also be `G(end_json)`
[]
```


## data types ##

#### `json_term()` ####

```erlang
json_term() = [json_term()]
    | [{binary() | atom() | integer(), json_term()}]
    | #{} % map of any size, not just the empty map
    | true
    | false
    | null
    | integer()
    | float()
    | binary()
    | atom()
		| datetime()
```

the erlang representation of json. binaries should be `utf8` encoded, or close 
at least

#### `json_text()` ####

```erlang
json_text() = binary()
```

a utf8 encoded binary containing a json string

#### `event()` ####

```erlang
event() = start_object
    | end_object
    | start_array
    | end_array
    | {key, binary()}
    | {string, binary()}
    | {integer, integer()}
    | {float, float()}
    | {literal, true}
    | {literal, false}
    | {literal, null}
    | end_json
```

the subset of [`token()`](#token) emitted by the decoder and encoder to handlers

#### `option()` ####

```erlang
option() = dirty_strings
    | escaped_forward_slashes
    | escaped_strings
    | repeat_keys
    | stream
    | strict
    | {strict, [strict_option()]}
    | return_tail
    | uescape
    | unescaped_jsonp

strict_option() = comments
    | trailing_commas
    | utf8
    | single_quotes
    | escapes
``` 

**jsx** functions all take a common set of options. not all flags have meaning 
in all contexts, but they are always valid options. functions may have 
additional options beyond these. see 
[individual function documentation](#exports) for details

- `dirty_strings`

    json escaping is lossy; it mutates the json string and repeated application 
    can result in unwanted behaviour. if your strings are already escaped (or 
    you'd like to force invalid strings into "json" you monster) use this flag 
    to bypass escaping. this can also be used to read in **really** invalid json 
    strings. everything between unescaped quotes are passed as is to the resulting 
    string term. note that this takes precedence over any other options

- `escaped_forward_slashes`

    json strings are escaped according to the json spec. this means forward 
    slashes (solidus) are only escaped when this flag is present. otherwise they 
    are left unescaped. you may want to use this if you are embedding json 
    directly into a html or xml document

- `escaped_strings`

    by default both the encoder and decoder return strings as utf8 binaries 
    appropriate for use in erlang. escape sequences that were present in decoded 
    terms are converted into the appropriate codepoint while encoded terms are 
    unaltered. this flag escapes strings as if for output in json, removing 
    control codes and problematic codepoints and replacing them with the 
    appropriate escapes

- `stream`

    see [incomplete input](#incomplete-input)

- `strict`

    as mentioned [earlier](#description), **jsx** is pragmatic. if you're more of a
    json purist or you're really into bdsm stricter adherence to the spec is
    possible. the following restrictions are available
    
    * `comments`
    
        comments are disabled and result in a `badarg` error
    
    * `trailing_commas`
    
        trailing commas in an object or list result in `badarg` errors
    
    * `utf8`
    
        invalid codepoints and malformed unicode result in `badarg` errors

    * `single_quotes`
    
        only keys and strings delimited by double quotes (`u+0022`) are allowed. the
        single quote (`u+0027`) results in a `badarg` error
    
    * `escapes`

        escape sequences not adhering to the json spec result in a `badarg` error
    
    * `control_codes`

        control codes in strings result in `badarg` errors

    any combination of these can be passed to **jsx** by using `{strict, [strict_option()]}`.
    `strict` is equivalent to `{strict, [comments, trailing_commas, utf8, single_quotes, escapes, control_codes]}`

- `return_tail`

    upon reaching the end of a valid json term in an input stream return the term and any
    remaining bytes in the input stream as `{with_tail, term(), binary()}` where the second
    member of the tuple is the json term and the third is any remaining bytes. note that
    leading whitespace will be stripped from the tail

- `uescape`

    escape all codepoints outside the ascii range for 7 bit clean output. note
    this escaping takes place even if no other string escaping is requested (via
    `escaped_strings`)

- `unescaped_jsonp`

    javascript interpreters treat the codepoints `u+2028` and `u+2029` as 
    significant whitespace. json strings that contain either of these codepoints 
    will be parsed incorrectly by some javascript interpreters. by default, 
    these codepoints are escaped (to `\u2028` and `\u2029`, respectively) to 
    retain compatibility. this option simply removes that escaping


## exports ##


#### `encoder/3`, `decoder/3` & `parser/3` ####

```erlang
decoder(Module, Args, Opts) -> Fun((JSONText) -> any())
encoder(Module, Args, Opts) -> Fun((JSONTerm) -> any())
parser(Module, Args, Opts) -> Fun((Tokens) -> any())

  Module = atom()
  Args = any()
  Opts = [option()]
  JSONText = json_text()
  JSONTerm = json_term()
  Tokens = event() | [event()]
```

**jsx** is a json compiler with interleaved tokenizing, syntactic analysis and 
semantic analysis stages. included are two tokenizers; one that handles json 
texts (`decoder/3`) and one that handles erlang terms (`encoder/3`). there is 
also an entry point to the syntactic analysis stage for use with user-defined 
tokenizers (`parser/3`)

all three functions return an anonymous function that takes the appropriate type 
of input and returns the result of performing semantic analysis, the tuple 
`{incomplete, F}` where `F` is a new anonymous function (see 
[incomplete input](#incomplete_input)) or a `badarg` error exception if 
syntactic analysis fails

`Module` is the name of the callback module

`Args` is any term that will be passed to `Module:init/1` prior to syntactic 
analysis to produce an initial state

`Opts` are detailed [here](#option)

check out [callback module documentation](#callback_exports) for details of 
the callback module interface

#### `decode/1,2` ####

```erlang
decode(JSON) -> Term
decode(JSON, Opts) -> Term

  JSON = json_text()
  Term = json_term()
  Opts = [option() | labels | {labels, Label} | return_maps]
    Label = binary | atom | existing_atom | attempt_atom
    F = fun((any()) -> any())
```

`decode` parses a json text (a `utf8` encoded binary) and produces an erlang 
term

the option `labels` controls how keys are converted from json to
erlang terms.  `binary` (the default behavior) does no conversion
beyond normal escaping. `atom` converts keys to erlang atoms and
results in a `badarg` error if the keys fall outside the range of erlang
atoms. `existing_atom` is identical to `atom` except it will not add
new atoms to the atom table and will result in a `badarg` error if the atom
does not exist. `attempt_atom` will convert keys to atoms when they exist,
and leave them as binary otherwise

the option `return_maps` will attempt to return objects as maps instead of
proplists. this option has no effect when used with releases that do not
support maps

raises a `badarg` error exception if input is not valid json


#### `encode/1,2` ####

```erlang
encode(Term) -> JSON
encode(Term, Opts) -> JSON

  Term = json_term()
  JSON = json_text()
  Opts = [option() | space | {space, N} | indent | {indent, N}]
    N = pos_integer()
```

`encode` converts an erlang term into json text (a `utf8` encoded binary)

the option `{space, N}` inserts `N` spaces after every comma and colon in your 
json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of 
indentation in your json output. note that this overrides spaces inserted after 
a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`

raises a `badarg` error exception if input is not a valid 
[erlang representation of json](#json---erlang-mapping)


#### `format/1,2` ####

```erlang
format(JSON) -> JSON
format(JSON, Opts) -> JSON

  JSON = json_text()
  Opts = [option() | space | {space, N} | indent | {indent, N}]
    N = pos_integer()
```

`format` parses a json text (a `utf8` encoded binary) and produces a new json 
text according to the format rules specified by `Opts`

the option `{space, N}` inserts `N` spaces after every comma and colon in your 
json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of 
indentation in your json output. note that this overrides spaces inserted after 
a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`

raises a `badarg` error exception if input is not valid json


#### `minify/1` ####

```erlang
minify(JSON) -> JSON

  JSON = json_text()
```

`minify` parses a json text (a `utf8` encoded binary) and produces a new json 
text stripped of whitespace

raises a `badarg` error exception if input is not valid json


#### `prettify/1` ####

```erlang
prettify(JSON) -> JSON

  JSON = json_text()
```

`prettify` parses a json text (a `utf8` encoded binary) and produces a new json 
text equivalent to `format(JSON, [{space, 1}, {indent, 2}])`

raises a `badarg` error exception if input is not valid json


#### `is_json/1,2` ####

```erlang
is_json(MaybeJSON) -> true | false
is_json(MaybeJSON, Opts) -> true | false

  MaybeJSON = any()
  Opts = options()
```

returns true if input is a valid json text, false if not

what exactly constitutes valid json may be [altered](#option)


#### `is_term/1,2` ####

```erlang
is_term(MaybeJSON) -> true | false
is_term(MaybeJSON, Opts) -> true | false

  MaybeJSON = any()
  Opts = options()
```

returns true if input is a valid erlang representation of json, false if not

what exactly constitutes valid json may be altered via [options](#option)


#### `maps_support/0` ####

```erlang
maps_support() -> true | false
```

if **jsx** was compiled with map support enabled returns `true`, else
`false`


## callback exports ##

the following functions should be exported from a **jsx** callback module

#### `Module:init/1` ####

```erlang
Module:init(Args) -> InitialState

  Args = any()
  InitialState = any()
```

whenever any of `encoder/3`, `decoder/3` or `parser/3` are called, this function 
is called with the `Args` argument provided in the calling function to obtain 
`InitialState`

#### `Module:handle_event/2` ####

```erlang
Module:handle_event(Event, State) -> NewState

  Event = [event()]
  State = any()
  NewState = any()
```

semantic analysis is performed by repeatedly calling `handle_event/2` with a 
stream of events emitted by the tokenizer and the current state. the new state 
returned is used as the input to the next call to `handle_event/2`. the 
following events must be handled:

-   `start_object`

    the start of a json object

-   `end_object`

    the end of a json object

-   `start_array`

    the start of a json array

-   `end_array`

    the end of a json array

-   `{string, binary()}`

    a json string. it will usually be a `utf8` encoded binary. see the 
    [options](#option) for possible exceptions. note that keys are also
    json strings

-   `{integer, integer()}`

    an erlang integer (bignum)

-   `{float, float()}`

    an erlang float

-   `{literal, true}`

    the atom `true`

-   `{literal, false}`

    the atom `false`

-   `{literal, null}`

    the atom `null`

-   `end_json`

    this event is emitted when syntactic analysis is completed. you should 
    do any cleanup and return the result of your semantic analysis


## acknowledgements ##

jsx wouldn't be what it is without the contributions of [Paul J. Davis](https://hub.fastgit.org/davisp), [Lloyd Hilaiel](https://hub.fastgit.org/lloyd), [John Engelhart](https://hub.fastgit.org/johnezang), [Bob Ippolito](https://hub.fastgit.org/etrepum), [Brujo Benavides](https://hub.fastgit.org/elbrujohalcon), [Alex Kropivny](https://hub.fastgit.org/amtal), [Steve Strong](https://hub.fastgit.org/srstrong), [Michael Truog](https://hub.fastgit.org/okeuday), [Devin Torres](https://hub.fastgit.org/devinus), [fogfish](https://hub.fastgit.org/fogfish), [emptytea](https://hub.fastgit.org/emptytea), [John Daily](https://hub.fastgit.org/macintux), [Ola Bäckström](https://hub.fastgit.org/olabackstrom), [Joseph Crowe](https://hub.fastgit.org/JosephCrowe), [Patrick Gombert](https://hub.fastgit.org/patrickgombert), [Eshengazin S. Kuat](https://hub.fastgit.org/eskuat), [Max Lapshin](https://hub.fastgit.org/maxlapshin), [Bikram Chatterjee](https://hub.fastgit.org/c-bik), [Michael Uvarov](https://hub.fastgit.org/arcusfelis), [Led](https://hub.fastgit.org/Ledest) and [tvv](https://hub.fastgit.org/tvv)

[json]: http://json.org
[yajl]: http://lloyd.github.com/yajl
[MIT]: http://www.opensource.org/licenses/mit-license.html
[rebar3]: https://rebar3.org
[rebar]: https://hub.fastgit.org/rebar/rebar
[mix]: http://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html
[meck]: https://hub.fastgit.org/eproxus/meck
[rfc4627]: http://tools.ietf.org/html/rfc4627
[travis]: https://travis-ci.org/
[jsxn]: https://hub.fastgit.org/talentdeficit/jsxn
[iso8601]: http://www.iso.org/iso/iso8601
