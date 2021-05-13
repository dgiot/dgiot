v2.8.2

* enable `debug_info` for rebar3

v2.8.1

* enable `debug_info` when used via mix
* accept `erlang:timestamp` as input to the parser


v2.8.0

* add `JSX_FORCE_MAPS` env var for forcing decoding to maps rather than
  attempting to autodetect

v2.7.2

* fix an issue where tuples were assumed to be jsx ast and not checked
* mask a `function_clause` error in encoder with a `badarg` error for api unity

v2.7.1

* support for milliseconds in datetimes

v2.7.0

* `return_tail` option
* fixes for edoc generation

v2.6.2

* ensure maps are always enabled when compiling via mix

v2.6.1

* hex.pm maintenance release

v2.6.0

* equivalent to v2.5.3 but created for semver reasons

v2.5.3

* add a `mix.exs` to be buildable by both mix and rebar
* minor README updates

v2.5.2

* fix regression parsing <<"-0e...">> (thanks @c-bik)

v2.5.1

* assume all datetimes are UTC time and add `Z` designator to indicate
* fix parsing issue with datetimes in arrays

v2.5.0

* `consult/2` function for reading a file directly to a json term
* `maps_always` build flag for always returning maps on platforms
  that support them
* dialyzer fixes

v2.4.0

* enough performance improvements to justify a new version. 2-3x
  speedup depending on mode of operation

v2.3.1

* fixes an issue where astral plane json escape sequences were
  inadvertently being converted to the unicode replacement
  character

v2.3

* switched to a faster implementation of string parsing in both
  the decoder and encoder
* expand `uescape` option to the decoder
* allow control codes in json passed to decoder (contrary to the spec,
  yes)

v2.2

* `return_maps` option
* `uescape` option for 7-bit clean output
* add `Makefile` for slightly better `erlang.mk` compatibility
* add `maps_support/0` call to determine whether `jsx` was compiled
  with support for maps or not

v2.1.1

* faster generation of json via iolists
* `repeat_keys` option

v2.1

* force the end of streams with `end_json` in addition to `end_stream`
* support for encoding erlang datetime tuples to iso8601 format
* allow a single trailing comma in objects and arrays

v2.0.4

* more typespec adjustments

v2.0.3

* update some typespecs to make them more comprehensive

v2.0.2

* fixes travis-ci spec

v2.0.1

* fix regression in output of empty objects/arrays

v2.0

* jsx is much more pragmatic by default; common json errors are silently
    ignored (and fixed). stricter parsing must be enabled with options
* add support for encoding otp 17.0's new maps data type
* removed `pre_encode` and `post_decode` options in favour of making jsx
    functions easier to wrap and customize
* streaming behavior is now disabled by default and must be requested explicitly
* removed deprecated function names (`to_json`, `to_term`, `term_to_json`, etc) 
* expanded test coverage
    

v1.4.5

* various fixes to typespecs uncovered by dialyzer
* allow integer keys during encoding
* convert atoms (other than `true`, `false` and `null`) to strings during encoding

v1.4.4

* typespec for `json_term/0` fixed
* incorrect boolean shortcircuiting fixed in multibyte escape processing

v1.4.3

* add empty rebar.config for mix build tool
* add `attempt_atom` option for decoding json objects
* fix a bug related to multibyte codepoints and streaming input
* add a missing error state in the encoder

v1.4.2

* build apparatus cleaned up and streamlined
* new `{raw, <<"json goes here">>}` intermediate form to support direct generation of json
* bugfixes involving inappropriate exceptions from jsx functions

v1.4.1

* fixes a bug with interaction between `dirty_strings` and even numbers of escape characters
* performance enhancements

v1.4

* radically refactored decoder
* `dirty_strings` now behaves intuitively in decoding. bad codepoints, bad utf8, illegal characters and escapes (except `"` and `'` if `single_quoted_strings` is enabled) are ignored completely
* `incomplete_handler` & `error_handler` are now available for use, see documentation in README

v1.3.3

* `pre_encode` now orders input in the order you'd expect

v1.3.2

* `pre_encode` is now able to handle tuples *correctly*

v1.3.1

* `pre_encode` is now able to handle tuples

v1.3

* introduces `prettify/1` and `minify/1`, shortcuts for `format/2`
* introduce `encode/1,2` and `decode/1,2` as primary interface to built in tokenizers. `to_json/1,2` and `to_term/1,2` remain accessible but not advertised
* new `parser/3` function exposes syntactic analysis stage for use with user defined tokenizers
* improved documentation

v1.2.1

* fixes incorrect handling of escaped forward slashes, thanks bob ippolito

v1.2

* rewritten handling of string escaping to improve performance
* `pre_encode` and `post_decode` hooks, see README
* `relax` option

v1.1.2

* add `dirty_strings` option
* more fixes for invalid unicode in strings

v1.1.1

* fixes bug regarding handling of invalid unicode in R14Bxx

v1.1

* improvements to string escaping and json generation performance

v1.0.2

* fixes to function specs
* rewritten README
* `comments` option

v1.0.1

* rebar fix
