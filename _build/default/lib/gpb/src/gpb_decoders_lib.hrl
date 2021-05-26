-ifndef(gpb_decoders_lib_hrl).
-define(gpb_decoders_lib_hrl, true).

-record(fn, {
          name :: atom(),
          initializes_fields   = false :: boolean(),
          has_finalizer        = false :: boolean(),
          fields_in_tail_calls = false :: boolean(),
          passes_msg           = false :: boolean(),
          tree   % the syntax tree
         }).

-endif. % gpb_decoders_lib_hrl
