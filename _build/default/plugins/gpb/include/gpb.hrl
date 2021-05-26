-ifndef(gpb_hrl).
-define(gpb_hrl, true).

-type gpb_scalar() ::
        int32 | int64 | uint32 | uint64 | sint32 | sint64
        | fixed32 | fixed64 | sfixed32 | sfixed64
        | bool
        | float | double
        | string
        | bytes.

-type gpb_map_key() :: % "any scalar type except floating point types and bytes"
        int32 | int64 | uint32 | uint64 | sint32 | sint64
        | fixed32 | fixed64 | sfixed32 | sfixed64
        | bool
        | string.

%% It is not possible to have maps in maps directly,
%% so this type is any gpb_field_type() except {map,K,V}.
-type gpb_map_value() ::
        gpb_scalar()
        | {enum,atom()}
        | {msg,atom()}.

-type gpb_field_type() ::     %% Erlang type  Comment
        int32 | int64         % integer()    variable-length encoded
        | uint32 | uint64     % integer()    variable-length encoded
        | sint32 | sint64     % integer()    variable-length zig-zag encoded
        | fixed32 | fixed64   % integer()    always 4 | 8 bytes on wire
        | sfixed32 | sfixed64 % integer()    always 4 | 8 bytes on wire
        | bool                % true | false
        | float | double      % float()
        | string              % string()     UTF-8 encoded
                              % | binary()   iff option `strings_as_binaries'
        | bytes               % binary()
        | {enum,atom()}       % atom()       the enum literal is the atom
        | {msg,atom()}        % record()     the message name is record name
                              % | map()      iff option `maps'
        | {group,atom()}      % record()     name is <msg name>_<field name>
                              % | map()      iff option `maps'
        | {map,gpb_map_key(),gpb_map_value()}. % [{K,V}] | map()

%% An intermediary type temporarily used internally within gpb during parsing,
%% neither returned from gpb, nor accepted as input go gpb.
-type gpb_internal_intermediary_ref() ::
        {ref, term()} |
        {msg, list()} |
        {group, list()} |
        {enum, list()}.

-type gpb_internal_intermediary_map_ref() ::
        {map, gpb_map_key(), gpb_map_value() | gpb_internal_intermediary_ref()}.

%% The following two definitions (`gpb_field' and `gpb_rpc') are to
%% avoid clashes with other code, since the `field' and `rpc' are
%% really too general names, they should have been prefixed.
%%
%% Unfortunately, they are already part of the API, so they can't
%% be changed without breaking backwards compatibility.
%% (They appear as parameters or return values for functions in `gpb'
%% in generated code.)
%%
%% In case a clash, it is possible to redefine the name locally.
%% The recommendation is to redefine them with prefix, ie to `gpb_field'
%% and `gpb_rpc', since this is what they will change to in some future.
%%
-ifdef(gpb_field_record_name).
-define(gpb_field, ?gpb_field_record_name).
-else.
-define(gpb_field, field). %% odd definition is due to backwards compatibility
-endif.

-ifdef(gpb_rpc_record_name).
-define(gpb_rpc, ?gpb_rpc_record_name).
-else.
-define(gpb_rpc, rpc). %% odd definition is due to backwards compatibility
-endif.

-record(?gpb_field, % NB: record name is (currently) `field' (not `gpb_field')!
        {name               :: atom()
                             | undefined, % temporarily in some phases
         fnum               :: integer()
                             | undefined, % temporarily in some phases
         rnum               :: pos_integer() % field number in the record
                               | undefined,  % temporarily, during parsing
         type               :: gpb_field_type() |
                               gpb_internal_intermediary_ref() |
                               gpb_internal_intermediary_map_ref()
                             | undefined, % temporarily in some phases
         occurrence         :: 'required' | 'optional' | 'repeated'
                             | undefined, % temporarily in some phases
         opts      = []     :: [term()]
        }).

-record(gpb_oneof,
        {name   :: atom()
                 | undefined,      % temporarily in some phases
         rnum   :: pos_integer()   % field number in the record
                 | undefined,      % temporarily, during parsing
         fields :: [#?gpb_field{}] % all fields have the same rnum
                 | undefined       % temporarily in some phases
        }).

-record(?gpb_rpc, % NB: record name is (currently) `rpc' (not `gpb_rpc')!
        {name               :: atom()
                             | undefined, % temporarily in some phases
         input,
         output,
         input_stream       :: boolean()
                             | undefined, % temporarily in some phases
         output_stream      :: boolean()
                             | undefined, % temporarily in some phases
         opts               :: [term()]
                             | undefined  % temporarily in some phases
        }).

-endif.
