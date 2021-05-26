-module(provider).

-export([]).

-ifdef(no_callback_support).

%% In the case where R14 or lower is being used to compile the system
%% we need to export a behaviour info
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{init, 1},
     {do, 1},
     {format_error, 1}];
behaviour_info(_) ->
    undefined.

-else.

-callback init(any()) -> {ok, any()}.
-callback do(any()) -> {ok, any()} | {error, string()} | {error, {module(), any()}}.
-callback format_error(any()) -> iolist().

-endif.
