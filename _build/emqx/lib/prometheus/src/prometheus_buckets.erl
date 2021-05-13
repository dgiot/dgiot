-module(prometheus_buckets).

-export([default/0,
         exponential/3,
         linear/3]).

-export_type([bucket_bound/0,
              buckets/0]).

%%====================================================================
%% Types
%%====================================================================

-type bucket_bound() :: number() | infinity.
-type buckets() :: [bucket_bound(), ...].

%%====================================================================
%% Public API
%%====================================================================

%% @doc
%% Default histogram buckets.
%% <pre lang="erlang">
%% 1> prometheus_buckets:default().
%% [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]
%% </pre>
%% Please note these buckets are floats and represent seconds so you'll
%% have to use {@link prometheus_histogram:dobserve/3} or
%% configure `duration_unit` as `seconds'.
%% @end
-spec default() -> buckets().
default() -> [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10].

%% @doc
%% Creates `Count' buckets, where the lowest bucket has an
%% upper bound of `Start' and each following bucket's upper bound is `Factor'
%% times the previous bucket's upper bound. The returned list is meant to be
%% used for the `buckets' key of histogram constructors options.
%% <pre lang="erlang">
%% 3> prometheus_buckets:exponential(100, 1.2, 3).
%% [100, 120, 144]
%% </pre>
%% The function raises `{invalid_value, Value, Message}' error if `Count'
%% isn't positive, if `Start' isn't positive, or if `Factor' is less than or
%% equals to 1.
%% @end
-spec exponential(number(), number(), pos_integer()) -> buckets().
exponential(_Start, _Factor, Count) when Count < 1 ->
  erlang:error({invalid_value, Count, "Buckets count should be positive"});
exponential(Start, _Factor, _Count) when Start =< 0 ->
  erlang:error({invalid_value, Start, "Buckets start should be positive"});
exponential(_Start, Factor, _Count) when Factor =< 1 ->
  erlang:error({invalid_value, Factor,
                "Buckets factor should be greater than 1"});
exponential(Start, Factor, Count) ->
  [try_to_maintain_integer_bounds(Start*math:pow(Factor, I)) ||
    I <- lists:seq(0, Count-1)].

%% @doc
%% Creates `Count' buckets, each `Width' wide, where the lowest
%% bucket has an upper bound of `Start'. The returned list is meant to be
%% used for the `buckets' key of histogram constructors options.
%% <pre lang="erlang">
%% 2> prometheus_buckets:linear(10, 5, 6).
%% [10, 15, 20, 25, 30, 35]
%% </pre>
%% The function raises `{invalid_value, Value, Message}' error if `Count'
%% is zero or negative.
%% @end
-spec linear(number(), number(), pos_integer()) -> buckets().
linear(_Start, _Step, Count) when Count < 1 ->
  erlang:error({invalid_value, Count, "Buckets count should be positive"});
linear(Start, Step, Count) ->
  linear(Start, Step, Count, []).

%%====================================================================
%% Private Parts
%%====================================================================

linear(_Current, _Step, 0, Acc) ->
  lists:reverse(Acc);
linear(Current, Step, Count, Acc) ->
  linear(try_to_maintain_integer_bounds(Current + Step),
         Step,
         Count - 1,
         [Current|Acc]).

-spec try_to_maintain_integer_bounds(integer()) -> integer();
                                    (float())   -> integer() | float().
try_to_maintain_integer_bounds(Bound) when is_integer(Bound) -> Bound;
try_to_maintain_integer_bounds(Bound) when is_float(Bound) ->
  TBound = trunc(Bound),
  case TBound == Bound of
    true  -> TBound;
    false -> Bound
  end.
