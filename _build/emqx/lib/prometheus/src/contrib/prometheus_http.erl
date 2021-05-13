%% @doc HTTP instrumentation helpers
-module(prometheus_http).

-export([microseconds_duration_buckets/0,
         status_class/1]).

-export_type([status_code/0,
              status_class/0]).

%%====================================================================
%% Types
%%====================================================================

-type status_code() :: pos_integer().
-type status_class() :: prometheus_model_helpers:label_value().

%%====================================================================
%% Public API
%%====================================================================

%% @doc
%% Returns default microseconds buckets for measuring http requests duration.
%%
%% <pre lang="erlang">
%% 1> prometheus_http:microseconds_duration_buckets().
%% [10, 25, 50, 100, 250, 500,
%%  1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
%%  1000000, 2500000, 5000000, 10000000]
%% </pre>
%% @end
-spec microseconds_duration_buckets() -> prometheus_buckets:buckets().
microseconds_duration_buckets() ->
  [10, 25, 50, 100, 250, 500,
   1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
   1000000, 2500000, 5000000, 10000000].

%% @doc
%% Returns status class for the http status code `SCode'.
%%
%% <pre lang="erlang">
%% 2> prometheus_http:status_class(202).
%% "success"
%% </pre>
%%
%% Raises `{invalid_value_error, SCode, Message}' error if `SCode'
%% isn't a positive integer.
%% @end
-spec status_class(SCode) -> StatusClass when
    SCode :: status_code(),
    StatusClass :: status_class().
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 100 ->
  "unknown";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 200 ->
  "informational";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 300 ->
  "success";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 400 ->
  "redirection";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 500 ->
  "client-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 600 ->
  "server-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode >= 600 ->
  "unknown";
status_class(C) ->
  erlang:error({invalid_value, C, "status code must be a positive integer"}).
