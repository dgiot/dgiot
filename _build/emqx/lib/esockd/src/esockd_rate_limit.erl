%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc Token-bucket based Rate Limit.
%%
%% [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket).
%%
%% @end
-module(esockd_rate_limit).

-export([ new/1
        , new/2
        , info/1
        , check/2
        ]).

-export_type([config/0, bucket/0]).

-record(bucket, {
          rate   :: float(),
          burst  :: pos_integer(),
          tokens :: non_neg_integer(),
          time   :: pos_integer()
         }).

-opaque(bucket() :: #bucket{}).

-opaque(config() :: {float()|pos_integer(), pos_integer()}).

%% @doc Create a rate limit. The time unit of rate is second.
-spec(new(config()) -> bucket()).
new({Rate, Burst}) -> new(Rate, Burst).

-spec(new(float()|pos_integer(), pos_integer()) -> bucket()).
new(Rate, Burst) when is_integer(Burst), 0 < Rate andalso Rate =< Burst ->
    #bucket{rate   = Rate,
            burst  = Burst,
            tokens = Burst,
            time   = erlang:system_time(milli_seconds)
           }.

-spec(info(bucket()) -> map()).
info(#bucket{rate = Rate, burst = Burst, tokens = Tokens, time = Lastime}) ->
    #{rate   => Rate,
      burst  => Burst,
      tokens => Tokens,
      time   => Lastime
     }.

-spec(check(pos_integer(), bucket()) -> {non_neg_integer(), bucket()}).
check(Tokens, Bucket) ->
    check(Tokens, erlang:system_time(milli_seconds), Bucket).

-spec(check(pos_integer(), integer(), bucket()) -> {non_neg_integer(), bucket()}).
check(Tokens, Now, Bucket = #bucket{rate   = Rate,
                                    burst  = Burst,
                                    tokens = Remaining,
                                    time   = Lastime}) ->
    Limit = min(Burst, Remaining + round((Rate * (Now - Lastime)) / 1000)),
    case Limit >= Tokens of
        true  -> %% Tokens available
            {0, Bucket#bucket{tokens = Limit - Tokens, time = Now}};
        false -> %% Tokens not enough
            Pause = round((Tokens - Remaining) * 1000 / Rate),
            {Pause, Bucket#bucket{tokens = 0, time = Now}}
    end.

