% @private
% Excerpt from https://github.com/erlang/otp/blob/OTP-20.0.1/lib/stdlib/src/filename.erl#L761-L788
% with modifications for changing local function calls to remote function calls
% to the `filename` module, for the functions `pathtype/1`, `split/1`, and `join/1`
%
% safe_relative_path/1 was not present in earlier OTP releases.

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%

-module(hex_filename).
-export([safe_relative_path/1]).

safe_relative_path(Path) ->
    case filename:pathtype(Path) of
        relative ->
            Cs0 = filename:split(Path),
            safe_relative_path_1(Cs0, []);
        _ ->
            unsafe
    end.

safe_relative_path_1(["."|T], Acc) ->
    safe_relative_path_1(T, Acc);
safe_relative_path_1([<<".">>|T], Acc) ->
    safe_relative_path_1(T, Acc);
safe_relative_path_1([".."|T], Acc) ->
    climb(T, Acc);
safe_relative_path_1([<<"..">>|T], Acc) ->
    climb(T, Acc);
safe_relative_path_1([H|T], Acc) ->
    safe_relative_path_1(T, [H|Acc]);
safe_relative_path_1([], []) ->
    [];
safe_relative_path_1([], Acc) ->
    filename:join(lists:reverse(Acc)).

climb(_, []) ->
    unsafe;
climb(T, [_|Acc]) ->
    safe_relative_path_1(T, Acc).
