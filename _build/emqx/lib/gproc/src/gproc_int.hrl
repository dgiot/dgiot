%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% --------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% --------------------------------------------------
%%
%% @author Ulf Wiger <ulf.wiger@feuerlabs.com
%%
%% gproc_int.hrl: Shared internal definitions

-define(CATCH_GPROC_ERROR(Expr, Args),
	try Expr
	catch
	    throw:{gproc_error, GprocError} ->
		erlang:error(GprocError, Args)
	end).

-define(THROW_GPROC_ERROR(E), throw({gproc_error, E})).

%% Used to wrap operations that may fail, but we ignore the exception.
%% Use instead of catch, to avoid building a stacktrace unnecessarily.
-define(MAY_FAIL(Expr), try (Expr) catch _:_ -> '$caught_exception' end).
