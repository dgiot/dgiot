%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(rest_api_books).

-author("Feng Lee <feng@emqx.io>").

-rest_api(#{name   => list_books,
            method => 'GET',
            path   => "/books/",
            func   => list,
            descr  => "List books"}).

-rest_api(#{name   => get_book,
            method => 'GET',
            path   => "/books/:int:id",
            func   => get,
            descr  => "Get book by Id"}).

-rest_api(#{name   => put_book,
            method => 'PUT',
            path   => "/books/:int:id",
            func   => put,
            descr  => "Put book name by Id"}).

-rest_api(#{name   => error_api,
            method => 'DELETE',
            path   => "/books/:int:id",
            func   => delete,
            descr  => "delete book name by Id"}).

-export([list/2, get/2, put/2, delete/2]).

list(_Bindings, _Params) ->
    Books = [#{id => I, name => list_to_binary("book" ++ integer_to_list(I))}
             || I <- lists:seq(1, 100)],
    {ok, Books}.

get(#{id := Id}, _Params) ->
    {200, #{id => Id, name => list_to_binary("book" ++ integer_to_list(Id))}}.

put(#{id := Id}, Params) ->
    Name = proplists:get_value(<<"name">>, Params),
    case Name of
        <<"ok">> ->
            ok;
        <<"error">> ->
            {error, Id}
    end.

delete(#{id := Id}, _Params) ->
    {error, "error_api"}.
