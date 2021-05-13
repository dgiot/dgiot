-module(jpparse_fold).

-export([foldtd/3, foldbu/3, string/1, roots/1]).

% Folds a json path parse tree into a string that represents a same jppath from
% which the parse tree was originally constructed
-spec string(tuple()) -> {ok, binary()}.
string(Pt) ->
    {ok, list_to_binary(foldbu(fun stringfun/3, [], Pt))}.

stringfun(_Depth, {'fun',_,Args} = _Pt, Stk) ->
    {PArgs, [A|Rest]} = lists:split(length(Args), Stk),
    [string:join([A, "(", string:join(lists:reverse(PArgs), ","), ")"], "")
     | Rest];
stringfun(_Depth, {Op,_,Args} = _Pt, Stk) when Op =:= '[]'; Op =:= '{}' ->
    {PArgs, [A|Rest]} = lists:split(length(Args), Stk),
    {Lb,Rb} = case Op of
                  '[]' -> {"[","]"};
                  '{}' -> {"{","}"}
              end,
    [string:join([A, Lb, string:join(lists:reverse(PArgs), ","), Rb], "")
     | Rest];
stringfun(_Depth, {O,_,_} = _Pt, Stk) when O =:= ':'; O =:= '::'; O =:= '#' ->
    [B,A|Rest] = Stk,
    [string:join([A,atom_to_list(O),B], "")|Rest];
stringfun(_Depth, Pt, Stk) when is_binary(Pt) ->
    [binary_to_list(Pt)|Stk];
stringfun(_Depth, Pt, Stk) when is_integer(Pt) ->
    [integer_to_list(Pt)|Stk].

% Folds a json path parse tree returning path roots
%  as sorted array
-spec roots(tuple()) -> {ok, list()}.
roots(Pt) ->
    case foldbu(fun rootsfun/3, [], Pt) of
        {error, _} = Error -> Error;
        Folded -> {ok, lists:usort(Folded)}
    end.

rootsfun(_Depth, {':',_,R},  Rs) when is_binary(R) -> [R|Rs];
rootsfun(_Depth, {'::',_,R}, Rs) when is_binary(R) -> [R|Rs];
rootsfun(_Depth, {'#',_,R},  Rs) when is_binary(R) -> [R|Rs];
rootsfun(_Depth, {'[]',R,_}, Rs) when is_binary(R) -> [R|Rs];
rootsfun(_Depth, {'{}',R,_}, Rs) when is_binary(R) -> [R|Rs];
rootsfun(_Depth, _Pt, Rs) -> Rs.

% Folds a jppath parsee tree applying fold function
%  in a Top-Down walk
-spec foldtd(Function :: fun((Depth :: integer(), SubParseTree :: any(),
                              AccIn :: any()) -> AccOut :: any()),
             Acc :: any(),
             ParseTree :: tuple()) -> any().
foldtd(Fun, Acc, Pt) when is_function(Fun, 3) ->
    fold_i({td,Fun}, Acc, Pt).

% Folds a jppath parsee tree applying fold function
%  in a Boottom-Up walk
-spec foldbu(Function :: fun((Depth :: integer(), SubParseTree :: any(),
                              AccIn :: any()) -> AccOut :: any()),
             Acc :: any(),
             ParseTree :: tuple()) -> any().
foldbu(Fun, Acc, Pt) when is_function(Fun, 3) ->
    fold_i({bu,Fun}, Acc, Pt).

% Internal fold function that walks the jpparse parse
%  tree recursively applying the fold function in
%  top-down or bottom-up manner
-define(TD(__L,__Pt,__AccIn),
        if T =:= td -> Fun(__L,__Pt,__AccIn); true -> __AccIn end).
-define(BU(__L,__Pt,__AccIn),
        if T =:= bu -> Fun(__L,__Pt,__AccIn); true -> __AccIn end).
fold_i(Fun, Acc, Pt) ->
    case catch fold_i(Fun, Acc, Pt, 0) of
        {'EXIT',Error} -> {error, Error};
        Result -> Result
    end.
fold_i({T,Fun}, Acc, B, Lvl) when is_binary(B); is_integer(B) ->
    Acc1 = ?TD(Lvl,B,Acc),
    ?BU(Lvl,B,Acc1);
fold_i({T,Fun}, Acc, {O, R, L}, Lvl) when O =:= '::'; O =:= ':'; O =:= '#' ->
    Acc1 = ?TD(Lvl,{O, R, L},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, L, Lvl+1),
    Acc3 = fold_i({T,Fun}, Acc2, R, Lvl+1),
    ?BU(Lvl,{O, R, L},Acc3);
fold_i({T,Fun}, Acc, {Op, L, R}, Lvl)
  when ((Op =:=  '{}') orelse (Op =:=  '[]')) andalso is_list(R) ->
    Acc1 = ?TD(Lvl,{Op, L, R},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, L, Lvl+1),
    Acc3 = lists:foldl(fun(Ri, Acci) ->
                               fold_i({T,Fun}, Acci, Ri, Lvl+1)
                       end, Acc2, R),
    ?BU(Lvl,{Op, L, R},Acc3);
fold_i({T,Fun}, Acc, {'fun',Fn,Args}, Lvl) when is_binary(Fn), is_list(Args) ->
    Acc1 = ?TD(Lvl,{'fun', Fn, Args},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, Fn, Lvl+1),
    Acc3 = lists:foldl(fun(Arg, Acci) ->
                               fold_i({T,Fun}, Acci, Arg, Lvl+1)
                       end, Acc2, Args),
    ?BU(Lvl,{'fun',Fn,Args},Acc3);
fold_i({T,Fun}, Acc, empty, Lvl) ->
    Acc1 = ?TD(Lvl,<<>>,Acc),
    ?BU(Lvl,<<>>,Acc1);
fold_i(_, _Acc, Pt, _) ->
    exit({unsupported, Pt}).

%%-----------------------------------------------------------------------------
