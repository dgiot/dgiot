-ifndef(gpb_codegen_hrl).
-define(gpb_codegen_hrl, true).

-compile({parse_transform,gpb_codegen}).

-define(case_clause(Clause),
        gpb_codegen:case_clause(case dummy of Clause end)).

-define(case_clause(Clause, Transforms), % FIXME; Transforms gets misindented
        gpb_codegen:case_clause(case dummy of Clause end,
                                Transforms)).

-define(fn_clause(FunExpr),
        gpb_codegen:fn_clause(FunExpr)).

-define(fn_clause(FunExpr, Transforms),
        gpb_codegen:fn_clause(FunExpr, Transforms)).

-define(if_clause(Clause),
        gpb_codegen:if_clause(if Clause end)).

-define(if_clause(Clause, Transforms), % FIXME; Transforms gets misindented
        gpb_codegen:if_clause(if Clause end, Transforms)).

-define(receive_clause(Clause),
        gpb_codegen:receive_clause(receive Clause end)).

-define(receive_clause(Clause, Transforms), % FIXME; Transforms gets misindented
        gpb_codegen:receive_clause(receive Clause end, Transforms)).

-define(expr(X),
        gpb_codegen:expr(X)).

-define(expr(X, Transforms),
        gpb_codegen:expr(X, Transforms)).

-define(exprs(X1, Transforms),
        gpb_codegen:exprs(X1, Transforms)).

-define(exprs(X1, X2, Transforms),
        gpb_codegen:exprs(X1, X2, Transforms)).

-define(exprs(X1, X2, X3, Transforms),
        gpb_codegen:exprs(X1, X2, X3, Transforms)).

-define(exprs(X1, X2, X3, X4, Transforms),
        gpb_codegen:exprs(X1, X2, X3, X4, Transforms)).

-define(exprs(X1, X2, X3, X4, X5, Transforms),
        gpb_codegen:exprs(X1, X2, X3, X4, X5, Transforms)).

-endif. %% gpb_codegen_hrl.
