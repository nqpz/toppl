append(X, List, list(X, List)).

lookup(X, list(X, _)).
lookup(X, list(_Head, List)) :-
    lookup(X, List).

% eval/3
eval(true, _Ctx, true).
eval(false, _Ctx, false).
eval(val(X), _Ctx, val(X)).
eval(var(X), Ctx, Res) :-
    lookup(binding(X, Res), Ctx).
eval(let(Var, Val, NextExp), Ctx, Res) :-
    append(binding(Var, Val), Ctx, Ctx1),
    eval(NextExp, Ctx1, Res).
eval(list(X, Term), Ctx, list(X1, Term1)) :-
    eval(X, Ctx, X1),
    eval(Term, Ctx, Term1).
eval(list_empty, _Ctx, list_empty).
eval(if(Cond, Texp, _Fexp), Ctx, Res) :-
    eval(Cond, Ctx, true),
    eval(Texp, Ctx, Res).
eval(if(Cond, _Texp, Fexp), Ctx, Res) :-
    eval(Cond, Ctx, false),
    eval(Fexp, Ctx, Res).

% eval/2
eval(Exp, Res) :- eval(Exp, list_empty, Res).

test_eval0(Res) :-
    eval(list(val(stuff), list_empty), Res).

test_eval1(Res) :-
    eval(let(foo, val(bar), var(foo)), Res).

test_eval2(Res) :-
    eval(let(foo, val(bar),
             list(val(baz), list(var(foo), list_empty))), Res).

test_eval3(Res) :-
    eval(if(true, val(a), val(b)), Res).

test_eval4(Res) :-
    eval(let(x, list(s, list(val(z), list_empty)),
             let(y, true,
                 if(var(y),
                    list(val(s), var(x)),
                    list(val(s), list(val(s), var(x))))
             )
    ),
         Res).
