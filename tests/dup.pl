dup(zero, zero).
dup(succ(X), succ(succ(Y))) :-
    dup(X, Y).
