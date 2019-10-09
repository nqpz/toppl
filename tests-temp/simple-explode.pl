% 'happy/1' will be inlined into a predicate with 8192 rules that all succeed.
% This is very simple, and seems to slightly outperform swipl in runtime, but
% it's also very slow to compile and pretty stupid.

happy(X) :-
    h1(X).
happy(X) :-
    h1(X).

h1(X) :-
    h2(X).
h1(X) :-
    h2(X).

h2(X) :-
    h3(X).
h2(X) :-
    h3(X).

h3(X) :-
    h4(X).
h3(X) :-
    h4(X).

h4(X) :-
    h5(X).
h4(X) :-
    h5(X).

h5(X) :-
    h6(X).
h5(X) :-
    h6(X).

h6(X) :-
    h7(X).
h6(X) :-
    h7(X).

h7(X) :-
    h8(X).
h7(X) :-
    h8(X).

h8(X) :-
    h9(X).
h8(X) :-
    h9(X).

h9(X) :-
    h10(X).
h9(X) :-
    h10(X).

h10(X) :-
    h11(X).
h10(X) :-
    h11(X).

h11(X) :-
    h12(X).
h11(X) :-
    h12(X).

h12(X) :-
    h13(X).
h12(X) :-
    h13(X).

h13(h).
