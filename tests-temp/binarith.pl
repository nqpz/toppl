% XXX: This does not fully work yet 'test_add0/0' works, but multiplication and
% division does not.  Also, there is an easy risk of eating up all memory both
% in the interpreter and in the compiled program.

no_trailing_zeros(list(one, list_empty)).
no_trailing_zeros(list(_, list(T, Tail))) :-
    no_trailing_zeros(list(T, Tail)).

nonzero(list(_, _)).

less_unsigned(M, N) :- sub_unsigned(N, M, list(_, _)).

leq_unsigned(M, N) :- less_unsigned(M, N).
leq_unsigned(M, M).

add_unsigned(list_empty, list_empty, list_empty).
add_unsigned(M, list_empty, M) :- nonzero(M).
add_unsigned(list_empty, N, N) :- nonzero(N).
add_unsigned(M, N, Res) :-
    nonzero(M),
    nonzero(N),
    add_unsigned(M, N, zero, Res),
    no_trailing_zeros(M),
    no_trailing_zeros(N),
    no_trailing_zeros(Res).

add_unsigned(list(MBit, M), list(NBit, N), Carry, list(ResBit, Sum)) :-
    add_bit(MBit, NBit, Carry, ResBit, Carry1),
    add_unsigned(M, N, Carry1, Sum).
add_unsigned(list_empty, list(NBit, N), Carry, list(ResBit, Sum)) :-
    add_bit(zero, NBit, Carry, ResBit, Carry1),
    add_unsigned(list_empty, N, Carry1, Sum).
add_unsigned(list(MBit, M), list_empty, Carry, list(ResBit, Sum)) :-
    add_bit(MBit, zero, Carry, ResBit, Carry1),
    add_unsigned(M, list_empty, Carry1, Sum).
add_unsigned(list_empty, list_empty, zero, list_empty).
add_unsigned(list_empty, list_empty, one, list(one, list_empty)).

add_bit(zero, zero, zero, zero, zero).
add_bit(zero, zero, one, one, zero).
add_bit(zero, one, zero, one, zero).
add_bit(zero, one, one, zero, one).
add_bit(one, zero, zero, one, zero).
add_bit(one, zero, one, zero, one).
add_bit(one, one, zero, zero, one).
add_bit(one, one, one, one, one).

sub_unsigned(M, N, Res) :- add_unsigned(N, Res, M).

mul_unsigned(_, list_empty, list_empty).
mul_unsigned(list_empty, _, list_empty).
mul_unsigned(M, N, Res) :-
    leq_unsigned(M, Res),
    leq_unsigned(N, Res),
    mul_unsigned1(M, N, Res).

mul_unsigned1(_, list_empty, list_empty).
mul_unsigned1(M, N, Res) :-
    floored_power_of_two(N, POT),
    mul_pow(M, list(one, list_empty), POT, Res1),
    sub_unsigned(N, POT, N1),
    mul_unsigned1(M, N1, Res2),
    add_unsigned(Res1, Res2, Res).

floored_power_of_two(list_empty, list_empty).
floored_power_of_two(list(_, list_empty), list(one, list_empty)).
floored_power_of_two(list(_, list(T, Tail)), list(zero, Tail1)) :-
    floored_power_of_two(list(T, Tail), Tail1).

mul_pow(M, Mtimes, POT, M) :-
    sub_unsigned(POT, Mtimes, list_empty).
mul_pow(M, Mtimes, POT, Res) :-
    sub_unsigned(POT, Mtimes, list(_, _)),
    add_unsigned(M, M, M2),
    mul_pow(M2, list(zero, Mtimes), POT, Res).

divmod_unsigned(M, N, Div, Mod) :-
    less_unsigned(Mod, N),
    add_unsigned(T, Mod, M),
    mul_unsigned(Div, N, T).

div_unsigned(M, N, Div) :- divmod_unsigned(M, N, Div, _).
mod_unsigned(M, N, Mod) :- divmod_unsigned(M, N, _, Mod).

% Example numbers
n_0(list_empty).
n_1(list(one, list_empty)).
n_2(list(zero, list(one, list_empty))).
n_10(list(zero, list(one, list(zero, list(one, list_empty))))).
n_17(list(one, list(zero, list(zero, list(zero, list(one, list_empty)))))).
n_27(list(one, list(one, list(zero, list(one, list(one, list_empty)))))).
n_170(list(zero, list(one, list(zero, list(one, list(zero, list(one, list(zero, list(one, list_empty))))))))).

test_add0 :-
    n_10(N10),
    n_17(N17),
    n_27(N27),
    add_unsigned(N10, N17, N27).

test_mul0 :-
    n_10(N10),
    n_17(N17),
    n_170(N170),
    mul_unsigned(N10, N17, N170).

test_div0 :-
    n_27(N27),
    n_10(N10),
    n_2(N2),
    div_unsigned(N27, N10, N2).
