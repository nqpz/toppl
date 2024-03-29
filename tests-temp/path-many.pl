select(X, list(X, Tail), Tail).
select(X, list(Y, Tail1), list(Y, Tail2)) :-
    select(X, Tail1, Tail2).

path(Conns, From, To) :-
    select(conn(From, To), Conns, _).
path(Conns, From, To) :-
    select(conn(From, Via), Conns, Conns1),
    path(Conns1, Via, To).

stuff_path(From, To) :-
    stuff_connections(Conns),
    path(Conns, From, To).

stuff_connections(
    list(conn(a, b),
    list(conn(a, c),
    list(conn(a, d),
    list(conn(a, e),
    list(conn(a, f),
    list(conn(a, g),
    list(conn(a, h),
    list(conn(a, i),
    list(conn(a, j),
    list(conn(a, k),
    list(conn(a, l),
    list(conn(a, m),
    list(conn(a, n),
    list(conn(b, c),
    list(conn(b, d),
    list(conn(b, e),
    list(conn(b, f),
    list(conn(b, g),
    list(conn(b, h),
    list(conn(b, i),
    list(conn(b, j),
    list(conn(b, k),
    list(conn(b, l),
    list(conn(b, m),
    list(conn(b, n),
    list(conn(c, d),
    list(conn(c, e),
    list(conn(c, f),
    list(conn(c, g),
    list(conn(c, h),
    list(conn(c, i),
    list(conn(c, j),
    list(conn(c, k),
    list(conn(c, l),
    list(conn(c, m),
    list(conn(c, n),
    list(conn(d, e),
    list(conn(d, f),
    list(conn(d, g),
    list(conn(d, h),
    list(conn(d, i),
    list(conn(d, j),
    list(conn(d, k),
    list(conn(d, l),
    list(conn(d, m),
    list(conn(d, n),
    list(conn(e, f),
    list(conn(e, g),
    list(conn(e, h),
    list(conn(e, i),
    list(conn(e, j),
    list(conn(e, k),
    list(conn(e, l),
    list(conn(e, m),
    list(conn(e, n),
    list(conn(f, g),
    list(conn(f, h),
    list(conn(f, i),
    list(conn(f, j),
    list(conn(f, k),
    list(conn(f, l),
    list(conn(f, m),
    list(conn(f, n),
    list(conn(g, h),
    list(conn(g, i),
    list(conn(g, j),
    list(conn(g, k),
    list(conn(g, l),
    list(conn(g, m),
    list(conn(g, n),
    list(conn(h, i),
    list(conn(h, j),
    list(conn(h, k),
    list(conn(h, l),
    list(conn(h, m),
    list(conn(h, n),
    list_empty))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))).
