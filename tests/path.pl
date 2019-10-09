human_connections(list(conn(sock, foot),
                       list(conn(sock, leg),
                            list(conn(leg, foot),
                                 list(conn(trousers, leg),
                                      list(conn(sock, trousers),
                                           list(conn(shorts, leg),
                                                list(conn(tshirt, torso),
                                                     list(conn(torso, leg),
                                                          list(conn(torso, head),
                                                               list(conn(head, hat),
                                                                    list(conn(longbeard, head),
                                                                         list(conn(longbeard, tshirt),
                                                                              list_empty))))))))))))).

select(X, list(X, Tail), Tail).
select(X, list(Y, Tail1), list(Y, Tail2)) :-
    select(X, Tail1, Tail2).

select_conn(From, To, Conns, Conns1) :-
    select(conn(From, To), Conns, Conns1).
select_conn(From, To, Conns, Conns1) :-
    select(conn(To, From), Conns, Conns1).

path(Conns, From, To, list(conn(From, To), list_empty)) :-
    select_conn(From, To, Conns, _).
path(Conns, From, To, list(conn(From, Via), Path)) :-
    select_conn(From, Via, Conns, Conns1),
    path(Conns1, Via, To, Path).

human_path(From, To, Path) :-
    human_connections(Conns),
    path(Conns, From, To, Path).
