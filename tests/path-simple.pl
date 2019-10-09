% facts
conn(bee, flower).
conn(bird, nest).
conn(flower, grass).
conn(nest, grass).
conn(grass, earth).

% transitivity
path(X, Y) :-
    conn(X, Y).
path(X, Z) :-
    conn(X, Y),
    path(Y, Z).
