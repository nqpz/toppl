% This tests the stack emulation used when going from pass 3 to pass 4.
foo(baz(test)).
foo(car(test)).
foo(bar(X)) :-
    foo(baz(X)),
    foo(car(X)).
