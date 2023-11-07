foo:-
    write('foo\n'),
    !.
foo:-
    write('foo2\n'),
    !.

bar:-
    write('bar\n').
bar:-
    write('bar2\n').

test:-
    bar,
    fail.
test2:-
    foo,
    fail.
