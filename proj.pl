clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.

% Initial board
initial_board([[[a, 8, x], [b, 8, x], [c, 8, x], [d, 8, x], [e, 8, x], [f, 8, x], [g, 8, x], [h, 8, x]],
              [[a, 7, x], [b, 7, x], [c, 7, x], [d, 7, x], [e, 7, x], [f, 7, x], [g, 7, x], [h, 7, x]],
              [[a, 6, x], [b, 6, x], [c, 6, x], [d, 6, x], [e, 6, x], [f, 6, x], [g, 6, x], [h, 6, x]],
              [[a, 5, x], [b, 5, x], [c, 5, x], [d, 5, x], [e, 5, x], [f, 5, x], [g, 5, x], [h, 5, x]],
              [[a, 4, x], [b, 4, x], [c, 4, x], [d, 4, x], [e, 4, x], [f, 4, x], [g, 4, x], [h, 4, x]],
              [[a, 3, x], [b, 3, x], [c, 3, x], [d, 3, x], [e, 3, x], [f, 3, x], [g, 3, x], [h, 3, x]],
              [[a, 2, x], [b, 2, x], [c, 2, x], [d, 2, x], [e, 2, x], [f, 2, x], [g, 2, x], [h, 2, x]],
              [[a, 1, x], [b, 1, x], [c, 1, x], [d, 1, x], [e, 1, x], [f, 1, x], [g, 1, x], [h, 1, x]]]).


% state(TurnoN, Player1Info, Player2Info, Board).
% Turn 1, P1 largest segment (0), P2 largest segment (0), initial board).
initial_state(state(1, 0, 0, Board)):- 
	initial_board(Board).


display_game(state(_, _, _, Board)):-
	nl,
    print_board(Board).


print_board([]):-
	write('/a b c d e f g h').
print_board([Row | Tail]):-
    print_row(Row),
    print_board(Tail).


print_row([]):-nl.
print_row([[a,N,X] | Tail]):-
	write(N),
    write(X),
	write(' '),
    print_row(Tail).
print_row([[_,_,X] | Tail]):-
    write(X),
	write(' '),
    print_row(Tail).

start:-
    initial_state(State),
    display_game(State).
