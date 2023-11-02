% --------------------------------------------------------
% -------------------- Game Display ----------------------
% --------------------------------------------------------

% print_board(+Length, +LocalMax)
print_letters(0,_):-
	write('\n').
print_letters(Length, LocalMax):-
	Length>0,
	Code is LocalMax - Length + 1,
	put_code(Code),
	write('     '),
	Length1 is Length - 1,
	print_letters(Length1, LocalMax).


print_board_header(0):-nl.
print_board_header(Length):-
	write('______'),
	Length1 is Length - 1,
	print_board_header(Length1).

print_board([],_,Length):-
	write(' +    '),
	char_code('a', A),
    LocalMax is A + Length - 1,
	print_letters(Length, LocalMax).
	
print_board([Row | Tail], Height, Length):-
    print_row_first(Row, Height, Length),
	Height1 is Height - 1,
    print_board(Tail,Height1,Length).

print_row_first(Row,Height,Length):-
	print_top(Length),
	write(' '),
	write(Height),
	write(' |  '),
	print_row(Row),
	write('   |'),
	print_bottom(Length).

print_row([]):-nl.
print_row([x | Tail]):-
    write(' '),
	write('  |  '),
    print_row(Tail).
print_row([X | Tail]):-
    write(X),
	write('  |  '),
    print_row(Tail).


print_top(0):-
	write('   |'),
	nl.
print_top(L):-
	L1 is L - 1,
	write('   |  '),
    print_top(L1).

print_bottom(0):-nl.
print_bottom(L):-
	L1 is L - 1,
	write('_____|'),
    print_bottom(L1).

print_board_turn(Turn):-
	is_odd(Turn),
	write('-----------> Turn Number '),
	write(Turn),
	nl,
	nl,
	write('-----> Red\'s Turn\n').
print_board_turn(Turn):-
	is_even(Turn),
	write('-----------> Turn Number '),
	write(Turn),
	nl,
	nl,
	write('-----> Blue\'s Turn\n').

display_game:-
	get_game_state(state(Turn, _, _, Board,H,L)),
	nl,
	DisplayTurn is Turn + 1,
	print_board_turn(DisplayTurn),
	display_board(state(_, _, _, Board,H,L)),
    !.

display_board(state(_, _, _, Board,H,L)):-
	write('   _'),
	print_board_header(L),
	print_board(Board,H,L),
	!.

display_finished_game(Winner):-
	get_game_state(state(_, _, _, Board,H,L)),
	nl,
	write('------------------------Game Over---------------------\n\n'),
	write('-----> Winner: '),
	write_winner(Winner),
	nl,
	nl,
	display_board(state(_, _, _, Board,H,L)),
	nl,
	write('-----> Play Again? (y/n)\n'),
	repeat,
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	clear_buffer,
	play_again(ChLetter),
	!.

write_winner('R'):-
	write('Red').
write_winner('B'):-
	write('Blue').

play_again('y'):-
	play.
play_again('n'):-
	write('\nThanks for playing!\n').

% --------------------------------------------------------
% ----------------- Board Generation ---------------------
% --------------------------------------------------------

initial_state(size(Height, Length), state(0, 0, 0, Board, Height, Length)) :-
    create_board(Board, Height, Length, []).

create_board(Board, 0, _, Board).
create_board(Board, Height, Length, Acc) :-
    Height > 0,
    Height1 is Height - 1,
    create_row(Length, Row),
    create_board(Board, Height1, Length, [Row | Acc]).

create_row(0, []).
create_row(Length, [x | Tail]) :-
    Length > 0,
    Length1 is Length - 1,
    create_row(Length1, Tail).
	
validate_height_length(N):-
    N >= 5, N < 10,
	!.
validate_height_length(_):-
    fail.

get_height(size(Height,_)) :-
	repeat,
    write('Enter the chosen Height(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Height),
	validate_height_length(Height),
	!.
	
get_length(size(_,Length)) :-
	repeat,
    write('Enter the chosen Length(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Length),
	validate_height_length(Length),
	!.

get_size(size(Height, Length)):-
	get_height(size(Height,_)),
	get_length(size(_,Length)),
	!.