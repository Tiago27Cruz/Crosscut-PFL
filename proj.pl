% --------------------------------------------------------
% BUFFER READING RELATED FUNCTIONS

clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.
	
get_char_not_nl('\n',Next):-
	get_char(_),
	get_char_not_nl(Next).
get_char_not_nl(Char,Char):-
	get_char(_).

convert_char_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 48.

convert_letter_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 96.

% --------------------------------------------------------

% state(TurnN, Player1Info, Player2Info, Board).
% Turn 1, P1 largest segment (0), P2 largest segment (0), initial board.

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
	
create_board(state(1, 0, 0, Board), Height, Length) :-
    create_board_aux(Board, Height, Length, []).

create_board_aux(Board, 0, _, Board).
create_board_aux(Board, Height, Length, Acc) :-
    Height > 0,
    Height1 is Height - 1,
    create_row(Length, Row),
    create_board_aux(Board, Height1, Length, [Row | Acc]).

create_row(0, []).
create_row(Length, [x | Tail]) :-
    Length > 0,
    Length1 is Length - 1,
    create_row(Length1, Tail).

display_game(state(_, _, _, Board),H,L):-
	nl,
	write('   _'),
	print_board_header(L),
    print_board(Board,H,L),
    !.

validate_height_length(N):-
    N >= 5, N < 10,
	!.
validate_height_length(_):-
    fail.

get_height(Height) :-
	repeat,
    write('Enter the chosen Height(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Height),
	validate_height_length(Height).
	
get_length(Length) :-
	repeat,
    write('Enter the chosen Length(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Length),
	validate_height_length(Length).
	
% //////////////////// START //////////////////////////
start:-
    write('Please choose the board dimensions:\n'),
	get_height(H),
	get_length(L),
    create_board(State, H, L),
    display_game(State,H,L),
    blue_turn(State).

% ----------------
%  Players Turn
% ----------------

blue_turn(state(TurnN, P1, P2, Board)):-
    Turn is TurnN + 1,
    % Get move
    get_blue_input(Board, N, L, 1),
    % Change board

    % reds turn
    red_turn(state(Turn, P1, P2, Board)),
    !.

red_turn(state(TurnN, P1, P2, Board)):-
    Turn is TurnN + 1, 
    % Get move
    get_red_input(Board, N, L, 1),
    % Change board

    % blues turn
    blue_turn(state(Turn, P1, P2, Board)),
    !.


get_blue_input(Board, N, L, 1):-
    write('Blue\'s turn.\n'),
    repeat,
    get_human_input(L,N),
    validate_blue_move(Board,N,L,1),
    !.
	
get_red_input(Board, N, L, 1):-
    write('Red\'s turn.\n'),
    repeat,
    get_human_input(L,N),
    validate_red_move(Board,N,L),
    !.


get_human_input(L,N):-
	write('Please input your move in the format lN (a4 p.e.)'),
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	peek_char(Ch1),
	get_char_not_nl(Ch1,ChNumber),
	convert_char_to_number(ChNumber, N),
    convert_letter_to_number(ChLetter, L),
	clear_buffer.

validate_blue_move([X|_],N,L,N):-
    check_line(X,L,1,x).

validate_blue_move([_|Y],N,L,N_at):-
    N_at1 is N_at + 1,
    validate_blue_move(Y,N,L,N_at1).

validate_blue_move([],_,_,_):-fail.

check_line(_, L, L, x).

check_line([X|Y], L, L_at,X):-
    L_at1 is L_at +1,
    check_line(Y,L,L_at1,X).

check_line([],_,_,_):-fail.

validate_red_move(N,L).