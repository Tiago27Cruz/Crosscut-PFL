% --------------------------------------------------------
% ----------------------IMPORTS---------------------------
% --------------------------------------------------------


:- use_module(library(lists)).

% --------------------------------------------------------
% ------------BUFFER READING RELATED FUNCTIONS------------
% --------------------------------------------------------

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
% ---------------------Game Display-----------------------
% --------------------------------------------------------
% state(TurnN, Player1Info, Player2Info, Board, Height, Length).
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
		
display_game(state(_, _, _, Board,H,L)):-
	nl,
	write('   _'),
	print_board_header(L),
    print_board(Board,H,L),
    !.
% --------------------------------------------------------
% --------------------Board Generation--------------------
% --------------------------------------------------------
	
create_board(state(1, 0, 0, Board, Height, Length)) :-
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
	
validate_height_length(N):-
    N >= 5, N < 10,
	!.
validate_height_length(_):-
    fail.

get_height(state(_,_,_,_,Height,_)) :-
	repeat,
    write('Enter the chosen Height(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Height),
	validate_height_length(Height),
	!.
	
get_length(state(_,_,_,_,_,Length)) :-
	repeat,
    write('Enter the chosen Length(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Length),
	validate_height_length(Length),
	!.
	
% --------------------------------------------------------
% ----------------------Make Move-------------------------
% --------------------------------------------------------

% make_move(+Board,+N,+L,+Piece,-NewBoard)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go.
make_move(Board,N,L,blue,NewBoard):-
	make_move_aux(Board,N,L,1,[],'B',NewBoard).

make_move(Board,N,L,red,NewBoard):-
	make_move_aux(Board,N,L,1,[],'R',NewBoard).

make_move_aux([Head|Tail],N,L,N,Saved,Piece,Acc):-
	change_piece_in_line(Head,L,Row,Piece),
	append(Saved,[Row],Saved1),
	append(Saved1,Tail,Acc).

make_move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc):-
	N > CurPos,
	CurPos1 is CurPos + 1,
	append(Saved,[Head],Saved1),
	make_move_aux(Tail,N,L,CurPos1,Saved1,Piece,Acc).


make_move_aux([],_,_,_,_,_,_):-
	write('Wrong Input\n'),
	!,
	fail.
	
change_piece_in_line(Line,L,Row,Piece):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row).

change_piece_in_line_aux(['R'|_],L,L,_,_,_):-
	write('r is already here\n'),
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_):-
	write('b is already here\n'),
	!,
	fail.

change_piece_in_line_aux([x|Tail],L,L,Saved,Piece, Acc):-
	append(Saved, [Piece], Saved1),
	append(Saved1, Tail, Acc).

change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc):-
	CurPos1 is CurPos + 1,
	append(Saved, [Head], Saved1),
	change_piece_in_line_aux(Tail,L,CurPos1,Saved1,Piece,Acc).

change_piece_in_line_aux([],_,_,_,_,_):-
	write('Wrong Input\n'),
	!,
	fail.
% --------------------------------------------------------

	
% //////////////////// START //////////////////////////
start:-
    write('Please choose the board dimensions:\n'),
	get_height(State),
	get_length(State),
    create_board(State),
    display_game(State),
    \+blue_turn(State),
	write('YOU WON!!'),
	!.

% --------------------------------------------------------
% --------------------Players Turn------------------------
% --------------------------------------------------------

move_loop(Board, NewBoard, Player, Number, Letter):-
	repeat,
    get_blue_input(Number, Letter, 1),
	% Change board
	reverse(Board, ReversedBoard),
	make_move(ReversedBoard,Number,Letter,Player,ReversedNewBoard),
	reverse(ReversedNewBoard, NewBoard),
	!.

blue_turn(state(TurnN, P1, P2, Board, Height,Length)):-
    Turn is TurnN + 1,
	write('Blue\'s turn.\n'),
    % move
	move_loop(Board, NewBoard, blue, Number, Letter),
	display_game(state(TurnN,P1,P2,NewBoard,Height,Length)),
	% check win
	check_vertical_win(NewBoard, Letter, Height),
	check_horizontal_win(NewBoard, Number, Length),
    % reds turn
    red_turn(state(Turn, P1, P2, NewBoard, Height, Length)),
    !.

red_turn(state(TurnN, P1, P2, Board,Height,Length)):-
    Turn is TurnN + 1,
	write('Red\'s turn.\n'),
    % Get move
	move_loop(Board, NewBoard, red, Number, Letter),
    display_game(state(TurnN,P1,P2,NewBoard,Height,Length)),
	% check win
	check_vertical_win(NewBoard, Letter, Height),
	check_horizontal_win(NewBoard, Number, Length),
    % blues turn
    blue_turn(state(Turn, P1, P2, NewBoard,Height,Length)),
    !.

% --------------------------------------------------------
% ------------------------Input---------------------------
% --------------------------------------------------------

get_blue_input(N, L, 1):-
    repeat,
    get_human_input(N,L),
    !.
	
get_red_input(N, L, 1):-
    repeat,
    get_human_input(N,L),
    !.


get_human_input(N,L):-
	write('Please input your move in the format lN (a4 p.e.): '),
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	peek_char(Ch1),
	get_char_not_nl(Ch1,ChNumber),
	convert_char_to_number(ChNumber, N),
    convert_letter_to_number(ChLetter, L),
	clear_buffer.

% --------------------------------------------------------
% ------------------Check Vertical Win--------------------
% --------------------------------------------------------

check_vertical_win([Head|Tail], Letter,Height):-
	nth1(Letter, Head, N),
	check_vertical_win_aux(Tail, Letter, N, N, 0, Height).

check_vertical_win_aux([], _, N, N, Counter, Height):-
	N \= x,
	Counter1 is Counter + 1,
	Counter1 >= Height-2,
	!,
	fail.

check_vertical_win_aux([], _, _, _, _, _).

check_vertical_win_aux([Head|Tail], Letter, x, x, _, Height):-
	nth1(Letter, Head, N),
	check_vertical_win_aux(Tail, Letter, x, N, 1, Height).

check_vertical_win_aux([Head|Tail], Letter, N, N, Counter, Height):-
	N \= x,
	Counter1 is Counter + 1,
	Counter1 < Height-2,
	nth1(Letter, Head, N1),
	check_vertical_win_aux(Tail, Letter, N, N1, Counter1, Height).

check_vertical_win_aux([Head|Tail], Letter, N, N1, _, Height):-
	N \= N1,
	nth1(Letter, Head, N2),
	check_vertical_win_aux(Tail, Letter, N1, N2, 1, Height).


% --------------------------------------------------------
% -----------------Check Horizontal Win-------------------
% --------------------------------------------------------

check_horizontal_win(Board, Number, Length):-
	Pos is Length-Number,
	nth0(Pos, Board, Row),
	nth1(1, Row, N),
	check_horizontal_win_aux(Row, N, 0, Length).

check_horizontal_win_aux([], _,_,_).

check_horizontal_win_aux([x|Tail], x, _, Length):-
	check_horizontal_win_aux(Tail, x, 0, Length).

check_horizontal_win_aux([Head|Tail], Head, Counter, Length):-
	Head \= x,
	Counter1 is Counter + 1,
	Counter1 < Length-2,
	check_horizontal_win_aux(Tail, Head, Counter1, Length).

check_horizontal_win_aux([Head|Tail], N, _, Length):-
	N \= Head,
	check_horizontal_win_aux(Tail, Head, 1, Length).
