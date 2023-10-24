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
	validate_height_length(Height).
	
get_length(state(_,_,_,_,_,Length)) :-
	repeat,
    write('Enter the chosen Length(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Length),
	validate_height_length(Length).
% --------------------------------------------------------

	
% //////////////////// START //////////////////////////
start:-
    write('Please choose the board dimensions:\n'),
	get_height(State),
	get_length(State),
    create_board(State),
    display_game(State),
    blue_turn(State).

% --------------------------------------------------------
% --------------------Players Turn------------------------
% --------------------------------------------------------

blue_turn(state(TurnN, P1, P2, Board, Height,Length)):-
    Turn is TurnN + 1,
    % Get move
    get_blue_input(N, L, 1),
	make_move(Board,N,L,blue,NewBoard),
    display_game(state(TurnN,P1,P2,NewBoard,Height,Length)),

    % reds turn
	
    red_turn(state(Turn, P1, P2, NewBoard),Height,Length),
    !.

red_turn(state(TurnN, P1, P2, Board),Height,Length):-
    Turn is TurnN + 1, 
    % Get move
    get_red_input(Board, N, L, 1),
    % Change board

    % blues turn
    blue_turn(state(Turn, P1, P2, Board),Height,Length),
    !.

% Falta checar se está dentro das dimensões do board
get_blue_input(N, L, 1):-
    write('Blue\'s turn.\n'),
    repeat,
    get_human_input(N,L),
    !.
	
get_red_input(N, L, 1):-
    write('Red\'s turn.\n'),
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

% make_move(+Board,+N,+L,+Piece,-NewBoard)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go.
make_move(Board,N,L,blue,NewBoard):-
	make_move_aux(Board,N,L,1,[],'b',NewBoard).

make_move_aux([Head|Tail],N,L,N,Saved,Piece,Acc):-
	change_piece_in_line(Head,L,Row,Piece),
	append([Saved|Row],Tail,Acc).
make_move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc):-
	CurPos1 is CurPos + 1,
	make_move_aux(Tail,N,L,CurPos1,[Saved|Head],Piece,Acc).




	
change_piece_in_line(Line,L,Row,Piece):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row).

change_piece_in_line_aux([b|_],L,L,_,_,_):-fail.
change_piece_in_line_aux([r|_],L,L,_,_,_):-fail.
change_piece_in_line_aux([_|Tail],L,L,Saved,Piece,[Saved,Piece|Tail]).
change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc):-
	CurPos1 is CurPos + 1,
	change_piece_in_line_aux(Tail,L,CurPos1,[Saved|Head],Piece,Acc).
change_piece_in_line_aux([],_,_,_,_,_):-fail.