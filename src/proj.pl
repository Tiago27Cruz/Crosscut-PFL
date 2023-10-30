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
	
get_char_not_nl('\n',_):-
	get_char(_),
	!,
	fail.
get_char_not_nl(Char,Char):-
	get_char(_).

convert_char_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 48.

convert_letter_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 96.
	
% --------------------------------------------------------
% ----------------------Math Functions--------------------
% --------------------------------------------------------

is_even(N):-
    0 is mod(N,2).

is_odd(N):-
    1 is mod(N,2).
	
% --------------------------------------------------------
% -----------------------Game State----------------------
% --------------------------------------------------------	
% state(TurnN, Player1Info, Player2Info, Board, Height, Length).

:- dynamic game_state/1.

initialize_game_state(State):-
	write('Please choose the board dimensions:\n'),
	get_height(State),
	get_length(State),
    create_board(State),
    retractall(game_state(_)),
    asserta(game_state(State)).

get_game_state(State):-
    game_state(State).
	
update_game_state(State):-
	write('updated game state\n'),
    retract(game_state(_)),
    asserta(game_state(State)).

% --------------------------------------------------------
% ---------------------Game Display-----------------------
% --------------------------------------------------------

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
	
create_board(state(0, 0, 0, Board, Height, Length)) :-
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

move_loop(Board, NewBoard, Player, Number, Letter, Height, Length):-
	repeat,
    get_input(Number, Letter, 1),
	validate_move(Letter, Length, Number, Height),
	reverse(Board, ReversedBoard),
	make_move(ReversedBoard,Number,Letter,Player,ReversedNewBoard,0),
	reverse(ReversedNewBoard, NormalBoard),
	try_to_flip(Letter, Number, NormalBoard, Player, NewBoard),
	!.

% make_move(+Board,+N,+L,+Piece,-NewBoard, +Bypass)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go.
make_move(Board,N,L,'B',NewBoard,Bypass):-
	make_move_aux(Board,N,L,1,[],'B',NewBoard,Bypass).

make_move(Board,N,L,'R',NewBoard,Bypass):-
	make_move_aux(Board,N,L,1,[],'R',NewBoard,Bypass).

make_move_aux([Head|Tail],N,L,N,Saved,Piece,Acc,Bypass):-
	change_piece_in_line(Head,L,Row,Piece,Bypass),
	append(Saved,[Row],Saved1),
	append(Saved1,Tail,Acc).

make_move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc,Bypass):-
	N > CurPos,
	CurPos1 is CurPos + 1,
	append(Saved,[Head],Saved1),
	make_move_aux(Tail,N,L,CurPos1,Saved1,Piece,Acc,Bypass).


make_move_aux([],_,_,_,_,_,_,_):-
	write('Invalid Input\n'),
	!,
	fail.
	
change_piece_in_line(Line,L,Row,Piece,Bypass):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row,Bypass).

change_piece_in_line_aux(['R'|_],L,L,_,_,_,0):-
	write('A Red piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0):-
	write('A Blue piece is already here!\n'),
	!,
	fail.

change_piece_in_line_aux([_|Tail],L,L,Saved,Piece, Acc,_):-
	append(Saved, [Piece], Saved1),
	append(Saved1, Tail, Acc).

change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc,Bypass):-
	CurPos1 is CurPos + 1,
	append(Saved, [Head], Saved1),
	change_piece_in_line_aux(Tail,L,CurPos1,Saved1,Piece,Acc,Bypass).

change_piece_in_line_aux([],_,_,_,_,_,_):-
	write('Wrong Input\n'),
	!,
	fail.

validate_move(1,_,_,_):-
	write('Still needs to check if it\'s a flip // Letter = a \n'),
	!,
	fail.
validate_move(_,_,1,_):-
	write('Still needs to check if it\'s a flip // number = 1\n'),
	!,
	fail.
validate_move(Letter,Letter,_,_):-
	write('Still needs to check if it\'s a flip // Letter = MAX\n'),
	!,
	fail.
validate_move(_,_,Number,Number):-
	write('Still needs to check if it\'s a flip // NUMBER = MAX\n'),
	!,
	fail.
validate_move(Letter, Length, Number, Height):-
	write('Needs to check if its a flip and that the size of the friendly flip result isnt higher than the enemy\n').
	

% --------------------------------------------------------
	
% //////////////////// PLAY //////////////////////////
play:-
    initialize_game_state(State),
    display_game(State),
    \+blue_turn,
	finish,
	!.

finish:-
	get_game_state(state(Turn,_,_,_,_,_)),
	is_odd(Turn),
	!,
	write('BLUE WON!!').
finish:-
	write('RED WON!!').
	
% --------------------------------------------------------
% --------------------Players Turn------------------------
% --------------------------------------------------------

blue_turn:-
	get_game_state(state(TurnN,P1,P2,Board,Height,Length)),
    Turn is TurnN + 1,
	write('Blue\'s turn.\n'),
    % move
	move_loop(Board, NewBoard, 'B', Number, Letter, Height, Length),
	update_game_state(state(Turn,P1, P2, NewBoard, Height,Length)),
	display_game(state(Turn,P1,P2,NewBoard,Height,Length)),
	% check win
	check_win(NewBoard, Letter, Height, Number, Length),
    % reds turn
    red_turn,
    !.

red_turn:-
	get_game_state(state(TurnN,P1,P2,Board,Height,Length)),
    Turn is TurnN + 1,
	write('Red\'s turn. '),
	nl,
    % Get move
	move_loop(Board, NewBoard, 'R', Number, Letter, Height, Length),
	update_game_state(state(Turn,P1, P2, NewBoard, Height,Length)),
    display_game(state(Turn,P1,P2,NewBoard,Height,Length)),
	% check win
	check_win(NewBoard, Letter, Height, Number, Length),
    % blues turn
    blue_turn,
    !.

% --------------------------------------------------------
% ------------------------Input---------------------------
% --------------------------------------------------------

get_input(N, L, 1):-
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
% ----------------------Check Win-------------------------
% --------------------------------------------------------

check_win(Board, Letter, Height, Number, Length):-
	check_vertical_win(Board, Letter, Height),
	check_horizontal_win(Board, Number, Length).

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

% --------------------------------------------------------
% --------------------Game Mechanics----------------------
% --------------------------------------------------------

try_to_flip(Letter, Number, Board, Piece, NewBoard):-
	check_if_can_flip_vertical(Board, Letter, Piece, List),
	write('List: '),
	write(List),
	nl,
	flip_pieces(Letter, Number, 'Vertical', Board, Piece, NewBoard, List).
try_to_flip(_, _, Board, _, Board).

% check_if_can_flip_vertical(+Board, +Letter, +Piece, -List)	
check_if_can_flip_vertical([Head|Tail], Letter, Piece, List):-
	nth1(Letter, Head, N),
	check_if_can_flip_vertical_aux(Tail, Letter, x, N, [], List, Piece, 1).

% caso base para falhar
check_if_can_flip_vertical_aux([], _, _, _, _, _,_):-
	!,
	fail.
% Se recebeu um piece pela primeira vez
check_if_can_flip_vertical_aux([Head|Tail], Letter, _, Piece, [], Acc, Piece, Idx):-
	!,
	Idx1 is Idx + 1,
	nth1(Letter, Head, N),
	check_if_can_flip_vertical_aux(Tail, Letter, Piece, N, [], Acc, Piece, Idx1).
% Se recebeu um piece pela segunda vez
check_if_can_flip_vertical_aux(_, _, _, Piece, List, List, Piece, _):-
	!.

% Se recebe um x então recomeça a contar
check_if_can_flip_vertical_aux([Head|Tail], Letter, _, x, _, Acc, Piece, Idx):-
	nth1(Letter, Head, N1),
	Idx1 is Idx + 1,
	check_if_can_flip_vertical_aux(Tail, Letter, x, N1, [], Acc, Piece, Idx1).

% Se recebe um N=N onde é diferente de x
check_if_can_flip_vertical_aux([Head|Tail], Letter, N, N, List, Acc, Piece, Idx):-
	N \= Piece,
	N \= x,
	Idx1 is Idx + 1,
	nth1(Letter, Head, N1),
	append(List, [Idx], List1),
	check_if_can_flip_vertical_aux(Tail, Letter, N, N1, List1, Acc, Piece, Idx1).

% Se tem um Piece e depois um R/B
check_if_can_flip_vertical_aux([Head|Tail], Letter, Piece, N, _, Acc, Piece, Idx):-
	N \= Piece,
	N \= x,
	Idx1 is Idx + 1,
	nth1(Letter, Head, N1),
	check_if_can_flip_vertical_aux(Tail, Letter, N, N1, [Idx], Acc, Piece, Idx1).


% flip_pieces(+Letter, +Number, +Direction, +Board, +Piece, -NewBoard, +List)
flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, List):-
	reverse(Board, ReversedBoard),
	flip_pieces_aux(Letter, 'Vertical', ReversedBoard, Piece, ReversedNewBoard, List),
	reverse(ReversedNewBoard, NewBoard).
flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, List):-
	reverse(Board, ReversedBoard),
	flip_pieces_aux(Number, 'Horizontal', ReversedBoard, Piece, ReversedNewBoard, List),
	reverse(ReversedNewBoard, NewBoard).

flip_pieces_aux(_, _, Board, _, Board, []):-!.
flip_pieces_aux(Letter, 'Vertical', Board, Piece, NewBoard, [Head|Tail]):-
	write('Head: '),
	write(Head),
	nl,
	make_move(Board,Head,Letter,Piece,ChangedBoard,1),
	flip_pieces_aux(Letter, 'Vertical', ChangedBoard, Piece, NewBoard, Tail).
flip_pieces_aux(Number, 'Horizontal', Board, Piece, NewBoard, [Head|Tail]):-
	make_move(Board,Number,Head,Piece,ChangedBoard,1),
	flip_pieces_aux(Number, 'Horizontal', ChangedBoard, Piece, NewBoard, Tail).	
	