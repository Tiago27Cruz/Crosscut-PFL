% --------------------------------------------------------
% ----------------------IMPORTS---------------------------
% --------------------------------------------------------

:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('board.pl').

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
    0 =:= mod(N,2).

is_odd(N):-
    1 =:= mod(N,2).
	
% --------------------------------------------------------
% -----------------------Game State----------------------
% --------------------------------------------------------	
% state(TurnN, Player1Info, Player2Info, Board, Height, Length).

:- dynamic game_state/1.
:- dynamic player_info/2.

initialize_game_state:-
	write('----------------Welcome to Crosscut!----------------\n'),
	write('Please choose who is playing:\n'),
	write('Options:\n'),
	write('1 - Human [write 1]\n'),
	write('2 - Easy Computer [write 2]\n'),
	write('3 - Hard Computer [write 3]\n'),
	get_player_info,
	write('Please choose the board dimensions:\n'),
	get_height(State),
	get_length(State),
    create_board(State),
    retractall(game_state(_)),
    asserta(game_state(State)).

get_game_state(State):-
    game_state(State).
	
update_game_state(State):-
    retract(game_state(_)),
    asserta(game_state(State)).

get_player_info:-
	repeat,
	write('Player 1 is: '),
	peek_char(Ch),
	get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char, Blue),
	Blue > 0,
	Blue < 4,
	!,
	repeat,
	write('Player 2 is: '),
	peek_char(Ch1),
	get_char_not_nl(Ch1, Char1),
	clear_buffer,
	convert_char_to_number(Char1, Red),
	Red > 0,
	Red < 4,
	asserta(player_info(Blue, Red)),
	!.

get_player_info(Blue, Red):-
	player_info(Blue, Red).
	
% --------------------------------------------------------
% ----------------------Make Move-------------------------
% --------------------------------------------------------

move_loop(Board, NewBoard, Player, Number, Letter, Height, Length):-
	repeat,
    get_input(Number, Letter, Player),
	reverse(Board, ReversedBoard),
	make_move(ReversedBoard,Number,Letter,Player,ReversedNewBoard,0),
	reverse(ReversedNewBoard, NormalBoard),
	validate_move(Letter, Length, Number, Height, NormalBoard, NewBoard, Player),
	!.

% make_move(+Board,+N,+L,+Piece,-NewBoard, +Bypass)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go.
make_move(Board,N,L,'B',NewBoard,Bypass):-
	make_move_aux(Board,N,L,1,[],'B',NewBoard,Bypass).

make_move(Board,N,L,'R',NewBoard,Bypass):-
	make_move_aux(Board,N,L,1,[],'R',NewBoard,Bypass).

make_move(Board,N,L,x,NewBoard,Bypass):-
	make_move_aux(Board,N,L,1,[],x,NewBoard,Bypass).	

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

% --------------------------------------------------------
% ----------------------Valid Moves-----------------------
% --------------------------------------------------------

% valid_moves(+Player, -ListOfMoves)
valid_moves(Player, ListOfMoves):-
	get_game_state(state(_,_,_,Board,Height,Length)),
	valid_moves_column(Board, Height, Length, Player, 1, [], ListOfMoves),
	!.

valid_moves_column(Board, Height, Length, Player, Number, List, ListOfMoves):-
	Number =< Height,
	Number >= 1,
	!,
	valid_moves_row(Board, Length, 1, Number, Height, Player, [], List1),
	Number1 is Number + 1,
	append(List1, List, List2),
	valid_moves_column(Board, Height, Length, Player, Number1, List2, ListOfMoves),
	!.
valid_moves_column(_,_,_,_,_,List,List):-
	write('Column List: '),
	write(List),
	nl,
	!.

valid_moves_row(Board, Length, Letter, Number, Height, Player, List, Acc):-
	Letter =< Length,
	Letter >= 1,
	validate_move(Letter, Length, Number, Height, Board, NewBoard, Player),
	Board \= NewBoard,
	!,
	append(List, [Letter-Number], List1),
	Letter1 is Letter + 1,
	valid_moves_row(Board, Length, Letter1, Number, Height, Player, List1, Acc),
	!.
valid_moves_row(Board, Length, Letter, Number, Height, Player, List, Acc):-
	Letter =< Length,
	Letter >= 1,
	!,
	Letter1 is Letter + 1,
	valid_moves_row(Board, Length, Letter1, Number, Height, Player, List, Acc),
	!.
valid_moves_row(_,_,_,_,_,_,List,List):-
	!.


validate_move(1,_,Number,_,Board, NewBoard, Player):-
	try_to_flip_horizontal(Number, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	reverse(FlippedBoard, ReversedBoard),
	make_move(ReversedBoard,Number,1,x,ReversedNewBoard,1),
	reverse(ReversedNewBoard, NewBoard),
	!.
validate_move(1,_,_,_,Board,Board,_):-
	!,
	fail.
validate_move(Letter,_,1,_,Board, NewBoard, Player):-
	try_to_flip_vertical(Letter, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	reverse(FlippedBoard, ReversedBoard),
	make_move(ReversedBoard,1,Letter,x,ReversedNewBoard,1),
	reverse(ReversedNewBoard, NewBoard),
	!.
validate_move(_,_,1,_,Board, Board,_):-
	!,
	fail.
validate_move(Letter,Letter,Number,_,Board, NewBoard, Player):-
	try_to_flip_horizontal(Number, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	reverse(FlippedBoard, ReversedBoard),
	make_move(ReversedBoard,Number,Letter,x,ReversedNewBoard,1),
	reverse(ReversedNewBoard, NewBoard),
	!.
validate_move(Letter,Letter,_,_,Board, Board, _):-
	!,
	fail.
validate_move(Letter,_,Number,Number, Board, NewBoard, Player):-
	try_to_flip_vertical(Letter, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	reverse(FlippedBoard, ReversedBoard),
	make_move(ReversedBoard,Number,Letter,x,ReversedNewBoard,1),
	reverse(ReversedNewBoard, NewBoard),
	!.
validate_move(_,_,Number,Number, Board, Board, _):-
	!,
	fail.
validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1,
	Number < Height,
	Letter > 1,
	Letter < Length,
	try_to_flip_vertical(Letter, Board, Player, NewBoard),
	!.
validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1,
	Number < Height,
	Letter > 1,
	Letter < Length,
	try_to_flip_horizontal(Number, Board, Player, NewBoard),
	!.
validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1,
	Number < Height,
	Letter > 1,
	Letter < Length,
	make_move(Board,Number,Letter,Player,NewBoard,0),
	!.
validate_move(_,_,_,_,Board,Board,_):-
	!.
% --------------------------------------------------------
	
% //////////////////// PLAY //////////////////////////
play:-
    initialize_game_state,
    display_game,
    game_loop,
	game_over,
	!.

game_over:-
	get_game_state(state(Turn,_,_,_,_,_)),
	is_odd(Turn),
	!,
	write('BLUE WON!!').
game_over:-
	write('RED WON!!').
	
% --------------------------------------------------------
% --------------------Players Turn------------------------
% --------------------------------------------------------

game_loop:-
	blue_turn,
	red_turn,
	game_loop,
	!.
game_loop:-
	!.

blue_turn:-
	get_game_state(state(TurnN,P1,P2,Board,Height,Length)),
    Turn is TurnN + 1,
	write('Blue\'s turn.\n'),
    % move
	move_loop(Board, NewBoard, 'B', Number, Letter, Height, Length),
	update_game_state(state(Turn,P1, P2, NewBoard, Height,Length)),
	display_game,
	% check win
	check_win(NewBoard, Letter, Height, Number, Length),
    !.

red_turn:-
	get_game_state(state(TurnN,P1,P2,Board,Height,Length)),
    Turn is TurnN + 1,
	write('Red\'s turn.\n'),
    % Get move
	move_loop(Board, NewBoard, 'R', Number, Letter, Height, Length),
	update_game_state(state(Turn,P1, P2, NewBoard, Height,Length)),
    display_game,
	% check win
	check_win(NewBoard, Letter, Height, Number, Length),
    !.

% --------------------------------------------------------
% ------------------------Input---------------------------
% --------------------------------------------------------

get_input(N, L, 'B'):-
	get_player_info(Blue, _),
	get_player_input(N, L, Blue),
	!.
get_input(N, L, 'R'):-
    get_player_info(_, Red),
	get_player_input(N, L, Red),
    !.

get_player_input(N, L, 1):-
	repeat,
	get_human_input(N,L),
	!.
get_player_input(N, L, 2):-
	repeat,
	get_easy_bot_input(N,L),
	!.
get_player_input(N, L, 3):-
	repeat,
	get_hard_bot_input(N,L),
	!.

get_human_input(N,L):-
	write('Please input your move in the format letterNumber (b2 p.e.): '),
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	peek_char(Ch1),
	get_char_not_nl(Ch1,ChNumber),
	convert_char_to_number(ChNumber, N),
    convert_letter_to_number(ChLetter, L),
	clear_buffer.

get_easy_bot_input(N,L):-
	get_game_state(state(Turn,_,_,_,_,_)),
	is_odd(Turn),
	valid_moves('R', ListOfMoves),
	length(ListOfMoves, Length),
	random(0, Length, Index) ,
	nth0(Index, ListOfMoves, Move),
	nth0(0, Move, L),
	nth0(1, Move, N),
	write('Red move: '),
	write('Letter: '),
	write(L),
	write(' Number: '),
	write(N),
	nl,
	!.
get_easy_bot_input(N,L):-
	valid_moves('B', ListOfMoves),
	length(ListOfMoves, Length),
	random(0, Length, Index),
	nth0(Index, ListOfMoves, Move),
	nth0(0, Move, L),
	nth0(1, Move, N),
	write('Blue move: '),
	write('Letter: '),
	write(L),
	write(' Number: '),
	write(N),
	nl,
	!.

get_hard_bot_input:-
	write('Not implemented yet!'),
	!,
	fail.


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

try_to_flip_vertical(Letter, Board, Piece, NewBoard):-
	reverse(Board, ReversedBoard),
	check_if_can_flip_vertical(ReversedBoard, Letter, Piece, List),
	write('List: '),
	write(List),
	nl,
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, List).
try_to_flip_vertical(_, Board, _, Board):-
	fail,
	!.

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

try_to_flip_horizontal(Number, Board, Piece, NewBoard):-
	check_if_can_flip_horizontal(Board, Number, Piece, List),
	write('Horizontal List: '),
	write(List),
	nl,
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, List).
try_to_flip_horizontal(_, Board, _, Board):-
	!,
	fail.

check_if_can_flip_horizontal(Board, Number, Piece, List):-
	get_game_state(state(_,_,_,_,_,Length)),
	Pos is Length-Number,
	nth0(Pos, Board, Row),
	check_if_can_flip_horizontal_aux(Row, x, Piece, [], List, 1).

% caso encontrar piece pela primeira vez
check_if_can_flip_horizontal_aux([Piece|Tail], _, Piece, [], Acc, Idx):-
	!,
	Idx1 is Idx + 1,
	check_if_can_flip_horizontal_aux(Tail, Piece, Piece, [], Acc, Idx1).
% caso encontrar piece pela segunda vez
check_if_can_flip_horizontal_aux([Piece|_], _, Piece, List, List, _):-
	!.
% caso encontrar x deve resetar
check_if_can_flip_horizontal_aux([x|Tail], _, Piece, _, Acc, Idx):-
	Idx1 is Idx + 1,
	check_if_can_flip_horizontal_aux(Tail, x, Piece, [], Acc, Idx1).
% caso encontrar N a seguir a um N
check_if_can_flip_horizontal_aux([Head|Tail], Head, Piece, List, Acc, Idx):-
	Head \= Piece,
	Head \= x,
	Idx1 is Idx + 1,
	append(List, [Idx], List1),
	check_if_can_flip_horizontal_aux(Tail, Head, Piece, List1, Acc, Idx1).
% caso encontrar N a seguir a um Piece
check_if_can_flip_horizontal_aux([Head|Tail], Piece, Piece, List, Acc, Idx):-
	Head \= Piece,
	Head \= x,
	Idx1 is Idx + 1,
	append(List, [Idx], List1),
	check_if_can_flip_horizontal_aux(Tail, Head, Piece, List1, Acc, Idx1).

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
	make_move(Board,Head,Letter,Piece,ChangedBoard,1),
	flip_pieces_aux(Letter, 'Vertical', ChangedBoard, Piece, NewBoard, Tail).
flip_pieces_aux(Number, 'Horizontal', Board, Piece, NewBoard, [Head|Tail]):-
	make_move(Board,Number,Head,Piece,ChangedBoard,1),
	flip_pieces_aux(Number, 'Horizontal', ChangedBoard, Piece, NewBoard, Tail).	
	