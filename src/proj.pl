% --------------------------------------------------------
% --------------------- IMPORTS --------------------------
% --------------------------------------------------------

:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('board.pl').
:- consult('move.pl').
:- consult('flipping.pl').
:- consult('valid_moves.pl').
:- consult('win.pl').

% --------------------------------------------------------
% ----------- BUFFER READING RELATED FUNCTIONS -----------
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
% ------------------ Math Functions ----------------------
% --------------------------------------------------------

% is_even(+Number)
% Checks if the given number is even
is_even(N):-
    0 =:= mod(N,2).
% is_odd(+Number)
% Checks if the given number is odd
is_odd(N):-
    1 =:= mod(N,2).
	
% --------------------------------------------------------
% ------------------- Game State -------------------------
% --------------------------------------------------------	
% state(TurnN, Player1Info, Player2Info, Board, Height, Length).


initialize_game_state(State):-
	write('----------------Welcome to Crosscut!----------------\n\n'),
	write('Please choose who is playing:\n'),
	write('Options:\n'),
	write('1 - Human [write 1]\n'),
	write('2 - Easy Computer [write 2]\n'),
	write('3 - Hard Computer [write 3]\n'),
	set_player_info(State),
	write('------------------Board Dimensions------------------\n\n'),
	write('Please choose the board dimensions:\n'),
	get_size(Size),
    initial_state(Size, State),
	write('\n--------------------Let\'s Start!--------------------\n\n'),
    retractall(game_state(_)),
    asserta(game_state(State)).

get_game_state(State):-
    game_state(State).
	
update_game_state(State):-
    retract(game_state(_)),
    asserta(game_state(State)).

set_player_info(state(_,Red,Blue,_,_,_)):-
	repeat,
	nl,
	write('Player 1 (Red) is: '),
	peek_char(Ch),
	get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char, Red),
	Red > 0,
	Red < 4,
	!,
	repeat,
	write('Player 2 (Blue) is: '),
	peek_char(Ch1),
	get_char_not_nl(Ch1, Char1),
	clear_buffer,
	convert_char_to_number(Char1, Blue),
	Blue > 0,
	Blue < 4,
	nl,
	!.

get_player_info(Red, Blue):-
	get_game_state(state(_,Red,Blue,_,_,_)).

	
% //////////////////// PLAY //////////////////////////
play:-
    initialize_game_state(State),
    display_game(State),
    game_loop,
	end,
	!.

end:-
	get_game_state(state(Turn,_,_,_,_,_)),
	is_odd(Turn),
	!,
	display_finished_game('B').
end:-
	display_finished_game('R').
	
% --------------------------------------------------------
% ------------------- Players Turn -----------------------
% --------------------------------------------------------

get_player(state(Turn,_,_,_,_,_), 'R'):-
	is_odd(Turn),
	!.
get_player(state(Turn,_,_,_,_,_), 'B'):-
	is_even(Turn),
	!.

game_loop:-
	get_game_state(State),
	make_play(State, Move, NewState),
	update_game_state(NewState),
	game_over(NewState, Move, Winner),
	keep_going(Winner),
	display_game(NewState),
	game_loop,
	!.
game_loop:-
	!.

keep_going(x):-!.
keep_going('R'):- fail.
keep_going('B'):- fail.

next_turn(state(Turn, P1, P2, Board, Height, Length), state(NewTurn, P1, P2, Board, Height, Length)):-
	NewTurn is Turn + 1.

% --------------------------------------------------------
% ---------------------- Input ---------------------------
% --------------------------------------------------------

get_input(state(Turn,Red,Blue,Board,Height,Length), move(N, L), 'B'):-
	get_player_input(move(N, L), state(Turn,Red,Blue,Board,Height,Length), Blue, 'B'),
	!.
get_input(state(Turn,Red,Blue,Board,Height,Length), move(N, L), 'R'):-
	get_player_input(move(N, L), state(Turn,Red,Blue,Board,Height,Length), Red, 'R'),
    !.

get_player_input(move(N, L), _, 1, _):-
	repeat,
	get_human_input(move(N, L)),
	!.
get_player_input(move(N, L), _, 1, _):-
	repeat,
	get_human_input(move(N, L)),
	!.
get_player_input(move(N, L), State, Mode, Player):-
	repeat,
	Level is Mode - 1,
	choose_move(State, Player, Level, move(N, L)),
	!.

get_human_input(move(N, L)):-
	write('\nPlease input your move in the format letterNumber (b2 p.e.): '),
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	peek_char(Ch1),
	get_char_not_nl(Ch1,ChNumber),
	convert_char_to_number(ChNumber, N),
    convert_letter_to_number(ChLetter, L),
	clear_buffer.

choose_move(State, Player, 1, Move):-
	valid_moves(State, Player, ListOfMoves),
	length(ListOfMoves, Length),
	random(0, Length, Index) ,
	nth0(Index, ListOfMoves, Move),
	!.

% get all moves and choose the one with the biggest value
choose_move(State, Player, 2, Move):-
	valid_moves(State, Player, ListOfMoves),
	get_best_moves(State, ListOfMoves, Player, 100, [], BestMoves),
	write('Best Moves: '),
	write(BestMoves),
	nl,
	length(BestMoves, Length),
	random(0, Length, Index),
	nth0(Index, BestMoves, Move),
	!.

get_best_moves(_, [],_,_,BestMoves, BestMoves):-!.
get_best_moves(State, [Move|Tail], Player, BestValue, CurMoves, BestMoves):-
    move(State, Move, PlayedState),
    value(PlayedState, Move, Player, Value),
    Value =< BestValue,
    !,
    get_new_list(BestValue, Value, Move, CurMoves, NewMoves),
    get_best_moves(State, Tail, Player, Value, NewMoves, BestMoves).
get_best_moves(State, [_|Tail], Player, BestValue, CurMoves, BestMoves):-
    get_best_moves(State, Tail, Player, BestValue, CurMoves, BestMoves).


get_new_list(BestValue, NewValue, Move, _, [Move]):-
	NewValue < BestValue,
	!.
get_new_list(_, _, Move, CurList, [Move|CurList]):-!.

% Game State Evaluation: describe how to evaluate the game state. The predicate should be called value(+GameState, +Player, -Value).

% Case in which it finds a winning move
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, -100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	VerticalValue is Height - VerticalSegment - 2,
	HorizontalValue is Length - HorizontalSegment -2,
	Value is min(VerticalValue, HorizontalValue),
	Value =:= 0,
	!.
% Case in which it finds a move that will create a segment bigger than the opponent's adjacent longest segment so it will not be flipped if done and may enable a flip for the bot
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, OpHorizontalSegment),
	VerticalValue is Height - VerticalSegment - 2,
	HorizontalValue is Length - HorizontalSegment -2,
	Value is min(VerticalValue, HorizontalValue),
	MaxFriendly is max(VerticalSegment, HorizontalSegment),
	MaxEnemy is max(OpVerticalSegment, OpHorizontalSegment) -1,
	Number1 is Number + 1, % gets adjacent pieces
	Letter1 is Letter + 1, % gets adjacent pieces
	is_an_edge(move(Number1, Letter1), Height, Length, Result),
	MaxEnemyReal is MaxEnemy + 2 - Result, % +2 because the piece will be flipped so its the enemy's piece + the one it just put, - result so if its an edge it will only add 1 to the enemy's segment
	MaxFriendly > MaxEnemyReal, % if the move is done the enemy will be able to counter flip
	!.

% When in the second turn, the best play possible is to block 2 edges of the opponent's segment by placing your piece on a diagonal
value(state(2, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, OpHorizontalSegment),
	VerticalValue is Height - VerticalSegment - 2,
	HorizontalValue is Length - HorizontalSegment -2,
	Value is min(VerticalValue, HorizontalValue),
	MaxEnemy is max(OpVerticalSegment, OpHorizontalSegment) - 1,
	MaxEnemy =:= 0,
	check_diagonal_enemy(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, Result),
	Result > 0,
	!.

% Case that gives away a flip to the opponent
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, 100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, OpHorizontalSegment),
	MaxFriendly is max(VerticalSegment, HorizontalSegment),
	MaxEnemy is max(OpVerticalSegment, OpHorizontalSegment) - 1,
	MaxEnemy > 0,
	Number1 is Number + 1, % gets adjacent pieces
	Letter1 is Letter + 1, % gets adjacent pieces
	is_an_edge(move(Number1, Letter1), Height, Length, Result),
	MaxEnemyReal is MaxEnemy + 2 - Result, % +2 because the piece will be flipped so its the enemy's piece + the one it just put, - result so if its an edge it will only add 1 to the enemy's segment
	MaxEnemyReal > MaxFriendly, % if the move is done the enemy will be able to counter flip
	!.

% case where the opponent will flip the whole thing and win (horizontal) [x,B,B,B,*B*,R,x]
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, 100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, _, OpHorizontalSegment),
	OpHorizontalSegment > 1, % opponent has a segment adjacent to the move on the horizontal ( >1 because it counts with my piece)
	FutureFlip is OpHorizontalSegment + HorizontalSegment - 1, % Future flip would be the result of the whole flipped segment -1 because is repetition
	FutureFlip > VerticalSegment,
	!.

% case where the opponent will flip the whole thing and win (vertical) 
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, 100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, _),
	OpVerticalSegment > 1, % opponent has a segment adjacent to the move on the horizontal ( >1 because it counts with my piece)
	FutureFlip is OpVerticalSegment + VerticalSegment - 1, % Future flip would be the result of the whole flipped segment -1 because is repetition
	FutureFlip > HorizontalSegment,
	!.

% Base case in which it adds the length of the enemy's adjacent longest segment to the value so it chooses the one where the enemy has the smallest adjacent segment (or none at all)
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, OpHorizontalSegment),
	!,
	VerticalValue is Height - VerticalSegment - 2 + OpVerticalSegment,
	HorizontalValue is Length - HorizontalSegment -2 + OpHorizontalSegment,
	Value is min(VerticalValue, HorizontalValue),
	!.

% get_new_segments_length(+GameState, +Move, +Player, -VerticalSegment, -HorizontalSegment)
% Calculates the length of the new segments that will be created by the move
get_segments_length(state(_,_,_,Board,Height,Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment):-
	reverse(Board, ReversedBoard),
	count_up(ReversedBoard, Number, Letter, Height, Player, Player, -1, Up),
	count_down(ReversedBoard, Number, Letter, Player, Player, 0, Down),
	VerticalSegment is Up + Down,
	Pos is Height - Number + 1,
	nth1(Pos, Board, Row),
	count_left(Row, Letter, Player, Player, -1, Left),
	count_right(Row, Letter, Length, Player, Player, 0, Right),
	HorizontalSegment is Left + Right.

% Checks if the move is a diagonal of an enemy segment 
check_diagonal_enemy(state(_,_,_,Board,Height,_), move(Number, Letter), Player, Result):-
	Pos is Height - Number + 1,
	PosAbove is Pos - 1,
	PosBelow is Pos + 1,
	nth1(PosAbove, Board, RowAbove),
	nth1(PosBelow, Board, RowBelow),
	PosRight is Letter + 1,
	PosLeft is Letter - 1,
	nth1(PosRight, RowAbove, Elem1),
	nth1(PosLeft, RowAbove, Elem2),
	nth1(PosRight, RowBelow, Elem3),
	nth1(PosLeft, RowBelow, Elem4),
	check_piece(Elem1, Player, Result1),
	check_piece(Elem2, Player, Result2),
	check_piece(Elem3, Player, Result3),
	check_piece(Elem4, Player, Result4),
	Result is Result1 + Result2 + Result3 + Result4.

% check_piece(+Piece, +Player, -Result)
% Checks if the piece is a player piece, result is 1 if it is, 0 otherwise
% The piece is from the player 
check_piece(Piece, Piece, 1):-!.
% The piece is from the opponent
check_piece(_, _, 0):-!.

% get_opponent(+Player, -Opponent)
% Returns the opponent of the given player
get_opponent('R', 'B').
get_opponent('B', 'R').

% is_an_edge(+Move, +Height, +Length, -Result)
is_an_edge(move(Number, _), Number, _, 1).
is_an_edge(move(_, Letter), _, Letter, 1).
is_an_edge(move(2,_),_,_, 1).
is_an_edge(move(_,2),_,_, 1).
is_an_edge(_, _, _, 0).
	
:- dynamic game_state/1.
:- dynamic player_info/2.