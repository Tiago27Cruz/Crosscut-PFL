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

is_even(N):-
    0 =:= mod(N,2).

is_odd(N):-
    1 =:= mod(N,2).
	
% --------------------------------------------------------
% ------------------- Game State -------------------------
% --------------------------------------------------------	
% state(TurnN, Player1Info, Player2Info, Board, Height, Length).


initialize_game_state:-
	write('----------------Welcome to Crosscut!----------------\n\n'),
	write('Please choose who is playing:\n'),
	write('Options:\n'),
	write('1 - Human [write 1]\n'),
	write('2 - Easy Computer [write 2]\n'),
	write('3 - Hard Computer [write 3]\n'),
	set_player_info,
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

set_player_info:-
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
	asserta(player_info(Red, Blue)),
	!.

get_player_info(Red, Blue):-
	player_info(Red, Blue).

	
% //////////////////// PLAY //////////////////////////
play:-
    initialize_game_state,
    display_game,
    game_loop,
	game_over,
	!.

game_over:-
	get_game_state(state(Turn,_,_,_,_,_)),
	is_even(Turn),
	!,
	display_finished_game('B').
game_over:-
	display_finished_game('R').
	
% --------------------------------------------------------
% ------------------- Players Turn -----------------------
% --------------------------------------------------------

get_player(state(Turn,_,_,_,_,_), 'R'):-
	is_even(Turn),
	!.
get_player(state(Turn,_,_,_,_,_), 'B'):-
	is_odd(Turn),
	!.

game_loop:-
	get_game_state(State),
	make_play(State, Move, PlayedState),
	next_turn(PlayedState, NewState),
	update_game_state(NewState),
	check_win(NewState, Move),
	display_game,
	game_loop,
	!.
game_loop:-
	!.

next_turn(state(Turn, P1, P2, Board, Height, Length), state(NewTurn, P1, P2, Board, Height, Length)):-
	NewTurn is Turn + 1.

% --------------------------------------------------------
% ---------------------- Input ---------------------------
% --------------------------------------------------------

get_input(move(N, L), 'B'):-
	get_player_info(_, Blue),
	get_player_input(move(N, L), Blue, 'B'),
	!.
get_input(move(N, L), 'R'):-
    get_player_info(Red, _),
	get_player_input(move(N, L), Red, 'R'),
    !.

get_player_input(move(N, L), 1, _):-
	repeat,
	get_human_input(move(N, L)),
	!.
get_player_input(move(N, L), Mode, Player):-
	repeat,
	Level is Mode - 1,
	choose_move(move(N, L), Player, Level),
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

choose_move(move(N, L), Player, 1):-
	valid_moves(Player, ListOfMoves),
	length(ListOfMoves, Length),
	random(0, Length, Index) ,
	nth0(Index, ListOfMoves, move(N,L)),
	!.

% get all moves and choose the one with the biggest value
choose_move(Move, Player, 2):-
	valid_moves(Player, ListOfMoves),
	get_best_move(ListOfMoves, Player, 100, _, Move),
	write('Chosen Move: '),
	write(Move),
	nl,
	!.

get_best_move([],_,_,BestMove, BestMove):-!.
get_best_move([Move|Tail], Player, BestValue, _, BestMove):-
    get_game_state(State),
    move(State, Move, PlayedState),
    value(PlayedState, Move, Player, Value),
    Value < BestValue,
    !,
    write('New Best Move: '),
    write(Move),
    nl,
    get_best_move(Tail, Player, Value, Move, BestMove).
get_best_move([_|Tail], Player, BestValue, CurrentBestMove, BestMove):-
    get_best_move(Tail, Player, BestValue, CurrentBestMove, BestMove).

% Game State Evaluation: describe how to evaluate the game state. The predicate should be called value(+GameState, +Player, -Value).
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	count_up(Board, Number, Letter, Height, Player, Player, -1, Up),
	count_down(Board, Number, Letter, Player, Player, 0, Down),
	VerticalSegment is Up + Down,
	VerticalValue is Height - VerticalSegment -2,
	Pos is Height - Number + 1,
	nth1(Pos, Board, Row),
	count_left(Row, Letter, Player, Player, -1, Left),
	count_right(Row, Letter, Length, Player, Player, 0, Right),
	HorizontalSegment is Left + Right,
	HorizontalValue is Length - HorizontalSegment -2,
	Value is min(VerticalValue, HorizontalValue),
	write('Move: '),
	write(move(Number, Letter)),
	write(' Vertical: '),
	write(VerticalValue),
	write(' Up: '),
	write(Up),
	write(' Down: '),
	write(Down),
	write(' Horizontal: '),
	write(HorizontalValue),
	write(' Left: '),
	write(Left),
	write(' Right: '),
	write(Right),
	write(' Value: '),
	write(Value),
	nl,
	!.



:- dynamic game_state/1.
:- dynamic player_info/2.