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
	get_height(State),
	get_length(State),
    create_board(State),
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
	write('BLUE WON!!').
game_over:-
	write('RED WON!!').
	
% --------------------------------------------------------
% ------------------- Players Turn -----------------------
% --------------------------------------------------------

game_loop:-
	red_turn,
	blue_turn,
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
% ---------------------- Input ---------------------------
% --------------------------------------------------------

get_input(N, L, 'B'):-
	get_player_info(Blue, _),
	get_player_input(N, L, Blue, 'B'),
	!.
get_input(N, L, 'R'):-
    get_player_info(_, Red),
	get_player_input(N, L, Red, 'R'),
    !.

get_player_input(N, L, 1, _):-
	repeat,
	get_human_input(N,L),
	!.
get_player_input(N, L, Difficulty, Player):-
	repeat,
	choose_move(N,L, Player, Difficulty),
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

choose_move(N,L, Player, 2):-
	valid_moves(Player, ListOfMoves),
	length(ListOfMoves, Length),
	random(0, Length, Index) ,
	nth0(Index, ListOfMoves, Move),
	nth0(0, Move, L),
	nth0(1, Move, N),
	!.

choose_move(N,L, Player, 3):-
	write('Not implemented yet!'),
	!,
	fail.


:- dynamic game_state/1.
:- dynamic player_info/2.