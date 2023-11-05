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
:- consult('input.pl').

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

convert_number_to_letter(Number, Letter):-
	Letter is Number + 96.
	
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
	game_over(NewState, Winner),
	keep_going(Winner),
	display_game(NewState),
	display_move(NewState, Move),
	game_loop,
	!.
game_loop:-
	!.

keep_going(x):-!.
keep_going('R'):- fail.
keep_going('B'):- fail.

next_turn(state(Turn, P1, P2, Board, Height, Length), state(NewTurn, P1, P2, Board, Height, Length)):-
	NewTurn is Turn + 1.

:- dynamic game_state/1.
:- dynamic player_info/2.