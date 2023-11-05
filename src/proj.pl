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

% clear_buffer
% Clears the buffer
clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.

% get_char_not_nl(-Char, +Stream)
% Gets a character from the stream, but not a new line
get_char_not_nl('\n',_):-
	get_char(_),
	!,
	fail.
get_char_not_nl(Char,Char):-
	get_char(_).

% convert_char_to_number(+Char, -Number)
% Converts a number character to a number
convert_char_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 48.

% convert_letter_to_number(+Ascii, -Number)
% Converts a letter character to a number (a = 1, b = 2, ...)
convert_letter_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 96.
% convert_number_to_letter(+Number, -Ascii)
% Converts a number to a letter (1 = a, 2 = b, ...)
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

% initialize_game_state(-State)
% Initializes the game state
initialize_game_state(State):-
	write('----------------Welcome to Crosscut!----------------\n\n'),
	write('Please choose who is playing:\n'),
	write('Options:\n'),
	write('1 - Human [write 1]\n'),
	write('2 - Easy Computer [write 2]\n'),
	write('3 - Hard Computer [write 3]\n'),
	get_player_info(State),
	write('------------------Board Dimensions------------------\n\n'),
	write('Please choose the board dimensions:\n'),
	get_size(Size),
    initial_state(Size, State),
	write('\n--------------------Let\'s Start!--------------------\n\n'),
    retractall(game_state(_)), % remove any previous state left out from the previous game
    asserta(game_state(State)). % add the new state

% get_game_state(-State)
% Gets the current game state
get_game_state(State):-
    game_state(State).

% update_game_state(+State)
% Updates the current game state
update_game_state(State):-
    retract(game_state(_)), % remove the previous state
    asserta(game_state(State)). % add the new state
% get_player_info(-State)
% Gets the player info (who is playing)
get_player_info(state(_,Red,Blue,_,_,_)):-
	repeat,
	nl,
	write('Player 1 (Red) is: '),
	peek_char(Ch),
	get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char, Red),
	Red > 0, % =< 0 are not valid players
	Red < 4, % >= 4 are not valid players
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
	
% //////////////////// PLAY //////////////////////////
% play
% Main function to play the game
play:-
    initialize_game_state(State),
    display_game(State),
    game_loop,
	end,
	!.

% end
% Ends the game by getting the winner and displaying it
end:-
	get_game_state(state(Turn,_,_,_,_,_)),
	is_odd(Turn), % means that the winner would be Blue since it would be red's turn now if the game didn't end
	!,
	display_finished_game('B').
end:-
	display_finished_game('R').
	
% --------------------------------------------------------
% ------------------- Players Turn -----------------------
% --------------------------------------------------------

% get_player(+State, -Player)
% Gets the current player
get_player(state(Turn,_,_,_,_,_), 'R'):-
	is_odd(Turn), % means that the current player is Red
	!.
get_player(state(Turn,_,_,_,_,_), 'B'):-
	is_even(Turn), % means that the current player is Blue
	!.
% game_loop
% Main game loop. Keeps going until the game is over
game_loop:-
	get_game_state(State), % get the current GameState
	make_play(State, Move, NewState), % Makes a play
	update_game_state(NewState), % Updates the game state with the new GameState after a valid play
	game_over(NewState, Winner), % Checks if the game is over and gets the winner
	keep_going(Winner), % Checks if there is a winner.
	display_game(NewState), % Displays the game
	display_move(NewState, Move), % Displays the move that was made if it was done by a bot
	game_loop, % Loops again
	!.
game_loop:-
	!.
% keep_going(+Winner)
% Checks if there is a winner
% Case where Winner is x means that there is no winner yet
keep_going(x):-!.
% Case where Winner is R means that Red won
keep_going('R'):- fail.
% Case where Winner is B means that Blue won
keep_going('B'):- fail.

% next_turn(+State, -NewState)
% Gets the next turn
next_turn(state(Turn, P1, P2, Board, Height, Length), state(NewTurn, P1, P2, Board, Height, Length)):-
	NewTurn is Turn + 1.

:- dynamic game_state/1.
:- dynamic player_info/2.