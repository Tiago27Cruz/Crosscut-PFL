% --------------------------------------------------------
% -------------------- Game Display ----------------------
% --------------------------------------------------------

% print_board(+Length, +LocalMax)
% Prints the letters of the board
% Finished printing letters so it prints a new line
print_letters(0,_):-
	write('\n').
% Calculates the correct ascii code for the letter and prints it
print_letters(Length, LocalMax):-
	Length>0,
	Code is LocalMax - Length + 1,
	put_code(Code),
	write('     '),
	Length1 is Length - 1,
	print_letters(Length1, LocalMax).

% print_board_header(+Length)
% Prints the header of the board, which is just a line of underscores
% Finished printing the header so it prints a new line
print_board_header(0):-nl.
% Prints a column of underscores and calls itself recursively until it reaches the end
print_board_header(Length):-
	write('______'),
	Length1 is Length - 1,
	print_board_header(Length1).
% print_board(+Board, +Height, +Length)
% Prints the board
% Finished printing the board itself so it starts printing the letters, calculates the max ascii code for the letter and calls print_letters
print_board([],_,Length):-
	write(' +    '),
	char_code('a', A),
    LocalMax is A + Length - 1,
	print_letters(Length, LocalMax).
% Prints the according row of the board and calls itself recursively until it reaches the end
print_board([Row | Tail], Height, Length):-
    print_row_first(Row, Height, Length),
	Height1 is Height - 1,
    print_board(Tail,Height1,Length).
% print_row_first(+Row, +Height, +Length)
% Prints the first part of a row of the board (like the number coordinate)
print_row_first(Row,Height,Length):-
	print_top(Length),
	write(' '),
	write(Height),
	write(' |  '),
	print_row(Row),
	write('   |'),
	print_bottom(Length).
% print_row(+Row)
% Prints the row by printing the according element of the row and calls itself recursively until it reaches the end. 
% Finished printing the row so it prints a new line
print_row([]):-nl.
% If it finds a x in the representation of the board it prints an empty space
print_row([x | Tail]):-
    write(' '),
	write('  |  '),
    print_row(Tail).
% If it finds a player piece, it prints the player piece
print_row([X | Tail]):-
    write(X),
	write('  |  '),
    print_row(Tail).

% print_top(+Length)
% prints the '|' of the board
% Finished printing the '|' so it prints a new line
print_top(0):-
	write('   |'),
	nl.
% Prints a column of '|' and calls itself recursively until it reaches the end
print_top(L):-
	L1 is L - 1,
	write('   |  '),
    print_top(L1).
% print_bottom(+Length)
% prints the '_' of the board with a | when it finishes the line to separate columns
% Finished printing the '_' so it prints a new line
print_bottom(0):-nl.
% Prints a column of '_' and calls itself recursively until it reaches the end
print_bottom(L):-
	L1 is L - 1,
	write('_____|'),
    print_bottom(L1).

% print_board_turn(+Turn)
% Prints the turn of the game and who's playing
% Red's turn
print_board_turn(Turn):-
	is_odd(Turn),
	write('-----------> Turn Number '),
	write(Turn),
	nl,
	nl,
	write('-----> Red\'s Turn\n').
% Blue's turn
print_board_turn(Turn):-
	is_even(Turn),
	write('-----------> Turn Number '),
	write(Turn),
	nl,
	nl,
	write('-----> Blue\'s Turn\n').

% display_game(+GameState)
% Displays the game
display_game(state(Turn, _, _, Board,H,L)):-
	nl,
	print_board_turn(Turn),
	display_board(state(_, _, _, Board,H,L)),
    !.
% display_board(+GameState)
% Displays the board
display_board(state(_, _, _, Board,H,L)):-
	write('   _'),
	print_board_header(L),
	print_board(Board,H,L),
	!.

% display_finished_game(+Winner)
% Displays the fact that the game is over, it's winner, the final board and asks if the player wants to play again
display_finished_game(Winner):-
	get_game_state(state(_, _, _, Board,H,L)), % get's the final game state
	nl,
	write('------------------------Game Over---------------------\n\n'),
	write('-----> Winner: '),
	write_winner(Winner), % writes the correct winner
	nl,
	nl,
	display_board(state(_, _, _, Board,H,L)), % display the final board
	nl,
	write('-----> Play Again? (y/n)\n'),
	repeat,
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	clear_buffer,
	play_again(ChLetter), % validates if the player wants to play again
	!.

% write_winner(+Winner)
% Writes the winner of the game
% Red wins
write_winner('R'):-
	write('Red').
% Blue wins
write_winner('B'):-
	write('Blue').

% play_again(+Choice)
% Validates if the player wants to play again
% Valid Case. Play again
play_again('y'):-
	play.
% Invalid Case. Quit the game
play_again('n'):-
	write('\nThanks for playing!\n').

% display_move(+GameState, +Move)
% Displays the move that was played by a bot
% Blue bot played
display_move(state(Turn,_,Blue,_,_,_), move(Number, Letter)):-
	is_odd(Turn), % it's changed because turn's value is already updated
	Blue > 1, % if it's not a player
	nl,
	write('-----> Blue bot played '),
	convert_number_to_letter(Letter, LetterChar), % convert the number to the letter
	put_code(LetterChar), % print the letter with it's ascii number
	write(Number),
	nl,
	nl,
	write('-----> Press ENTER to continue!\n'),
	get_char(_),
	nl.
% Red bot played
display_move(state(Turn,Red,_,_,_,_), move(Number, Letter)):-
	is_even(Turn), % it's changed because turn's value is already updated
	Red > 1, % if it's not a player
	nl,
	write('-----> Red bot played '),
	convert_number_to_letter(Letter, LetterChar), % convert the number to the letter
	put_code(LetterChar), % print the letter with it's ascii number
	write(Number),
	nl,
	nl,
	write('-----> Press ENTER to continue!\n'),
	get_char(_),
	nl.
display_move(_,_).

% --------------------------------------------------------
% ----------------- Board Generation ---------------------
% --------------------------------------------------------

% initial_state(+Size, -GameState)
% Creates the initial state of the game
initial_state(size(Height, Length), state(1, _, _, Board, Height, Length)) :-
    create_board(Board, Height, Length, []).

% create_board(-Board, +Height, +Length, +Acc)
% Creates the board
% Finished creating the board so it returns the board
create_board(Board, 0, _, Board).
% Creates a row, adds it to the board and calls itself recursively until it reaches the end to create all columns
create_board(Board, Height, Length, Acc) :-
    Height > 0,
    Height1 is Height - 1,
    create_row(Length, Row),
    create_board(Board, Height1, Length, [Row | Acc]).
% create_row(+Lenght, -Row)
% Creates a row
% Finished creating the row so it returns the row. Creates it in the recursive call
create_row(0, []).
create_row(Length, [x | Tail]) :-
    Length > 0,
    Length1 is Length - 1,
    create_row(Length1, Tail).

% validate_height_length(+N)
% Validates if the height and length are between 5 and 9
% Valid Case
validate_height_length(N):-
    N >= 5, N < 10,
	!.
% Invalid Case
validate_height_length(_):-
    fail.

% get_height(-Height)
% Gets the height of the board
get_height(size(Height,_)) :-
	repeat,
    write('Enter the chosen Height(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Height),
	validate_height_length(Height),
	!.
% get_length(-Length)
% Gets the length of the board
get_length(size(_,Length)) :-
	repeat,
    write('Enter the chosen Length(Between 5 and 9): '),
    peek_char(Ch),
    get_char_not_nl(Ch, Char),
	clear_buffer,
	convert_char_to_number(Char,Length),
	validate_height_length(Length),
	!.
% get_size(-Size)
% Gets the size of the board
get_size(size(Height, Length)):-
	get_height(size(Height,_)),
	get_length(size(_,Length)),
	!.

