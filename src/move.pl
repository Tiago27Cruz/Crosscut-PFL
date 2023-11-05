% --------------------------------------------------------
% -------------------- Make a move -----------------------
% --------------------------------------------------------

% make_play(+GameState, -Move, -NewGameState)
% Receives the current state of the game, returns the Move made and the new state of the game.
% Case where the player has valid moves to be made
make_play(State, Move, NewState):-
	get_player(State, Player), % Gets the player that is going to play
	valid_moves(State, Player, ListOfMoves), % Gets the list of valid moves
	length(ListOfMoves, Len), % Gets the length of the list of valid moves
	Len > 0, % If the length is greater than 0, there are valid moves so the player may play
	!,
	repeat,
    get_input(State, Move, Player), % Gets the input from the user
	move(State, Move, MovedState), % Moves the piece
	next_turn(MovedState, NewState), % Changes the turn
	!.
% Case where the player has no valid moves to be made
make_play(State, move(1,1), NewState):-
	write('No more moves available! Skipping this turn\n'),
	next_turn(State, NewState), % Changes the turn
	!.

% move(+GameState, +Move, -NewGameState)
% Receives the current state of the game, the move to be made and returns the new state of the game.
move(state(Turn,Red,Blue,Board,Height,Length), move(Number, Letter), state(Turn,Red,Blue,NewBoard,Height,Length)):-
	get_player(state(Turn,_,_,_,_,_), Player), % Gets the player that is going to play
	make_move(Board,Number,Letter,Player,MovedBoard,0,0), % Makes the move
	validate_move(Letter, Length, Number, Height, MovedBoard, NewBoard, Player), % Validates the move
	!.

% make_move(+Board,+N,+L,+Piece,-NewBoard, +Bypass, +Silent)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go. If Bypass is 1, than it will be able to replace a piece already placed. If Silent is 1, it will not print anything.
make_move(Board,N,L,Piece,NewBoard,Bypass, Silent):-
    reverse(Board, ReversedBoard), % Reverses the board since make_move_aux makes it's calculation using a reversed board
	make_move_aux(ReversedBoard,N,L,1,[],Piece,ReversedNewBoard, Bypass, Silent),
	reverse(ReversedNewBoard, NewBoard). % Reverses the board again
% make_move_aux(+Board,+N,+L,+CurPos,+Saved,+Piece,-NewBoard, +Bypass, +Silent)
% Auxiliar function to place a piece on the board
% Case where the current position is the one we want to change
make_move_aux([Head|Tail],N,L,N,Saved,Piece,Acc,Bypass, Silent):-
	change_piece_in_line(Head,L,Row,Piece,Bypass, Silent), % Changes the piece in the line
	append(Saved,[Row],Saved1), % Saves the row that was changed
	append(Saved1,Tail,Acc). % Appends the rest of the unchanged rows to the accumulator
% Case where the current position is not the one we want to change
make_move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc,Bypass, Silent):-
	N > CurPos,
	CurPos1 is CurPos + 1,
	append(Saved,[Head],Saved1), % Saves the piece that is not going to be changed
	make_move_aux(Tail,N,L,CurPos1,Saved1,Piece,Acc,Bypass, Silent).
% Case where the input is invalid, since it is invalid
make_move_aux([],_,_,_,_,_,_,_,0):-
	write('Invalid Input\n'),
	!,
	fail.
% Case where the input is invalid, since it is invalid, but doesn't print anything
make_move_aux([],_,_,_,_,_,_,_,1):-
	!,
	fail.

% --------------------------------------------------------
% ------------------- Change Piece -----------------------
% --------------------------------------------------------

% change_piece_in_line(+Line,+Letter,-Row,+Piece, +Bypass, +Silent)
% Receives the line, the letter, the piece to be placed and returns the row where the piece was placed.
change_piece_in_line(Line,L,Row,Piece,Bypass, Silent):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row,Bypass, Silent).
% change_piece_in_line_aux(+Line,+Letter,+CurPos,+Saved,+Piece,-Row, +Bypass, +Silent)
% Auxiliar function to place a piece on a line
% Case where the current position is the one we want to change, but there is already a Red piece there
change_piece_in_line_aux(['R'|_],L,L,_,_,_,0,0):-
	write('A Red piece is already here!\n'),
	!,
	fail.
% Case where the current position is the one we want to change, but there is already a Red piece there, but doesn't print anything
change_piece_in_line_aux(['R'|_],L,L,_,_,_,0,1):-
	!,
	fail.
% Case where the current position is the one we want to change, but there is already a Blue piece there
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0,0):-
	write('A Blue piece is already here!\n'),
	!,
	fail.
% Case where the current position is the one we want to change, but there is already a Blue piece there, but doesn't print anything
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0,1):-
	!,
	fail.
% Case where the current position is the one we want to change
change_piece_in_line_aux([_|Tail],L,L,Saved,Piece, Acc,_,_):-
	append(Saved, [Piece], Saved1), % Appends the piece to the accumulator
	append(Saved1, Tail, Acc). % Appends the rest of the row to the accumulator
% Case where the current position is not the one we want to change
change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc,Bypass,Silent):-
	CurPos1 is CurPos + 1,
	append(Saved, [Head], Saved1), % Appends the piece that is not going to be changed
	change_piece_in_line_aux(Tail,L,CurPos1,Saved1,Piece,Acc,Bypass,Silent).
% Case where the input is invalid, since it is invalid move
change_piece_in_line_aux([],_,_,_,_,_,0,0):-
	write('Invalid Input\n'),
	!,
	fail.
% Case where the input is invalid, since it is invalid, but doesn't print anything
change_piece_in_line_aux([],_,_,_,_,_,_,1):-
	!,
	fail.