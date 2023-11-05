% --------------------------------------------------------
% ------------------- Valid Moves ------------------------
% --------------------------------------------------------

% valid_moves(+GameState, +Player, -ListOfMoves)
% Returns a list of valid moves for the Player in the current GameState
valid_moves(state(_,_,_,Board,Height,Length), Player, ListOfMoves):-
    valid_moves_column(Board, Height, Length, Player, 1, [], ListOfMoves),
    !.

% valid_moves_column(+Board, +Height, +Length, +Player, +Number, +List, -ListOfMoves)
% Goes through all the columns of the board and calls valid_moves_row to go through all the rows so it can validate every possible move.
valid_moves_column(Board, Height, Length, Player, Number, List, ListOfMoves):-
    Number =< Height,
    Number >= 1,
    !,
    valid_moves_row(Board, Length, 1, Number, Height, Player, [], List1), % finds the valid moves in said row
    Number1 is Number + 1,
    append(List1, List, List2), % appends the moves found in the row to the list of moves found in the previous rows
    valid_moves_column(Board, Height, Length, Player, Number1, List2, ListOfMoves),
    !.
valid_moves_column(_,_,_,_,_,List,List):-
    !.

% valid_moves_row(+Board, +Length, +Letter, +Number, +Height, +Player, +List, -ListOfMoves)
% Goes through all the positions in a row and checks if the move is valid.
valid_moves_row(Board, Length, Letter, Number, Height, Player, List, Acc):-
    Letter =< Length,
    Letter >= 1,
    make_move(Board, Number, Letter, Player, MovedBoard, 0,1), % makes the move
    validate_move(Letter, Length, Number, Height, MovedBoard, NewBoard, Player), % checks if the move is valid
    Board \= NewBoard,
    !,
    append(List, [move(Number,Letter)], List1), % adds the move to the list of moves
    Letter1 is Letter + 1,
    valid_moves_row(Board, Length, Letter1, Number, Height, Player, List1, Acc),
    !.
% if the move is not valid, it goes to the next position
valid_moves_row(Board, Length, Letter, Number, Height, Player, List, Acc):-
    Letter =< Length,
    Letter >= 1,
    !,
    Letter1 is Letter + 1,
    valid_moves_row(Board, Length, Letter1, Number, Height, Player, List, Acc),
    !.
valid_moves_row(_,_,_,_,_,_,List,List):-
    !.

% --------------------------------------------------------
% ------------------ Validate move -----------------------
% --------------------------------------------------------

% validate_move(+Letter, +Length, +Number, +Height, +Board, -NewBoard, +Player)
% Checks if the move is valid and returns a new board if it is. If not NewBoard will be the same as Board.
% A move on the left edge (letter a) is valid if it flips at least one piece.
validate_move(1,Length,Number,Height,Board, NewBoard, Player):-
	try_to_flip_horizontal(Number, 1, Board, Player, FlippedBoard, Height, Length), % tries to flip the pieces
	FlippedBoard \= Board, % if the board is the same, it means that the move is not valid
	make_move(FlippedBoard,Number,1,x,NewBoard,1,1), % makes the edge an empty space again (x)
	!.
% if the move on the edge is not valid, it fails
validate_move(1,_,_,_,Board,Board,_):-
	!,
	fail.
% same as the previous one but for the bottom edge (number 1)
validate_move(Letter,_,1,Height,Board, NewBoard, Player):-
	try_to_flip_vertical(Letter, 1, Board, Player, FlippedBoard, Height), % tries to flip the pieces
	FlippedBoard \= Board, % if the board is the same, it means that the move is not valid
	make_move(FlippedBoard,1,Letter,x,NewBoard,1,1), % makes the edge an empty space again (x)
	!.
% if the move on the edge is not valid, it fails
validate_move(_,_,1,_,Board, Board,_):-
	!,
	fail.
% same as the previous one but for the right edge (letter Length)
validate_move(Letter,Letter,Number,Height,Board, NewBoard, Player):-
	try_to_flip_horizontal(Number, Letter, Board, Player, FlippedBoard, Height, Letter), % tries to flip the pieces
	FlippedBoard \= Board, % if the board is the same, it means that the move is not valid
	make_move(FlippedBoard,Number,Letter,x,NewBoard,1,1), % makes the edge an empty space again (x)
	!.
% if the move on the edge is not valid, it fails
validate_move(Letter,Letter,_,_,Board, Board, _):-
	!,
	fail.
% same as the previous one but for the top edge (number Height)
validate_move(Letter,_,Number,Number, Board, NewBoard, Player):-
	try_to_flip_vertical(Letter, Number, Board, Player, FlippedBoard, Number), % tries to flip the pieces
	FlippedBoard \= Board, % if the board is the same, it means that the move is not valid
	make_move(FlippedBoard,Number,Letter,x,NewBoard,1,1), % makes the edge an empty space again (x)
	!.
% if the move on the edge is not valid, it fails
validate_move(_,_,Number,Number, Board, Board, _):-
	!,
	fail.
% if the move is not on the edge, it checks if it is valid and tries to flip any vertical and horizontal flips that it may be able to
validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1, % move must not be on the edge
	Number < Height, % move must not be on the edge
	Letter > 1, % move must not be on the edge
	Letter < Length, % move must not be on the edge
	try_to_flip_vertical(Letter, Number, Board, Player, FlippedBoard, Height),
	try_to_flip_horizontal(Number, Letter, FlippedBoard, Player, NewBoard, Height, Length),
	!.
% if the move is not on the edge, it checks if it is valid and tries to flip any vertical flip that it may be able to
validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1, % move must not be on the edge
	Number < Height, % move must not be on the edge
	Letter > 1, % move must not be on the edge
	Letter < Length, % move must not be on the edge
	try_to_flip_vertical(Letter, Number, Board, Player, NewBoard, Height),
	!.
% if the move is not on the edge, it checks if it is valid and tries to flip any horizontal flip that it may be able to
validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1, % move must not be on the edge
	Number < Height, % move must not be on the edge
	Letter > 1, % move must not be on the edge
	Letter < Length, % move must not be on the edge
	try_to_flip_horizontal(Number, Letter, Board, Player, NewBoard, Height, Length),
	!.
% if the move is not on the edge and no flips were found it's accepted.
validate_move(_,_,_,_,Board,Board,_):-
	!.