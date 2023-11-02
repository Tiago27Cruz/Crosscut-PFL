% --------------------------------------------------------
% ------------------- Valid Moves ------------------------
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
	move(Board, Number, Letter, Player, MovedBoard, 0),
	validate_move(Letter, Length, Number, Height, MovedBoard, NewBoard, Player),
	Board \= NewBoard,
	!,
	append(List, [[Letter,Number]], List1),
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

% --------------------------------------------------------
% ------------------ Validate Move -----------------------
% --------------------------------------------------------

validate_move(1,_,Number,_,Board, NewBoard, Player):-
	try_to_flip_horizontal(Number, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	move(FlippedBoard,Number,1,x,NewBoard,1),
	!.

validate_move(1,_,_,_,Board,Board,_):-
	!,
	fail.

validate_move(Letter,_,1,_,Board, NewBoard, Player):-
	try_to_flip_vertical(Letter, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	move(FlippedBoard,1,Letter,x,NewBoard,1),
	!.

validate_move(_,_,1,_,Board, Board,_):-
	!,
	fail.

validate_move(Letter,Letter,Number,_,Board, NewBoard, Player):-
	try_to_flip_horizontal(Number, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	move(FlippedBoard,Number,Letter,x,NewBoard,1),
	!.

validate_move(Letter,Letter,_,_,Board, Board, _):-
	!,
	fail.

validate_move(Letter,_,Number,Number, Board, NewBoard, Player):-
	try_to_flip_vertical(Letter, Number, Board, Player, FlippedBoard),
	FlippedBoard \= Board,
	move(FlippedBoard,Number,Letter,x,NewBoard,1),
	!.

validate_move(_,_,Number,Number, Board, Board, _):-
	!,
	fail.

validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1,
	Number < Height,
	Letter > 1,
	Letter < Length,
	try_to_flip_vertical(Letter, Number, Board, Player, NewBoard),
	!.

validate_move(Letter, Length, Number, Height, Board, NewBoard, Player):-
	Number > 1,
	Number < Height,
	Letter > 1,
	Letter < Length,
	try_to_flip_horizontal(Number, Board, Player, NewBoard),
	!.

validate_move(_,_,_,_,Board,Board,_):-
	!.