% --------------------------------------------------------
% ---------------------- Input ---------------------------
% --------------------------------------------------------

% get_input(+GameState, -Move, +Player)
% Gets the input from the player and returns the move
% Blue player input
get_input(state(Turn,Red,Blue,Board,Height,Length), move(N, L), 'B'):-
	get_player_input(move(N, L), state(Turn,Red,Blue,Board,Height,Length), Blue, 'B'),
	!.
% Red player input
get_input(state(Turn,Red,Blue,Board,Height,Length), move(N, L), 'R'):-
	get_player_input(move(N, L), state(Turn,Red,Blue,Board,Height,Length), Red, 'R'),
    !.

% get_player_input(-Move, +State, +Mode, +Player)
% Gets the input from the player and returns the move
% Human player input
get_player_input(move(N, L), _, 1, _):-
	repeat,
	get_human_input(move(N, L)),
	!.
% Bot player input
get_player_input(move(N, L), State, Mode, Player):-
	repeat,
	Level is Mode - 1,
	choose_move(State, Player, Level, move(N, L)),
	!.

% get_human_input(-Move)
% Gets the input from the player and returns the move
get_human_input(move(N, L)):-
	write('\nPlease input your move in the format letterNumber (b2 p.e.): '),
	peek_char(Ch),
	get_char_not_nl(Ch,ChLetter),
	peek_char(Ch1),
	get_char_not_nl(Ch1,ChNumber),
	convert_char_to_number(ChNumber, N),
    convert_letter_to_number(ChLetter, L),
	clear_buffer.
% choose_move(+GameState, +Player, +Mode, -Move)
% Chooses the move to be done by the easy bot
choose_move(State, Player, 1, Move):-
	valid_moves(State, Player, ListOfMoves), % gets a list of all the valid moves
	length(ListOfMoves, Length), % gets the length of said list
	random(0, Length, Index) , % gets a random number between 0 and the length of the list
	nth0(Index, ListOfMoves, Move), % gets the move in the index of the random number
	!.
% get all moves and choose the one with the lowest value (the best move)
choose_move(State, Player, 2, Move):-
	valid_moves(State, Player, ListOfMoves), % gets a list of all the valid moves
	get_best_moves(State, ListOfMoves, Player, 100, [], BestMoves), % gets the best moves
	length(BestMoves, Length), % gets the length of said list
	random(0, Length, Index), % gets a random number between 0 and the length of the list
	nth0(Index, BestMoves, Move), % gets the move in the index of the random number
	!.
% get_best_moves(+GameState, +ListOfMoves, +Player, +BestValue, +CurMoves, -BestMoves)
% Gets the best moves from a list of moves
% Final case, returns the best moves
get_best_moves(_, [],_,_,BestMoves, BestMoves):-!.
% Case in which it finds a better move or as good as the others in the list
get_best_moves(State, [Move|Tail], Player, BestValue, CurMoves, BestMoves):-
    move(State, Move, PlayedState), % plays the move
    value(PlayedState, Move, Player, Value), % gets the value of the move
    Value =< BestValue, % if the value is as good or better than the best value continues
    !,
    get_new_list(BestValue, Value, Move, CurMoves, NewMoves), % gets the new list of best moves
    get_best_moves(State, Tail, Player, Value, NewMoves, BestMoves). % continues with the next move
% Case in which it finds a worse move than the others in the list
get_best_moves(State, [_|Tail], Player, BestValue, CurMoves, BestMoves):-
    get_best_moves(State, Tail, Player, BestValue, CurMoves, BestMoves).

% get_new_list(+BestValue, +NewValue, +Move, +CurList, -NewList)
% Gets the new list of best moves
% Case in which it finds a better move
get_new_list(BestValue, NewValue, Move, _, [Move]):-
	NewValue < BestValue,
	!.
% Case in which it finds a move as good as the others
get_new_list(_, _, Move, CurList, [Move|CurList]):-!.

% value(+GameState, +move, +Player, -Value)
% Evaluates the game state. Uses the last move played to evaluate the state in a more efficient way and returns said value
% Case in which it finds a winning move, returns -100 since it will win the game
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, -100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	VerticalValue is Height - VerticalSegment - 2,
	HorizontalValue is Length - HorizontalSegment - 2,
	Value is min(VerticalValue, HorizontalValue),
	Value =:= 0,
	!.

% case where the opponent will flip the whole segment so it doesn't play it in order to avoid losing (horizontal) [x,B,B,B,*B*,R,x]
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, 100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, _, OpHorizontalSegment),
	OpHorizontalSegment > 1, % opponent has a segment adjacent to the move on the horizontal ( >1 because it counts with my piece)
	FutureFlip is OpHorizontalSegment + HorizontalSegment - 1, % Future flip would be the result of the whole flipped segment -1 because is repetition
	FutureFlip > VerticalSegment,
	!.

% case where the opponent will flip the whole segment so it doesn't play it in order to avoid losing (vertical)
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, 100):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, _),
	OpVerticalSegment > 1, % opponent has a segment adjacent to the move on the horizontal ( >1 because it counts with my piece)
	FutureFlip is OpVerticalSegment + VerticalSegment - 1, % Future flip would be the result of the whole flipped segment -1 because is repetition
	FutureFlip > HorizontalSegment,
	!.

% Case in which it finds a move that will create a segment bigger than the opponent's perpendicular longest segment so it will not be flipped if done and may enable a flip for the bot
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, OpHorizontalSegment),
	VerticalValue is Height - VerticalSegment - 2,
	HorizontalValue is Length - HorizontalSegment - 2,
	Value is min(VerticalValue, HorizontalValue),
	MaxFriendly is max(VerticalSegment, HorizontalSegment),
	MaxEnemy is max(OpVerticalSegment, OpHorizontalSegment) - 1,
	Number1 is Number + 1, % gets adjacent pieces
	Letter1 is Letter + 1, % gets adjacent pieces
	is_an_edge(move(Number1, Letter1), Height, Length, Result),
	MaxEnemyReal is MaxEnemy + 2 - Result, % +2 because the piece will be flipped so its the enemy's piece + the one it just put, - result so if its an edge it will only add 1 to the enemy's segment
	MaxFriendly > MaxEnemyReal, % if the move is done the enemy will be able to counter flip
	!.

% Case that gives away a flip to the opponent
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
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
    Value is MaxEnemyReal,
	!.


% A valuable play would be to block an opponents segment by leaving an empty space between their segment and the placed piece
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	Pos is Height - Number + 1,
    Up is Pos + 1,
    Down is Pos - 1,
    Left is Letter - 1,
    Right is Letter + 1,
    nth1(Pos, Board, Row),
    nth1(Up, Board, UpRow),
    nth1(Down, Board, DownRow),
    nth1(Right, Row, RightVal), % Piece to the right of the one just placed
    nth1(Left, Row, LeftVal), % Piece to the left of the one just placed
    nth1(Letter, UpRow, UpVal), % Piece above the one just placed
    nth1(Letter, DownRow, DownVal), % Piece below the one just placed
    Up1 is Number + 1,
    Down1 is Number - 1,
    is_x(RightVal,state(_, _, _, Board, Height, Length), move(Number, Right), Opponent, _, RightHorizontal), % checks if it's an x and returns the value of the segment adjacent to the move
    is_x(LeftVal,state(_, _, _, Board, Height, Length), move(Number, Left), Opponent, _, LeftHorizontal), % checks if it's an x and returns the value of the segment adjacent to the move
    is_x(UpVal,state(_, _, _, Board, Height, Length), move(Up1, Letter), Opponent, UpVertical, _), % checks if it's an x and returns the value of the segment adjacent to the move
    is_x(DownVal,state(_, _, _, Board, Height, Length), move(Down1, Letter), Opponent, DownVertical,_), % checks if it's an x and returns the value of the segment adjacent to the move
    ValueHorizontal is Length - HorizontalSegment - RightHorizontal - LeftHorizontal, % Horizontal value is the length of the board - the length of the player segment - the length of the segment adjacent to the move that it may be blocking from the opponent
    ValueVertical is Height - VerticalSegment - UpVertical - DownVertical, % Vertical value is the height of the board - the length of the player segment - the length of the segment adjacent to the move that it may be blocking from the opponent
    Value is min(ValueHorizontal, ValueVertical), % Chooses the smallest value because it better represents the value of the move
    !.

% Base case in which it adds the length of the enemy's adjacent longest segment to the value so it chooses the one where the enemy has the smallest adjacent segment (or none at all)
value(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, Value):-
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment),
	get_opponent(Player, Opponent),
	get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, OpVerticalSegment, OpHorizontalSegment),
	!,
	VerticalValue is Height - VerticalSegment - 2 + OpVerticalSegment,
	HorizontalValue is Length - HorizontalSegment - 2 + OpHorizontalSegment,
	Value is min(VerticalValue, HorizontalValue),
	!.

% get_new_segments_length(+GameState, +Move, +Player, -VerticalSegment, -HorizontalSegment)
% Calculates the length of the new segments that will be created by the move
get_segments_length(state(_,_,_,Board,Height,Length), move(Number, Letter), Player, VerticalSegment, HorizontalSegment):-
	reverse(Board, ReversedBoard),
	count_up(ReversedBoard, Number, Letter, Height, Player, Player, -1, Up),
	count_down(ReversedBoard, Number, Letter, Player, Player, -1, Down),
	VerticalSegment is Up + Down + 1,
	Pos is Height - Number + 1,
	nth1(Pos, Board, Row),
	count_left(Row, Letter, Player, Player, -1, Left),
	count_right(Row, Letter, Length, Player, Player, -1, Right),
	HorizontalSegment is Left + Right + 1.

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
% Checks if the move is an edge of the board
is_an_edge(move(Number, _), Number, _, 1).
is_an_edge(move(_, Letter), _, Letter, 1).
is_an_edge(move(2,_),_,_, 1).
is_an_edge(move(_,2),_,_, 1).
is_an_edge(_, _, _, 0).

% is_x(+Piece, +GameState, +Move, +Opponent, -VerticalSegment, -HorizontalSegment)
% Checks if the piece is an x and returns the value of the enemy segment adjacent to the move
is_x(x, state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, VerticalSegment, HorizontalSegment):-
    get_segments_length(state(_, _, _, Board, Height, Length), move(Number, Letter), Opponent, VerticalSegment, HorizontalSegment),
    !.
is_x(_, _, _, 0).
