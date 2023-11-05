% --------------------------------------------------------
% -------------------- Make a move -----------------------
% --------------------------------------------------------

% make_play(+GameState, -Move, -NewGameState)
% Receives the current state of the game,
make_play(State, Move, NewState):-
	get_player(State, Player),
	valid_moves(State, Player, ListOfMoves),
	length(ListOfMoves, Len),
	Len > 0,
	!,
	repeat,
    get_input(State, Move, Player),
	move(State, Move, MovedState),
	next_turn(MovedState, NewState),
	!.
make_play(State, move(1,1), NewState):-
	write('No more moves available! Skipping this turn\n'),
	next_turn(State, NewState),
	!.

% move(+GameState, +Move, -NewGameState)
move(state(Turn,Red,Blue,Board,Height,Length), move(Number, Letter), state(Turn,Red,Blue,NewBoard,Height,Length)):-
	get_player(state(Turn,_,_,_,_,_), Player),
	make_move(Board,Number,Letter,Player,MovedBoard,0,0),
	validate_move(Letter, Length, Number, Height, MovedBoard, NewBoard, Player),
	!.

% make_move(+Board,+N,+L,+Piece,-NewBoard, +Bypass, +Silent)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go. If Bypass is 1, than it will be able to replace a piece already placed. If Silent is 1, it will not print anything.
make_move(Board,N,L,Piece,NewBoard,Bypass, Silent):-
    reverse(Board, ReversedBoard),
	make_move_aux(ReversedBoard,N,L,1,[],Piece,ReversedNewBoard, Bypass, Silent),
	reverse(ReversedNewBoard, NewBoard).

make_move_aux([Head|Tail],N,L,N,Saved,Piece,Acc,Bypass, Silent):-
	change_piece_in_line(Head,L,Row,Piece,Bypass, Silent),
	append(Saved,[Row],Saved1),
	append(Saved1,Tail,Acc).

make_move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc,Bypass, Silent):-
	N > CurPos,
	CurPos1 is CurPos + 1,
	append(Saved,[Head],Saved1),
	make_move_aux(Tail,N,L,CurPos1,Saved1,Piece,Acc,Bypass, Silent).

make_move_aux([],_,_,_,_,_,_,_,0):-
	write('Invalid Input\n'),
	!,
	fail.
make_move_aux([],_,_,_,_,_,_,_,1):-
	!,
	fail.

% --------------------------------------------------------
% ------------------- Change Piece -----------------------
% --------------------------------------------------------

change_piece_in_line(Line,L,Row,Piece,Bypass, Silent):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row,Bypass, Silent).

change_piece_in_line_aux(['R'|_],L,L,_,_,_,0,0):-
	write('A Red piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['R'|_],L,L,_,_,_,0,1):-
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0,0):-
	write('A Blue piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0,1):-
	!,
	fail.

change_piece_in_line_aux([_|Tail],L,L,Saved,Piece, Acc,_,_):-
	append(Saved, [Piece], Saved1),
	append(Saved1, Tail, Acc).

change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc,Bypass,Silent):-
	CurPos1 is CurPos + 1,
	append(Saved, [Head], Saved1),
	change_piece_in_line_aux(Tail,L,CurPos1,Saved1,Piece,Acc,Bypass,Silent).

change_piece_in_line_aux([],_,_,_,_,_,0,0):-
	write('Invalid Input\n'),
	!,
	fail.
change_piece_in_line_aux([],_,_,_,_,_,_,1):-
	!,
	fail.