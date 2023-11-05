% --------------------------------------------------------
% -------------------- Make a move -----------------------
% --------------------------------------------------------

make_play(State, Move, NewState):-
	get_player(State, Player),
	%valid_moves(State, Player, ListOfMoves),
	%length(ListOfMoves, Len),
	%Len > 0,
	repeat,
    get_input(State, Move, Player),
	move(State, Move, MovedState),
	next_turn(MovedState, NewState),
	!.
make_play(State, move(1,1), NewState):-
	get_player(State, Player),
	valid_moves(State, Player, ListOfMoves),
	ListOfMoves =< 0,
	next_turn(State, NewState),
	!.

% move(+GameState, +Move, -NewGameState)
move(state(Turn,Red,Blue,Board,Height,Length), move(Number, Letter), state(Turn,Red,Blue,NewBoard,Height,Length)):-
	get_player(state(Turn,_,_,_,_,_), Player),
	make_move(Board,Number,Letter,Player,MovedBoard,0),
	validate_move(Letter, Length, Number, Height, MovedBoard, NewBoard, Player),
	!.

% make_move(+Board,+N,+L,+Piece,-NewBoard, +Bypass)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go.
make_move(Board,N,L,'B',NewBoard,Bypass):-
    reverse(Board, ReversedBoard),
	make_move_aux(ReversedBoard,N,L,1,[],'B',ReversedNewBoard,Bypass),
	reverse(ReversedNewBoard, NewBoard).

make_move(Board,N,L,'R',NewBoard,Bypass):-
    reverse(Board, ReversedBoard),
	make_move_aux(ReversedBoard,N,L,1,[],'R',ReversedNewBoard,Bypass),
	reverse(ReversedNewBoard, NewBoard).

make_move(Board,N,L,x,NewBoard,Bypass):-
    reverse(Board, ReversedBoard),
	make_move_aux(ReversedBoard,N,L,1,[],x,ReversedNewBoard, Bypass),
	reverse(ReversedNewBoard, NewBoard).

make_move_aux([Head|Tail],N,L,N,Saved,Piece,Acc,Bypass):-
	change_piece_in_line(Head,L,Row,Piece,Bypass),
	append(Saved,[Row],Saved1),
	append(Saved1,Tail,Acc).

make_move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc,Bypass):-
	N > CurPos,
	CurPos1 is CurPos + 1,
	append(Saved,[Head],Saved1),
	make_move_aux(Tail,N,L,CurPos1,Saved1,Piece,Acc,Bypass).

make_move_aux([],_,_,_,_,_,_,_):-
	get_game_state(state(Turn,Red,_,_,_,_)),
	is_odd(Turn),
	Red < 2,
	write('Invalid Input\n'),
	!,
	fail.
make_move_aux([],_,_,_,_,_,_,_):-
	get_game_state(state(Turn,_,Blue,_,_,_)),
	is_even(Turn),
	Blue < 2,
	write('Invalid Input\n'),
	!,
	fail.
make_move_aux([],_,_,_,_,_,_,_):-
	!,
	fail.

% --------------------------------------------------------
% ------------------- Change Piece -----------------------
% --------------------------------------------------------

change_piece_in_line(Line,L,Row,Piece,Bypass):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row,Bypass).

change_piece_in_line_aux(['R'|_],L,L,_,_,_,0):-
	get_game_state(state(Turn,Red,_,_,_,_)),
	is_odd(Turn),
	Red < 2,
	write('A Red piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['R'|_],L,L,_,_,_,0):-
	get_game_state(state(Turn,_,Blue,_,_,_)),
	is_even(Turn),
	Blue < 2,
	write('A Red piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['R'|_],L,L,_,_,_,0):-
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0):-
	get_game_state(state(Turn,Red,_,_,_,_)),
	is_odd(Turn),
	Red < 2,
	write('A Blue piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0):-
	get_game_state(state(Turn,_,Blue,_,_,_)),
	is_even(Turn),
	Blue < 2,
	write('A Blue piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0):-
	!,
	fail.

change_piece_in_line_aux([_|Tail],L,L,Saved,Piece, Acc,_):-
	append(Saved, [Piece], Saved1),
	append(Saved1, Tail, Acc).

change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc,Bypass):-
	CurPos1 is CurPos + 1,
	append(Saved, [Head], Saved1),
	change_piece_in_line_aux(Tail,L,CurPos1,Saved1,Piece,Acc,Bypass).

change_piece_in_line_aux([],_,_,_,_,_,0):-
	get_game_state(state(Turn,_,Blue,_,_,_)),
	is_even(Turn),
	Blue < 2,
	write('Invalid Input\n'),
	!,
	fail.
change_piece_in_line_aux([],_,_,_,_,_,0):-
	get_game_state(state(Turn,Red,_,_,_,_)),
	is_odd(Turn),
	Red < 2,
	write('Invalid Input\n'),
	!,
	fail.
change_piece_in_line_aux([],_,_,_,_,_,_):-
	!,
	fail.