% --------------------------------------------------------
% -------------------- Make a Move -----------------------
% --------------------------------------------------------

move_loop(Board, NewBoard, Player, Number, Letter, Height, Length):-
	repeat,
    get_input(Number, Letter, Player),
	move(Board,Number,Letter,Player,MovedBoard,0),
	validate_move(Letter, Length, Number, Height, MovedBoard, NewBoard, Player),
	!.

% move(+Board,+N,+L,+Piece,-NewBoard, +Bypass)
% Receives the board, N is the Number it wants to go, L is the letter it wants to go.
move(Board,N,L,'B',NewBoard,Bypass):-
    reverse(Board, ReversedBoard),
	move_aux(ReversedBoard,N,L,1,[],'B',ReversedNewBoard,Bypass),
	reverse(ReversedNewBoard, NewBoard).

move(Board,N,L,'R',NewBoard,Bypass):-
    reverse(Board, ReversedBoard),
	move_aux(ReversedBoard,N,L,1,[],'R',ReversedNewBoard,Bypass),
	reverse(ReversedNewBoard, NewBoard).

move(Board,N,L,x,NewBoard,Bypass):-
    reverse(Board, ReversedBoard),
	move_aux(ReversedBoard,N,L,1,[],x,ReversedNewBoard, Bypass),
	reverse(ReversedNewBoard, NewBoard).

move_aux([Head|Tail],N,L,N,Saved,Piece,Acc,Bypass):-
	change_piece_in_line(Head,L,Row,Piece,Bypass),
	append(Saved,[Row],Saved1),
	append(Saved1,Tail,Acc).

move_aux([Head|Tail],N,L,CurPos,Saved,Piece,Acc,Bypass):-
	N > CurPos,
	CurPos1 is CurPos + 1,
	append(Saved,[Head],Saved1),
	move_aux(Tail,N,L,CurPos1,Saved1,Piece,Acc,Bypass).

move_aux([],_,_,_,_,_,_,_):-
	write('Invalid Input\n'),
	!,
	fail.

% --------------------------------------------------------
% ------------------- Change Piece -----------------------
% --------------------------------------------------------

change_piece_in_line(Line,L,Row,Piece,Bypass):-
	change_piece_in_line_aux(Line,L,1,[],Piece,Row,Bypass).

change_piece_in_line_aux(['R'|_],L,L,_,_,_,0):-
	write('A Red piece is already here!\n'),
	!,
	fail.
change_piece_in_line_aux(['B'|_],L,L,_,_,_,0):-
	write('A Blue piece is already here!\n'),
	!,
	fail.

change_piece_in_line_aux([_|Tail],L,L,Saved,Piece, Acc,_):-
	append(Saved, [Piece], Saved1),
	append(Saved1, Tail, Acc).

change_piece_in_line_aux([Head|Tail],L,CurPos,Saved,Piece,Acc,Bypass):-
	CurPos1 is CurPos + 1,
	append(Saved, [Head], Saved1),
	change_piece_in_line_aux(Tail,L,CurPos1,Saved1,Piece,Acc,Bypass).

change_piece_in_line_aux([],_,_,_,_,_,_):-
	write('Wrong Input\n'),
	!,
	fail.
