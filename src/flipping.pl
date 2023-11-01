% --------------------------------------------------------
% ------------------------ Flip --------------------------
% --------------------------------------------------------

% flip_pieces(+Letter, +Number, +Direction, +Board, +Piece, -NewBoard, +List)
flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, List):-
	flip_pieces_aux(Letter, 'Vertical', Board, Piece, NewBoard, List).
flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, List):-
	flip_pieces_aux(Number, 'Horizontal', Board, Piece, NewBoard, List).

flip_pieces_aux(_, _, Board, _, Board, []):-!.
flip_pieces_aux(Letter, 'Vertical', Board, Piece, NewBoard, [Head|Tail]):-
	move(Board,Head,Letter,Piece,ChangedBoard,1),
	flip_pieces_aux(Letter, 'Vertical', ChangedBoard, Piece, NewBoard, Tail).
flip_pieces_aux(Number, 'Horizontal', Board, Piece, NewBoard, [Head|Tail]):-
	move(Board,Number,Head,Piece,ChangedBoard,1),
	flip_pieces_aux(Number, 'Horizontal', ChangedBoard, Piece, NewBoard, Tail).

% --------------------------------------------------------
% ------------------ Vertical Flip -----------------------
% --------------------------------------------------------

try_to_flip_vertical(Letter, Board, Piece, NewBoard):-
	reverse(Board, ReversedBoard),
	check_if_can_flip_vertical(ReversedBoard, Letter, Piece, List),
	write('List: '),
	write(List),
	nl,
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, List).
try_to_flip_vertical(_, Board, _, Board):-
	fail,
	!.

% check_if_can_flip_vertical(+Board, +Letter, +Piece, -List)
check_if_can_flip_vertical([Head|Tail], Letter, Piece, List):-
	nth1(Letter, Head, N),
	check_if_can_flip_vertical_aux(Tail, Letter, x, N, [], List, Piece, 1),
	length(List, FlipSize),
	nth1(FlipSize, List, Last),
	nth1(1, List, First),
	Last1 is Last + 1,
	First1 is First - 1,
	get_game_state(state(_, _, _, _, Height, _)),
	count_up([Head|Tail], Last1, Letter, Height, Piece, Piece, 0, AboveLastCount),
	count_down([Head|Tail], First1, Letter, Piece, Piece, 0, BelowFirstCount),
	FlipSize1 is FlipSize + AboveLastCount + BelowFirstCount,
    check_prohibited_vertical_flip([Head|Tail], List, Letter, FlipSize1).

% caso base para falhar
check_if_can_flip_vertical_aux([], _, _, _, _, _,_):-
	!,
	fail.
% Se recebeu um piece pela primeira vez
check_if_can_flip_vertical_aux([Head|Tail], Letter, _, Piece, [], Acc, Piece, Idx):-
	!,
	Idx1 is Idx + 1,
	nth1(Letter, Head, N),
	check_if_can_flip_vertical_aux(Tail, Letter, Piece, N, [], Acc, Piece, Idx1).
% Se recebeu um piece pela segunda vez
check_if_can_flip_vertical_aux(_, _, _, Piece, List, List, Piece, _):-
	!.
% Se recebe um x então recomeça a contar
check_if_can_flip_vertical_aux([Head|Tail], Letter, _, x, _, Acc, Piece, Idx):-
	nth1(Letter, Head, N1),
	Idx1 is Idx + 1,
	check_if_can_flip_vertical_aux(Tail, Letter, x, N1, [], Acc, Piece, Idx1).

% Se recebe um N=N onde é diferente de x
check_if_can_flip_vertical_aux([Head|Tail], Letter, N, N, List, Acc, Piece, Idx):-
	N \= Piece,
	N \= x,
	Idx1 is Idx + 1,
	nth1(Letter, Head, N1),
	append(List, [Idx], List1),
	check_if_can_flip_vertical_aux(Tail, Letter, N, N1, List1, Acc, Piece, Idx1).

% Se tem um Piece e depois um R/B
check_if_can_flip_vertical_aux([Head|Tail], Letter, Piece, N, _, Acc, Piece, Idx):-
	N \= Piece,
	N \= x,
	Idx1 is Idx + 1,
	nth1(Letter, Head, N1),
	check_if_can_flip_vertical_aux(Tail, Letter, N, N1, [Idx], Acc, Piece, Idx1).

% --------------------------------------------------------
% -------------- Verticle Flip Exeption ------------------
% --------------------------------------------------------

check_prohibited_vertical_flip(_, [], _, _):-!.

check_prohibited_vertical_flip(Board, [Head|Tail], Letter, FlipSize):-
    get_game_state(state(_,_,_,_,_,Length)),
    nth1(Head, Board, Row),
    nth1(Letter, Row, Piece),
    count_left(Row, Letter, Piece, Piece, -1, Left),
    count_right(Row, Letter, Length, Piece, Piece, -1, Right),
    Size is Left + Right + 1,
    FlipSize > Size,
    check_prohibited_vertical_flip(Board, Tail, Letter, FlipSize).

count_left(_, 1, Piece, Piece, Count, Count):-!.

count_left(Row, CurPos, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos - 1,
    nth1(CurPos1, Row, N),
    count_left(Row, CurPos1, N, Piece, Count1, Final).

count_left(_, _, N, Piece, Count, Count):-
    N \= Piece.


count_right(_, Length, Length, Piece, Piece, Count, Count):-!.

count_right(Row, CurPos, Length, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos + 1,
    nth1(CurPos1, Row, N),
    count_right(Row, CurPos1, Length, N, Piece, Count1, Final).

count_right(_, _, _, N, Piece, Count, Count):-
    N \= Piece.


% --------------------------------------------------------
% ----------------- Horizontal Flip ----------------------
% --------------------------------------------------------

try_to_flip_horizontal(Number, Board, Piece, NewBoard):-
	check_if_can_flip_horizontal(Board, Number, Piece, List),
	write('Horizontal List: '),
	write(List),
	nl,
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, List).
try_to_flip_horizontal(_, Board, _, Board):-
	!,
	fail.

check_if_can_flip_horizontal(Board, Number, Piece, List):-
	get_game_state(state(_,_,_,_,Height,_)),
	Pos is Height - Number + 1,
	nth1(Pos, Board, Row),
	check_if_can_flip_horizontal_aux(Row, x, Piece, [], List, 1),
	length(List, FlipSize),
	nth1(FlipSize, List, Last),
    nth1(1, List, First),
    Last1 is Last + 1,
    First1 is First - 1,
    get_game_state(state(_, _, _, _, _, Length)),
    count_right(Row, Last1, Length, Piece, Piece, 0, AfterLastCount),
    count_left(Row, First1, Piece, Piece, 0, BeforeFistCount),
	FlipSize1 is FlipSize + BeforeFistCount + AfterLastCount,
	check_prohibited_horizontal_flip(Board, List, Row, Pos, FlipSize1).

% caso encontrar piece pela primeira vez
check_if_can_flip_horizontal_aux([Piece|Tail], _, Piece, [], Acc, Idx):-
	!,
	Idx1 is Idx + 1,
	check_if_can_flip_horizontal_aux(Tail, Piece, Piece, [], Acc, Idx1).
% caso encontrar piece pela segunda vez
check_if_can_flip_horizontal_aux([Piece|_], _, Piece, List, List, _):-
	!.
% caso encontrar x deve resetar
check_if_can_flip_horizontal_aux([x|Tail], _, Piece, _, Acc, Idx):-
	Idx1 is Idx + 1,
	check_if_can_flip_horizontal_aux(Tail, x, Piece, [], Acc, Idx1).
% caso encontrar N a seguir a um N
check_if_can_flip_horizontal_aux([Head|Tail], Head, Piece, List, Acc, Idx):-
	Head \= Piece,
	Head \= x,
	Idx1 is Idx + 1,
	append(List, [Idx], List1),
	check_if_can_flip_horizontal_aux(Tail, Head, Piece, List1, Acc, Idx1).
% caso encontrar N a seguir a um Piece
check_if_can_flip_horizontal_aux([Head|Tail], Piece, Piece, List, Acc, Idx):-
	Head \= Piece,
	Head \= x,
	Idx1 is Idx + 1,
	append(List, [Idx], List1),
	check_if_can_flip_horizontal_aux(Tail, Head, Piece, List1, Acc, Idx1).

% --------------------------------------------------------
% ------------- Horizontal Flip Exeption -----------------
% --------------------------------------------------------

check_prohibited_horizontal_flip(_, [], _, _, _):-!.

check_prohibited_horizontal_flip(Board, [Letter|Tail], Row, Number, FlipSize):-
    get_game_state(state(_,_,_,_,Height,_)),
    nth1(Letter, Row, Piece),
    count_up(Board, Number, Letter, Height, Piece, Piece, -1, Up),
    count_down(Board, Number, Letter, Piece, Piece, -1, Down),
    Size is Up + Down + 1,
    FlipSize > Size,
    check_prohibited_horizontal_flip(Board, Tail, Row, Number, FlipSize).


count_up(_, Height, _, Height, Piece, Piece, Count, Count):-!.

count_up(Board, CurPos, Letter, Height, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos + 1,
    nth1(CurPos1, Board, Row),
    nth1(Letter, Row, N),
    count_up(Board, CurPos1,Letter, Height, N, Piece, Count1, Final).

count_up(_, _, _, _, N, Piece, Count, Count):-
    N \= Piece.

count_down(_, 1, _, Piece, Piece, Count, Count):-!.

count_down(Board, CurPos, Letter, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos - 1,
    nth1(CurPos1, Board, Row),
    nth1(Letter, Row, N),
    count_down(Row, CurPos1, Letter, N, Piece, Count1, Final).

count_down(_, _, _, N, Piece, Count, Count):-
    N \= Piece.
