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
	make_move(Board,Head,Letter,Piece,ChangedBoard,1),
	flip_pieces_aux(Letter, 'Vertical', ChangedBoard, Piece, NewBoard, Tail).
flip_pieces_aux(Number, 'Horizontal', Board, Piece, NewBoard, [Head|Tail]):-
	make_move(Board,Number,Head,Piece,ChangedBoard,1),
	flip_pieces_aux(Number, 'Horizontal', ChangedBoard, Piece, NewBoard, Tail).

% --------------------------------------------------------
% ------------------ Vertical Flip -----------------------
% --------------------------------------------------------

try_to_flip_vertical(Letter, Number, Board, Piece, NewBoard):-
	get_game_state(state(_, _, _, _, Height, _)),
	reverse(Board, ReversedBoard),
	check_if_can_flip_vertical(ReversedBoard, Letter,Number, Piece, ListAbove, ListBelow, Height),
	append(ListAbove, ListBelow, List),
	List \= [],
	validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, List, ListAbove, ListBelow).
try_to_flip_vertical(_,_, Board, _, Board):-
    !,
    fail.

validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, List, _, _):-
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, List),
	reverse(NewBoard, NewBoard1),
	reverse(Board, ReversedBoard),
	count_new_friendly_vertical_segment(List, NewBoard1, Letter, Height, Piece, FriendlySegment),
	check_prohibited_vertical_flip(ReversedBoard, List, Letter, FriendlySegment),
	!.
validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, _, ListAbove, _):-
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, ListAbove),
	reverse(NewBoard, NewBoard1),
	reverse(Board, ReversedBoard),
	count_new_friendly_vertical_segment(ListAbove, NewBoard1, Letter, Height, Piece, FriendlySegment),
	check_prohibited_vertical_flip(ReversedBoard, ListAbove, Letter, FriendlySegment),
	!.
validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, _, _, ListBelow):-
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, ListBelow),
	reverse(NewBoard, NewBoard1),
	reverse(Board, ReversedBoard),
	count_new_friendly_vertical_segment(ListBelow, NewBoard1, Letter, Height, Piece, FriendlySegment),
	check_prohibited_vertical_flip(ReversedBoard, ListBelow, Letter, FriendlySegment),
	!.

% check_if_can_flip_vertical(+Board, +Letter, +Piece, -List)
check_if_can_flip_vertical(Board, Letter,Number, Piece, ListAbove, ListBelow, Height):-
	get_flip_list_vertical(Board, Letter, Number, Height, Piece, UnsortedListAbove, UnsortedListBelow),
	sort(UnsortedListAbove, ListAbove),
	sort(UnsortedListBelow, ListBelow).

count_new_friendly_vertical_segment([], _, _, _, _, 0):-!.
count_new_friendly_vertical_segment(SortedList, Board, Letter, Height, Piece, FriendlySegment):-
	nth1(1, SortedList, First),
	count_up(Board, First, Letter, Height, Piece, Piece, -1, AboveFirstCount),
	count_down(Board, First, Letter, Piece, Piece, 0, BelowFirstCount),
	FriendlySegment is AboveFirstCount + BelowFirstCount.

get_flip_list_vertical(Board, Letter, 1, Height, Piece, ListAbove, []):-
	PosUp is Number + 1,
	nth1(PosUp, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board,PosUp, Letter, Height, N, Piece, [], ListAbove),
	!.

get_flip_list_vertical(Board, Letter, Height, Height, Piece, ListAbove, []):-
	PosUp is Number + 1,
	nth1(PosUp, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board,PosUp, Letter, Height, N, Piece, [], ListAbove),
	!.

get_flip_list_vertical(Board, Letter, Number, Height, Piece, ListAbove, ListBelow):-
	PosUp is Number + 1,
	PosDown is Number - 1,
	nth1(PosUp, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board,PosUp, Letter, Height, N, Piece, [], ListAbove),
	nth1(PosDown, Board, Row1),
	nth1(Letter, Row1, N1),
	check_down(Board,PosDown, Letter, N1, Piece, [], ListBelow).

check_up(_, Height, _, Height, Piece, Piece, Acc, Acc):-!.
check_up(_, Height, _, Height, _, _, _, []):-!.
check_up(_,_,_,_,x,_,_,[]):-!.
check_up(_,_,_,_,Piece,Piece,Acc,Acc):-!.
check_up(Board, CurPos, Letter, Height, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece,
	CurPiece \= x,
	append(CurList, [CurPos], CurList1),
	CurPos1 is CurPos + 1,
	nth1(CurPos1, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board, CurPos1, Letter, Height, N, Piece, CurList1, Acc).
	
check_down(_, 1, _, Piece, Piece, Acc, Acc):-!.
check_down(_, 1, _, _, _, _, []):-!.
check_down(_,_,_,x,_,_,[]):-!.
check_down(_,_,_,Piece,Piece,Acc,Acc):-!.
check_down(Board, CurPos, Letter, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece,
	CurPiece \= x,
	append(CurList, [CurPos], CurList1),
	CurPos1 is CurPos - 1,
	nth1(CurPos1, Board, Row),
	nth1(Letter, Row, N),
	check_down(Board, CurPos1, Letter, N, Piece, CurList1, Acc).

% --------------------------------------------------------
% -------------- Verticle Flip Exception -----------------
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

try_to_flip_horizontal(Number, Letter, Board, Piece, NewBoard):- % board is not flipped
	get_game_state(state(_,_,_,_,Height,Length)),
    Pos is Height - Number + 1,
    nth1(Pos, Board, Row),
	check_if_can_flip_horizontal(Row, Letter, Length, Piece, ListRight, ListLeft),
	append(ListRight, ListLeft, List),
	write(List),
	validate_horizontal_flipping(Board, Row, Number, Height,Length, Piece, NewBoard, List, ListRight, ListLeft).
try_to_flip_horizontal(_, _, Board, _, Board):-
	!,
	fail.

validate_horizontal_flipping(Board, Row, Number, Height, Length, Piece, NewBoard, List, _, _):-
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, List),
	Pos is Height - Number + 1,
    nth1(Pos, NewBoard, FlippedRow),
	count_new_friendly_horizontal_segment(List, FlippedRow, Length, Piece, FriendlySegment),
	check_prohibited_horizontal_flip(Board, List, Row, Number, FriendlySegment),
	!.
validate_horizontal_flipping(Board, Row, Number, Height,Length, Piece, NewBoard, _, ListRight, _):-
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, ListRight),
	Pos is Height - Number + 1,
    nth1(Pos, NewBoard, FlippedRow),
	count_new_friendly_horizontal_segment(ListRight, FlippedRow, Length, Piece, FriendlySegment),
	check_prohibited_horizontal_flip(Board, ListRight, Row, Number, FriendlySegment),
	!.
validate_horizontal_flipping(Board, Row, Number, Height,Length, Piece, NewBoard, _, _, ListLeft):-
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, ListLeft),
	Pos is Height - Number + 1,
    nth1(Pos, NewBoard, FlippedRow),
	count_new_friendly_horizontal_segment(ListLeft, FlippedRow, Length, Piece, FriendlySegment),
	check_prohibited_horizontal_flip(Board, ListLeft, Row, Number, FriendlySegment),
	!.


check_if_can_flip_horizontal(Row, Letter, Length, Piece, ListRight, ListLeft):-
	get_flip_list_horizontal(Row, Letter, Piece, Length, UnsortedListRight, UnsortedListLeft),
	sort(UnsortedListRight, ListRight),
    sort(UnsortedListLeft, ListLeft).


count_new_friendly_horizontal_segment([], _, _, _, 0):-!.
count_new_friendly_horizontal_segment(SortedList, Row, Length, Piece, FriendlySegment):-
	nth1(1, SortedList, First),
	count_right(Row, First, Length, Piece, Piece, -1, AfterFirstCount),
	count_left(Row, First, Piece, Piece, 0, BeforeFirstCount),
	FriendlySegment is AfterFirstCount + BeforeFirstCount.


get_flip_list_horizontal(Row, Letter, Piece, Length, ListRight, ListLeft):-
	PosRight is Letter + 1,
	PosLeft is Letter - 1,
	nth1(PosRight, Row, N),
	check_right(Row, PosRight, Length, N, Piece, [], ListRight),
	nth1(PosLeft, Row, N1),
	check_left(Row, PosLeft, N1, Piece, [], ListLeft).

check_right(_,_,_,Piece,Piece,Acc,Acc):-!.
check_right(_, Length, Length, _, _, _, []):-!.
check_right(_,_,_,x,_,_,[]):-!.
check_right(Row, CurPos, Length, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece,
	CurPiece \= x,
	append(CurList, [CurPos], CurList1),
	CurPos1 is CurPos + 1,
	nth1(CurPos1, Row, N),
	check_right(Row, CurPos1, Length, N, Piece, CurList1, Acc).

check_left(_,_,Piece,Piece,Acc,Acc):-!.
check_left(_, 1, _, _, _, []):-!.
check_left(_,_,x,_,_,[]):-!.
check_left(Row, CurPos, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece,
	CurPiece \= x,
	append(CurList, [CurPos], CurList1),
	CurPos1 is CurPos - 1,
	nth1(CurPos1, Row, N),
	check_left(Row, CurPos1, N, Piece, CurList1, Acc).























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
% ------------- Horizontal Flip Exception ----------------
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

count_down(_, 1, _, _, _, Count, Count):-!.
count_down(Board, CurPos, Letter, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos - 1,
    nth1(CurPos1, Board, Row),
    nth1(Letter, Row, N),
    count_down(Board, CurPos1, Letter, N, Piece, Count1, Final).

count_down(_, _, _, N, Piece, Count, Count):-
    N \= Piece.
