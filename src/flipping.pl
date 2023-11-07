% --------------------------------------------------------
% ------------------------ Flip --------------------------
% --------------------------------------------------------


% flip_pieces(+Letter, +Direction, +Board, +Piece, -NewBoard, +List)
% flip_pieces(+Number, +Direction, +Board, +Piece, -NewBoard, +List)
% Receives a letter or a number, depending on the direction and flips every piece on the given List. Returns the NewBoard with the pieces flipped.
% When it finishes going through the list, returns the new board
flip_pieces(_, _, Board, _, Board, []):-!.
% Vertical flip
flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, [Head|Tail]):-
	make_move(Board,Head,Letter,Piece,ChangedBoard,1,1), % Calls make_move with bypass and silent on so it may change pieces already placed and not print anything
	flip_pieces(Letter, 'Vertical', ChangedBoard, Piece, NewBoard, Tail). % Recursive call to flip the next piece
% Horizontal flip
flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, [Head|Tail]):-
	make_move(Board,Number,Head,Piece,ChangedBoard,1,1), % Calls make_move with bypass and silent on so it may change pieces already placed and not print anything
	flip_pieces(Number, 'Horizontal', ChangedBoard, Piece, NewBoard, Tail). % Recursive call to flip the next piece

% --------------------------------------------------------
% ------------------ Vertical Flip -----------------------
% --------------------------------------------------------

% try_to_flip_vertical(+Letter, +Number, +Board, +Piece, -NewBoard, +Height)
% Receives a letter and a number and tries to flip the pieces vertically. Returns the NewBoard with the pieces flipped.
% Case where the it flips the pieces vertically
try_to_flip_vertical(Letter, Number, Board, Piece, NewBoard, Height):-
	reverse(Board, ReversedBoard), % Reverses the board so it can be used in the check_if_can_flip_vertical predicate
	check_if_can_flip_vertical(ReversedBoard, Letter,Number, Piece, ListAbove, ListBelow, Height), % Checks if the vertical flip is valid by checking the pieces above and below the piece in the given position and returns the list of pieces to be flipped sorted
	append(ListAbove, ListBelow, List), % Appends the list of pieces above and below the piece in the given position
	List \= [], % Checks if the list is not empty, meaning there are pieces to be flipped
	validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, List, ListAbove, ListBelow). % Flips the pieces and validates the move according to the rules
% Case where the it doesn't flip the pieces vertically
try_to_flip_vertical(_,_, Board, _, Board,_):-
    !,
    fail.

% validate_vertical_flipping(+Board, +Letter, +Height, +Piece, -NewBoard, +List, +ListAbove, +ListBelow)
% Validates the vertical flip by flipping the pieces (assuming it's right) and checking if the new segment made is bigger than the opponent's segment flanked. If not it fails and the move is declared invalid
% Checks if both the flip above and below is valid at the same time
validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, List, _, _):-
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, List),
	reverse(NewBoard, NewBoard1),
	reverse(Board, ReversedBoard),
	count_new_friendly_vertical_segment(List, NewBoard1, Letter, Height, Piece, FriendlySegment),
	check_prohibited_vertical_flip(ReversedBoard, List, Letter, FriendlySegment),
	!.
% Checks if only the flip above is valid
validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, _, ListAbove, _):-
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, ListAbove),
	reverse(NewBoard, NewBoard1),
	reverse(Board, ReversedBoard),
	count_new_friendly_vertical_segment(ListAbove, NewBoard1, Letter, Height, Piece, FriendlySegment),
	check_prohibited_vertical_flip(ReversedBoard, ListAbove, Letter, FriendlySegment),
	!.
% Checks if only the flip below is valid
validate_vertical_flipping(Board, Letter, Height, Piece, NewBoard, _, _, ListBelow):-
	flip_pieces(Letter, 'Vertical', Board, Piece, NewBoard, ListBelow),
	reverse(NewBoard, NewBoard1),
	reverse(Board, ReversedBoard),
	count_new_friendly_vertical_segment(ListBelow, NewBoard1, Letter, Height, Piece, FriendlySegment),
	check_prohibited_vertical_flip(ReversedBoard, ListBelow, Letter, FriendlySegment),
	!.

% check_if_can_flip_vertical(+Board, +Letter, +Piece, -List)
% Gets the list of pieces to be flipped above and below the piece in the given position
check_if_can_flip_vertical(Board, Letter,Number, Piece, ListAbove, ListBelow, Height):-
	get_flip_list_vertical(Board, Letter, Number, Height, Piece, UnsortedListAbove, UnsortedListBelow),
	sort(UnsortedListAbove, ListAbove),
	sort(UnsortedListBelow, ListBelow).
% count_new_friendly_vertical_segment(+SortedList, +Board, +Letter, +Height, +Piece, -FriendlySegment)
% Counts the number of pieces of the same type as Piece in the new vertical segment and returns the result in FriendlySegment
% Case where the list is empty so none were found
count_new_friendly_vertical_segment([], _, _, _, _, 0):-!.
% Case where it found at least 1 piece
count_new_friendly_vertical_segment(SortedList, Board, Letter, Height, Piece, FriendlySegment):-
	nth1(1, SortedList, First), % get's the first piece of the list since it's already sorted
	count_up(Board, First, Letter, Height, Piece, Piece, -1, AboveFirstCount), % counts the number of pieces of the same type as Piece above the first piece
	count_down(Board, First, Letter, Piece, Piece, 0, BelowFirstCount), % counts the number of pieces of the same type as Piece below the first piece, Since it's 0, it counts with the current piece already
	FriendlySegment is AboveFirstCount + BelowFirstCount. % calculates the total number of pieces of the same type as Piece in the vertical segment

% get_flip_list_vertical(+Board, +Letter, +Number, +Height, +Piece, -ListAbove, -ListBelow)
% Gets the list of pieces to be flipped above and below the piece in the given position
% Case where the piece is in the first position of the column, meaning it only needs to check above
get_flip_list_vertical(Board, Letter, 1, Height, Piece, ListAbove, []):-
	nth1(2, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board,2, Letter, Height, N, Piece, [], ListAbove),
	!.
% Case where the piece is in the last position of the column, meaning it only needs to check below
get_flip_list_vertical(Board, Letter, Height, Height, Piece, [], ListBelow):-
	PosDown is Height - 1,
    nth1(PosDown, Board, Row1),
	nth1(Letter, Row1, N1),
	check_down(Board,PosDown, Letter, N1, Piece, [], ListBelow),
	!.
% Case where the piece is in the middle of the column, meaning it needs to check above and below
get_flip_list_vertical(Board, Letter, Number, Height, Piece, ListAbove, ListBelow):-
	PosUp is Number + 1,
	PosDown is Number - 1,
	nth1(PosUp, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board,PosUp, Letter, Height, N, Piece, [], ListAbove),
	nth1(PosDown, Board, Row1),
	nth1(Letter, Row1, N1),
	check_down(Board,PosDown, Letter, N1, Piece, [], ListBelow).

% check_up(+Board, +CurPos, +Letter, +Height, +CurPiece, +Piece, +CurList, -Acc)
% Checks the pieces above the given position and returns the list of pieces to be flipped
% Case where it reaches the top of the board and finds a Piece the same as the playing player, meaning a flip is valid
check_up(_, Height, _, Height, Piece, Piece, Acc, Acc):-!.
% Case where it reaches the top of the board without finding a Piece the same as the playing player, meaning a flip is not valid
check_up(_, Height, _, Height, _, _, _, []):-!.
% Case where it finds an empty space, meaning a flip is not valid
check_up(_,_,_,_,x,_,_,[]):-!.
% Case where it finds a second piece, meaning the flip is valid and returns the list of pieces between those two
check_up(_,_,_,_,Piece,Piece,Acc,Acc):-!.
% Case where it finds an opponent piece, adding it to the list and calling itself recursively
check_up(Board, CurPos, Letter, Height, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece,
	CurPiece \= x,
	append(CurList, [CurPos], CurList1),
	CurPos1 is CurPos + 1,
	nth1(CurPos1, Board, Row),
	nth1(Letter, Row, N),
	check_up(Board, CurPos1, Letter, Height, N, Piece, CurList1, Acc).

% check_down(+Board, +CurPos, +Letter, +CurPiece, +Piece, +CurList, -Acc)
% Checks the pieces below the given position and returns the list of pieces to be flipped
% Case where it reaches the end of the board and finds a Piece the same as the playing player, meaning a flip is valid
check_down(_, 1, _, Piece, Piece, Acc, Acc):-!.
% Case where it reaches the end of the board without finding a Piece the same as the playing player, meaning a flip is not valid
check_down(_, 1, _, _, _, _, []):-!.
% Case where it finds an empty space, meaning a flip is not valid
check_down(_,_,_,x,_,_,[]):-!.
% Case where it finds a second piece, meaning the flip is valid and returns the list of pieces between those two
check_down(_,_,_,Piece,Piece,Acc,Acc):-!.
% Case where it finds an opponent piece, adding it to the list and calling itself recursively
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

% check_prohibited_vertical_flip(+Board, +List, +Letter, +FlipSize)
% Checks if the vertical flip is prohibited due to the opponent having a bigger segment that would be flanked by the flip by going through the list and checking it's horizontal segment
% Base case where the flip is not prohibited since it reaches the end of the list without failing
check_prohibited_vertical_flip(_, [], _, _):-!.
% Recursively checks every horizontal segment on the list of flipped pieces of the opponent
check_prohibited_vertical_flip(Board, [Head|Tail], Letter, FlipSize):-
    get_game_state(state(_,_,_,_,_,Length)), % Gets the length of the board
    nth1(Head, Board, Row),
    nth1(Letter, Row, Piece),
    count_left(Row, Letter, Piece, Piece, -1, Left), % Counts the number of pieces of the same type as Piece to the left of the current position
    count_right(Row, Letter, Length, Piece, Piece, -1, Right), % Counts the number of pieces of the same type as Piece to the right of the current position
    Size is Left + Right + 1, % Calculates the size of the horizontal segment, +1 because it counts the current piece
    FlipSize > Size, % Checks if the size of the enemy horizontal segment is smaller than the size of the flip
    check_prohibited_vertical_flip(Board, Tail, Letter, FlipSize). % Recursive call to check the next horizontal segment

% count_left(+Row, +CurPos, +CurPiece, +Piece, +Count, -Final)
% Counts the number of pieces of the same type as Piece to the left of the current position
% Case where it reaches the left edge of the board without finding any piece
count_left(_, 1, _, _, -1, 0):-!.
% Case where it found at least 1 piece
count_left(_, 1, _, _, Count, Count):-!.
% When it finds a piece of the same type as Piece, it increments the counter and calls itself recursively
count_left(Row, CurPos, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos - 1,
    nth1(CurPos1, Row, N),
    count_left(Row, CurPos1, N, Piece, Count1, Final).
% When it finds a piece of a different type, it returns the counter
count_left(_, _, N, Piece, Count, Count):-
    N \= Piece.

% count_right(+Row, +CurPos, +Length, +CurPiece, +Piece, +Count, -Final)
% Counts the number of pieces of the same type as Piece to the right of the current position
% Case where it reaches the end of the row without finding any piece
count_right(_, Length, Length, _, _, -1, 0):-!.
% Case where it found at least 1 piece
count_right(_, Length, Length, Piece, Piece, Count, Count):-!.
% When it finds a piece of the same type as Piece, it increments the counter and calls itself recursively
count_right(Row, CurPos, Length, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos + 1,
    nth1(CurPos1, Row, N),
    count_right(Row, CurPos1, Length, N, Piece, Count1, Final).
% When it finds a piece of a different type, it returns the counter
count_right(_, _, _, N, Piece, Count, Count):-
    N \= Piece.


% --------------------------------------------------------
% ----------------- Horizontal Flip ----------------------
% --------------------------------------------------------

% try_to_flip_horizontal(+Number, +Letter, +Board, +Piece, -NewBoard, +Height, +Length)
% Receives a letter and a number and tries to flip the pieces horizontally. Returns the NewBoard with the pieces flipped.
% Case where the it flips the pieces horizontally 
try_to_flip_horizontal(Number, Letter, Board, Piece, NewBoard, Height, Length):- % board is not reversed
    Pos is Height - Number + 1,
    nth1(Pos, Board, Row),
	check_if_can_flip_horizontal(Row, Letter, Length, Piece, ListRight, ListLeft), % Checks if the horizontal flip is valid by checking the pieces to the right and to the left of the piece in the given position and returns the list of pieces to be flipped sorted
	append(ListRight, ListLeft, List), % Appends the list of pieces to the right and to the left of the piece in the given position
	validate_horizontal_flipping(Board, Row, Number, Height,Length, Piece, NewBoard, List, ListRight, ListLeft). % Flips the pieces and validates the move according to the rules
% Case where the it doesn't flip the pieces horizontally
try_to_flip_horizontal(_, _, Board, _, Board, _, _):-
	!,
	fail.

% validate_horizontal_flipping(+Board, +Row, +Number, +Height, +Length, +Piece, -NewBoard, +List, +ListRight, +ListLeft)
% Validates the horizontal flip by flipping the pieces (assuming it's right) and checking if the new segment made is bigger than the opponent's segment flanked
% Checks if both the flip on the left and on the right is valid at the same time
validate_horizontal_flipping(Board, Row, Number, Height, Length, Piece, NewBoard, List, _, _):-
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, List), % Flips the pieces on the left and on the right of the piece placed
	Pos is Height - Number + 1,
    nth1(Pos, NewBoard, FlippedRow), % Gets the row where the flip was made
	count_new_friendly_horizontal_segment(List, FlippedRow, Length, Piece, FriendlySegment), % Counts the number of pieces of the same type as Piece in the new horizontal segment
	check_prohibited_horizontal_flip(Board, List, Row, Pos, FriendlySegment, Height), % Checks if the horizontal flip is prohibited due to the opponent having a bigger segment that would be flanked by the flip
	!.
% Checks if only the flip on the right is valid
validate_horizontal_flipping(Board, Row, Number, Height,Length, Piece, NewBoard, _, ListRight, _):-
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, ListRight), % Flips only the pieces on the right of the piece placed
	Pos is Height - Number + 1,
    nth1(Pos, NewBoard, FlippedRow),
	count_new_friendly_horizontal_segment(ListRight, FlippedRow, Length, Piece, FriendlySegment), % Counts the number of pieces of the same type as Piece in the new horizontal segment
	check_prohibited_horizontal_flip(Board, ListRight, Row, Pos, FriendlySegment, Height), % Checks if the horizontal flip is prohibited due to the opponent having a bigger segment that would be flanked by the flip
	!.
% Checks if only the flip on the left is valid
validate_horizontal_flipping(Board, Row, Number, Height,Length, Piece, NewBoard, _, _, ListLeft):-
	flip_pieces(Number, 'Horizontal', Board, Piece, NewBoard, ListLeft), % Flips the pieces on the left of the piece placed
	Pos is Height - Number + 1,
    nth1(Pos, NewBoard, FlippedRow), % Gets the row where the flip was made
	count_new_friendly_horizontal_segment(ListLeft, FlippedRow, Length, Piece, FriendlySegment), % Counts the number of pieces of the same type as Piece in the new horizontal segment
	check_prohibited_horizontal_flip(Board, ListLeft, Row, Pos, FriendlySegment, Height), % Checks if the horizontal flip is prohibited due to the opponent having a bigger segment that would be flanked by the flip
	!.

% check_if_can_flip_horizontal(+Row, +Letter, +Piece, -List)
% Checks if the horizontal flip is valid by checking the pieces to the right and to the left of the piece in the given position and returns the list of pieces to be flipped sorted
check_if_can_flip_horizontal(Row, Letter, Length, Piece, ListRight, ListLeft):-
	get_flip_list_horizontal(Row, Letter, Piece, Length, UnsortedListRight, UnsortedListLeft),
	sort(UnsortedListRight, ListRight),
    sort(UnsortedListLeft, ListLeft).

% count_new_friendly_horizontal_segment(+SortedList, +Row, +Length, +Piece, -FriendlySegment)
% Counts the number of pieces of the same type as Piece in the new horizontal segment and returns the result in FriendlySegment
% Case where the list is empty so none were found
count_new_friendly_horizontal_segment([], _, _, _, 0):-!.
% Case where it found at least 1 piece
count_new_friendly_horizontal_segment(SortedList, Row, Length, Piece, FriendlySegment):-
	nth1(1, SortedList, First), % get's the first piece of the list since it's already sorted
	count_right(Row, First, Length, Piece, Piece, -1, AfterFirstCount), % counts the number of pieces of the same type as Piece to the right of the first piece
	count_left(Row, First, Piece, Piece, 0, BeforeFirstCount), % counts the number of pieces of the same type as Piece to the left of the first piece
	FriendlySegment is AfterFirstCount + BeforeFirstCount. % calculates the total number of pieces of the same type as Piece in the horizontal segment

% get_flip_list_horizontal(+Row, +Letter, +Piece, +Length, -ListRight, -ListLeft)
% Gets the list of pieces to be flipped to the right and to the left of the piece in the given position
% Case where the piece is in the first position of the row, meaning it only needs to check to the right
get_flip_list_horizontal(Row, 1, Piece, Length, ListRight, []):-
	nth1(2, Row, N), % starts at 2 because the first position won't have any piece
	check_right(Row, 2, Length, N, Piece, [], ListRight),
    !.
% Case where the piece is in the last position of the row, meaning it only needs to check to the left
get_flip_list_horizontal(Row, Length, Piece, Length, [], ListLeft):-
	PosLeft is Length - 1, % -1 because the edge won't have any piece
	nth1(PosLeft, Row, N1),
	check_left(Row, PosLeft, N1, Piece, [], ListLeft),
	!.
% Case where the piece is in the middle of the row, meaning it needs to check to the right and to the left
get_flip_list_horizontal(Row, Letter, Piece, Length, ListRight, ListLeft):-
	PosRight is Letter + 1, % Gets the position to the right of the piece
	PosLeft is Letter - 1, % Gets the position to the left of the piece
	nth1(PosRight, Row, N), % Gets the piece to the right of the piece
	check_right(Row, PosRight, Length, N, Piece, [], ListRight), % Checks the pieces to the right of the piece
	nth1(PosLeft, Row, N1), % Gets the piece to the left of the piece
	check_left(Row, PosLeft, N1, Piece, [], ListLeft). % Checks the pieces to the left of the piece

% check_right(+Row, +CurPos, +Length, +CurPiece, +Piece, +CurList, -Acc)
% Checks the pieces to the right of the given position and returns the list of pieces to be flipped
% Case where it reaches the end of the row and finds a Piece the same as the playing player, meaning a flip is valid
check_right(_,_,_,Piece,Piece,Acc,Acc):-!.
% Case where it reaches the end of the row without finding a Piece the same as the playing player, meaning a flip is not valid
check_right(_, Length, Length, _, _, _, []):-!.
% Case where it finds an empty space, meaning a flip is not valid
check_right(_,_,_,x,_,_,[]):-!.
% Case where it finds an opponent piece, adding it to the list and calling itself recursively
check_right(Row, CurPos, Length, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece, % Checks if the piece is not the same as the playing player
	CurPiece \= x, % Checks if the piece is not an empty space
	append(CurList, [CurPos], CurList1), % Adds the current position to the list
	CurPos1 is CurPos + 1, % Increments the current position since it's going right
	nth1(CurPos1, Row, N), % Gets the next piece to check
	check_right(Row, CurPos1, Length, N, Piece, CurList1, Acc). % Recursive call to check the next piece

% check_left(+Row, +CurPos, +CurPiece, +Piece, +CurList, -Acc)
% Checks the pieces to the left of the given position and returns the list of pieces to be flipped
% Case where it reaches the end of the row and finds a Piece the same as the playing player, meaning a flip is valid
check_left(_,_,Piece,Piece,Acc,Acc):-!.
% Case where it reaches the end of the row without finding a Piece the same as the playing player, meaning a flip is not valid
check_left(_, 1, _, _, _, []):-!.
% Case where it finds an empty space, meaning a flip is not valid
check_left(_,_,x,_,_,[]):-!.
% Case where it finds an opponent piece, adding it to the list and calling itself recursively
check_left(Row, CurPos, CurPiece, Piece, CurList, Acc):-
	CurPiece \= Piece, % Checks if the piece is not the same as the playing player
	CurPiece \= x, % Checks if the piece is not an empty space
	append(CurList, [CurPos], CurList1), % Adds the current position to the list
	CurPos1 is CurPos - 1, % Decrements the current position since it's going left
	nth1(CurPos1, Row, N), % Gets the next piece to check
	check_left(Row, CurPos1, N, Piece, CurList1, Acc). % Recursive call to check the next piece

% --------------------------------------------------------
% ------------- Horizontal Flip Exception ----------------
% --------------------------------------------------------

% check_prohibited_horizontal_flip(+Board, +List, +Row, +Number, +FlipSize, +Height)
% Checks if the horizontal flip is prohibited due to the opponent having a bigger segment that would be flanked by the flip by going through the list and checking it's vertical segment
% Base case where the flip is not prohibited since it reaches the end of the list without failing
check_prohibited_horizontal_flip(_, [], _, _, _,_):-!.
% Recursively checks every vertical segment on the list of flipped pieces of the opponent
check_prohibited_horizontal_flip(Board, [Letter|Tail], Row, Number, FlipSize, Height):-
    nth1(Letter, Row, Piece), % Get's the correct piece from the row
    count_up(Board, Number, Letter, Height, Piece, Piece, -1, Up), % Counts the number of pieces of the same type as Piece above the current position
    count_down(Board, Number, Letter, Piece, Piece, -1, Down), % Counts the number of pieces of the same type as Piece below the current position
    Size is Up + Down + 1, % Calculates the size of the vertical segment, +1 because it counts the current piece
    FlipSize > Size, % Checks if the size of the vertical segment is smaller than the size of the flip
    check_prohibited_horizontal_flip(Board, Tail, Row, Number, FlipSize, Height). % Recursive call to check the next vertical segment

% count_up(+Board, +CurPos, +Letter, +Height, +CurPiece, +Piece, +Count, -Final)
% Counts the number of pieces of the same type as Piece above the current position
% Case where it reaches the top of the board without finding any piece
count_up(_, Height, _, Height, Piece, Piece, -1, 0):-!.
% Case where it found at least 1 piece
count_up(_, Height, _, Height, Piece, Piece, Count, Count):-!.
% When it finds a piece of the same type as Piece, it increments the counter and calls itself recursively
count_up(Board, CurPos, Letter, Height, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos + 1,
    nth1(CurPos1, Board, Row),
    nth1(Letter, Row, N),
    count_up(Board, CurPos1,Letter, Height, N, Piece, Count1, Final).
% When it finds a piece of a different type, it returns the counter
count_up(_, _, _, _, N, Piece, Count, Count):-
    N \= Piece.

% count_down(+Board, +CurPos, +Letter, +CurPiece, +Piece, +Count, -Final)
% Counts the number of pieces of the same type as Piece below the current position
% Case where it reaches the end of the board
count_down(_, 1, _, _, _, -1, 0):-!.
% Case where it found at least 1 piece
count_down(_, 1, _, _, _, Count, Count):-!.
% When it finds a piece of the same type as Piece, it increments the counter and calls itself recursively
count_down(Board, CurPos, Letter, Piece, Piece, Count, Final):-
    Count1 is Count + 1,
    CurPos1 is CurPos - 1,
    nth1(CurPos1, Board, Row),
    nth1(Letter, Row, N),
    count_down(Board, CurPos1, Letter, N, Piece, Count1, Final).
% When it finds a piece of a different type, it returns the counter
count_down(_, _, _, N, Piece, Count, Count):-
    N \= Piece.
