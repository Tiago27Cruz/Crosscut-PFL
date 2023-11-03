% --------------------------------------------------------
% --------------------- Check Win ------------------------
% --------------------------------------------------------

check_win(state(_,_,_,Board,Height,Length), move(Number, Letter)):-
	check_vertical_win(Board, Letter, Height),
	check_horizontal_win(Board, Number, Length, Height).

% --------------------------------------------------------
% ----------------- Check Vertical Win -------------------
% --------------------------------------------------------

check_vertical_win([Head|Tail], Letter,Height):-
	nth1(Letter, Head, N),
	check_vertical_win_aux(Tail, Letter, N, N, 0, Height).

check_vertical_win_aux([], _, N, N, Counter, Height):-
	N \= x,
	Counter1 is Counter + 1,
	Counter1 >= Height - 2,
	!,
	fail.

check_vertical_win_aux([], _, _, _, _, _).

check_vertical_win_aux([Head|Tail], Letter, x, x, _, Height):-
	nth1(Letter, Head, N),
	check_vertical_win_aux(Tail, Letter, x, N, 1, Height).

check_vertical_win_aux([Head|Tail], Letter, N, N, Counter, Height):-
	N \= x,
	Counter1 is Counter + 1,
	Counter1 < Height - 2,
	nth1(Letter, Head, N1),
	check_vertical_win_aux(Tail, Letter, N, N1, Counter1, Height).

check_vertical_win_aux([Head|Tail], Letter, N, N1, _, Height):-
	N \= N1,
	nth1(Letter, Head, N2),
	check_vertical_win_aux(Tail, Letter, N1, N2, 1, Height).


% --------------------------------------------------------
% ---------------- Check Horizontal Win ------------------
% --------------------------------------------------------

check_horizontal_win(Board, Number, Length, Height):-
	Pos is Height-Number,
	nth0(Pos, Board, Row),
	nth1(1, Row, N),
	check_horizontal_win_aux(Row, N, 0, Length).

check_horizontal_win_aux([], _,_,_).

check_horizontal_win_aux([x|Tail], x, _, Length):-
	check_horizontal_win_aux(Tail, x, 0, Length).

check_horizontal_win_aux([Head|Tail], Head, Counter, Length):-
	Head \= x,
	Counter1 is Counter + 1,
	Counter1 < Length - 2,
	check_horizontal_win_aux(Tail, Head, Counter1, Length).

check_horizontal_win_aux([Head|Tail], N, _, Length):-
	N \= Head,
	check_horizontal_win_aux(Tail, Head, 1, Length).
