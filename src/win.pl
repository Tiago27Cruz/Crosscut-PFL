% --------------------------------------------------------
% --------------------- Check Win ------------------------
% --------------------------------------------------------

% game_over(+State, +Move, -Winner)
% Checks if the game is over. If yes, Winner is the player that won, else Winner = x. Since in order to win, a winning move must be done, the move is also given as input in order to avoid checking the whole board.
game_over(state(Turn,_,_,Board,Height,Length), move(Number, Letter), Winner):-
	check_vertical_win(Board, Letter, Height, ResultVertical),
	check_horizontal_win(Board, Number, Length, Height, ResultHorizontal),
	verify_winner(Turn, ResultVertical, ResultHorizontal, Winner).

% verify_winner(+Turn, +ResultVertical, +ResultHorizontal, -Winner)
% Checks if there is a winner. If yes, Winner is the player that won, else Winner = x
% Case in which there is a horizontal win
verify_winner(Turn, _, 1, Winner):-
	get_player(state(Turn,_,_,_,_,_), Winner),
	!.
% Case in which there is a vertical win
verify_winner(Turn, 1, _, Winner):-
	get_player(state(Turn,_,_,_,_,_), Winner), 
	!.
% Case in which there is no win
verify_winner(_, _, _, x).


% --------------------------------------------------------
% ----------------- Check Vertical Win -------------------
% --------------------------------------------------------

% check_vertical_win(+Board, +Letter, +Height, -Result)
% Checks if there are (Height - 2) consecutive pieces of the same color in a vertical line. If yes result = 1, else result = 0
check_vertical_win([Head|Tail], Letter,Height, Result):-
	nth1(Letter, Head, N),
	check_vertical_win_aux(Tail, Letter, N, N, 0, Height, Result).

% check_vertical_win_aux(+Board, +Letter, +LastNumber, +CurrentNumber, +Counter, +Height, -Result)
% Auxiliar function to check if there are (Height - 2) consecutive pieces of the same color in a vertical line. If yes result = 1, else result = 0
% Case in which there is a vertical win
check_vertical_win_aux([], _, N, N, Counter, Height, 1):-
	N \= x,
	Counter1 is Counter + 1,
	Counter1 >= Height - 2,
	!.
% Case in which there is no vertical win
check_vertical_win_aux([], _, _, _, _, _, 0).
% Case in which there is an x in the vertical segment
check_vertical_win_aux([Head|Tail], Letter, x, x, _, Height, Result):-
	nth1(Letter, Head, N),
	check_vertical_win_aux(Tail, Letter, x, N, 1, Height, Result).

check_vertical_win_aux([Head|Tail], Letter, N, N, Counter, Height, Result):-
	N \= x,
	Counter1 is Counter + 1,
	Counter1 < Height - 2,
	nth1(Letter, Head, N1),
	check_vertical_win_aux(Tail, Letter, N, N1, Counter1, Height, Result).

check_vertical_win_aux([Head|Tail], Letter, N, N1, _, Height, Result):-
	N \= N1,
	nth1(Letter, Head, N2),
	check_vertical_win_aux(Tail, Letter, N1, N2, 1, Height, Result).


% --------------------------------------------------------
% ---------------- Check Horizontal Win ------------------
% --------------------------------------------------------

% check_horizontal_win(+Board, +Number, +Length, +Height, -Result)
% Checks if there are (Length - 2) consecutive pieces of the same color in a horizontal line. If yes result = 1, else result = 0
check_horizontal_win(Board, Number, Length, Height, Result):-
	Pos is Height-Number,
	nth0(Pos, Board, Row),
	nth1(1, Row, N),
	check_horizontal_win_aux(Row, N, 0, Length, Result).

check_horizontal_win_aux([], _,_,_,0).

check_horizontal_win_aux([x|Tail], x, _, Length, Result):-
	check_horizontal_win_aux(Tail, x, 0, Length, Result).

check_horizontal_win_aux([Head|Tail], Head, Counter, Length, Result):-
	Head \= x,
	Counter1 is Counter + 1,
	Counter1 < Length - 2,
	check_horizontal_win_aux(Tail, Head, Counter1, Length, Result).

check_horizontal_win_aux([Head|_], Head, Counter, Length, 1):-
	Head \= x,
	Counter1 is Counter + 1,
	Counter1 >= Length - 2,
	!.

check_horizontal_win_aux([Head|Tail], N, _, Length, Result):-
	N \= Head,
	check_horizontal_win_aux(Tail, Head, 1, Length, Result).
