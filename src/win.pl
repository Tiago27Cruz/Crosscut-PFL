% --------------------------------------------------------
% --------------------- Check Win ------------------------
% --------------------------------------------------------

% game_over(+State, -Winner)
% Checks if the game is over. If yes, Winner is the player that won, else Winner = x. Since in order to win, a winning move must be done, the move is also given as input in order to avoid checking the whole board.
game_over(state(_,_,_,Board,Height,Length), Winner):-
	check_vertical_win(state(_,_,_,Board,Height,Length), 2, WinnerVertical),
	check_horizontal_win(state(_,_,_,Board,Height,Length), 2, WinnerHorizontal),
	verify_winner(WinnerVertical, WinnerHorizontal, Winner).

% verify_winner(+WinnerVertical, +WinnerHorizontal, -Winner)
% Checks if there is a winner. If yes, Winner is the player that won, else Winner = x
% Case in which there is a vertical win
verify_winner(Winner, _, Winner):-
	Winner \= x,
	!.
% Case in which there is a horizontal win
verify_winner(_, Winner, Winner):-
	Winner \= x,
	!.
% Case in which there is no winner
verify_winner(_,_,x):-!.

% --------------------------------------------------------
% ----------------- Check Vertical Win -------------------
% --------------------------------------------------------

% check_vertical_win(+State, +CurLetter, -Winner)
% Check if there are (Height - 2) consecutive pieces of the same color in a vertical line.
% Base case where it didnt find a winner
check_vertical_win(state(_,_,_,_,_,Length), Length,x):-!.
% Case in which there is a vertical win
check_vertical_win(state(_,_,_,Board,Height,_), CurLetter, Winner):-
	nth1(2, Board, Row), % Get the top row
	TopNumber is Height - 1,
	nth1(CurLetter, Row, Winner), % Get the letter of the top row
	Winner \= x,
	reverse(Board, ReversedBoard),
	count_down(ReversedBoard, TopNumber, CurLetter, Winner, Winner, 0, Result), % Check if there are (Height - 2) consecutive pieces of the same color in a vertical line
	WinningCondition is Height - 2,
	Result >= WinningCondition, % If true, there is a vertical win
	!.
% Case in which there is no vertical win so it checks the next row
check_vertical_win(state(_,_,_,Board,Height,Length), CurLetter, Winner):-
	CurLetter1 is CurLetter + 1,
	check_vertical_win(state(_,_,_,Board,Height,Length), CurLetter1, Winner),
	!.

% --------------------------------------------------------
% ---------------- Check Horizontal Win ------------------
% --------------------------------------------------------

% check_horizontal_win(+State, +CurNumber, -Winner)
% Check if there are (Length - 2) consecutive pieces of the same color in a horizontal line.
% Base case where it didnt find a winner
check_horizontal_win(state(_,_,_,_,Height,_), Height,x):-!.
% Case in which there is a horizontal win
check_horizontal_win(state(_,_,_,Board,_,Length),CurNumber,Winner):-
	nth1(CurNumber, Board, Row), % Get the row
	nth1(2, Row, Winner), % Get the letter of the row
	Winner \= x,
	count_right(Row, 2, Length, Winner, Winner, 0, Result), % Check if there are (Length - 2) consecutive pieces of the same color in a horizontal line
	WinningCondition is Length - 2,
	Result >= WinningCondition, % If true, there is a horizontal win
	!.
% Case in which there is no horizontal win so it checks the next row
check_horizontal_win(state(_,_,_,Board,Height,Length),CurNumber,Winner):-
	CurNumber1 is CurNumber + 1,
	check_horizontal_win(state(_,_,_,Board,Height,Length),CurNumber1,Winner),
	!.
