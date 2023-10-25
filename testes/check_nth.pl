:- use_module(library(lists)).

% Check if N-2 contiguous elements are the same in a list
check_contiguous_n_minus_2(N, List) :-
    length(List, Len),
    Len >= N,
    check_contiguous_n_minus_2(N, List, 0).

check_contiguous_n_minus_2(N, [X|Rest], Count) :-
    N1 is N - 2,
    (check_contiguous_n_minus_2_helper(N1, X, Rest, Count) ; NewCount is Count + 1, check_contiguous_n_minus_2(N, Rest, NewCount)).

check_contiguous_n_minus_2(_, _, Count) :- Count > 0.

check_contiguous_n_minus_2_helper(0, _, _, _).
check_contiguous_n_minus_2_helper(N, X, [X|Rest], Count) :-
    N1 is N - 1,
    check_contiguous_n_minus_2_helper(N1, X, Rest, Count).

% Predicate to check if there are N - 2 contiguous nth elements with the same value
check_contiguous_nth_elements(N, List) :-
    length(List, Len),
    Len > 1, % Ensure there are at least two sublists to compare
    check_contiguous_n_minus_2(N, List).

% Example usage:
test_list([[x,b,b], [x,c,d], [x,x,x],[a,b,b], [a,c,d], [a,x,x],[a,b,b], [a,c,d], [a,x,x]]).

test :-
    N = 9, % Set N to the desired value
    test_list(List),
    check_contiguous_nth_elements(N, List),
    write('There are '), write(N-2), write(' contiguous nth elements with the same value.'), nl.
