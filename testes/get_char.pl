clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.
	
convert_char_to_number(Ascii, Number):-
	char_code(Ascii, Code),
	Number is Code - 48.

get_height(Height) :-
    write('Enter the chosen Height: '),
    peek_char(Ch),
    get_char_not_nl(Ch, Height).

get_length(Length) :-
    write('Enter the chosen Length: '),
    peek_char(Ch),
    get_char_not_nl(Ch, Length).
	
get_char_not_nl('\n',Next):-
	get_char(_),
	get_char_not_nl(Next).
get_char_not_nl(Char,Char):-
	get_char(_).

get_human_input(L,N):-
	write('Please input your move in the format lN (a4 p.e.)'),
	peek_char(Ch),
	get_char_not_nl(Ch,L),
	peek_char(Ch1),
	get_char_not_nl(Ch1,ChNumber),
	convert_char_to_number(ChNumber, N),
	clear_buffer. % Missing validation

test:-
	write('Testing:\n'),
	get_human_input(L,N),
	write(L),
	nl,
	write(N).