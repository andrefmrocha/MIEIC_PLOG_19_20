:- use_module(library(ansi_term)).
:- ensure_loaded('board_pieces.pl').
:- ensure_loaded('board_generation.pl').

% created to ensure there's a clear separation line between board lines
middle_separator(Line, Head, Tail) :- 
	write(' '), Head, 
	middle_separator_helper(Line, Tail).

% create a separator between the board and the bottom left and right corners
middle_separator_helper([_ | []], Element) :- hdiv, Element, nl.

middle_separator_helper([_ | T], Last) :- hdiv, mdiv, middle_separator_helper(T, Last).

% creates the outskirt lines for board
outskirt_separator_helper([_ | []], _, End) :- hdiv, End, nl.

outskirt_separator_helper([_ | R], Middle, End) :- hdiv, Middle, outskirt_separator_helper(R, Middle, End).

outskirt_separator([_, _ | Rest], Head, Middle, End) :-
	write(' '), Head,
	outskirt_separator_helper(Rest, Middle, End).

% general_line_display(Line, Row_Number). -> 2 - 6
general_line_display([],_).
general_line_display([H | []], N):-
	display_last_line(H, N), !.
general_line_display([H | T], N) :-
	middle_separator(H, tr, tl),
	write(N), vdiv, display_line(H), nl, !,
	N1 is N + 1,
	general_line_display(T, N1).

% display_second_line(Line, Row_Number). -> 1
display_second_line([H | T], N) :-
	middle_separator(H, ulc, urc),
	write(N), vdiv, display_line(H), nl, !,
	N1 is N + 1,
	general_line_display(T, N1).

% display_first_line(Board, Row_Number). -> 0
display_first_line([H | T], N) :-
	write('   '), outskirt_separator(H, ulc, td, urc),
	write(N), write('  '), display_line(H), nl, !,
	N1 is N + 1,
	display_second_line(T, N1).

% display_last_line(Line, Row_Number). -> 7
display_last_line(Line, N) :-
	middle_separator(Line, llc, lrc),
    write(N),
	write('  '), display_line(Line), nl, !,
	write('   '), outskirt_separator(Line, llc, tu, lrc).

% display_line(Line).
display_line([]).
display_line([corner | []]):-
	corner,
	null.
display_line([Element | Rest]) :- 
	Element, vdiv,
	display_line(Rest).

display_game([Line | TBoard], Player):-
	write('Player turn: '), write(Player), nl, nl,
	write_columns_index(Line, 'A'),
	display_first_line([Line | TBoard], 0).

write_columns_helper([_ | []], Letter) :-
	!, write(Letter), nl.

write_columns_helper([_ | TLine], Letter) :-
	write(Letter), write('  '),
	char_code(Letter, LetterCode),	% TODO: ver se há forma mais simples de iterar
    NextLetterCode is LetterCode + 1,
	char_code(NextLetter, NextLetterCode),
	write_columns_helper(TLine, NextLetter).

write_columns_index(Line, Letter) :-
	write('   '), write_columns_helper(Line, Letter).