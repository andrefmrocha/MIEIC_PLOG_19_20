:-ensure_loaded('board_pieces.pl').

outskirt_separator_helper([_ | []], _, End) :- hdiv, End, nl.
outskirt_separator_helper([_ | R], Middle, End) :- hdiv, Middle, outskirt_separator_helper(R, Middle, End).
outskirt_separator(Line, Head, Middle, End) :-
	write(' '), Head,
	outskirt_separator_helper(Line, Middle, End).

middle_separator_helper([_ | []], Element) :- hdiv, Element, nl.
middle_separator_helper([_ | T], Last) :- hdiv, mdiv, middle_separator_helper(T, Last).
middle_separator(Line, Head, Tail) :- 
	write(' '), Head, 
	middle_separator_helper(Line, Tail).

display_line([]).
display_line([Element | Rest]) :- 
	display_element(Element), vdiv,
	display_line(Rest).

display_last_line(Line) :-
	middle_separator(Line, tr, tl),
	write(' '), vdiv, display_line(Line), nl, !,
	outskirt_separator(Line, llc, tu, lrc).

general_line_display([H | []]):-
	display_last_line(H), !.
general_line_display([H | T]) :-
	middle_separator(H, tr, tl),
	write(' '), vdiv, display_line(H), nl, !,
	general_line_display(T).


display_board([H | T]) :-
	outskirt_separator(H, ulc, td, urc),
	write(' '), vdiv, display_line(H), nl, !,
	general_line_display(T).



