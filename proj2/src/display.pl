:-ensure_loaded('board_pieces.pl').


%! outskirt_separator_helper(+Line, +Middle, +End)
% Helps to print separators for the outskirt Line's
% Switches between hdiv and Middle until the Line is empty, when it prints the End char
outskirt_separator_helper([_ | []], _, End) :- hdiv, End, nl.
outskirt_separator_helper([_ | R], Middle, End) :- hdiv, Middle, outskirt_separator_helper(R, Middle, End).

%! outskirt_separator(+Line, +Head, +Middle, +End)
% Prints separators for the outskirt Line's
% using the help of @see outskirt_separator_helper
% Prints the Head of the Line and the helper takes care of the rest
outskirt_separator(Line, Head, Middle, End) :-
	write(' '), Head,
	outskirt_separator_helper(Line, Middle, End).


%! middle_separator_helper(+Line, +End)
% Helps to print separators for the middle Line's
% Switches between hdiv and midv until the Line is empty, when it prints the End char
middle_separator_helper([_ | []], End) :- hdiv, End, nl.
middle_separator_helper([_ | T], End) :- hdiv, mdiv, middle_separator_helper(T, End).

%! middle_separator(+Line, +Head, +End)
% Prints separators for the middle Line's
% using the help of @see middle_separator_helper
% Begins by printing the Head of the Line and then calls the helper
middle_separator(Line, Head, End) :- 
	write(' '), Head, 
	middle_separator_helper(Line, End).

%! display_line(+Line).
% Displays one puzzle row alternating between the element
% (@see display_element) and a vertical separator (vdiv)
display_line([]).
display_line([Element | Rest]) :- 
	display_element(Element), vdiv,
	display_line(Rest).

%! display_last_line(+Line).
% Displays last Line of the Puzzle Board
% Calls @see middle_separator to display the second last separator
% with tr as the head of the line and tl as the end
% Uses @see display_line to display the line and, finally,
% calls @see outskirt_separator to display the last separator
% with llc as the head of the line, tu as the middle separators and lrc as the end
display_last_line(Line) :-
	middle_separator(Line, tr, tl),
	write(' '), vdiv, display_line(Line), nl, !,
	outskirt_separator(Line, llc, tu, lrc).


%! general_line_display(+Board).
% Displays every line of the puzzle except the last one
% Calls @see middle_separator to display the middle separator
% with tr as the head of the line and tl as the end
% Uses @see display_line to display the line and
% calls itself to print the next line
% When there is only one row left calls @see display_last_line.
general_line_display([H | []]):-
	display_last_line(H), !.
general_line_display([H | T]) :-
	middle_separator(H, tr, tl),
	write(' '), vdiv, display_line(H), nl, !,
	general_line_display(T).

%! display_board(+Board)
% Prints the puzzle board
% Uses @see outskirt_separator with ulc as the head, td as the middle and urc as the end
% to the initial border of the puzzle and then calls @see display_line to print the first row
% Calls @see general_line_display that will print all the lines and its separators except the last one
display_board([H | T]) :-
	outskirt_separator(H, ulc, td, urc),
	write(' '), vdiv, display_line(H), nl, !,
	general_line_display(T).



