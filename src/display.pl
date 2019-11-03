:- use_module(library(ansi_term)).
:- ensure_loaded('board_pieces.pl').
:- ensure_loaded('board_generation.pl').

% created to ensure there's a clear separation line between board lines
middle_separator(Line) :- 
	write(' '), tr, 
	middle_separator_helper(Line, 'M').

% creates a separator between the board and the upper left and right corners
second_separator(Line) :- 
	write(' '), ulc, 
	middle_separator_helper(Line, 'S').

% create a separator between the board and the bottom left and right corners
middle_separator_helper([_ | []], 'S') :- hdiv, urc, nl.
middle_separator_helper([_ | []], 'M') :- hdiv, tl, nl.
middle_separator_helper([_ | []], 'P') :- hdiv, lrc, nl.

middle_separator_helper([_ | T], Last) :- hdiv, mdiv, middle_separator_helper(T, Last).

penultimate_separator(Line) :- 
	write(' '), llc,
	middle_separator_helper(Line, 'P').

% creates the line first line for board
first_separator_helper([_ | []]) :- hdiv, urc, nl.

first_separator_helper([_ | T]) :- hdiv, td, first_separator_helper(T).

first_separator([_, _ | Tail]) :- 
	write(' '), ulc,
	first_separator_helper(Tail).  

% creates the last line for the board
last_separator_helper([_ | []]) :- hdiv, lrc, nl.

last_separator_helper([_ | T]) :- hdiv, tu, last_separator_helper(T).

last_separator([_, _ | Tail]) :- 
	write(' '), llc, 
	last_separator_helper(Tail).

% general_line_display(Line, Row_Number). -> 2 - 6
general_line_display([],_).
general_line_display([H | []], N):-
	display_last_line(H, N), !.
general_line_display([H | T], N) :-
	middle_separator(H),
	write(N), vdiv, display_line(H), nl, !,
	N1 is N + 1,
	general_line_display(T, N1).

% display_second_line(Line, Row_Number). -> 1
display_second_line([H | T], N) :-
	second_separator(H),
	write(N), vdiv, display_line(H), nl, !,
	N1 is N + 1,
	general_line_display(T, N1).

% display_first_line(Board, Row_Number). -> 0
display_first_line([H | T], N) :-
	write('   '), first_separator(H),
	write(N), write('  '), display_line(H), nl, !,
	N1 is N + 1,
	display_second_line(T, N1).

% display_last_line(Line, Row_Number). -> 7
display_last_line(Line, N) :-
	penultimate_separator(Line),
    write(N),
	write('  '), display_line(Line), nl, !,
	write('   '), last_separator(Line).

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
