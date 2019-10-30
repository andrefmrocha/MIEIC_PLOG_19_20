:- use_module(library(ansi_term)).
:- ensure_loaded('board_pieces.pl').
:- ensure_loaded('board_states.pl').
:- ensure_loaded('board_generation.pl').

% created to ensure there's a clear separation line between board lines
middle_separator :- write(' '), tr, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, tl, nl.

% creates a separator between the board and the upper left and right corners
second_separator :- write(' '), ulc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, urc, nl.

% create a separator between the board and the bottom left and right corners
penultimate_separator :- write(' '), llc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, lrc, nl.

% creates the line first line for board
first_separator :- write(' '), ulc, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, urc, nl.

% creates the last line for the board
last_separator :- write(' '), write('   '), llc, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, lrc, nl.

% general_line_display(Line, Row_Number). -> 2 - 6
general_line_display([],_).
general_line_display([H | []], N):-
	display_last_line(H, N).
general_line_display([H | T], N) :-
	middle_separator,
	write(N), vdiv, display_line(H), nl,
	N1 is N + 1,
	general_line_display(T, N1).

% display_second_line(Line, Row_Number). -> 1
display_second_line([H | T], N) :-
	second_separator,
	write(N), vdiv, display_line(H), nl,
	N1 is N + 1,
	general_line_display(T, N1).

% display_first_line(Line, Row_Number). -> 0
display_first_line([H | T], N) :-
	write('   '), first_separator,
	write(N), write('  '), display_line(H), nl,
	N1 is N + 1,
	display_second_line(T, N1).

% display_last_line(Line, Row_Number). -> 7
display_last_line(Line, N) :-
	penultimate_separator,
    write(N),
	write('  '), display_line(Line), nl,
	last_separator.

% display_line(Line).
display_line([]).
display_line([corner | []]):-
	corner,
	null.
display_line([Element | Rest]) :- 
	Element, vdiv,
	display_line(Rest).

display_game(Board, Player):-
	write('Player turn: '),
	write(Player), nl, nl,
	write('   A  B  C  D  E  F  G  H'),nl,
	display_first_line(Board, 0).

cute_display :- 
	clear,
	empty_board(X),
	%debug_board(X),
	initialize_board(X, Board),
	display_game(Board, 'John Doe').




