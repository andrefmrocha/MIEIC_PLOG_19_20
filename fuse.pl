use_module(library(ansi_term)).

clear :- write('\e[2J').

ulc :- ansi_format([fg(cyan)], '~c', [9556]). % ╔ : upper left corner
urc :- ansi_format([fg(cyan)], '~c', [9559]). % ╗ : upper right corner
llc :- ansi_format([fg(cyan)], '~c', [9562]). % ╚ : lower left corner
lrc :- ansi_format([fg(cyan)], '~c', [9565]). % ╝ : lower right corner

hdiv :- ansi_format([fg(cyan)], '~c', [9552]), ansi_format([fg(cyan)], '~c', [9552]). % ═ : horizontal division
vdiv :- ansi_format([fg(cyan)], '~c', [9553]). % ║ : vertical division
mdiv :- ansi_format([fg(cyan)], '~c', [9580]). % ╬ : middle division

tr :- ansi_format([fg(cyan)], '~c', [9568]). % ╠ : T right
tl :- ansi_format([fg(cyan)], '~c', [9571]). % ╣ : T left
td :- ansi_format([fg(cyan)], '~c', [9574]). % ╦ : T down
tu :- ansi_format([fg(cyan)], '~c', [9577]). % ╩ : T up

wt:- ansi_format([bold, fg(white)], '~c', [11044]), write(' '). % White piece
bl:- ansi_format([bold, fg(black)], '~c', [11044]), write(' '). % Black piece
null:- write('  '). % Null space
corner:- write(' '). % Board corner
empty:- ansi_format([bold, bg(cyan)], '~s', [' ']), ansi_format([bold, bg(cyan)], '~s', [' ']). % Empty space

middle_separator :- write(' '), tr, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, tl, nl.

second_separator :- write(' '), ulc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, urc, nl.

penultimate_separator :- write(' '), llc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, lrc, nl.

first_separator :- write(' '), ulc, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, urc, nl.

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

final_board([
    [corner, null, null, null, null, wt, null, corner], %1
    [null, empty, empty, bl, bl, bl, empty, null], %2
    [null, empty, wt, empty, wt, wt, bl, null], %3
    [null, bl, bl, bl, bl, bl, wt, null], %4
    [null, empty, empty, empty, wt, bl, wt, null], %5
    [null, bl, wt, empty, wt, wt, wt, null], %6
    [null, empty, empty, empty, bl, wt, empty, null], %7
    [corner, null, null, null, null, null, null, corner]  %8
]).

middle_state_board([
    [corner, null, null, null, null, wt, null, corner], %1
    [null, bl, empty, bl, bl, empty, empty, null], %2
    [null, empty, wt, empty, wt, wt, bl, null], %3
    [null, bl, bl, bl, bl, bl, wt, null], %4
    [null, empty, bl, empty, wt, bl, wt, null], %5
    [null, bl, wt, empty, wt, wt, wt, null], %6
    [null, empty, empty, empty, bl, wt, empty, null], %7
    [corner, null, null, null, null, null, null, corner]  %8
]).

second_middle_board([
    [corner, wt, bl, null, bl, bl, empty, corner], %1
    [wt, empty, empty, bl, empty, empty, empty, wt], %2
    [bl, empty, empty, empty, bl, empty, wt, null], %3
    [wt, empty, empty, empty, empty, empty, empty, wt], %4
    [wt, empty, empty, wt, empty, empty, empty, bl], %5
    [null, empty, empty, empty, bl, empty, empty, bl], %6
    [wt, empty, empty, empty, empty, empty, empty, wt], %7
    [corner, bl, wt, wt, bl, wt, wt, corner]  %8
]).

empty_board([
    [corner, wt, bl, wt, bl, bl, wt, corner], %1
    [wt, empty, empty, empty, empty, empty, empty, wt], %2
    [bl, empty, empty, empty, empty, empty, empty, bl], %3
    [wt, empty, empty, empty, empty, empty, empty, wt], %4
    [wt, empty, empty, empty, empty, empty, empty, bl], %5
    [bl, empty, empty, empty, empty, empty, empty, bl], %6
    [wt, empty, empty, empty, empty, empty, empty, wt], %7
    [corner, bl, wt, wt, bl, wt, wt, corner]  %8
]).

display_game(Board, Player):-
	write('Player turn: '),
	write(Player), nl, nl,
	write('   A  B  C  D  E  F  G  H'),nl,
	display_first_line(Board, 0).

cute_display :- 
	clear,
	final_board(X),
	display_game(X, 'John Doe').




