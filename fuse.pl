% display() :- display_board(board1, 'Ola').
use_module(library(ansi_term)).

clear :- write('\e[2J').

ulc :- ansi_format([fg(cyan)], '~c', [9556]). % ╔
urc :- ansi_format([fg(cyan)], '~c', [9559]). % ╗
llc :- ansi_format([fg(cyan)], '~c', [9562]). % ╚
lrc :- ansi_format([fg(cyan)], '~c', [9565]). % ╝

hdiv :- ansi_format([fg(cyan)], '~c', [9552]). % ═
vdiv :- ansi_format([fg(cyan)], '~c', [9553]). % ║
mdiv :- ansi_format([fg(cyan)], '~c', [9580]). % ╬

tr :- ansi_format([fg(cyan)], '~c', [9568]). % ╠
tl :- ansi_format([fg(cyan)], '~c', [9571]). % ╣
td :- ansi_format([fg(cyan)], '~c', [9574]). % ╦
tu :- ansi_format([fg(cyan)], '~c', [9577]). % ╩

wt:- ansi_format([bold, fg(white)], '~c', [9679]).
bl:- ansi_format([bold, fg(black)], '~c', [9679]).
% null:- ansi_format([bold, fg(red)], '~c', [9618]).
null:- write(' ').
empty:- ansi_format([bold, bg(cyan)], '~s', [' ']).

middle_separator :- write(' '), tr, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, tl, nl.

second_separator :- write(' '), ulc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, urc, nl.

penultimate_separator :- write(' '), llc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, lrc, nl.

first_separator :- write(' '), ulc, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, urc, nl.

last_separator :- write(' '), write('  '), llc, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, lrc, nl.

% display_middle_line([' ', '.', '.', '.', '.', '.', '.', ' ']).

general_line_display([],_).
general_line_display([H | []], _):-
	display_last_line(H).
general_line_display([H | T], N) :-
	middle_separator,
	write(N), vdiv, display_line(H), nl,
	N1 is N + 1,
	general_line_display(T, N1).

display_second_line([H | T]) :-
	second_separator,
	write('0'), vdiv, display_line(H), nl,
	general_line_display(T, 1).

% TODO: fazer depois display_line diferente para a primeira e ultima line para poder ser array quadrado
display_first_line([H | T]) :-
	write('  '), first_separator,
	write('  '), display_line(H), nl,
	display_second_line(T).

% TODO: fazer depois display_line diferente para a primeira e ultima line para poder ser array quadrado
display_last_line(Line) :-
	penultimate_separator,
	write('  '), display_line(Line), nl,
	last_separator.

display_board([], _).
display_board([Line | Rest], Player) :-
	display_line(Line), nl, 
	display_board(Rest, Player).

display_line([]).
display_line([Element | []]):-
	Element = null,
	null.
display_line([Element | Rest]) :- 
	Element, vdiv,
	display_line(Rest).

empty_board([
    [null, wt, bl, wt, bl, bl, wt, null], %1
    [wt, empty, empty, empty, empty, empty, empty, wt], %2
    [bl, empty, empty, empty, empty, empty, empty, bl], %3
    [wt, empty, empty, empty, empty, empty, empty, wt], %4
    [wt, empty, empty, empty, empty, empty, empty, bl], %5
    [bl, empty, empty, empty, empty, empty, empty, bl], %6
    [wt, empty, empty, empty, empty, empty, empty, wt], %7
    [null, bl, wt, wt, bl, wt, wt, null]  %8
]).

% pq n sei passar argumentos
cute_display :- 
	clear,
	empty_board(X),
	write('    A B C D E F'),nl,
	display_first_line(X).




