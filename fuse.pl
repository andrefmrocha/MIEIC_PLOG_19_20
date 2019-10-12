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
null:- ansi_format([bold, fg(red)], '~c', [9618]).
empty:- ansi_format([bold, bg(cyan)], '~s', [' ']).

middle_separator :- tr, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, tl, nl.

second_separator :- ulc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, urc, nl.

penultimate_separator :- llc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, lrc, nl.

first_separator :- ulc, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, urc, nl.

last_separator :- write('  '), llc, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, lrc, nl.

% display_middle_line([' ', '.', '.', '.', '.', '.', '.', ' ']).

display_middle_line(Line) :-
	middle_separator,
	vdiv, display_line(Line), nl.

display_second_line(Line) :-
	second_separator,
	vdiv, display_line(Line), nl.

% TODO: fazer depois display_line diferente para a primeira e ultima line para poder ser array quadrado
display_first_line(Line) :-
	write('  '), first_separator,
	write('  '), vdiv, display_line(Line), nl.

% TODO: fazer depois display_line diferente para a primeira e ultima line para poder ser array quadrado
display_last_line(Line) :-
	penultimate_separator,
	write('  '), vdiv, display_line(Line), nl,
	last_separator.

display_board([], _).
display_board([Line | Rest], Player) :-
	display_line(Line), nl, 
	display_board(Rest, Player).

display_line([Element | []]) :- 
	Element.
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
	write('  0 1 2 3 4 5'),nl,
	display_board(X, 'Meias').




