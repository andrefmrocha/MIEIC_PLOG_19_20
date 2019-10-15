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

wt:- ansi_format([bold, fg(white)], '~c', [9679]). % Disco branco
bl:- ansi_format([bold, fg(black)], '~c', [9679]). % Disco preto
null:- write(' '). % Disco nulo
corner:- write(' '). % Canto do Tabuleiro
empty:- ansi_format([bold, bg(cyan)], '~s', [' ']). % Lugar vazio

middle_separator :- write(' '), tr, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, tl, nl.

second_separator :- write(' '), ulc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, urc, nl.

penultimate_separator :- write(' '), llc, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, lrc, nl.

first_separator :- write(' '), ulc, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, td, hdiv, urc, nl.

last_separator :- write(' '), write('  '), llc, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, tu, hdiv, lrc, nl.

% display_middle_line([' ', '.', '.', '.', '.', '.', '.', ' ']).

general_line_display([],_).
general_line_display([H | []], N):-
	display_last_line(H, N).
general_line_display([H | T], N) :-
	middle_separator,
	write(N), vdiv, display_line(H), nl,
	N1 is N + 1,
	general_line_display(T, N1).

display_second_line([H | T]) :-
	second_separator,
	write('1'), vdiv, display_line(H), nl,
	general_line_display(T, 2).

% TODO: fazer depois display_line diferente para a primeira e ultima line para poder ser array quadrado
display_first_line([H | T]) :-
	write('  '),first_separator,
	write('0 '), display_line(H), nl,
	display_second_line(T).

% TODO: fazer depois display_line diferente para a primeira e ultima line para poder ser array quadrado
display_last_line(Line, N) :-
	penultimate_separator,
    write(N),
	write(' '), display_line(Line), nl,
	last_separator.

display_board([], _).
display_board([Line | Rest], Player) :-
	display_line(Line), nl, 
	display_board(Rest, Player).

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

% pq n sei passar argumentos
display_game(Board, Player):-
	write('Player: '),
	write(Player), nl, nl,
	write('  A B C D E F G H'),nl,
	display_first_line(Board).

cute_display :- 
	clear,
	middle_state_board(X),
	display_game(X, 'Meias').




