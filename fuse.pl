% display() :- display_board(board1, 'Ola').

% display_board( [
%     [' ', [], [], [], [], [], [], ' '], %1
%     [[], '.', '.', '.', '.', '.', '.', []], %2
%     [[], '.', '.', '.', '.', '.', '.', []], %3
%     [[], '.', '.', '.', '.', '.', '.', []], %4
%     [[], '.', '.', '.', '.', '.', '.', []], %5
%     [[], '.', '.', '.', '.', '.', '.', []], %6
%     [[], '.', '.', '.', '.', '.', '.', []], %7
%     [' ', [], [], [], [], [], [], ' ']  %8
% ], 'Ola').

clear :- write('\e[2J').

ulc :- put_code(9556). % ╔
urc :- put_code(9559). % ╗
llc :- put_code(9562). % ╚
lrc :- put_code(9565). % ╝

hdiv :- put_code(9552). % ═
vdiv :- put_code(9553). % ║
mdiv :- put_code(9580). % ╬

tr :- put_code(9568). % ╠
tl :- put_code(9571). % ╣
td :- put_code(9574). % ╦
tu :- put_code(9577). % ╩

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

display_line([]).
display_line([Element | Rest]) :- 
	write(Element), vdiv,
	display_line(Rest).

% pq n sei passar argumentos
cuteDisplay :- 
	clear,

	write('Ola\n').




