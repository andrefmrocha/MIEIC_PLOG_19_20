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
llc :- put_code(9561). % ╚
llc :- put_code(9565). % ╝

hdiv :- put_code(9552). % ═
vdiv :- put_code(9553). % ║
mdiv :- put_code(9580). % ╬

tr :- put_code(9568). % ╠
tl :- put_code(9571). % ╣
td :- put_code(9574). % ╦
tu :- put_code(9577). % ╩

middle_separator :- tr, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, mdiv, hdiv, tl, nl.

display_middle_line(Line) :-
	middle_separator,
	vdiv, display_line(Line), nl,
	middle_separator.

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




