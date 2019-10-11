% display() :- display_board(board1, 'Ola').

% display_board( [
%     [[], [], [], [], [], []], %1
%     [[], '.', '.', '.', '.', '.', '.', []], %2
%     [[], '.', '.', '.', '.', '.', '.', []], %3
%     [[], '.', '.', '.', '.', '.', '.', []], %4
%     [[], '.', '.', '.', '.', '.', '.', []], %5
%     [[], '.', '.', '.', '.', '.', '.', []], %6
%     [[], '.', '.', '.', '.', '.', '.', []], %7
%     [[], [], [], [], [], []]  %8
% ], 'Ola').

clear :- write('\e[2J').

display_board([], _).
display_board([Line | Rest], Player) :-
	display_line(Line, Player), nl, 
	display_board(Rest, Player).

display_line([], _).
display_line([Element | Rest], Player) :- 
	write(Element), write('|'),
	display_line(Rest, Player).

% pq n sei passar argumentos
cuteDisplay :- 
	clear,
	write('Ola\n').




