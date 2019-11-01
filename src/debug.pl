:- ensure_loaded('board_states.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display.pl').

% Only for debug purposes
read_move_debug(Move) :-
	empty_board(X),
	read_move(Move, X).

move_debug(Player) :- 
	%empty_board(X),
	%initialize_board(X, Board), !, 
	second_middle_board(Board),
	display_game(Board, Player),
	read_move(Move, Board),
	move(Board, Move, NewBoard, Player), !,
	next_player(Player, NextPlayer),
	display_game(NewBoard, NextPlayer).

display_debug :- 
	clear,
	empty_board(X),
	initialize_board(X, Board), !,
	display_game(Board, 'John Doe').