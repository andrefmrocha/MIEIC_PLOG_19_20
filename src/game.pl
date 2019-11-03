:- ensure_loaded('board_states.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display.pl').
:- ensure_loaded('points_calculation.pl').

start_game :-
	% TODO: por rand seed
	empty_board(EB),
	initialize_board(EB, BeginBoard),
	game_loop(BeginBoard, 0).

% finish
game_loop(Board, _) :- 
	pass_move(Board, 0),
	pass_move(Board, 1),
	points_calculation(Board, wt, Points0),
    write('Player 0 Points : '), write(Points0),
    points_calculation(Board, bl, Points1),
    write('Player 1 Points : '), write(Points1).


game_loop(Board, Player) :-
	display_game(Board, Player),
	\+ pass_move(Board, Player),
	read_move(Move, Board),
	move(Board, Move, NewBoard, Player), !,
	next_player(Player, NextPlayer),
	game_loop(NewBoard, NextPlayer).

game_loop(Board, Player) :-
	display_game(Board, Player),
	pass_move(Board, Player),
	next_player(Player, NextPlayer),
	game_loop(Board, NextPlayer).


