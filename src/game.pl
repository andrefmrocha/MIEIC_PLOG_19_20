:- ensure_loaded('interface.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display.pl').
:- ensure_loaded('points_calculation.pl').
:- ensure_loaded('menu.pl').


order_difficulty(human, Difficulty, _ - Difficulty).
order_difficulty(bot, Difficulty, Difficulty - _).

init_game(pvp, _, human).

init_game(pvb, DifficultyTuple, FirstPlayer) :- 
	display_bot_menu('', Difficulty), !,
	choose_order(FirstPlayer),
	order_difficulty(FirstPlayer, Difficulty, DifficultyTuple).

init_game(bvb, Difficulty1 - Difficulty2, bot) :- 
	display_bot_menu('1 ', Difficulty1),!,
	display_bot_menu('2 ', Difficulty2), !.

init_game(_, _, _):-
	!, start_game.

initial(GameMode, Difficulty, FirstPlayer) :-
	display_menu(GameMode),
	init_game(GameMode, Difficulty, FirstPlayer).

play:-
	start_game,
	play.

start_game :-
	menu_option(main).

% finish
game_loop(Board, [Player, _], _, _, 2) :- 
	write('\nGame Finished\n\n'),
	display_game(Board, Player),
	points_calculation(Board, wt, Points0),
    write('Player 0 Points : '), write(Points0), nl,
    points_calculation(Board, bl, Points1),
    write('Player 1 Points : '), write(Points1), nl, !,
	write('Press enter to continue...'), read_string(_).

game_loop(Board, [Player, Type], GameMode, Difficulty, _) :-
	\+ pass_move(Board, Player),
	display_game(Board, Player),
	choose_move(Board, [Player, Type], NewBoard, Difficulty), !,
	next_player([Player, Type], NextPlayer, GameMode, Difficulty, NewDifficulty),
	game_loop(NewBoard, NextPlayer, GameMode, NewDifficulty, 0).

game_loop(Board, [Player, Type], GameMode, Difficulty, Passes) :-
	display_game(Board, Player),
	%pass_move(Board, Player),
	write(' Next Player\n'),
	NewPasses is Passes + 1,
	next_player([Player, Type], NextPlayer, GameMode, Difficulty, NewDifficulty), !,
	game_loop(Board, NextPlayer, GameMode, NewDifficulty, NewPasses).


