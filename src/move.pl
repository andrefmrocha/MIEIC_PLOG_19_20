:- use_module(library(random)).
:- use_module(library(lists)).
:- ensure_loaded('interface.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('board_pieces.pl').
:- ensure_loaded('points_calculation.pl').

move(Board, Move, NewBoard, Player) :-
	[IC, IR, FC, FR] = Move,
	get_element_matrix(Board, IR, IC, Element),
	player_element(Player, Element),
	valid_push(IC, IR, FC, FR, Board, ProvNewBoard), %!, % descomentar para só dar T/F
	replace_matrix(IR, IC, null, ProvNewBoard, NewBoard).

valid_push(IC, IR, FC, FR, Board, NewBoard) :- top_move(IC, IR, FC, FR, Board, NewBoard).
valid_push(IC, IR, FC, FR, Board, NewBoard) :- bottom_move(IC, IR, FC, FR, Board, NewBoard).
valid_push(IC, IR, FC, FR, Board, NewBoard) :- right_move(IC, IR, FC, FR, Board, NewBoard).
valid_push(IC, IR, FC, FR, Board, NewBoard) :- left_move(IC, IR, FC, FR, Board, NewBoard).

top_move(IC, 0, FC, FR, [Line | TailBoard], NewBoard) :-
	FR > 0,
	vertical_move(IC, FC , Line, UpperBound),
	Column is UpperBound - IC,
	push_top(Column, FR, [Line | TailBoard], NewBoard).

bottom_move(IC, IR, FC, FR, [Line | BoardT], NewBoard) :-
	FR < IR,
	length([Line | BoardT], Comp),
	LastRow is Comp - 1,
	IR == LastRow,
	vertical_move(IC, FC , Line, _),
	Index is IR - FR,
	push_bottom(IC, Index, [Line | BoardT], NewBoard).

right_move(IC, IR, FC, FR, [Line | BoardT], NewBoard) :-
	FC < IC,
	length(Line, Comp),
	LastColumn is Comp - 1,
	IC == LastColumn,
	horizontal_move(IR, FR , [Line | BoardT], UpperBound),
	Index is IC - FC,
	Row is UpperBound - IR,
	push_right(Row, Index, [Line | BoardT], NewBoard).

left_move(0, IR, FC, FR, Board, NewBoard) :-
	FC > 0,
	horizontal_move(IR, FR , Board, _),
	push_left(IR, FC, Board, NewBoard).

horizontal_move(Row, Row, Board, UpperBound) :-
	Row > 0, 
	length(Board, Comp),
	UpperBound is Comp - 1,
	Row < UpperBound.

vertical_move(Column, Column, Line, UpperBound) :-
	Column > 0,
	length(Line, Comp),
	UpperBound is Comp - 1,
	Column < UpperBound.

% next_player(+CurrentPlayer, -NextPlayer, +GameMode, +PrevDifficulty, -NewDifficulty)
next_player([PrevPlayer, human], [Player, human], pvp, _, _) :-
	ProvPlayer is PrevPlayer + 1,
	Player is mod(ProvPlayer, 2).

next_player([PrevPlayer, human], [Player, bot], pvb, PD - BD, BD - PD) :-
	ProvPlayer is PrevPlayer + 1,
	Player is mod(ProvPlayer, 2).

next_player([PrevPlayer, bot], [Player, human], pvb, BD - PD, PD - BD) :-
	ProvPlayer is PrevPlayer + 1,
	Player is mod(ProvPlayer, 2).

next_player([PrevPlayer, bot], [Player, bot], bvb, B1D - B2D, B2D - B1D) :-
	ProvPlayer is PrevPlayer + 1,
	Player is mod(ProvPlayer, 2).

% player_element(Player, Piece).
player_element(0, wt).
player_element(1, bl).

% push(+FinalIndex, +List, -NewList).
push(Index, [H | Tail], Solution) :- 
	push_helper(Index, Tail, Solution, H).

push_helper(0, Sol, [H | Sol], H) :- !.

push_helper(Index, [empty | Tail], [empty | TailSol], H) :-
	NewIndex is Index - 1,
	!, push_helper(NewIndex, Tail, TailSol, H).

push_helper(Index, [Other | Tail], [empty | TailSol], H) :-
	push(1, [Other | Tail], [_ | NewTail]),
	NewIndex is Index - 1,
	!, push_helper(NewIndex, NewTail, TailSol, H).

push_left(Row, Index, Board, NewBoard) :-
	nth0(Row, Board, SearchedRow),
	push(Index, SearchedRow, NewRow),
	replace_row(Row, NewRow, Board, NewBoard).

push_top(Column, Index, Board, NewBoard) :-
	rotate_board_clockwise(Board, RotatedBoard, -1),
	push_left(Column, Index, RotatedBoard, NewRotatedBoard),
	rotate_board_clockwise(NewRotatedBoard, NewBoard, 1).
	
push_right(Row, Index, Board, NewBoard) :-
	rotate_board_clockwise(Board, RotatedBoard, 2),
	push_left(Row, Index, RotatedBoard, NewRotatedBoard),
	rotate_board_clockwise(NewRotatedBoard, NewBoard, 2).

push_bottom(Column, Index, Board, NewBoard) :-
	rotate_board_clockwise(Board, RotatedBoard, 1),
	push_left(Column, Index, RotatedBoard, NewRotatedBoard),
	rotate_board_clockwise(NewRotatedBoard, NewBoard, -1).
	
inside_board([Line | TBoard], Column, Row) :-
	get_element_matrix([Line | TBoard], Row, Column, _),
	horizontal_move(Row, Row, [Line | TBoard], _), 
	vertical_move(Column, Column, Line, _).

findall_moves_helper([], _, _, Prov, Prov) :- !.

findall_moves_helper([[FC, FR] | Remain], Board, Player, Moves, Sol) :-
	findall([IC, IR] - [FC, FR], move(Board, [IC, IR, FC, FR], _, Player), List),
	!, append(Moves, List, NewMoves),
	!, findall_moves_helper(Remain, Board, Player, NewMoves, Sol).

findall_moves(Board, Player, Moves) :-
	findall([FC, FR], inside_board(Board, FC, FR), FinalPos),
	!, findall_moves_helper(FinalPos, Board, Player, _, Moves).

random_move(Board, Player, Move) :-
	findall_moves(Board, Player, Moves),
	length(Moves, NMoves),
	NMoves > 0,
	LastIndex is NMoves - 1,
	random_between(0, LastIndex, RandomIndex),
	nth0(RandomIndex, Moves, Move).


pass_move(Board, Player) :-
	findall_moves(Board, Player, []).

generate_move_points(Board, Player, [IC, IR]-[FC, FR], Points, NewBoard):-
	move(Board, [IC, IR, FC, FR], NewBoard, Player),
	player_element(Player, Disc),
	points_calculation(NewBoard, Disc, Points).


get_best_move(Player, Board, Moves, Move):-
	maplist(generate_move_points(Board, Player), Moves, Scores, _),
	max_list(Scores, Max),
	nth0(Index, Scores, Max),
	nth0(Index, Moves, Move).

greedy_move(Player, Board, Move):-
	findall_moves(Board, Player, Moves),
	get_best_move(Player, Board, Moves, Move).


get_new_boards(_,  [], [], _).
get_new_boards(InitialBoard, [[IC, IR]-[FC, FR] | MoveT], [NewBoard | T2], Player):-
	move(InitialBoard, [IC, IR, FC, FR], NewBoard, Player),
	get_new_boards(InitialBoard, MoveT, T2, Player).

% Currently unused but may be useful for a next iteration of minimax
generate_boards_points(_, [], [], []).
generate_boards_points(Player, [BoardH | BoardT], [MoveH | MoveT], [NewPoints | T2]):-
	generate_move_points(BoardH, Player, MoveH, NewPoints, _),
	generate_boards_points(Player, BoardT, MoveT, T2).

get_best_move_points(Opponent, Player, Board, [], OpponentPoints, PlayerPoints):-
	player_element(Opponent, OpponentDisc),
	player_element(Player, PlayerDisc),
	points_calculation(Board, OpponentDisc, OpponentPoints),
	points_calculation(Board, PlayerDisc, PlayerPoints).
get_best_move_points(Opponent, Player, Board, Moves, OpponentPoints, PlayerPoints):-
	get_best_move(Opponent, Board, Moves, Move),
	generate_move_points(Board, Opponent, Move, OpponentPoints, NewBoard),
	player_element(Player, PlayerDisc),
	points_calculation(NewBoard, PlayerDisc, PlayerPoints).

generate_board_points(Opponent, Player, Board, OpponentPoints, PlayerPoints):-
	findall_moves(Board, Opponent, Moves),
	get_best_move_points(Opponent, Player, Board, Moves, OpponentPoints, PlayerPoints).

difference(PlayerPoints, OpponentPoints, PointsDifference):-
	PointsDifference is PlayerPoints - OpponentPoints.

minimax(Board, Player, Move):-
	findall_moves(Board, Player, FirstDegreeMoves),
	get_new_boards(Board, FirstDegreeMoves, FirstDegreeBoards, Player),
	NextPlayer is Player + 1,
	Opponent is mod(NextPlayer, 2),
	maplist(generate_board_points(Opponent, Player), FirstDegreeBoards, OpponentPoints, PlayerPoints),
	maplist(difference, PlayerPoints, OpponentPoints, PointsDifference),
	max_list(PointsDifference, MaxDifference),
	nth0(Index, PointsDifference, MaxDifference),
	nth0(Index, FirstDegreeMoves, Move).

choose_move(Board, [Player, human], NewBoard, _):-
	read_move(Move, Board),
	move(Board, Move, NewBoard, Player).

choose_move(Board, [Player, bot], NewBoard, 0 - _):-
	random_move(Board, Player, [IC, IR]-[FC, FR]),
	move(Board, [IC, IR, FC, FR], NewBoard, Player).

choose_move(Board, [Player, bot], NewBoard, 1 - _):-
	greedy_move(Player, Board , [IC, IR]-[FC, FR]),
	move(Board, [IC, IR, FC, FR], NewBoard, Player).

choose_move(Board, [Player, bot], NewBoard, 2 - _):-
	minimax(Board, Player , [IC, IR]-[FC, FR]),
	move(Board, [IC, IR, FC, FR], NewBoard, Player).