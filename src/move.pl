:- ensure_loaded('interface.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('board_pieces.pl').

move_simple :- 
	%empty_board(X),
	%initialize_board(X, Board), !, 
	second_middle_board(Board), !,
	move(Board, _, 1).

move(Board, NewBoard, Player) :-
	display_game(Board, Player), !,
	read_move(Move, Board),
	[IC, IR, FC, FR] = Move,
	get_element_matrix(IR, IC, Element, Board),
	player_element(Player, Element),
	valid_push(IC, IR, FC, FR, Board, ProvNewBoard),
	replace_matrix(IR, IC, null, ProvNewBoard, NewBoard),
	next_player(Player, NextPlayer),
	display_game(NewBoard, NextPlayer).

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

% next_player(+CurrentPlayer, -NextPlayer)
next_player(PrevPlayer, NextPlayer) :-
	ProvPlayer is PrevPlayer + 1,
	NextPlayer is mod(ProvPlayer, 2).

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
	