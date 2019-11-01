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
	valid_move(Move, Board),
	[IC, IR, FC, FR] = Move,
	get_element_matrix(IR, IC, Element, Board),
	player_element(Player, Element),
	push(IC, IR, FC, FR, Board, ProvNewBoard),
	replace_matrix(IR, IC, null, ProvNewBoard, NewBoard),
	next_player(Player, NextPlayer),
	display_game(NewBoard, NextPlayer).

valid_move(Move, Board) :- top_move(Move, Board).
valid_move(Move, Board) :- bottom_move(Move, Board).
valid_move(Move, Board) :- right_move(Move, Board).
valid_move(Move, Board) :- left_move(Move, Board).

top_move([IC, IR, FC, FR], [Line | _]) :-
	IR == 0,
	FR > IR,
	vertical_move(IC, FC , Line).

bottom_move([IC, IR, FC, FR], [Line | BoardT]) :-
	FR < IR,
	length([Line | BoardT], Comp),
	LastRow is Comp - 1,
	IR == LastRow,
	vertical_move(IC, FC , Line).

right_move([IC, IR, FC, FR], [Line | BoardT]) :-
	FC < IC,
	length(Line, Comp),
	LastColumn is Comp - 1,
	IC == LastColumn,
	horizontal_move(IR, FR , [Line | BoardT]).

left_move([IC, IR, FC, FR], Board) :-
	IC == 0,
	FC > IC,
	horizontal_move(IR, FR , Board).

horizontal_move(IR, FR, Board) :-
	IR == FR,
	IR > 0, 
	length(Board, Comp),
	UpperBound is Comp - 1,
	IR < UpperBound.

vertical_move(IC, FC, Line) :-
	FC == IC,
	IC > 0,
	length(Line, Comp),
	UpperBound is Comp - 1,
	IC < UpperBound.

% next_player(+CurrentPlayer, -NextPlayer)
next_player(PrevPlayer, NextPlayer) :-
	ProvPlayer is PrevPlayer + 1,
	NextPlayer is mod(ProvPlayer, 2).

% player_element(Player, Piece).
player_element(0, wt).
player_element(1, bl).

% push_inline(+FinalIndex, +List, -NewList).
push_inline(Index, [H | Tail], Solution) :- 
	push_helper(Index, Tail, Solution, H).

push_helper(0, Sol, [H | Sol], H) :- !.

push_helper(Index, [empty | Tail], [empty | TailSol], H) :-
	NewIndex is Index - 1,
	!, push_helper(NewIndex, Tail, TailSol, H).

push_helper(Index, [Other | Tail], [empty | TailSol], H) :-
	push_inline(1, [Other | Tail], [_ | NewTail]),
	NewIndex is Index - 1,
	!, push_helper(NewIndex, NewTail, TailSol, H).

push(0, IR, FC, _, Board, NewBoard) :- 
	push_left(IR, FC, Board, NewBoard).

push(IC, 0, _, FR, [Line | TailBoard], NewBoard) :- 
	length(Line, Comp),
	Column is Comp - IC - 1,
	push_top(Column, FR, [Line | TailBoard], NewBoard).

push(IC, IR, FC, _, [Line | TailBoard], NewBoard) :- 
	length(Line, Comp),
	LastColumn is Comp - 1,
	IC == LastColumn,
	Index is IC - FC,
	length([Line | TailBoard], Comp),
	Row is Comp - IR - 1,
	push_right(Row, Index, [Line | TailBoard], NewBoard).

push(IC, IR, _, FR, Board, NewBoard) :- 
	length(Board, Comp),
	LastRow is Comp - 1,
	IR == LastRow,
	Index is IR - FR,
	write(IC - Index), nl,
	push_bottom(IC, Index, Board, NewBoard).

push_left(Row, Index, Board, NewBoard) :-
	nth0(Row, Board, SearchedRow),
	push_inline(Index, SearchedRow, NewRow),
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
	