:- ensure_loaded('interface.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('board_pieces.pl').

move_simple :- 
	empty_board(X),
	initialize_board(X, Board), !, 
	move(Board, _).

move(Board, NewBoard) :-
	display_game(Board, 'John Doe'), !,
	read_move(Move, Board),
	valid_move(Move, Board),
	[IC, IR, FC, FR] = Move,
	get_element_matrix(IR, IC, Element, Board),
	replace_matrix(FR, FC, Element, Board, NewBoardProv),
	replace_matrix(IR, IC, null, NewBoardProv, NewBoard),
	clear,
	display_game(NewBoard, 'John Doe').

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



	
