:- use_module(library(random)).
:- ensure_loaded('utils.pl').
:- ensure_loaded('board_pieces.pl').

init_row_helper(1, [Ends], Ends, _) :- !.

init_row_helper(N, [Middle | T], Ends, Middle) :-
	N1 is N - 1,
	init_row_helper(N1, T, Ends, Middle).

initialize_row_helper(Columns, [Ends | T], Ends, Middle) :-
	N1 is Columns - 1,
	init_row_helper(N1, T, Ends, Middle).

initialize_row(Columns, Line, middle) :-
	!, initialize_row_helper(Columns, Line, null, empty).

initialize_row(Columns, Line, outskirt) :-
	!, initialize_row_helper(Columns, Line, corner, null).

initialize_board_helper(1, Columns, [Line]) :-
	!, initialize_row(Columns, Line, outskirt).

initialize_board_helper(N, Columns, [Line | TBoard]) :-
	initialize_row(Columns, Line, middle),
	N1 is N - 1,
	initialize_board_helper(N1, Columns, TBoard).
	
initialize_empty_board(Rows, Columns, [Line |TBoard]) :-
	Rows > 1, Columns > 1,
	NRows is Rows + 2,
	NColumns is Columns + 2,
	initialize_row(NColumns, Line, outskirt),
	N is NRows -1,
	initialize_board_helper(N, NColumns, TBoard).

initialize_board([Line | TBoard], FinalBoard):-
	length([Line | TBoard], NRows),
	length(Line, NColumns),
	NPieces is (NColumns + NRows - 4) * 2,
    generate_pieces(_, Pieces, NPieces),
	generate_board([Line | TBoard], FinalBoard, Pieces).

generate_board(Board, Board, []).
generate_board([FirstRow | Rest], Final, Pieces) :-
	generate_line(FirstRow, [], NewLine, Pieces, NewPieces),
	rotate_board_clockwise([NewLine | Rest], NewRot, -1),
	generate_board(NewRot, Final, NewPieces).	


generate_line([], Line, Line, Pieces, Pieces).
generate_line([null | T1], Line, FinalLine, [H | T2], NewPieces):-
    append(Line, [H], NewLine),
    generate_line(T1, NewLine, FinalLine, T2, NewPieces).
generate_line([H | T], Line, FinalLine, Pieces, NewPieces):-
    H \= null,
    append(Line, [H], NewLine),
    generate_line(T, NewLine, FinalLine, Pieces, NewPieces).


% TODO: ! para cortar varias soluções
generate_pieces(PiecesList, PiecesList, 0).

generate_pieces([], PiecesList, NumPieces):-
    random_between(0, 1, Random),
    select_piece(Random, Piece),
    Num is NumPieces - 1,
    generate_pieces([Piece], PiecesList, Num).
generate_pieces([H | []], PiecesList, NumPieces):-
    random_between(0, 1, Random),
    select_piece(Random, Piece),
    Num is NumPieces - 1,
    append([Piece], [H], NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, Num).

generate_pieces(List, PiecesList, 1):-
	secondLast(List, wt),
	last(List, wt),
    append([bl], List, NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, 0).
	
generate_pieces(List, PiecesList, 1):-
	secondLast(List, bl),
	last(List, bl),
    append([wt], List, NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, 0).

generate_pieces([wt | T], PiecesList, 1):-
	last([wt | T], wt),
    append([bl], [wt | T], NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, 0).
	
generate_pieces([bl | T], PiecesList, 1):-
	last([bl | T], bl),
    append([wt], [bl | T], NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, 0).

generate_pieces([wt, wt | T], PiecesList, NumPieces):-
    Num is NumPieces - 1,
    append([bl], [wt | [wt | T]], NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, Num).
generate_pieces([bl, bl | T], PiecesList, NumPieces):-
    Num is NumPieces - 1,
    append([wt], [bl| [bl | T]], NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, Num).
generate_pieces([H, H2 | T], PiecesList, NumPieces):-
    random_between(0, 1, Random),
    select_piece(Random, Piece),
    Num is NumPieces - 1,
    append([Piece], [H | [H2 | T]], NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, Num).

select_piece(0, wt).
select_piece(1, bl).
