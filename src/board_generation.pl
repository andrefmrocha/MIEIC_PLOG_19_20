:- use_module(library(random)).
:- ensure_loaded('board_traversing.pl').
:- ensure_loaded('board_pieces.pl').
:- ensure_loaded('board_states.pl').

% TODO: por num utils ou smt like that (talvez mudar o board_traversing para isso)
secondLast(L, X) :- append(_, [X, _], L).

initialize_board(Board, FinalBoard):-
    generate_pieces(_, Pieces, 24),
    NewBoard = [], 
    generate_board(Board, NewBoard, FinalBoard, Pieces),
    write(FinalBoard).


generate_board([], Board, Board, []).
generate_board([H | T], Board, FinalBoard, Pieces):-
    generate_line(H, [], NewLine, Pieces, NewPieces),
    append(Board, [NewLine], NewBoard),
    generate_board(T, NewBoard, FinalBoard, NewPieces).

generate_line([], Line, Line, Pieces, Pieces).
generate_line([null | T1], Line, FinalLine, [H | T2], NewPieces):-
    append(Line, [H], NewLine),
    generate_line(T1, NewLine, FinalLine, T2, NewPieces).
generate_line([H | T], Line, FinalLine, Pieces, NewPieces):-
    H \= null,
    append(Line, [H], NewLine),
    generate_line(T, NewLine, FinalLine, Pieces, NewPieces).

generate_pieces(PiecesList, PiecesList, 0).

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
