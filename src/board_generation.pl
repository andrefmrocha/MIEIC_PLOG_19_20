:- use_module(library(random)).
:- ensure_loaded('utils.pl').
:- ensure_loaded('board_pieces.pl').

%! init_row_helper(+ColumnNumber, -Line, +EndCell, +MiddleCell)
% Helper function of @see initialize_row
% Depending on the current ColumnNumber adds to the Line an EndCell or a MiddleCell
% Case 1: If it reached the end, adds EndCell to the List
init_row_helper(1, [Ends], Ends, _) :- !.

% Case middle: If it is in the middle of the Row adds MiddleCell to the Row and decrements ColumnNumber
init_row_helper(N, [Middle | T], Ends, Middle) :-
	N1 is N - 1,
	init_row_helper(N1, T, Ends, Middle).

%Case initial: If it is at the begging also adds an EndCell at the head of the List
initialize_row_helper(Columns, [Ends | T], Ends, Middle) :-
	N1 is Columns - 1,
	init_row_helper(N1, T, Ends, Middle).

%! initialize_row(+NumberColumns, -Line, +Type)
% Uses @see initialize_row_helper to either initialize a row of the middle type ([null, empty*, null])
% or of the outskirt type ([null, empty*, null]) - specified in Type argument.
% Returns the line with the desired number of columns (NumberColumns) to the Line argument
% Case middle: Ends must be null and middle empty
initialize_row(Columns, Line, middle) :-
	!, initialize_row_helper(Columns, Line, null, empty).

% Case outskirt: Ends must be a corner and the middle is null
initialize_row(Columns, Line, outskirt) :-
	!, initialize_row_helper(Columns, Line, corner, null).

%! initialize_board_helper(+RowNumber, +NumberColumns, -Board)
% @see initialize_empty_board helper function
% Depending on the current RowNumber adds a outskirt Line ([corner, null*, corner]) - beggining and end -
% or a middle Line ([null, empty*, null]) to the Board with the desired number of columns (NumberColumns) 
% Uses @see initialize_row to create each line
% Case 1: creates outskirt line
initialize_board_helper(1, Columns, [Line]) :-
	!, initialize_row(Columns, Line, outskirt).

% Otherwise (except first): creates middle line
initialize_board_helper(N, Columns, [Line | TBoard]) :-
	initialize_row(Columns, Line, middle),
	N1 is N - 1,
	initialize_board_helper(N1, Columns, TBoard).

%! initialize_empty_board(+Rows, +Columns, -Board)
% Initializes a board in the empty position with the number of Columns and Rows desired
% We only count the middle part as the main board so if you ask for a 4x4 board, in reality
% it will be 6x6 because you can't use the periphery (it is only a starting point)
% 2x2:
%    A  B  C  D
%     ╔══╦══╗
% 0   ║⬤║⬤║   
%  ╔══╬══╬══╬══╗
% 1║⬤║██║██║⬤║
%  ╠══╬══╬══╬══╣
% 2║⬤║██║██║⬤║
%  ╚══╬══╬══╬══╝
% 3   ║⬤║⬤║   
%     ╚══╩══╝
% The dimensions have to be greater than 1.
% Uses @see initialize_row to initialize each row and @see initialize_empty_board once for each row 
initialize_empty_board(Rows, Columns, [Line |TBoard]) :-
	Rows > 1, Columns > 1,
	NRows is Rows + 2,
	NColumns is Columns + 2,
	initialize_row(NColumns, Line, outskirt),
	N is NRows -1,
	initialize_board_helper(N, NColumns, TBoard).

%! initialize_board(+EmptyBoard, -InitializedBoard)
% With the help of @see generate_pieces (generates the random order of the pieces) and
% @see generate_board (places the generated pieces in the board), this function initializes
% a previously EmptyBoard and returns it via the InitiazliedBoard argument
initialize_board([Line | TBoard], FinalBoard):-
	length([Line | TBoard], NRows),
	length(Line, NColumns),
	NPieces is (NColumns + NRows - 4) * 2,
    generate_pieces(_, Pieces, NPieces),
	generate_board([Line | TBoard], FinalBoard, Pieces).

%! generate_board(+EmptyBoard, -InitializedBoard, +PiecesList)
% Applies the list of pieces (PiecesList) with a randomly generated around
% the EmptyBoard with the help of @see generate_line place part of the PiecesList
% in the first line of the board and @see rotate_board_clock_wise to rotate it counter
% clockwise after each placement.
% Final board is returned in the InitializedBoard argument
generate_board(Board, Board, []).
generate_board([FirstRow | Rest], Final, Pieces) :-
	generate_line(FirstRow, [], NewLine, Pieces, NewPieces),
	rotate_board_clockwise([NewLine | Rest], NewRot, -1),
	generate_board(NewRot, Final, NewPieces).	

%! generate_line(+OriginalRow, +AccumulateRow, -FinalRow, +InitialPieceList, -FinalPieceList)
% Whenever it encounters a null cell a new board piece from the InitialPieceList is placed
% The OriginalRow is a List of the outskirt type ([corner, null*, corner]) and builds the AccumulateRow
% that saves its result in the end of the recursion in the FinalRow argument
% The FinalPieceList is the InitialPieceList without the placed pieces
% TODO: refactor
generate_line([], Line, Line, Pieces, Pieces).
generate_line([null | T1], Line, FinalLine, [H | T2], NewPieces):-
    append(Line, [H], NewLine),
    generate_line(T1, NewLine, FinalLine, T2, NewPieces).
generate_line([H | T], Line, FinalLine, Pieces, NewPieces):-
    H \= null,
    append(Line, [H], NewLine),
    generate_line(T, NewLine, FinalLine, Pieces, NewPieces).


% TODO: ! para cortar varias soluções
% TODO: refactor
%! generate_pieces()
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
	second_last(List, wt),
	last(List, wt),
    append([bl], List, NewPiecesList),
    generate_pieces(NewPiecesList, PiecesList, 0).
	
generate_pieces(List, PiecesList, 1):-
	second_last(List, bl),
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

%! select_piece(?Player, ?PieceType)
% Associates a Player number (0 or 1) to a Piece Type (wt or bl)
select_piece(0, wt).
select_piece(1, bl).
