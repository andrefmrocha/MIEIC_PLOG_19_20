:- use_module(library(random)).


initialize_board(Board, FinalBoard):-
    NewBoard = [],
    generate_board(Board, NewBoard, FinalBoard).

generate_board([], Board, Board).
generate_board([H | T], Board, FinalBoard):-
    Line = [],
    generate_line(H, Line, NewLine),
    append(Board, [NewLine], NewBoard),
    generate_board(T, NewBoard, FinalBoard).    
    
generate_line([], Line, Line).
generate_line([null | T1], Line, Solution):-
    random_between(0, 1, Random),
    select_piece(Random, Piece),
    append(Line, [Piece], NewLine),
    generate_line(T1, NewLine, Solution).
    
generate_line([H1 | T1], Line, Solution):-
    H1 \= null,
    append(Line, [H1], NewLine),
    generate_line(T1, NewLine, Solution).


select_piece(0, wt).
select_piece(1, bl).
