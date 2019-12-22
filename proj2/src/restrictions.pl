:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-ensure_loaded('boards.pl').

% C is represented by 1, F is represented by 2

restrict(Line):-
    domain(Line, 0, 2),
    length(Line, LineLength),
    NumZeroes is LineLength - 4,
    global_cardinality(Line, [0-NumZeroes, 1-2, 2-2]),
    element(ElemC1, Line, 1),
    element(ElemC2, Line, 1),
    ElemC1 #< ElemC2,
    element(ElemF1, Line, 2),
    element(ElemF2, Line, 2),
    ElemF1 #< ElemF2,
    DistC #= abs(ElemC1 - ElemC2),
    DistF #= abs(ElemF1 - ElemF2),
    DistC #< DistF.


map_sol([], []).
map_sol([1 | T1], [X | T2]):-
    X#=1,
    map_sol(T1, T2).
map_sol([2 | T1], [X | T2]):-
    X#=2,
    map_sol(T1, T2).
map_sol([0 | T1], [_ | T2]):-
    map_sol(T1, T2).

close_or_far(Board):-
    maplist(restrict, Board),
    transpose(Board, TransposedBoard),
    maplist(restrict, TransposedBoard),
    maplist(labeling([]), Board).
