:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-use_module(library(random)).
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

% TODO: ver melhor 3 primeiros parametros, para ver se dá para evitar comparação 'ha mao'
restrict2(Line) :-
	NC #< NF,
	automaton(Line, _, Line,
		[source(a1), sink(c3)],
		[arc(a1, 0, a1), arc(a1, 1, a2), arc(a1, 2, b1),
		 arc(a2, 0, a2, [C + 1, F]), arc(a2, 1, a3), arc(a2, 2, b2, [C + 1, F]),
		 arc(a3, 0, a3), arc(a3, 2, b3),
		 arc(b1, 0, b1, [C, F + 1]), arc(b1, 1, b2, [C, F + 1]), arc(b1, 2, c1),
		 arc(b2, 0, b2, [C + 1, F + 1]), arc(b2, 1, b3, [C, F + 1]), arc(b2, 2, c2, [C + 1, F]),
		 arc(b3, 0, b3, [C, F + 1]), arc(b3, 2, c3),
		 arc(c1, 0, c1), arc(c1, 1, c2),
		 arc(c2, 0, c2, [C + 1, F]), arc(c2, 1, c3),
		 arc(c3, 0, c3)],
		 [C, F], [0, 0], [NC, NF]).


map_sol([], []).
map_sol([1 | T1], [X | T2]):-
    X#=1,
    map_sol(T1, T2).
map_sol([2 | T1], [X | T2]):-
    X#=2,
    map_sol(T1, T2).
map_sol([0 | T1], [_ | T2]):-
    map_sol(T1, T2).

% close_or_far(Board):-
%     maplist(restrict, Board),
%     transpose(Board, TransposedBoard),
%     maplist(restrict, TransposedBoard),
%     maplist(labeling([]), Board).

% close_or_far2(Board):-
%     maplist(restrict2, Board),
%     transpose(Board, TransposedBoard),
%     maplist(restrict2, TransposedBoard),
%     maplist(labeling([]), Board).

% Method: restrict or restrict2
close_or_far_geral(Board, Method) :-
	maplist(Method, Board),
    transpose(Board, TransposedBoard),
    maplist(Method, TransposedBoard),
    maplist(labeling([]), Board).

mappable_length(Length, List):-
    length(List, Length).


select_pieces(FinishedRow, Row):-
    domain(Row, 0, 2),
    length(Row, LineLength),
    NumZeroes is LineLength - 1,
    global_cardinality(Row, [0-NumZeroes, 1-CCount, 2-FCount]),
    CCount+FCount #= 1,
    (CCount #= 1) #<=> CloseFound,
    place_piece(CloseFound, FinishedRow, Row).

place_piece(1, FinishedRow, Row):-
    element(Index, Row, 1),
    element(Index, FinishedRow, 1).

place_piece(0, FinishedRow, Row):-
    element(Index, Row, 2),
    element(Index, FinishedRow, 2).

% generate(Rows, Columns, Board):-
generate(Rows, Columns, Board, Method):-
    length(FinishedBoard, Rows),
    maplist(mappable_length(Columns), FinishedBoard),
    %close_or_far(FinishedBoard),
	close_or_far_geral(FinishedBoard, Method),
    length(Board, Rows),
    maplist(mappable_length(Columns), Board),
    %close_or_far(FinishedBoard),
	close_or_far_geral(FinishedBoard, Method),
    maplist(select_pieces, FinishedBoard, Board),
    transpose(FinishedBoard, TransposedFinishedBoard),
    transpose(Board, TransposedBoard),
    maplist(select_pieces, FinishedBoard, Board),
    maplist(select_pieces, TransposedFinishedBoard, TransposedBoard),
    maplist(labeling([value(sel_random)]), Board).

sel_random(Var, _, BB0, BB1):-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    ( first_bound(BB0, BB1), Var#= Value;
    later_bound(BB0, BB1), Var#\= Value ).