:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-use_module(library(random)).
:-ensure_loaded('boards.pl').
:-ensure_loaded('display.pl').
:-ensure_loaded('statistics.pl').

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
	flatten(Board, FlatBoard),
	labeling([], FlatBoard).


close_or_far_stats(Board, Method, Labeling, Flag) :-
	% TODO: ver se aqui ou mais tarde
	reset_timer,
	maplist(Method, Board),
    transpose(Board, TransposedBoard),
    maplist(Method, TransposedBoard),
	print_time, write(','),
	flatten(Board, FlatBoard),
	labeling([time_out(60000, Flag) | Labeling], FlatBoard),
	print_time, nl.

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
generate(Side, Cols, Unique):-
    %close_or_far(FinishedBoard),
    board_length(Side, FinishedBoard),
	close_or_far_geral(FinishedBoard, restrict2),

    build_board(Side, Cols, FinishedBoard, Board),
    unique(Unique, FinishedBoard, Board),
	write(Board), nl.

generate_stats(Cols, Unique, Labeling, Side, Flag):-
	write(Side), write(','),
    %close_or_far(FinishedBoard),
    board_length(Side, FinishedBoard),
	close_or_far_stats(FinishedBoard, restrict2, Labeling, Flag),
	generate_on_flag(Side, Cols, Unique, FinishedBoard, _, Flag).

generate_on_flag(_, _, _, _, _, time_out).
generate_on_flag(Side, Cols, Unique, FinishedBoard, Board, _):-
    build_board(Side, Cols, FinishedBoard, Board),
    unique(Unique, FinishedBoard, Board).

build_board(Side, Cols, FinishedBoard, Board):-
	ntowers(Cols, FinishedBoard),

	% write(Cols), nl,
    board_length(Side, Board),

	generate_board(Cols, FinishedBoard, Board).


unique(unique, FinishedBoard, Board):-
	\+ two_solutions(FinishedBoard, Board).
unique(_, _, _).


board_length(Side, Board):-
    length(Board, Side),
    maplist(mappable_length(Side), Board).


sel_random(Var, _, BB0, BB1):-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    ( first_bound(BB0, BB1), Var#= Value;
    later_bound(BB0, BB1), Var#\= Value ).

ntowers(Cols, Board) :-
	length(Board, N),
	length(Cols, N),
	domain(Cols, 1, N),
	all_distinct(Cols), % redundante mas diminui tempo
	constrain(Cols, Board),
	labeling([], Cols).

constrain([], []).
constrain([H|RCols], [Row | TBoard]):-
	element(H, Row, Piece),
	Piece #\= 0,
	constrain(RCols, TBoard).


% TODO: mudar para maplist??
generate_board([], [], []).
generate_board([Index | TCols], [FRow | FinishedBoard], [H | Board]) :-
	element(Index, FRow, Element), % get element
	element(Index, H, Element), % Place element
	generate_board(TCols, FinishedBoard, Board).


diff_signature([], [], []) :- !.
diff_signature([B1|BT1], [B2|BT2], [S|Ss]) :-
	S in 0..1,
	B1 #= B2 #<=> S #= 0,
	B1 #\= B2 #<=> S #= 1,
	diff_signature(BT1, BT2, Ss).

diffBoards(Board1, Board2) :-
	diff_signature(Board1, Board2, Sign),
	automaton(Sign,
		[source(a), sink(b)],
		[arc(a, 0, a), arc(a, 1, b), arc(b, 0, b), arc(b, 1, b)]).


flatten([], []).
flatten([H | T], FlattenB) :-
	flatten(T, NewFlatten),
	append(H, NewFlatten, FlattenB).

two_solutions(FinishedBoard, Board) :-
	flatten(Board, FlatBoard),
	flatten(FinishedBoard, FlatFB),
	
	diffBoards(FlatFB, FlatBoard),

	close_or_far_geral(Board, restrict2).



	

% Tbm pode ser com findall resul length = 1