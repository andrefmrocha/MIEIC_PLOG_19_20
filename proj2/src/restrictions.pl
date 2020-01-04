:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-use_module(library(random)).
:-ensure_loaded('boards.pl').
:-ensure_loaded('statistics.pl').
:-ensure_loaded('display.pl').
:-ensure_loaded('utils.pl').

% C is represented by 1, F is represented by 2, Empty is represented by 0

%! restrict(+Line).
% Used in conjunction with the maplist predicate to apply the constraints
% to the rows and the columns of the board
% Begins by defining the domain of the variables and restricting the number of each one
% with the help of global_cardinality (if each row/column has two '2's and two '1's,
% all the other remaining spaces -- Length - 2 - 2 = Length - 4 -- must be 0)
% Then it gets the indexes of the C's and F's and applies a contraint that 
% makes the distance between the 2 F's greater than the 2 C's
% This version, although simpler to understand, performs worse than @see restrict2
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

%! restrict2(+Line).
% Similar to @see restrict in objective, 
% but implemented with an automaton that leads to much better results
% The idea behind the automaton is simpler: it is like a 2D 2x2 grid that increases the number
% of 1's along the rows (growing in the number part of the state name) and increases the number
% of 2's along the columns (growing in the letter part of the state name: a < b < c).
% Each state also has a self transition so that it can add 0's (empty spaces) before, after or between the 1's and 2's
% To count the distance between C's and F's, there are two accumulators: C and F, respectively, with final values NC and NF
% This counters are incremented in the middle states when there was already one C or F found but not the other
% There is a restriction to force the distance between C's (NC) to be lower than the distance between F's (NF)
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


% TODO: isto usa-se ou é para apagar
map_sol([], []).
map_sol([1 | T1], [X | T2]):-
    X#=1,
    map_sol(T1, T2).
map_sol([2 | T1], [X | T2]):-
    X#=2,
    map_sol(T1, T2).
map_sol([0 | T1], [_ | T2]):-
    map_sol(T1, T2).

%! close_or_far_geral(+Board, +Method).
% This general predicate allows us to choose between the @see resctrict 
% and @see restrict2 retrictions to test the performance of each one of them 
% The restrictions chosen are applied to every row of the Board with the help of maplist
% and then to the columns after a board transposition
% To minimize backtracking steps, the labeling is done in a flatten board @see flatten
% @param Method: Must be either restrict or restrict2
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


% TODO: ver se é para apagar
select_pieces(FinishedRow, Row):-
    domain(Row, 0, 2),
    length(Row, LineLength),
    NumZeroes is LineLength - 1,
    global_cardinality(Row, [0-NumZeroes, 1-CCount, 2-FCount]),
    CCount+FCount #= 1,
    (CCount #= 1) #<=> CloseFound,
    place_piece(CloseFound, FinishedRow, Row).

% TODO: ver se é para apagar
place_piece(1, FinishedRow, Row):-
    element(Index, Row, 1),
    element(Index, FinishedRow, 1).
% TODO: ver se é para apagar
place_piece(0, FinishedRow, Row):-
    element(Index, Row, 2),
    element(Index, FinishedRow, 2).

%! generate(+Side, -Cols, +Unique).
% Predicate that helps to generate a random puzzle board with the desired Size
% The Unique parameter indicates whether the puzzle must have a unique solution (unique1/2)
% or not (all other values). Note that the unique solution might not work with values much greater than 7
% Cols is a list with the column index of each row element
% The board generated will have one hint for each column and row following the examples of Prof. Erich Friedman
% The solved board is generated by @see close_or_far_geral with restrict2 after 
% making an empty square matrix with the desired Side size using @see board_length
% To build the final puzzle, @see build_board is called
% Finally there is a generate and test approach to the uniqueness solution problem by calling @see unique
generate(Side, Cols, Unique):-
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

% TODO: eu tbm tiraria o Cols daqui já que no outro TODO digo que ele já n é necessário para mostrar
%! build_board(+Side, -Cols, +FinishedBoard, -Board)
% Generates a puzzle with one hint by column and row and returns it to the Board parameter
% The Cols return parameter indicates where each hint is by specifing the column index in each row of the list
% @see ntowers initializes the Cols list with the correct indexes and, 
% after initializing an empty puzzle with @see board_length, the final puzzle is generated with
% @see generate_board
build_board(Side, Cols, FinishedBoard, Board):-
	ntowers(Cols, FinishedBoard),
    board_length(Side, Board),
	generate_board(Cols, FinishedBoard, Board).

%! unique(+Method, +FinishedBoard, +Board).
% Predicate that ensures the uniqueness of the solution
% The Method can be either unique1 that uses the @see two_solutions
% predicate or unique2 that uses findall and verifies if ther is only one solution
% FinishedBoard is the solution from where the hint Board parameter is generated
unique(unique1, FinishedBoard, Board):-
	\+ two_solutions(FinishedBoard, Board).
% TODO: testar se funciona
unique(unique2, _, Board):-
	length(List, 1),
	findall(_, close_or_far_geral(Board, restrict2), List).
unique(_, _, _).

% TODO: ver se não é para apagar
sel_random(Var, _, BB0, BB1):-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    ( first_bound(BB0, BB1), Var#= Value;
    later_bound(BB0, BB1), Var#\= Value ).

%! ntowers(-Cols, +Board)
% Inspired on the famous Queens Problem, this predicate generates the final hints for the puzzle
% Row and Column constraint are guarantee by the data structure (list) and all_distinct respectively
% Uses @see constrain not to ensure the diagonal part of the orginal problem, but to ensure that the chosen element
% is not an empty space (0). 
% Board is the solved puzzle
ntowers(Cols, Board) :-
	length(Board, N),
	length(Cols, N),
	domain(Cols, 1, N),
	all_distinct(Cols),
	constrain(Cols, Board),
	labeling([], Cols).

%! constrain(-Cols, +Board)
% Helper function to @see ntowers
% Ensures that the chosen element to each row is not an empty space in the original solution
constrain([], []).
constrain([H|RCols], [Row | TBoard]):-
	element(H, Row, Piece),
	Piece #\= 0,
	constrain(RCols, TBoard).

%! generate_board(+Cols, +FinishedBoard, -HintBoard)
% Generates a new board with the ntowers solution
% Goes through each element of Cols and inserts the corresponding FinishedBoard
% element into the HintBoard.
% Each element of Cols corresponds to the index of the column in that row
generate_board([], [], []).
generate_board([Index | TCols], [FRow | FinishedBoard], [H | Board]) :-
	element(Index, FRow, Element), % get element
	element(Index, H, Element), % Place element
	generate_board(TCols, FinishedBoard, Board).


%! diff_signature(+FlattenBoard1, +FlattenBoard2, -Signs)
% Helper function to map inequalities of two elements to numbers so that it can be used in the automaton
% Signs will have an element 0 if the two board elements match and 1 otherwise
diff_signature([], [], []) :- !.
diff_signature([B1|BT1], [B2|BT2], [S|Ss]) :-
	S in 0..1,
	B1 #= B2 #<=> S #= 0,
	B1 #\= B2 #<=> S #= 1,
	diff_signature(BT1, BT2, Ss).

%! diffBoards(+FlattenBoard1, +FlattenBoard2)
% Simple automaton that restricts the Board2 so that it has at least one different element
% making the two puzzle solutions different
% Uses @see diff_signature to map the transitions numbers to inequalities
diffBoards(Board1, Board2) :-
	diff_signature(Board1, Board2, Sign),
	automaton(Sign,
		[source(a), sink(b)],
		[arc(a, 0, a), arc(a, 1, b), arc(b, 0, b), arc(b, 1, b)]).

%! two_solutions(+FinishedBoard, +HintBoard)
% Predicate that succeeds if another solution is found using @see close_or_far_geral
% The restriction making the boards different is applied with @see diffBoards
two_solutions(FinishedBoard, Board) :-
	flatten(Board, FlatBoard),
	flatten(FinishedBoard, FlatFB),
	diffBoards(FlatFB, FlatBoard),
	close_or_far_geral(Board, restrict2).