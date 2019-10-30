:- use_module(library(lists)).
:- use_module(library(clpfd)).

getPiece(Board, X, Y, Elem):-
    nth0(Board, X, Line),
    nth0(Line, Y, Elem).

secondLast(L, X) :- append(_, [X, _], L).

rotateBoard(Board, NewBoard) :-
	transpose(Board, Prov),
	reverse(Prov, NewBoard).

% replace_column(+Column,+Value,+List,-NewList).
replace_column(0, Value, [_|T], [Value | T]) :- !.
replace_column(Column, Value, [H | T], [H | T2]) :-
	Column > 0,
	NextColumn is Column - 1,
	replace_column(NextColumn, Value, T, T2).

% replace_matrix(+Row, +Column, +Value, +List, -NewList).
replace_matrix(0, Column, Value, [Line | T], [Line2 | T]) :- !, replace_column(Column, Value, Line, Line2).
replace_matrix(Row, Column, Value, [H | T], [H | T2]) :-
	Row > 0,
	NextRow is Row - 1,
	replace_matrix(NextRow, Column, Value, T, T2).


get_element_matrix(Row, Column, Element, Matrix) :-
	nth0(Row, Matrix, SearchedRow),
	nth0(Column, SearchedRow, Element).

last_n_elements(N, List, LastN) :-
	length(LastN, N),
	append(_, LastN, List), !.