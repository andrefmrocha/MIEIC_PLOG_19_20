:- use_module(library(lists)).
:- use_module(library(clpfd)).

getPiece(Board, X, Y, Elem):-
    nth0(Board, X, Line),
    nth0(Line, Y, Elem).

secondLast(L, X) :- append(_, [X, _], L).

rotateBoard(Board, NewBoard) :-
	transpose(Board, Prov),
	reverse(Prov, NewBoard).
