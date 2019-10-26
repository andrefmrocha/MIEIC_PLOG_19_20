:- use_module(library(lists)).

getPiece(Board, X, Y, Elem):-
    nth0(Board, X, Line),
    nth0(Line, Y, Elem).