:- ensure_loaded('utils.pl').
:- ensure_loaded('board_states.pl').

points_calculation(Visited, Disc, MaxPoints):-
    points_calculation(Visited, _, Disc, _, MaxPoints, 0, 0).

points_calculation(Visited, Visited, _, _, _, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    YDown is Y + 1,
    \+ getPiece(Visited, 0, YDown, _).
points_calculation(Visited, NewVisited, Disc, Points, MaxPoints, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    YDown is Y + 1,
    getPiece(Visited, 0, YDown, Elem),
    Elem \= nil,
    points_calculation(Visited, NewVisited, Disc, Points, MaxPoints, X, YDown).
points_calculation(Visited, FinalVisited, Disc, Points, MaxPoints, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Elem \= Disc,
    X1 is X + 1,
    replace_matrix(X, Y, visited, Visited, NewVisited),
    points_calculation(NewVisited, FinalVisited, Disc, Points, MaxPoints, X1, Y).
points_calculation(Visited, NewVisited, Disc, Points, MaxPoints, X, Y):-
    getPiece(Visited, X, Y, visited),
    X1 is X + 1,
    points_calculation(Visited, NewVisited, Disc, Points, MaxPoints, X1, Y).
points_calculation(Visited, NewVisited, Disc, Points, MaxPoints, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Elem \= visited,
    XRight is X + 1,
    XLeft is X - 1,
    YDown is Y - 1,
    YUp is Y - 1,
    board_floor(Visited, FirstVisited, Disc, RightPoints, XRight, Y),
    board_floor(FirstVisited, SecondVisited, Disc, LeftPoints, XLeft, Y),
    board_floor(SecondVisited, ThirdVisited, Disc, DownPoints, X, YDown),
    board_floor(ThirdVisited, FourthVisited, Disc, UpPoints, X, YUp),
    replace_matrix(X, Y, visited, FourthVisited, NewVisited),
    pos_points(Elem, Disc, Value),
    Points is RightPoints + LeftPoints + UpPoints + DownPoints + Value,
    max_points(Points, MaxPoints),
    points_calculation(Visited, NewVisited, Disc, Points, MaxPoints, XRight, Y).



board_floor(Visited, NewVisited, _, Points, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    replace_matrix(X, Y, visited, Visited, NewVisited),
    Points is 0.
board_floor(Visited, Visited, _, Points, X, Y):-
    getPiece(Visited, X, Y, visited),
    Points is 0.
board_floor(Visited, NewVisited, Disc, Points, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Visited \= visited,
    getPiece(Visited, X, Y, Elem),
    XRight is X + 1,
    XLeft is X - 1,
    YDown is Y - 1,
    YUp is Y - 1,
    replace_matrix(X, Y, visited, Visited, FirstNewVisited),
    board_floor(FirstNewVisited, SecondNewVisited, Disc, RightPoints, XRight, Y),
    board_floor(SecondNewVisited, ThirdNewVisited, Disc, LeftPoints, XLeft, Y),
    board_floor(ThirdNewVisited, FourthNewVisited, Disc, DownPoints, X, YDown),
    board_floor(FourthNewVisited, NewVisited, Disc, UpPoints, X, YUp),
    pos_points(Elem, Disc, Value),
    Points is RightPoints + LeftPoints + UpPoints + DownPoints + Value.
    

pos_points(Elem, Elem, 1).
pos_points(_, _, 0).
max_points(Points, MaxPoints):-
    \+ number(MaxPoints),
    MaxPoints is Points.
max_points(Points, MaxPoints):-
    Points > MaxPoints,
    MaxPoints is Points.
max_points(Points, MaxPoints):-
    Points < MaxPoints.


test:-
    final_board(X),
    points_calculation(X, wt, Points),
    write(Points).
