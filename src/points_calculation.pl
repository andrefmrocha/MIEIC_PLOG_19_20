:- ensure_loaded('utils.pl').
:- ensure_loaded('board_states.pl').

%points_calculation(+Board, +CurrentDisc, -Points)
points_calculation(Visited, Disc, MaxPoints):-
    points_calculation(Visited, Disc, _, MaxPoints, 0, 0).

%points_calculation(+Board, +CurrentDisc, +Points, -MaxPoints, +X, +Y)
points_calculation(Visited, _, Points, Points, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    YDown is Y + 1,
    \+ getPiece(Visited, 0, YDown, _).
points_calculation(Visited, Disc, Points, MaxPoints, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    YDown is Y + 1,
    getPiece(Visited, 0, YDown, Elem),
    Elem \= nil,
    points_calculation(Visited, Disc, Points, MaxPoints, 0, YDown).
points_calculation(Visited, Disc, Points, MaxPoints, X, Y):-
    getPiece(Visited, X, Y, visited),
    X1 is X + 1,
    points_calculation(Visited, Disc, Points, MaxPoints, X1, Y).
points_calculation(Visited, Disc, Points, MaxPoints, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Elem \= Disc,
    X1 is X + 1,
    replace_matrix(X, Y, visited, Visited, NewVisited),
    points_calculation(NewVisited, Disc, Points, MaxPoints, X1, Y).
points_calculation(Visited, Disc, Points, MaxPoints, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Elem \= visited,
    XRight is X + 1,
    XLeft is X - 1,
    YDown is Y + 1,
    YUp is Y - 1,
    replace_matrix(X, Y, visited, Visited, FirstVisited),
    board_flood(FirstVisited, SecondVisited, Disc, RightPoints, XRight, Y),
    board_flood(SecondVisited, ThirdVisited, Disc, LeftPoints, XLeft, Y),
    board_flood(ThirdVisited, FourthVisited, Disc, DownPoints, X, YDown),
    board_flood(FourthVisited, FinalVisited, Disc, UpPoints, X, YUp),
    pos_points(Elem, Disc, Value),
    NewPoints is RightPoints + LeftPoints + UpPoints + DownPoints + Value,
    max_points(NewPoints, Points, NewMax),
    points_calculation(FinalVisited, Disc, NewMax, MaxPoints, XRight, Y).


%board_flood(+Board, -NewBoard,+CurrentDisc, -Points, +X, +Y)
board_flood(Visited, Visited, _, Points, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    Points is 0.
board_flood(Visited, Visited, _, Points, X, Y):-
    getPiece(Visited, X, Y, visited),
    Points is 0.
board_flood(Visited, Visited, Disc, Points, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Elem \= Disc,
    Points is 0.
board_flood(Visited, NewVisited, Disc, Points, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Visited \= visited,
    XRight is X + 1,
    XLeft is X - 1,
    YDown is Y + 1,
    YUp is Y - 1,
    replace_matrix(X, Y, visited, Visited, FirstNewVisited),
    board_flood(FirstNewVisited, SecondNewVisited, Disc, RightPoints, XRight, Y),
    board_flood(SecondNewVisited, ThirdNewVisited, Disc, LeftPoints, XLeft, Y),
    board_flood(ThirdNewVisited, FourthNewVisited, Disc, DownPoints, X, YDown),
    board_flood(FourthNewVisited, NewVisited, Disc, UpPoints, X, YUp),
    pos_points(Elem, Disc, Value),
    Points is RightPoints + LeftPoints + UpPoints + DownPoints + Value.
    
%pos_points(Element, CurrentDisc, -Value)
pos_points(Elem, Elem, 1).
pos_points(Elem, Disc, 0):-
    Disc \= Elem.


%max_points(Points, CurrentMaxPoints, -MaxPoints)
max_points(Points, MaxPoints, Points):-
    \+ number(MaxPoints), !.
max_points(Points, MaxPoints, Points):-
    Points > MaxPoints, !.
max_points(_, MaxPoints, MaxPoints).



