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
    board_floor(FirstVisited, SecondVisited, Disc, RightPoints, XRight, Y),
    board_floor(SecondVisited, ThirdVisited, Disc, LeftPoints, XLeft, Y),
    board_floor(ThirdVisited, FourthVisited, Disc, DownPoints, X, YDown),
    board_floor(FourthVisited, FinalVisited, Disc, UpPoints, X, YUp),
    pos_points(Elem, Disc, Value),
    NewPoints is RightPoints + LeftPoints + UpPoints + DownPoints + Value,
    max_points(NewPoints, Points, NewMax),
    points_calculation(FinalVisited, Disc, NewMax, MaxPoints, XRight, Y).


%points_calculation(+Board, -NewBoard,+CurrentDisc, -Points, +X, +Y)
board_floor(Visited, Visited, _, Points, X, Y):-
    \+ getPiece(Visited, X, Y, _),
    Points is 0.
board_floor(Visited, Visited, _, Points, X, Y):-
    getPiece(Visited, X, Y, visited),
    Points is 0.
board_floor(Visited, Visited, Disc, Points, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Elem \= Disc,
    Points is 0.
board_floor(Visited, NewVisited, Disc, Points, X, Y):-
    getPiece(Visited, X, Y, Elem),
    Visited \= visited,
    XRight is X + 1,
    XLeft is X - 1,
    YDown is Y + 1,
    YUp is Y - 1,
    replace_matrix(X, Y, visited, Visited, FirstNewVisited),
    board_floor(FirstNewVisited, SecondNewVisited, Disc, RightPoints, XRight, Y),
    board_floor(SecondNewVisited, ThirdNewVisited, Disc, LeftPoints, XLeft, Y),
    board_floor(ThirdNewVisited, FourthNewVisited, Disc, DownPoints, X, YDown),
    board_floor(FourthNewVisited, NewVisited, Disc, UpPoints, X, YUp),
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


test:-
    final_board(X),
    points_calculation(X, bl, Points),
    write('Points: '), write(Points).
