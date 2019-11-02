:- ensure_loaded('utils.pl').
:- ensure_loaded('board_states.pl').

%! points_calculation(+Board, +CurrentDisc, -Points)
% Calculate the number of points of the given Disc.
% Is true when given points are the maximum calculatable in the Board.
% @param Board must be a list of lists
points_calculation(Visited, Disc, MaxPoints):-
    points_calculation(Visited, Disc, _, MaxPoints, 0, 0).

%! points_calculation(+Board, +CurrentDisc, +Points, -MaxPoints, +Y, +X)
% Calculate the number of points starting off in some coordinates
% @param Board must be a list of lists
points_calculation(Visited, _, Points, Points, Y, X):-
    \+ get_element_matrix(Visited, Y, X, _),
    XRight is X + 1,
    \+ get_element_matrix(Visited, 0, XRight, _).
points_calculation(Visited, Disc, Points, MaxPoints, Y, X):-
    \+ get_element_matrix(Visited, Y, X, _),
    XRight is X + 1,
    get_element_matrix(Visited, 0, XRight, Elem),
    Elem \= nil,
    points_calculation(Visited, Disc, Points, MaxPoints, 0, XRight).
points_calculation(Visited, Disc, Points, MaxPoints, Y, X):-
    get_element_matrix(Visited, Y, X, visited),
    Y1 is Y + 1,
    points_calculation(Visited, Disc, Points, MaxPoints, Y1, X).
points_calculation(Visited, Disc, Points, MaxPoints, Y, X):-
    get_element_matrix(Visited, Y, X, Elem),
    Elem \= Disc,
    Y1 is Y + 1,
    replace_matrix(Y, X, visited, Visited, NewVisited),
    points_calculation(NewVisited, Disc, Points, MaxPoints, Y1, X).
points_calculation(Visited, Disc, Points, MaxPoints, Y, X):-
    get_element_matrix(Visited, Y, X, Elem),
    Elem \= visited,
    YDown is Y + 1,
    YUp is Y - 1,
    XRight is X + 1,
    XLeft is X - 1,
    replace_matrix(Y, X, visited, Visited, FirstVisited),
    board_flood(FirstVisited, SecondVisited, Disc, DownPoints, YDown, X),
    board_flood(SecondVisited, ThirdVisited, Disc, UpPoints, YUp, X),
    board_flood(ThirdVisited, FourthVisited, Disc, RightPoints, Y, XRight),
    board_flood(FourthVisited, FinalVisited, Disc, LeftPoints, Y, XLeft),
    NewPoints is DownPoints + UpPoints + LeftPoints + RightPoints + 1,
    max_points(NewPoints, Points, NewMax),
    points_calculation(FinalVisited, Disc, NewMax, MaxPoints, YDown, X).


%! board_flood(+Board, -NewBoard,+CurrentDisc, -Points, +Y, +X)
% Implements a flooding algoritm to calculate the nearby discs 
% the same type as CurrentDisc.
% @param Board must be a list of lists
board_flood(Visited, Visited, _, Points, Y, X):-
    \+ get_element_matrix(Visited, Y, X, _),
    Points is 0.
board_flood(Visited, Visited, _, Points, Y, X):-
    get_element_matrix(Visited, Y, X, visited),
    Points is 0.
board_flood(Visited, Visited, Disc, Points, Y, X):-
    get_element_matrix(Visited, Y, X, Elem),
    Elem \= Disc,
    Points is 0.
board_flood(Visited, NewVisited, Disc, Points, Y, X):-
    get_element_matrix(Visited, Y, X, Elem),
    Visited \= visited,
    YDown is Y + 1,
    YUp is Y - 1,
    XRight is X + 1,
    XLeft is X - 1,
    replace_matrix(Y, X, visited, Visited, FirstNewVisited),
    board_flood(FirstNewVisited, SecondNewVisited, Disc, DownPoints, YDown, X),
    board_flood(SecondNewVisited, ThirdNewVisited, Disc, UpPoints, YUp, X),
    board_flood(ThirdNewVisited, FourthNewVisited, Disc, RightPoints, Y, XRight),
    board_flood(FourthNewVisited, NewVisited, Disc, LeftPoints, Y, XLeft),
    pos_points(Elem, Disc, Value),
    Points is DownPoints + UpPoints + LeftPoints + RightPoints + Value.
    


%! max_points(Points, CurrentMaxPoints, -MaxPoints)
% Calculates the maximum number between two given numbers
max_points(Points, MaxPoints, Points):-
    \+ number(MaxPoints), !.
max_points(Points, MaxPoints, Points):-
    Points > MaxPoints, !.
max_points(_, MaxPoints, MaxPoints).



