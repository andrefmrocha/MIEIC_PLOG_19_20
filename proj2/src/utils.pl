:-use_module(library(lists)).

%! mappable_length(?Length, ?List).
% Utility function that exchanges the order of the parameters
% of the length predicate to make it mappable
mappable_length(Length, List):-
    length(List, Length).


%! board_length(+Side, -Board).
% Utility function that builds an empty square matrix Side by Side
% with the help of maplist and @see mappable_length
board_length(Side, Board):-
    length(Board, Side),
    maplist(mappable_length(Side), Board).

%! flatten(+OriginalList, -FlattenList)
% Utility function that flattens (puts all in the same base level)
% one OriginalList that may contain other lists inside it and 
% returns it via the FlattenList parameter
flatten([], []).
flatten([H | T], FlattenB) :-
	flatten(T, NewFlatten),
	append(H, NewFlatten, FlattenB).