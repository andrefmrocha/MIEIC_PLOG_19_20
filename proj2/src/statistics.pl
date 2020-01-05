
% reset_timer
% Has the ability to reset the timer for statistical manipulation
% It is often used in conjuction with @see print_time with the 
% objective of obtatining a current measure of the time and to 
% reset it to the next iteration.
reset_timer :- statistics(walltime, _).


% print_time
% Has the ability to print the current time before the last
% call to the @see reset_timer predicate. Time is written in 
% seconds
print_time :- 
	statistics(walltime, [_, T]),
	TS is ((T // 10) *  10) / 1000,
	write(TS).

% List of several predicates used for labeling
order([leftmost, min, max, ff, anti_first_fail, occurrence, ffc, max_regret]).
selection([enum, bisect, median, middle]).

% combinations(-Pair)
% Predicate to generate a pair of two elements: 1 from the order list 
% and the other from the selection list
% @param Pair: list of two elements, one from each list
combinations([X, Y]):-
	order(Order),
	member(X, Order),
	selection(Selection),
	member(Y, Selection).

% test_generate(-Min, +Max, +Combination, -Flag, +Method)
% Generates statistical information regarding the given Combination.
% Creates boards of Min size until Max size unless Flag 
% ever becomes time_out, meaning it did not finish in due time.
% @param Method - either restrict or restrict2
test_generate(_, _, _, time_out, _). 
test_generate(Min, Max, _, _, _):- 
	Min >= Max,
	nl, nl.
test_generate(Min, Max, Combination, _, Method):-
	generate_stats(no, Combination, Min, NewFlag, _, Method),
	NewMin is Min + 5,
	test_generate(NewMin, Max, Combination, NewFlag, Method).

% test(+Min, +Max, +Method, +Combination)
% Predicate to write some initial information regarding the
% the given Combination. Also prepares the columns names in 
% a csv file. Tightly coupled to @see test_generat
% @param Method - either restrict or restrict2
test(Min, Max, Method, Combination):-
	write('Statistics for: '),
	write(Combination), nl,
	write('Board Size, Posting Constraints, Labeling Time'), nl,
	test_generate(Min, Max, Combination, success, Method).


% run_statistics(+Min, +Max, +Method).
% Generates statistical information regarding all the possible
% combinations of the values given by @see order and @see selection.
% It will generate all boards from size Min until Max, unless any of 
% the given combinations cannot solve it in due time.
% @param Method - either restrict or restrict2
run_statistics(Min, Max, Method):-
	tell('stats.csv'),
	findall(List, combinations(List), Combinations),
	maplist(test(Min, Max, Method), Combinations),
	told.

% test_generate(+Values, +Combination).
% Generates all the board sizes given in Values for the given 
% Combination of Labeling predicates.
% @param Values: List of Integers
test_generate(Values, Combination):-
	write('Statistics for: '),
	write(Combination), nl,
	maplist(generate_stats(_, no, Combination), Values), nl.


% run_statistics(+Values)
% Generates all statistical information for the list of Values 
% that is passed down.
% @param Values: List of Integers
run_statistics(Values):-
	tell('stats.csv'),
	findall(List, combinations(List), Combinations),
	maplist(test_generate(Values), Combinations),
	told.

