% TODO: mexer com walltime e total_runtime (nos slides, mas n parece dar tao bons resultados)
reset_timer :- statistics(walltime, _).
print_time :- 
	statistics(walltime, [_, T]),
	TS is ((T // 10) *  10) / 1000,
	write(TS).

combs([],[]).
combs([H|T],[H|T2]) :-
    combs(T,T2).
combs([_|T],T2) :-
    combs(T,T2).

order([leftmost, min, max, ff, anti_first_fail, occurrence, ffc, max_regret]).
selection([enum, bisect, median, middle]).


combinations([X, Y]):-
	order(Order),
	member(X, Order),
	selection(Selection),
	member(Y, Selection).
		

make_exclusive_combs(List, Combination):-
	combs(List, Combination),
	restrict_bisect(Combination).

test_generate(Min, Max, _):- 
	Min >= Max,
	nl, nl.
test_generate(Min, Max, Combination):-
	generate_stats(_, no, Combination, Min),
	NewMin is Min + 5,
	test_generate(NewMin, Max, Combination).

test(Min, Max, Combination):-
	write('Statistics for: '),
	write(Combination), nl,
	write('Board Size, Posting Constraints, Labeling Time'), nl,
	test_generate(Min, Max, Combination).


run_statistics(Min, Max):-
	tell('stats.csv'),
	findall(List, combinations(List), Combinations),
	maplist(test(Min, Max), Combinations),
	told.

test_generate(Values, Combination):-
	write('Statistics for: '),
	write(Combination), nl,
	maplist(generate_stats(_, no, Combination), Values), nl.

run_statistics(Values):-
	tell('stats.csv'),
	findall(List, combinations(List), Combinations),
	maplist(test_generate(Values), Combinations),
	told.

run_statistics(Labeling):-
	tell('stats.csv'),
	maplist(test_generate([20]), Labeling),
	told.


