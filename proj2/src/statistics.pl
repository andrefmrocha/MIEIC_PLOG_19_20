% TODO: mexer com walltime e total_runtime (nos slides, mas n parece dar tao bons resultados)
reset_timer :- statistics(walltime, _).
print_time(Msg) :- 
	statistics(walltime, [_, T]),
	TS is ((T // 10) *  10) / 1000,
	write(Msg), write(TS), write('s'), nl.

combs([],[]).
combs([H|T],[H|T2]) :-
    combs(T,T2).
combs([_|T],T2) :-
    combs(T,T2).


test_generate(Max, Max, Combination):- 
	write('Statistics for: '),
	write(Combination), nl, nl.
test_generate(Min, Max, Combination):-
	write('Value: '), write(Min), nl,
	generate_stats(_, no, Combination, Min),
	NewMin is Min + 1,
	test_generate(NewMin, Max, Combination).

run_statistics(Labeling, Min, Max):-
	tell('stats.txt'),
	findall(List, combs(Labeling, List), Combinations),
	maplist(test_generate(Min, Max), Combinations),
	told.

test_generate(Values, Combination):-
	write('Statistics for: '),
	write(Combination), nl,
	maplist(generate_stats(_, no, Combination), Values),
	nl, nl.

run_statistics(Labeling, Values):-
	tell('stats.txt'),
	findall(List, combs(Labeling, List), Combinations),
	maplist(test_generate(Values), Combinations),
	told.