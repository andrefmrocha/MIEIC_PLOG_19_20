% TODO: mexer com walltime e total_runtime (nos slides, mas n parece dar tao bons resultados)
reset_timer :- statistics(walltime, _).
print_time(Msg) :- 
	statistics(walltime, [_, T]),
	TS is ((T // 10) *  10) / 1000, nl,
	write(Msg), write(TS), write('s'), nl.