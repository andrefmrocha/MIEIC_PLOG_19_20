:- ensure_loaded('interface.pl').
:- ensure_loaded('board_pieces.pl').

write_line(LineSize, Char, Color) :-
	write_center(LineSize, Char, Char, Color).

write_center(LineSize, Content, Stuffer, Color):-
	char_code(Stuffer, StufferCode),
	ansi_format([fg(Color), bold],'~*t~s~*t~*|', [StufferCode, Content, StufferCode, LineSize]).

display_menu(Option) :-
	clear,
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'FUSE', ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(50, '-', black), tl_oneline, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, '1: Human vs Human', ' ', white), vdiv, nl,
	vdiv, write_center(50, '2: Human vs Bot ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '3: Bot   vs Bot ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '4: Instructions ', ' ', green), vdiv, nl,
	vdiv, write_center(50, '0: Exit         ', ' ', red), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	llc, write_line(50, '═', cyan), lrc, nl,
	read_menu(Option, main).

display_bot_menu(BotNumber, Option) :-
	clear,
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	atom_concat('Bot ', BotNumber, ProvString), atom_concat(ProvString, 'Level', Title),
	vdiv, write_center(50, Title, ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(50, '-', black), tl_oneline, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, '1: Very Easy  ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '2.1: Easy     ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '2.2: Normal    ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '3.1: Hard     ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '3.2: Very Hard ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '0: Back        ', ' ', red), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	llc, write_line(50, '═', cyan), lrc, nl,
	read_menu(Option, bot),!.

choose_order(Option) :-
	clear, 
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'Choose Who Goes First', ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(50, '-', black), tl_oneline, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, '1: Player', ' ', white), vdiv, nl,
	vdiv, write_center(50, '2: Bot   ', ' ', white), vdiv, nl,
	vdiv, write_center(50, '0: Back  ', ' ', red), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	llc, write_line(50, '═', cyan), lrc, nl,
	read_menu(Option, order), !.



% state_machine(Type, State, NextState).
state_machine(exit, _):- write(' Exiting\n'), abort.
state_machine(pvp, pvp).
state_machine(pvp, main).

interpret_decision(back, _).
interpret_decision(Difficulty, pvb):-
	choose_order(FirstPlayer), !,
	interpret_decision(Difficulty, FirstPlayer, pvb).
interpret_decision(Difficulty1, bvb):-
	display_bot_menu('2 ', Difficulty2), !,
	interpret_decision(Difficulty1, Difficulty2, bvb).
	
interpret_decision(_, back, _).
interpret_decision(Difficulty, FirstPlayer, pvb):-
	order_difficulty(FirstPlayer, Difficulty, DifficultyTuple),  !,
	menu_option(game, pvb, DifficultyTuple, FirstPlayer).
interpret_decision(Difficulty1, Difficulty2, bvb):-
	write('Bot vs Bot'), !,
	menu_option(game, bvb, Difficulty1 - Difficulty2, bot).

% menu_option(State, GameMode, Difficulty, FirstPlayer):-
menu_option(main):-
	display_menu(Option),
	menu_option(Option).
menu_option(instructions):-
	display_instructions,
	get_char(_).
menu_option(back).
menu_option(exit):-
	write('Bye'),
	abort.
menu_option(pvb) :- 
	display_bot_menu('', Difficulty),
	interpret_decision(Difficulty, pvb).
menu_option(bvb) :- 
	display_bot_menu('1 ', Difficulty),!,
	interpret_decision(Difficulty, bvb).
menu_option(pvp) :- 
	menu_option(game, pvp, _, _).
menu_option(game, GameMode, Difficulty, FirstPlayer):-
	initialize_empty_board(3, 3, EB),
	initialize_board(EB, BeginBoard),
	!, game_loop(BeginBoard, [0, FirstPlayer], GameMode, Difficulty, 0).



% display_logo :-
% ███████╗██╗   ██╗███████╗███████╗
% ██╔════╝██║   ██║██╔════╝██╔════╝
% █████╗  ██║   ██║███████╗█████╗  
% ██╔══╝  ██║   ██║╚════██║██╔══╝  
% ██║     ╚██████╔╝███████║███████╗
% ╚═╝      ╚═════╝ ╚══════╝╚══════╝

display_logo:-
	write('   '), logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc, logo_sq, logo_sq, urc, write('   '),logo_sq, logo_sq, urc, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc,  nl,
	write('   '), logo_sq, logo_sq, ulc, hdiv, hdiv, lrc, logo_sq, logo_sq, vdiv, write('   '),logo_sq, logo_sq, vdiv, logo_sq, logo_sq, ulc, hdiv, hdiv, lrc, logo_sq, logo_sq, ulc, hdiv, hdiv, lrc, nl,
	write('   '), logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc, write('  '),logo_sq, logo_sq, vdiv, write('   '),logo_sq, logo_sq, vdiv, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc, nl,
	write('   '), logo_sq, logo_sq, ulc, hdiv, lrc, write('  '),logo_sq, logo_sq, vdiv, write('   '),logo_sq, logo_sq, vdiv, llc, hdiv, hdiv, logo_sq, logo_sq, vdiv, logo_sq, logo_sq, ulc, hdiv, lrc, nl,
	write('   '), logo_sq, logo_sq, vdiv, write('     '),llc, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, ulc, lrc, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, vdiv, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, logo_sq, urc, nl,
	write('   '), llc, halfhdiv, lrc, write('      '),llc, hdiv, hdiv, halfhdiv, lrc, write(' '),llc, hdiv, hdiv, hdiv, lrc, llc, hdiv, hdiv, hdiv, lrc,  nl.

display_instructions :-
	clear,
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'Instructions', ' ', green), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(50, '-', black), tl_oneline, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'Welcome to Fuse', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'Your objective is to make', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'the BIGGEST isle', ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, 'Move your pieces from the', ' ', white), vdiv, nl,	
	vdiv, write_center(50, 'periphery to the center', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'pushing other pieces around', ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'Move format: IC IR FC FR', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '(I: Initial; F: Final)', ' ', white), vdiv, nl,
	vdiv, write_center(50, '(C: Column ; R: Row  )', ' ', white), vdiv, nl,
	vdiv, write_center(50, 'WITHOUT SPACES', ' ', red), vdiv, nl,
	vdiv, write_center(50, 'Ex: a1b2', ' ', green), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	vdiv, write_center(50, '0: Back  ', ' ', red), vdiv, nl,
	vdiv, write_center(50, '', ' ', white), vdiv, nl,	
	vdiv, write_center(50, '', ' ', white), vdiv, nl,
	llc, write_line(50, '═', cyan), lrc, nl.
