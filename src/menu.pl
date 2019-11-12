:- ensure_loaded('interface.pl').
:- ensure_loaded('board_pieces.pl').

write_line(LineSize, Char, Color) :-
	write_center(LineSize, Char, Char, Color).

write_center(LineSize, Content, Stuffer, Color):-
	char_code(Stuffer, StufferCode),
	ansi_format([fg(Color)],'~*t~s~*t~*|', [StufferCode, Content, StufferCode, LineSize]).

display_menu(Option) :-
	clear,
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	vdiv, write_center(46, 'FUSE', ' ', white), vdiv, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(46, '-', yellow), tl_oneline, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	vdiv, write_center(46, '1: Human vs Human', ' ', white), vdiv, nl,
	vdiv, write_center(46, '2: Human vs Bot ', ' ', white), vdiv, nl,
	vdiv, write_center(46, '3: Bot   vs Bot ', ' ', white), vdiv, nl,
	vdiv, write_center(46, '4: Instructions ', ' ', green), vdiv, nl,
	vdiv, write_center(46, '0: Exit         ', ' ', red), vdiv, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	llc, write_line(46, '═', cyan), lrc, nl,
	read_menu(Option, main), !.

display_bot_menu(BotNumber, Option) :-
	clear,
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	atom_concat('Bot ', BotNumber, ProvString), atom_concat(ProvString, 'Level', Title),
	vdiv, write_center(46, Title, ' ', white), vdiv, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(46, '-', yellow), tl_oneline, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	vdiv, write_center(46, '1: Random Bot', ' ', white), vdiv, nl,
	vdiv, write_center(46, '2: Greedy Bot', ' ', white), vdiv, nl,
	vdiv, write_center(46, '3: MinMax Bot', ' ', white), vdiv, nl,
	vdiv, write_center(46, '0: Back      ', ' ', red), vdiv, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	llc, write_line(46, '═', cyan), lrc, nl,
	read_menu(Option, bot), !.

choose_order(Option) :-
	clear, 
	ulc, write_line(50, '═', cyan), urc, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	vdiv, write_center(46, 'Choose Who Goes First', ' ', white), vdiv, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	tr_oneline, write_line(46, '-', yellow), tl_oneline, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	vdiv, write_center(46, '1: Player', ' ', white), vdiv, nl,
	vdiv, write_center(46, '2: Bot   ', ' ', white), vdiv, nl,
	vdiv, write_center(46, '0: Back  ', ' ', red), vdiv, nl,
	vdiv, write_center(46, '', ' ', white), vdiv, nl,	
	vdiv, write_center(46, '', ' ', white), vdiv, nl,
	llc, write_line(46, '═', cyan), lrc, nl,
	read_menu(Option, order), !.


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
