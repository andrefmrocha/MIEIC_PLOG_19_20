ccA(X) :- char_code('A', X).	% A char code
cc0(X) :- char_code('0', X).	% 0 char code

% read_move(-Move, +Board) :- returns list with the move in the format [ICN, IRN, FCN, FRN].
% ICN :- Initial Column Number
% IRN :- Initial Row Number
% FCN :- Final Column Number
% FRN :- Final Row Number
read_move(Move, Board):-
    write(' Move: '),
    read_string(Input),
    parse_move(Input, Move, Board).

% parse_move(-Input, +Move, -Board) - tests the length of the input and calls @valid_move_input to validate the input
parse_move(Input, Move, Board) :-
	length(Input, 4),
	valid_move_input(Input, Move, Board).

parse_move(Input, Move, Board) :-
	length(Input, 4),
	valid_move_input(Input, _, Board), !, read_move(Move, Board).

% In case the input is empty calls read_move again
parse_move([], M, B) :- !, read_move(M, B).

% TODO: fazer funcao melhor
parse_move(['e', 'x', 'i', 't'], _, _) :- write(' Exiting\n'), !, abort.

% In case of invalid input, shows error message and fails (TODO: change to read_move again)
parse_move(_, M, B) :- write(' Wrong format: InitialColumn InitialRow FinalColumn FinalRow (without spacing).\n Ex:a1d1\n\n'), !, read_move(M, B).

% valid_move_input(-Input, +Move, -Board) :- validates each of the four input chars and transforms each one to a number
valid_move_input([IC, IR, FC, FR], [IC_N, IR_N, FC_N, FR_N], [Line | Board]) :-
	valid_column_input(IC, IC_N, Line),
	valid_row_input(IR, IR_N, [Line | Board]),
	valid_column_input(FC, FC_N, Line),
	valid_row_input(FR, FR_N, [Line | Board]),
	[IC_N, IR_N] \== [FC_N, FR_N].

valid_column_input(Input, ColumnNumber, BoardLine) :-
	upcase_atom(Input, Upper),	% upper cases the IC
	char_code(Upper, CodeC),
	ccA(CCA),
	ColumnNumber is CodeC - CCA,
	ColumnNumber >= 0,
	length(BoardLine, Comp),
	ColumnNumber < Comp.

valid_row_input(Input, RowNumber, Board) :-
	atom_number(Input, RowNumber),
	RowNumber >= 0,
	length(Board, Comp),
	RowNumber < Comp.

read_string(CharList) :-
    current_input(Input),
    read_line_to_codes(Input, Codes),
    string_codes(String, Codes),
	atom_chars(String, CharList).

read_menu(Option, Type):-
    write(' Option: '),
    read_string(Input), 
    parse_menu(Input, Option, Type).

% parse_menu([Input | []], Int, NOptions) :- 
% 	atom_number(Input, Int),
% 	Int =< NOptions.

parse_menu(['0'], exit, main).
parse_menu(['1'], pvp, main).
parse_menu(['2'], pvb, main).
parse_menu(['3'], bvb, main).
parse_menu(['4'], instructions, main).

parse_menu(['0' | []], back, bot).
parse_menu(['1' | []], 0, bot).
parse_menu(['2', '.', '1'  | []], 1, bot).
parse_menu(['2', '.', '2'  | []], 2, bot).
parse_menu(['3', '.', '1'  | []], 3, bot).
parse_menu(['3', '.', '2'  | []], 4, bot).

parse_menu(['0'], back, order).
parse_menu(['1'], human, order).
parse_menu(['2'], bot, order).

parse_menu([], O, Type) :- !, read_menu(O, Type).
parse_menu(_, O, Type) :- 
	noptions(Type, NOptions),
	format(' Invalid Menu Option (choose a number from 0 to ~d)~n', [NOptions]),
	!, read_menu(O, Type).

noptions(main, 4).
noptions(bot, 4).
noptions(order, 2).

read_dimension(Output, Dimension):-
	format(' Number of ~s: ', [Dimension]),
    read_string(Input),
    parse_dimension(Input, Output, Dimension).

parse_dimension([Input], Output, _) :-
	atom_number(Input, Output),
	Output > 0, Output < 8, !.

% In case the input is empty calls read_move again
parse_dimension([], Output, Dimension) :- !, read_dimension(Output, Dimension).

% TODO: fazer funcao melhor
parse_dimension(['e', 'x', 'i', 't'], _, _) :- write(' Exiting\n'), !, abort.

% In case of invalid input, shows error message and fails (TODO: change to read_move again)
parse_dimension(_, Output, Dimension) :- format(' ~s must be an integer between 0 and 8~n', [Dimension]), !, read_dimension(Output, Dimension).
