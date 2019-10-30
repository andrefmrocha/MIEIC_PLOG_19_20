:- ensure_loaded('board_states.pl').

ccA(X) :- char_code('A', X).	% A char code
cc0(X) :- char_code('0', X).	% 0 char code

% Only for debug purposes
read_move_simpl(Move) :-
	empty_board(X),
	read_move(Move, X).

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
	!, length(Input, Comp),
	Comp == 4,
	valid_move_input(Input, Move, Board).

% In case the input is empty calls read_move again
parse_move([], M, B) :- !, read_move(M, B).

% In case of invalid input, shows error message and fails (TODO: change to read_move again)
parse_move(_, _, _) :- write(' Move format is wrong: IC IR FC FR (without spacing)\n'), fail.

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
	char_code(Input, CodeR),
	cc0(CC0),
	RowNumber is CodeR - CC0,
	RowNumber >= 0,
	length(Board, Comp),
	RowNumber < Comp.

read_string(CharList) :-
    current_input(Input),
    read_line_to_codes(Input, Codes),
    string_codes(String, Codes),
	atom_chars(String, CharList).