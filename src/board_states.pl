:- ensure_loaded('board_pieces.pl').

final_board([
    [corner, null, null, null, null, wt, null, corner], %1
    [null, empty, empty, bl, bl, bl, empty, null], %2
    [null, empty, wt, empty, wt, wt, bl, null], %3
    [null, bl, bl, bl, bl, bl, wt, null], %4
    [null, empty, empty, empty, wt, bl, wt, null], %5
    [null, bl, wt, empty, wt, wt, wt, null], %6
    [null, empty, empty, empty, bl, wt, empty, null], %7
    [corner, null, null, null, null, null, null, corner]  %8
]).

middle_state_board([
    [corner, null, null, null, null, wt, null, corner], %1
    [null, bl, empty, bl, bl, empty, empty, null], %2
    [null, empty, wt, empty, wt, wt, bl, null], %3
    [null, bl, bl, bl, bl, bl, wt, null], %4
    [null, empty, bl, empty, wt, bl, wt, null], %5
    [null, bl, wt, empty, wt, wt, wt, null], %6
    [null, empty, empty, empty, bl, wt, empty, null], %7
    [corner, null, null, null, null, null, null, corner]  %8
]).

second_middle_board([
    [corner, wt, bl, null, bl, bl, empty, corner], %1
    [wt, empty, empty, bl, empty, empty, empty, wt], %2
    [bl, empty, empty, empty, bl, empty, wt, null], %3
    [wt, empty, empty, empty, empty, empty, empty, wt], %4
    [wt, empty, empty, wt, empty, empty, empty, bl], %5
    [null, empty, empty, empty, bl, empty, empty, bl], %6
    [wt, empty, empty, empty, empty, empty, empty, wt], %7
    [corner, bl, wt, wt, bl, wt, wt, corner]  %8
]).

empty_board([
    [corner, null, null, null, null, null, null, corner], %1
    [null, empty, empty, empty, empty, empty, empty, null], %2
    [null, empty, empty, empty, empty, empty, empty, null], %3
    [null, empty, empty, empty, empty, empty, empty, null], %4
    [null, empty, empty, empty, empty, empty, empty, null], %5
    [null, empty, empty, empty, empty, empty, empty, null], %6
    [null, empty, empty, empty, empty, empty, empty, null], %7
    [corner, null, null, null, null, null, null, corner]  %8
]).

debug_board([
    [corner, null, null, corner], %1
    [null,  empty, empty, null], %2
    [null, empty, empty, null], %7
    [corner, null, null, corner]  %8
]).