-module(sudoku_tests).
-include_lib("eunit/include/eunit.hrl").

unknowns_test() ->
	[[ {row, 2}, {col,2}, {size,2} ],
	[  {row, 1}, {col,3}, {size,3} ]] =
	sudoku:unknowns([[1,1,[2,3,4],1,1,1,1,1,1],[1,[1,2],1,1,1,1,1,1,1]]).

recombinate_test() ->
	Unknowns = [[{row,1},{col,1},{size,2}], [{row,1},{col,2},{size,2}]],
	Row = [[[1,2],[3,4]]],
	[[1,[3,4]]] = sudoku:recombinate(Row, Unknowns, 1),
	[[2,[3,4]]] = sudoku:recombinate(Row, Unknowns, 2),
	[[1,3]]     = sudoku:recombinate(Row, Unknowns, 3),
	[[2,3]]     = sudoku:recombinate(Row, Unknowns, 4),
	[[1,4]]     = sudoku:recombinate(Row, Unknowns, 5),
	[[2,4]]     = sudoku:recombinate(Row, Unknowns, 6).

shallow_flatten_test() ->
	[1,2,3,[4,5],6,7,8,9] = sudoku:shallow_flatten([[1],[2,3],[[4,5],6],7,8,[9]]).

