-module(sudoku).
-import(matrix).
-export([solve/1, solve_from_file/1, recombinate/3, shallow_flatten/1, unknowns/1]).

solve(Sudoku) ->
	case is_solved(Sudoku) of
		true -> { solved, Sudoku };
		false ->
			case fill(Sudoku) of
				Sudoku -> guess(Sudoku);
				ChangedSudoku -> solve(ChangedSudoku)
			end
	end.


guess(Sudoku) ->
	guess(Sudoku, 1).

guess(Sudoku, Iteration) ->
	Unknowns = unknowns(Sudoku),
	Guess = recombinate(Sudoku, Unknowns, Iteration),
	try
		solve(Guess)
	catch
		contradiction ->
			guess(Sudoku, Iteration + 1)
	end.


% fill the empty fields with the possible values
fill(Sudoku) ->
	Rows = update_missing(Sudoku),
	Columns = update_missing(matrix:transpose(Rows)),
	Squares = update_missing(squares_to_rows(Columns)),
	NewSudoku = matrix:transpose(squares_to_rows(Squares)),
	NewSudoku.


% read a Sudoku from file and solve it
solve_from_file(Path) ->
	{ ok, RawData } = file:read_file(Path),
	solve(parse(binary_to_list(RawData))).


% parse an ASCII Sudoku
parse(RawData) ->
	LessRawData = lists:filter(fun(Char) -> lists:member(Char, [10, 95 | lists:seq(49, 57)]) end, RawData),
	EvenLessRawData = string:tokens(string:strip(LessRawData, both, 10), "\n"),
	lists:map(fun(Row) -> parse_row(Row) end, EvenLessRawData).


% parses a row
parse_row([]) ->
	[];

parse_row([Char|Row]) when Char >= 49, Char =< 57 ->
	[ Char - 48 | parse_row(Row) ];

parse_row([_|Row]) ->
	[ nil | parse_row(Row) ].


update_missing(Rows) ->
	lists:map(fun(Row) -> update_missing(Row, lists:seq(1,9) -- Row) end, Rows).

update_missing([], _ )->
	[];

update_missing([[Number]|Row], Missing) when is_integer(Number) ->
	[ Number | update_missing(Row, Missing) ];

update_missing([Number|Row], Missing) when is_integer(Number) ->
	[ Number | update_missing(Row, Missing) ];

update_missing([Current|Row], Missing) when is_list(Current) ->
	Intersection = intersection(Missing, Current),
	if
		length(Intersection) == 0 -> throw(contradiction);
		true -> go_ahead
	end,
	[ Intersection | update_missing(Row, Missing) ];

update_missing([nil|Row], Missing) ->
	[ Missing | update_missing(Row, Missing) ].


intersection(List1, List2) ->
	[ E || E <- List1, lists:member(E, List2) ].


squares_to_rows(Columns) ->
	squares_to_rows(1, 1, Columns).

squares_to_rows(M, _, _) when M > 9 ->
	[];

squares_to_rows(M, N, Columns) when N > 9 ->
	squares_to_rows(M + 3, 1, Columns);

squares_to_rows(M, N, Columns) ->
	[A,B,C] = matrix:submatrix(M, 3, N, 3, Columns),
	Square = lists:append(A, lists:append(B, C)),
	[ Square | squares_to_rows(M, N + 3, Columns) ].


is_solved([]) ->
	true;

is_solved([nil|_]) ->
	false;

is_solved([Row|Sudoku]) ->
	Integers = lists:takewhile(fun(Current) -> is_integer(Current) end, Row),
	case length(Integers) of
		9 -> is_solved(Sudoku);
		_ -> false
	end.


% extracts all empty fields from a Sudoku and puts them into a list of
% proplists, containing useful information about these fields.
unknowns(Sudoku) ->	
	Unknowns = unknowns(shallow_flatten(Sudoku), 1, 1),
	lists:sort(fun(A,B) ->
		proplists:get_value(size, A) =< proplists:get_value(size, B)
	end, Unknowns).

unknowns([], _, _) ->
	[];

unknowns(FlatSudoku, Row, Col) when Col > 9 ->	
	unknowns(FlatSudoku, Row + 1, 1);

unknowns([ H | FlatSudoku ], Row, Col) when is_integer(H) ->	
	unknowns(FlatSudoku, Row, Col + 1);

unknowns([ H | FlatSudoku ], Row, Col) when is_list(H) ->	
	[[ {row,Row}, {col,Col}, {size,length(H)} ] | unknowns(FlatSudoku, Row, Col + 1) ].


recombinate(Sudoku, [ U | Unknowns ], Iteration) ->
	NumberOfPossibles = proplists:get_value(size, U),
	N = ((Iteration-1) rem NumberOfPossibles) + 1,
	Row = proplists:get_value(row, U),
	Col = proplists:get_value(col, U),
	{ PrecedingRows, [ RowToChange | RestOfTheRows ] } = lists:split(Row-1, Sudoku),
	{ PrecedingElements, [ ElementToChange | RestOfTheElements ] } = lists:split(Col-1, RowToChange),
	Guess = lists:nth(N, ElementToChange),
	NewRow = lists:append(PrecedingElements, [ Guess | RestOfTheElements ]),
	NewSudoku = lists:append(PrecedingRows, [ NewRow | RestOfTheRows ]),
	case Iteration > N of
		true ->
			NextIteration = (Iteration - 1) div NumberOfPossibles,
			recombinate(NewSudoku, Unknowns, NextIteration);
		false -> NewSudoku
	end.


% flattens a deep list one level
shallow_flatten([]) ->
	[];

shallow_flatten([H|T]) when is_list(H) ->
	lists:append(H, shallow_flatten(T));

shallow_flatten([H|T]) ->
	[ H | shallow_flatten(T) ].


