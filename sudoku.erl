-module(sudoku).
-import(matrix).
-export([solve/1, solve_from_file/1]).

solve(Sudoku) ->
	case is_solved(Sudoku) of
		true -> Sudoku;
		false ->
			%io:format("Sudoku: ~p~n", [Sudoku]),
			Rows = update_missing(Sudoku),
			%io:format("Rows: ~p~n", [Rows]),
			Columns = update_missing(matrix:transpose(Rows)),
			%io:format("Columns: ~p~n", [Columns]),
			Squares = update_missing(squares_to_rows(Columns)),
			%io:format("Squares: ~p~n", [Squares]),
			NewSudoku = matrix:transpose(squares_to_rows(Squares)),
			%io:format("NewSudoku: ~p~n", [NewSudoku]),
            case NewSudoku of
				Sudoku -> throw("I couldn't solve this. I am sorry.");
				_ -> solve(NewSudoku)
            end
	end.


solve_from_file(Path) ->
	{ ok, RawData } = file:read_file(Path),
	parse(binary_to_list(RawData)).


parse(RawData) ->
	LessRawData = lists:filter(fun(Char) -> lists:member(Char, [10, 95] ++ lists:seq(49, 57)) end, RawData),
	EvenLessRawData = string:tokens(string:strip(LessRawData, both, 10), "\n"),
	solve(lists:map(fun(Row) -> parse_row(Row) end, EvenLessRawData)).

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
	[ intersection(Missing, Current) | update_missing(Row, Missing) ];

update_missing([nil|Row], Missing) ->
	[ Missing | update_missing(Row, Missing) ].

intersection(List1, List2) ->
	[ E || E <- List1, lists:member(E, List2) ].


squares_to_rows(Columns) ->
	squares_to_rows(1, 1, Columns).

squares_to_rows(M, _, _) when M > 9 -> [];

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


