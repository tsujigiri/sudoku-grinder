-module(benchmark).
-import(sudoku).
-export([call_n_times/4]).

call_n_times(N, Module, Function, Args) ->
	call_n_times([], N, Module, Function, Args).

call_n_times(Data, 0, _, _, _) ->
	io:format("average: ~p us~n", [lists:sum(Data) / length(Data)]),
	io:format("fastest: ~p us~n", [lists:min(Data)]);

call_n_times(Data, N, Module, Function, Args) ->
	{ NewData, _ } = timer:tc(Module, Function, Args),
	call_n_times([NewData|Data], N-1, Module, Function, Args).
