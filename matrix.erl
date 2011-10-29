-module(matrix).
-export([submatrix/5, transpose/1]).


submatrix(M, Mlength, N, Nlength, Matrix) ->
	lists:foldr(fun(Row, SubMatrix) -> [lists:sublist(Row, N, Nlength)|SubMatrix] end, [], lists:sublist(Matrix, M, Mlength)).


transpose([[]|_]) -> [];

transpose(Matrix) ->
	[Column, Rows] = lists:foldr(fun(Row, [Column,Rows]) -> [Rh|Rt] = Row, [[Rh|Column], [Rt|Rows]] end, [[],[]], Matrix),
	[Column|transpose(Rows)].

