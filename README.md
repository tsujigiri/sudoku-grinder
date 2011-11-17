# Sudoku Grinder

A Sudoku solver written in Erlang as a little finger exercise.

![Sudoku Grinder](http://rausch.io/sudoku_grinder.jpg)

It takes a list of lists with 9 elements each. Each element has to be a number from 1 to 9 or '_' for an empty field.

    sudoku:solve(Sudoku) -> {solved, SolvedSudoku}

It also reads a Sudoku from a file, see the `puzzles` directory for examples.

    sudoku:solve_from_file('/path/to/puzzle') -> {solved, SolvedSudoku}
