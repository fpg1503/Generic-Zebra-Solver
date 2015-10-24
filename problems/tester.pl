:- module(tester, [test_solver/1]).

% test_solver(+Solver)
% Solver must have solve/2 (solve (+File, -Solution))
test_solver(Solver) :-
	load_solver(Solver),
	problems(Problems),
	test_and_print(Problems).

load_solver(Solver) :-
	[Solver].

test_and_print([]).
test_and_print([FirstProblem|OtherProblems]) :-
	test_and_print_(FirstProblem),
	test_and_print(OtherProblems).

test_and_print_(Problem) :-
	write('Testing: '), write(Problem), nl,
	statistics(walltime, _),
	ignore(solve(Problem, Solution)),
	statistics(walltime, [_|[ExecutionTime]]),
	write('Execution took '), write(ExecutionTime), write(' ms.'), nl,
	write('Solution: '), 
	(is_list(Solution) -> write(Solution);  write('No solution was found :/')),
	nl, nl.

% ========================= HELPERS - PROBLEM DEFINITION ========================= %

problems(Problems) :-
	ls_to_list(AllFiles),
	include(is_problem, AllFiles, Problems).


is_problem(Problem) :-
	sub_atom(Problem, _, _, 0, '.prob').

% ============================= HELPERS - LS TO LIST ============================= %

ls_to_list(List) :-
	expand_file_name('.', A),
	ls_to_list_(A, List).

ls_to_list_([A], List) :-
	exists_directory(A), !,
	working_directory(C, A),
	expand_file_name(*, B),
	call_cleanup(ls_to_list__(B, List), working_directory(_, C)).

ls_to_list_(A, List) :-
	ls_to_list__(A, List).

ls_to_list__([], _) :- !,
	warning('No Match', []),
	fail.

ls_to_list__(A, List) :-
	maplist(shell:tag_file, A, List).

