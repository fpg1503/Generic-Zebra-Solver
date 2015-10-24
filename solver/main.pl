:- module(main, [solve/2]).
:- use_module(tokenizer).
:- use_module(parser).
:- use_module(solver).

solve(File, Solution) :-
	tokenize_file(File, Tokens),
	parse_tokens(Tokens, Domains, Constraints),
	solve(Domains, Constraints, Solution).
