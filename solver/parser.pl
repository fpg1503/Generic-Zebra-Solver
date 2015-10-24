:- module(parser, [parse_tokens/3]).

% parse_tokens(+Tokens, -Domains, -Constraints)
parse_tokens(Tokens, Domains, Constraints) :-
	problem_description(Domains, Constraints, Tokens, []), !.


% ===================================== GRAMMAR =====================================

problem_description(DomainDescriptionList, ConstraintDescriptionList) -->
	domain_description_list(DomainDescriptionList),
	constraint_description_list(ConstraintDescriptionList), [eof].

item(Item) --> [Atom], {nonvar(Atom), atom_number(Atom, Item); Item = Atom}.

domain_description_list([DomainDescription]) --> domain_description(DomainDescription), [nl].

domain_description_list([FirstDomainDescription|OtherDomainDescriptions]) -->
	domain_description(FirstDomainDescription),
	domain_description_list(OtherDomainDescriptions).

domain_description([domain(H)|T]) --> item(H), [:], domain_list(T), [nl].

domain_list([Domain]) --> item(Domain).

domain_list([FirstDomain|OtherDomains]) --> item(FirstDomain), [,], domain_list(OtherDomains).

constraint_description_list([]) --> [].

constraint_description_list([FirstConstraintDescription|OtherConstraintDescriptions]) -->
	complete_constraint_description(FirstConstraintDescription), constraint_description_list(OtherConstraintDescriptions).

complete_constraint_description(ConstraintDescription) --> constraint_description(ConstraintDescription), [nl].

constraint_description([RelationalOperator, LeftHandSide, RightHandSide]) --> 
	expression(LeftHandSide), [RelationalOperator], {isRelationalOperator(RelationalOperator)}, expression(RightHandSide).

constraint_description([or, LeftHandSide, RightHandSide]) --> 
	constraint_description(LeftHandSide), [or], constraint_description(RightHandSide).

expression(Name) --> item(Name).

expression([BinaryOperator, LeftHandSide, RightHandSide]) --> 
	item(LeftHandSide), 
	[BinaryOperator], {isBinaryExpressionOperator(BinaryOperator)},
	item(RightHandSide).

expression([abs, Expression]) --> ['|'], expression(Expression), ['|'].

isRelationalOperator(=).
isRelationalOperator(<).
isRelationalOperator(>).

isBinaryExpressionOperator(+).
isBinaryExpressionOperator(-).
