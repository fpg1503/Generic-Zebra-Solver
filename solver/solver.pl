:- module(solver, [solve/3]).
:- use_module(library(clpfd)).

% Premature optimization is the root of all evil -- DonaldKnuth

% ========================================= I/O FORMAT ======================================== %
% Domains: List<Domain>
% Domain: [domain(DomainName), PseudoVariable1, PseudoVariable2, ..., PseudoVariableN]
% PseudoVariable: looks like a variable but quacks like an atom
% All domains are *guaranteed* to have the same number of PseudoVariables.
%
% Constraints: List<Constraint>
% Constraint is one of
%  - [=, lhs, rhs]
%  - [<, lhs, rhs]
%  - [>, lhs, rhs]
% Where lhs and rhs are Expressions.
% Expression is one of
%  - integer
%  - [+, lhs, rhs]
%  - [-, lhs, rhs]
%  - [abs, value]
% A Constraint can also be 
%  - [or, lhs, rhs]
% Where lhs and rhs are Constraints
%
% Solution: List<PseudoVariableAssignment>
% PseudoVariableAssignment: c(PseudoVariable, Variable)


% solve(+Domains, +Constraints, -Solution)
% is satisfied when Solutions is the Solution for
% Domains constrained by Constraints
solve(Domains, Constraints, Solution) :-
    % Create List of DecapitatedDomains
    % DecapitatedDomain: List<PseudoVariable>
    seriallyDecapitate(Domains, DecapitatedDomains), !,
    % Read Domains, bind PseudoVariables to Solution.
    bind(DecapitatedDomains, Solution, Constraints, SuperConstraints), !,
    seriallyDecapitateBind(DecapitatedDomains, Solution, BindedDecapitatedDomains),
    distinctify(BindedDecapitatedDomains), !,                                           % used solely for its side effect, created for readability purposes, not meant to be used by anyone else
    % Limit PseudoVariables to Domain                                                   % (same goes for limit/1)
    limit(BindedDecapitatedDomains), !,
    % Feed constraints into solving library (call)
    seriallyCall(SuperConstraints).                                                     % am I unnecessarily increasing the size of my program stack here? Will this convenience predicates degrade performance?

% decapitate(+List, ?DecapitatedList)
% is satified when DecapitatedList is List without its head ;)
decapitate([], []).
decapitate([_Head|DecapitatedList], DecapitatedList).

% seriallyDecapitate(+ListOfVictims, ?DecapitatedVictims)
seriallyDecapitate([],[]).
seriallyDecapitate([Victim|T], DecapitatedVictims) :-
    decapitate(Victim, DecapitatedVictim),
    seriallyDecapitate(T, T2),
    append([DecapitatedVictim], T2, DecapitatedVictims).

% bind(+DecapitatedDomains, ?Solution, +Constraints, ?SuperConstraints)
% is satified when every value in FlattenDecapitatedDomains is binded to 
% a varialbe in Solution (PseudoVariableAssignments of value->variable) and
% SuperConstraints is Constraints by replacing a given value with its variable.
bind(DecapitatedDomains, Solution, Constraints, SuperConstraints) :-
    initializeSolution(DecapitatedDomains, Solution),
    superfy(Constraints, Solution, SuperConstraints).

seriallyDecapitateBind([], _Solution, []).
seriallyDecapitateBind([DecapitatedDomain|T], Solution, BindedDecapitatedDomains) :-
    decapitateBind(DecapitatedDomain, Solution, BindedDecapitatedDomain),
    seriallyDecapitateBind(T, Solution, T2),
    append([BindedDecapitatedDomain], T2, BindedDecapitatedDomains).
    
decapitateBind([], _Solution, []).
decapitateBind([FirstDomainItem|OtherDomainItens], Solution, [BindedDomainsItem|OtherBindedDomainItems]) :-
    member(c(FirstDomainItem, BindedDomainsItem), Solution),
    decapitateBind(OtherDomainItens, Solution, OtherBindedDomainItems).


% initializeSolution(+DecapitatedDomain, ?Solution)
% is satified when Solution is a list of PseudoVariableAssignments
% initializeSolution(DecapitatedDomain, Solution)
initializeSolution([],[]).
initializeSolution([FirstDecapitatedDomain|OtherDecapitatedDomains], InitializedSolution) :-
    initializeSolutionForDecapitatedDomain(FirstDecapitatedDomain, FirstDecapitatedDomainInitializedSolution),
    initializeSolution(OtherDecapitatedDomains, OtherDecapitatedDomainsInitializedSolution),
    append(FirstDecapitatedDomainInitializedSolution, OtherDecapitatedDomainsInitializedSolution, InitializedSolution).


% initializeSolutionForDecapitatedDomain(+DecapitatedDomain, ?PseudoVariableAssignments)
% is satified when PseudoVariableAssignments is a list of PseudoVariableAssignments
% for all PseudoVariables in DecapitatedDomain
initializeSolutionForDecapitatedDomain([],[]).
initializeSolutionForDecapitatedDomain([FirstPseudoVariable|OtherPseudoVariables], [FirstPseudoVariableAssignment|OtherPseudoVariableAssignments]) :-
    functor(FirstPseudoVariableAssignment, c, 2),
    arg(1, FirstPseudoVariableAssignment, FirstPseudoVariable),
    initializeSolutionForDecapitatedDomain(OtherPseudoVariables, OtherPseudoVariableAssignments).

% distinctify(+Domains)
% is satified when all_distinct/1 is satisfied for
% all elements in Domains
% meant to be used for its side effect :/
distinctify([]).
distinctify([H|T]) :-
    all_distinct(H),
    distinctify(T). 

% superfy(+Constraints, +Solution, ?SuperConstraints)
% is satified when SuperConstraints is Constraints 
% by replacing a given value with its variable
% and evaluating them into CLPFD functors.
superfy([], _Solution, []).
superfy([FirstConstraint|OtherConstraints], Solution, [FirstSuperConstraint|OtherSuperContraints]) :-
    evaluate(FirstConstraint, Solution, FirstSuperConstraint),
    superfy(OtherConstraints, Solution, OtherSuperContraints).


% limit(+Domains)
% is satified when for each Domain in Domains
% with Size number of elements
% Domain ins 1..Size is satified
% meant to be used for its side effect :/
limit(Domains) :-
    [Head|_Tail] = Domains,
    length(Head, Length),                       % Premature optimization
    limit(Domains, Length).

limit([], _).
limit([H|T], Length) :-
    H ins 1..Length,
    limit(T, Length).

% seriallyCall(+Functors)
% is satisfied when call/1 is 
% satisfied for each functor in Functors
% meant to be used for its side effect :/
seriallyCall([]).
seriallyCall([FirstFunctor|OtherFunctors]) :-
    call(FirstFunctor),
    seriallyCall(OtherFunctors).

% ========================================= EXPRESSION EVALUTAION ======================================== %


evaluate(X, _Solution, X):- 
    integer(X), !.
evaluate(X, Solution, Y):-
    not(is_list(X)), member(c(X, Y), Solution), !.
evaluate([UnaryOperator, Value], Solution, ReturnValue) :- 
    isUnaryOperator(UnaryOperator), !,
    operatorMap(UnaryOperator, CLPFDOperator),
    evaluate(Value, Solution, ValueValue),
    ReturnValue =.. [CLPFDOperator, ValueValue].

evaluate([BinaryOperator, LHS, RHS], Solution, ReturnValue) :-
    isBinaryOperator(BinaryOperator), !,
    operatorMap(BinaryOperator, CLPFDOperator),
    evaluate(LHS, Solution, LHSValue),
    evaluate(RHS, Solution, RHSValue),
    ReturnValue =.. [CLPFDOperator, LHSValue, RHSValue].


% ============================================== OPERATORS ============================================== %

isUnaryOperator(abs).

isBinaryOperator(=).
isBinaryOperator(<).
isBinaryOperator(>).
isBinaryOperator(+).
isBinaryOperator(-).
isBinaryOperator(or).

                                % ========== Operator Mapping ==========
                                % =                 --->              #=
                                % <                 --->              #<
                                % >                 --->              #>
                                % +                 --->               +
                                % -                 --->               -
                                % abs               --->              abs
                                % or                --->              #\/

                                operatorMap(=, #=).
                                operatorMap(<, #<).
                                operatorMap(>, #>).
                                operatorMap(+, +).
                                operatorMap(-, -).
                                operatorMap(or, #\/).
                                operatorMap(abs, abs).
                                