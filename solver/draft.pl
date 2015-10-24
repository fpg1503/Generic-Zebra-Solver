:- use_module(library(clpfd)).

% solve(+Domains, +Constraints, -Solution)
%
% =========================== I/O FORMAT ============================
% Domains: List<Domain>
% Domain: [domain(DomainName), RaÕssa1, RaÕssa2, ..., RaÕssaN]
% RaÕssa: variable
% All domains are *guaranteed* to have the same number of RaÕssas.
%
% Constraints: List<Constraint>
% Constraint is one of
%  - [=, lhs, rhs]
%  - [<, lhs, rhs]
%  - [>, lhs, rhs]
% Where lhs and rhs are Expressions.
% Expression is one of
%  - integer
%  - [+, lhs, rhs]
%  - [-, lhs, rhs]
%  - [abs, value]
% A Constraint can also be 
%  - [or, lhs, rhs]
% Where lhs and rhs are Constraints
%
% Solution: List<RaÕssaAssignment>
% RaÕssaAssignemnt: c(RaÕssa, Value)
%
solve(Domains, Constraints, Solution) :-
    % Create List of DecapitatedDomains
    % DecapitatedDomain: List<RaÕssa>
    decapitate(Domains, DecapitatedDomains),
    % Read Domains, bind RaÕssas to Solution.
    bind(DecapitatedDomains, Solution),         % can use FlattenDecapitatedDomains
    % Declare Domains as distinct
    distinctify(DecapitatedDomains),            % used solely for its side effect, created for readability purposes, not meant to be used by anyone else
    % Limit RaÕssas to Domain                   % (same goes for limit/1)
    limit(DecapitatedDomains),                  % could use FlattenDecapitated by adding another parameter to limit domain size, is it a necessary optimization?
    % Feed constraints into solving library
    constraintify(Constraints).                 % same as distinctify/1 and limit/1; Am I unnecessarily increasing the size of my program stack here? Will this convenience predicates degrade performance?

% TODO: I am probably forgetting SEVERAL cuts :/

% ============= TO BE IMPLEMENTED =============
% decapitate/2
% bind/2
% distinctify/1
% limit/1
% constraintify/1
% =============================================
