:- module(tokenizer, [tokenize_file/2]).
:- use_module(library(readutil)).

% tokenize_file(+File, -Tokens)
tokenize_file(File, Tokens) :-
   read_file_to_codes(File, Codes, []),
   codes_to_tokens(Codes, Tokens).

code_is_delimiter(32).           % whitespace
code_is_delimiter(124).          % pipe
code_is_delimiter(58).           % colon
code_is_delimiter(44).           % comma
code_is_delimiter(10).           % new line

% delimiters that should be kept as a single atom
code_is_delimiter_keeper(Code) :- code_is_delimiter(Code), !, Code \= 32. % all but whitespace

% delimiters that should be replaced by other values
delimiter_code_to_value(10, nl) :- !.
delimiter_code_to_value(DelimiterCode, DelimiterValue) :-
   atom_codes(DelimiterValue, [DelimiterCode]).

% codes_to_tokens(+Codes, -Tokens)
codes_to_tokens([], [eof]) :- !.

% delimiter keepers
codes_to_tokens([FirstCode|OtherCodes], [FirstToken|OtherTokens]) :-
   code_is_delimiter(FirstCode),
   code_is_delimiter_keeper(FirstCode), !,
   delimiter_code_to_value(FirstCode, FirstToken),
   codes_to_tokens(OtherCodes, OtherTokens).

% ignored delimiters (whitespace)
codes_to_tokens([FirstCode|OtherCodes], Tokens) :-
   code_is_delimiter(FirstCode),
   \+ code_is_delimiter_keeper(FirstCode), !,
   codes_to_tokens(OtherCodes, Tokens).

% regular characters
codes_to_tokens([FirstCode|OtherCodes], [FirstToken|OtherTokens]) :-
   \+ code_is_delimiter(FirstCode), !,
   codes_to_token_until_code_is_delimiter([FirstCode|OtherCodes], FirstToken, CodesAfterNextDelimiter),
   codes_to_tokens(CodesAfterNextDelimiter, OtherTokens).

% auxiliar predicate
% codes_to_token_until_code_is_delimiter(+Codes, -Token -CodesAfterNextDelimiterIncludingTheDelimiterItself)
codes_to_token_until_code_is_delimiter([], _, []).

codes_to_token_until_code_is_delimiter([FirstCode|OtherCodes], _Token, CodesAfterNextDelimiter) :-
   code_is_delimiter(FirstCode), !,
   CodesAfterNextDelimiter = [FirstCode|OtherCodes].

codes_to_token_until_code_is_delimiter([FirstCode|OtherCodes], Atom, CodesAfterNextDelimiter) :-
   \+ code_is_delimiter(FirstCode), !,
   codes_until_delimiter_used_remaining([FirstCode|OtherCodes], Codes, CodesAfterNextDelimiter),
   atom_codes(Atom, Codes).

% yet another auxiliar predicate
% codes_until_delimiter_used_remaining(+Codes, -UsedCodes, -RemainingCodes)
codes_until_delimiter_used_remaining([], [], []).

codes_until_delimiter_used_remaining([FirstCode|OtherCodes], [], [FirstCode|OtherCodes]) :-
   code_is_delimiter(FirstCode), !.

codes_until_delimiter_used_remaining([FirstCode|OtherCodes], [FirstUsedCode|OtherUsedCodes], RemainingCodes) :-
   \+ code_is_delimiter(FirstCode), !,
   FirstUsedCode = FirstCode,
   codes_until_delimiter_used_remaining(OtherCodes, OtherUsedCodes, RemainingCodes).
   