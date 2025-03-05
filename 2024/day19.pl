
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% pattern([r|X]-X).
% pattern([w,r|X]-X).
% pattern([b|X]-X).
% pattern([g|X]-X).
% pattern([b,w,u|X]-X).
% pattern([r,b|X]-X).
% pattern([g,b|X]-X).
% pattern([b,r|X]-X).

% designs(P-[]):-
%   pattern(P-[]).

% designs(P-X):-
%   pattern(P-S1),
%   designs(S1-X).

% towel(S):- 
%   designs(S-[]).

pattern --> "r".
pattern --> "wr".
pattern --> "b".
pattern --> "g".
pattern --> "bwu".
pattern --> "rb".
pattern --> "gb".
pattern --> "br".

designs --> "".
designs --> pattern, designs.

as --> "".
as --> "a", as.

% stock_patterns([]) --> "".
stock_patterns(Line) --> 
  % P, ",", blank, stock_patterns(Ps).
  sequence(string, `, `, Line), eol.

b --> ",", blank.

% //https://stackoverflow.com/questions/14023852/prolog-dcg-parser-with-input-from-file
% https://github.com/gruhn/advent-of-code/blob/master/2020/Day04.pl

% lines([])           --> eos, !.
% lines([Line|Lines]) --> line(Line), lines(Lines).
% line(Ns) --> sequence(integer, ` `, Ns), eol.

% "r, wr, b, g, bwu, rb, gb, br"



% designs(P-[]):-
%   pattern(P-[]).

% designs(P-X):-
%   pattern(P-S1),
%   designs(S1-X).

% towel(S):- 
%   designs(S-[]).
/*
['day19.pl'].
phrase(stock_patterns(Ln), `jds, weq`).  

*/
