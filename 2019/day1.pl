% # :- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(aggregate)).
% # :- use_module(library(dcg/high_order)).

line(L) --> integer(L), eol.
lines([])           --> eos, !.
lines([Line|Lines]) --> integer(Line), eol, lines(Lines).

fuel(Mass, Fuel) :-
  X is div(Mass, 3),
  Fuel is X - 2.

sum_fuel(Input, Sum):-
  aggregate_all(sum(X), (member(M, Input),
                         fuel(M, X)), Sum).

fuel_fuel(Fuel, []):- 
  fuel(Fuel, Next),
  Next =< 0.

fuel_fuel(Fuel, [H|T]):- 
  fuel(Fuel, H),
  H > 0,
  fuel_fuel(H, T).

rec_sum_fuel(Input, Sum):-
  aggregate_all(sum(X), (member(M, Input),
                         fuel_fuel(M, L),
                         sumlist(L, X)), Sum).

test():- 
  fuel(12, 2),
  fuel(14, 2),
  fuel(1969, 654),
  fuel(100756, 33583).

solve(P1, P2):-
  phrase_from_file(lines(Input), 'input/day1_input.txt'),
  sum_fuel(Input, P1),
  rec_sum_fuel(Input, P2).

/*
['day1.pl'].
*/