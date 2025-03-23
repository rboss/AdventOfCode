:- use_module(library(dcg/basics)).
:- use_module(library(aggregate)).

lines([])           --> eos, !.
lines([Line|Lines]) --> integer(Line), eol, lines(Lines).

fuel(Mass, Fuel) :-
  Fuel is div(Mass, 3) - 2.

fuel_rec(Fuel, []):- 
  fuel(Fuel, Next),
  Next =< 0.

fuel_rec(Fuel, [H|T]):- 
  fuel(Fuel, H),
  H > 0,
  fuel_rec(H, T).

solve(P1, P2):-
  phrase_from_file(lines(Input), 'input/day1_input.txt'),
  aggregate_all(sum(X), (member(M, Input),
                         fuel(M, X)), P1),
  aggregate_all(sum(X), (member(M, Input),
                         fuel_rec(M, L),
                         sumlist(L, X)), P2).

/*
['day1.pl'].

fuel(12, 2),
fuel(14, 2),
fuel(1969, 654),
fuel(100756, 33583).
*/