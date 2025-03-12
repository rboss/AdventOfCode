:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(dcg/high_order)).
:- table pattern_exist(_,_,sum).

pattern(P) --> string_without(`,\n`, P).
patterns(Ps) --> sequence(pattern, `, `, Ps).

design(D) --> string_without(`,\n`, D).
designs(Ds) --> sequence(design, `\n`, Ds).

input(Patterns, Designs) --> 
  patterns(Patterns), eol, eol, designs(Designs).

pattern_exist(_, [], 1).
pattern_exist(Towels, Design, N) :-
    aggregate_all(sum(W), (member(T, Towels),
                           append(T, Rest, Design),
                           pattern_exist(Towels, Rest, W)), N).

solve(File, P1, P2):-
  phrase_from_file(input(Patterns, Designs), File),
  maplist(pattern_exist(Patterns), Designs, Ways),
  include(<(0), Ways, Out),
  length(Out, P1),
  sumlist(Out, P2).

/*
['day19.pl'].
solve('input/day19_input.txt', P1, P2).
solve('input/day19_example.txt', P1, P2).

*/