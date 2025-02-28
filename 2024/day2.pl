
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

lines([])           --> eos, !.
lines([Line|Lines]) --> line(Line), lines(Lines).
line(Ns) --> sequence(integer, ` `, Ns), eol.

level_check([]):- !.
level_check([_|[]]):- !.
level_check([H1|[H2|T]]) :-
  X is abs(H1-H2),
  X =< 3, X > 0,
  level_check([H2|T]).

same(_, []).
same(_, [_|[]]).
same(X, [H1|[H2|T]]) :-
  X is sign(H2-H1),
  same(Z, [H2|T]),
  X = Z, !.

same_direction(Ls):-
  same(_, Ls).

line_ok(Line):-
  level_check(Line),
  same_direction(Line).

solve1(File, Score):-
  phrase_from_file(lines(Ls), File),
  include(line_ok, Ls, Ls2),
  length(Ls2, Score).

all_line_ok(Line):-
  select(_, Line, Xs),
  line_ok(Xs).

solve2(File, Score):-
  phrase_from_file(lines(Ls), File),
  include(all_line_ok, Ls, Ls2),
  length(Ls2, Score).

main:-
  solve1('day2_input.txt', Out1),
  write('1: '), write(Out1), nl,
  solve2('day2_input.txt', Out2),
  write('2: '), write(Out2), nl.

/*
['day2.pl'].
*/