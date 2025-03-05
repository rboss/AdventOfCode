
% :- dynamic pattern/2.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(dcg/high_order)).

... --> [] ; [_], ... .

% pattern('a').

lexeme(Atom) --> string_without(` ,\n`, Codes),
	{ atom_codes(Atom, Codes) }.

available_patterns([]) --> [].
available_patterns([P|Ps]) --> 
  lexeme(P), `, `, available_patterns(Ps).
available_patterns([P]) --> 
  lexeme(P), eol.

designs([]) --> eos, !.
designs([D|Ds]) -->
  lexeme(D), eol, designs(Ds).

input(Patterns, Designs) --> 
  available_patterns(Patterns), eol, designs(Designs).

% line(Line) --> 
%   sequence(string, `, `, Line), eol. 
  % sequence(string, `, `, Line), eol, {set(Line)}.

% pass([C|T], V):-
%   atom_codes(V, [C]) .


% set([]).
% set([L|R]):- 
%   assert(pattern([L|X], X)),
%   set(R).

/*
phrase(line, "a").
phrase(line(C), "r, wr, b, g, bwu, rb, gb, br").

phrase(line(C), `r, wr, b, g, bwu, rb, gb, br`).

phrase(available_patterns(C), `r, wr, b, g, bwu, rb, gb, br`).
phrase(designs(C), `asdasr\nwwq`).
phrase(designs(C), `asdasr`).

phrase_from_file(input(C, V), 'day19_example.txt').

phrase(line(C), 'r, wr, b, g, bwu, rb, gb, br').
['test.pl'].

*/