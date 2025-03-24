:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

replace_nth0(Index, OldElement, NewElement, OldList, NewList):-
  nth0(Index, OldList, OldElement, Temp),
  nth0(Index, NewList, NewElement, Temp).

op_inputs(Index, List, Opcode, Op1, Op2, OutI):- 
  nth0(Index, List, Opcode), 
  XIndex is Index + 1,
  nth0(XIndex, List, Op1Index), 
  YIndex is Index + 2,
  nth0(YIndex, List, Op2Index), 
  nth0(Op1Index, List, Op1), 
  nth0(Op2Index, List, Op2),
  OutPos is Index + 3,
  nth0(OutPos, List, OutI).

instruction(Index, List, NewList):- 
  nth0(Index, List, 99),
  NewList = List, !.

instruction(Index, List, Out):- 
  op_inputs(Index, List, 1, Op1, Op2, OutI),
  NewVal is Op1 + Op2,
  replace_nth0(OutI, _, NewVal, List, NewList),
  NewIndex is Index + 4,
  instruction(NewIndex, NewList, Out).

instruction(Index, List, Out):- 
  op_inputs(Index, List, 2, Op1, Op2, OutI),
  NewVal is Op1 * Op2,
  replace_nth0(OutI, _, NewVal, List, NewList),
  NewIndex is Index + 4,
  instruction(NewIndex, NewList, Out).

run(IntCodes, Noun, Verb, Output):-
  replace_nth0(1, _, Noun, IntCodes, TempCodes),
  replace_nth0(2, _, Verb, TempCodes, StartCodes),
  instruction(0, StartCodes, ResultCodes),
  nth0(0, ResultCodes, Output).

solve(P1, P2):-
  phrase_from_file(sequence(integer, `,`, IntCodes), `input/day2_input.txt`),
  run(IntCodes, 12, 2, P1),
  run(IntCodes, Noun, Verb, 19690720),
  P2 is (Noun * 100) + Verb.

/*
['day2.pl'].

*/