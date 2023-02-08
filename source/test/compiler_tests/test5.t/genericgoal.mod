module genericgoal.

% universal quantified term variables
foo X Y :- pi Z\ pi X\ (foo Z X).
% universal quantified predicate name
foo X Y :- pi bar\ (bar X Y).
foo X Y :- pi foo\ (foo X Y).