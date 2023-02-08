module universalvars.

foo X :- pi Y\ ((bar Y :- foo X) => bar X).
foo X :- pi X\ ((bar X :- foo X) => bar X).
foo X :- pi Z\ ((bar X :- bar Y) => bar X).

foo X :- (pi X\ (bar X :- foo X)) => bar X.
