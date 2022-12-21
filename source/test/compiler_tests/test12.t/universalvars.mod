module universalvars.

foo X :- pi Y\ foo Y.

foo X :- pi X\ foo X.

foo X :- pi Z\ foo X.

foo X :- (pi X\ foo X), foo X. 