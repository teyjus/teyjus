module existentialvars.

foo X :- sigma Y\ foo Y.
foo X :- sigma X\ foo X.

foo X :- sigma Z\ foo X.

foo X :- (sigma X\ foo X), foo X.