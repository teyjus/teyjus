module cutgoal.

foo X :- !, bar X.

foo X :- bar X, !, bar X.

% check user defined !, fail
foo X :- !, fail.