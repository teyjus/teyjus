module flexgoal.

foo X Y :- F X Y.

foo X F :- F X.

foo X Y :- F X, foo X F.