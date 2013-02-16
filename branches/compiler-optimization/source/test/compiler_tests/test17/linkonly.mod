module linkonly.

type bar i -> o.

foo A.

bar X :- bar X.
bar X :- bar X, foo X.