module foo.

foo N :- M is (N - 1), ((bar M) => foo X).