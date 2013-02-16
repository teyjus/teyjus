sig accumsig1.
accum_sig accumsig2.

kind i type.
% kind i2 type -> type -> type. % Error, incorrect arity.

type c1 i -> o.
type c2 i -> i -> o.
type c3 int -> o.

% type c4 o -> o -> o.  % Error, incorrect type.

