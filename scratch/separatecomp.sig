sig separatecomp.

accum_sig accumsig1.

kind i type.
kind i2 type -> type.

type c1 i -> o.       % Found in accumsig1
type c2 i -> i -> o.  % Found in accumsig1 with different type.

type c3 int -> o. % Found in accumsig1.
type c4 o -> o. % Found in accumsig2 via accumsig1.
% type c5 o -> o. % Found in accumsig2 via accumsig1, with different type.
