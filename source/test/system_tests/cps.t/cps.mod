module cps.

kind nat    type.
type z      nat.
type s      nat -> nat.
type plus'  nat -> nat -> (nat -> o) -> o.
type times'  nat -> nat -> (nat -> o) -> o.

plus' z     N K :- K N.
plus' (s N) M K :- plus' N M (x\ K (s x)).

times' z     M K :- K z.
times' (s M) N K :- times' M N (x\ plus' x N K).