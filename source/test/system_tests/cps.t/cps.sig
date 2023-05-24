sig  cps.

%% This bug was reported by Dale Miller
%% https://github.com/teyjus/teyjus/issues/123

kind nat    type.
type z      nat.
type s      nat -> nat.

type plus'  nat -> nat -> (nat -> o) -> o.
type times'  nat -> nat -> (nat -> o) -> o.
