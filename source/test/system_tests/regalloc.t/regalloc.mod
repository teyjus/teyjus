module regalloc.

type foo int -> o.
foo 1.

true_fact :- foo 1.
is_one X :- (temp :- true_fact, X = 1) => temp.