module main.

test0 :- pi p \ (p => p).

test1 :- pi p \ ((p :- (pi r \ (r => r))) => p).

test2 :- sigma X \ pi p \ pi q \ q X => (p X :- (p 3 => (q X))) => p X.

test3 :- pi p \ pi r \ ((p :- ((r :- print "1") => r), ((r :- print "2") => r)) => p).

test4 X :- pi p \ pi q \ pi r \ (p X :- ((q X :- (((r X :- print "1\n"), (r X :- print "2\n")) => r X)) => q X)) => p X.

test5 :- pi p \ (p :- (pi q \ (q :- (pi r \ (r :- (pi s \ (s :- print "s") => s)) => r)) => q)) => p.
