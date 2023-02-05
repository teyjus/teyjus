module main.

main :- sigma X \ pi p \ pi q \ q X => (p X :- (p 3 => (q X))) => p X.

main2 :- pi p \ ((p :- (pi r \ (r => r))) => p).

