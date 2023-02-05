module queries.


main X :- pi p \ pi q \
	 q X X
	 => (pi X \ p X :- (pi r \ r 3 => (q X Y, r Y)))
	 => p X.


%% This segfaults, but the problem goes away
%% if main and main3 are commented??
main2 :- sigma X \ pi p \ pi q \ q X => (p X :- (p 3 => (q X))) => p X.



main3 :- pi p \ ((p :- (pi r \ (r => r))) => p).