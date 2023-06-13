module queens.

type col int -> o.
type diag1 int -> o.
type diag2 int -> o.

exportdef range int -> int -> o.
exportdef queen int -> int -> int -> o.
exportdef queens int -> int -> list (list int) -> o.

range _ 0 :- !, fail.
range I N :- I is (N - 1) ; (N0 is (N - 1), range I N0).

queen N X Y :- (range Y N), not(col Y),
	  	  	   D1 is (X + Y), not(diag1 D1),
	  	  	   D2 is (N - X + Y), not(diag2 D2).

queens N 0 [].
queens N K [[X,Y]|Solns] :- X is (K - 1), queen N X Y, D1 is (X + Y), D2 is (N - X + Y), 
	   	   				 	((col Y, diag1 D1, diag2 D2) => queens N X Solns).
				



type printn string -> int -> o.
printn Str 0 :- !.
printn Str K :- print Str, K0 is (K - 1), printn Str K0.

type printsoln int -> list (list int) -> o.
printsoln N [] :- print "\n".
printsoln N [[X,Y]|Solns] :- printn " . " Y, print " X ", Y0 is (N - Y - 1),
		  				  	 printn " . " Y0, print "\n",
		  				  	 printsoln N Solns.

queens' N Solns :- queens N N Solns, printsoln N Solns.
