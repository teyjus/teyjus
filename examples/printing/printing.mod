module printing.



var X :- (open_string "123" Str,
           (((X = "a", input Str 1 Val, fail) ;
              (X = "b", input Str 1 Val, fail)) ;
             (input Str 1 Val, Val = "3"))).
%% need to catch eta-expanded variables of the sort (x \ X x).
% var (x \ X) :- var X.   %% segfault!
% var X :- pi x \ (var (X x)).

type polyintp A -> o.
type polyrealp A -> o.
type polylistp A -> o.
type polystringp A -> o.

polyintp X :- X = 1 ; not (X = 1).
polyrealp X :- (X = 1.0) ; not (X = 1.0).
polylistp X :- X = []; not (X = []).
polystringp X :- X = "a"; not (X = "a").

stringp X :- (not (var X),
               not (polyintp X),
               polystringp X).

%% We must use printtm' because print X and stringp X cannot be in the same
%% clause, since print X constraints the type of X to string!
type printtm' A -> o.
printtm' X :- print X.
printtm X :- stringp X, printtm' X. 
printtm X :- printterm std_out X.

prints X :- (var X), printtm X.
prints (X ++ XS) :- (var XS), printtm X, printtm XS.
prints (X ++ XS) :- printtm X, prints XS.
prints (X ++ Y) :- printtm X, printtm Y.
prints T :- printtm T.

println XS :- prints XS, print "\n".
