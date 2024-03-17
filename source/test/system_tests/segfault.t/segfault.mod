module segfault.


var (x \ X) :- var X.
var X :- pi x \ (var (X x)).

main :- var (x \ x).