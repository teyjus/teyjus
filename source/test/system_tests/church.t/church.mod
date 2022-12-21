module church.

kind  i  type.

%  Notice that there are no constructors for objects in type i.  This
%  allows you to conclude that the only closed terms of 
%                          (i -> i) -> (i -> i) 
%  are the Church numerals.

%  Three combinators for Church arithmetic.  succ is not needed given
%  plus, but it's convenient.

succ    (N\F\X\ (N F (F X))).
plus  (N\M\F\X\ ((N F) (M F X))).
mult  (N\M\F\X\ (N (M F) X)).

%  The definitions for Church numerals below are not used in the
%  encoding.  They are included just for convenience.  They are not used
%  since they contribute flexible-rigid pairs: only flexible-flexible
%  pairs are wanted.

church (F\X\ X).
church (F\X\ (C F (F X))) :- church C.

%one O  :- plus P, succ S, (P O O) = (S O).

zero (F\X\(X)).
one (F\X\(F X)).

%  Now, encode the following set of equations:
%     { x = 1, u = x + x,  x + y = z, y * z = u }
%  This set has exactly one solution:  x = 1, u = 2, y = 1, z = 2.

problem1 X U Y Z :-
  one O, plus P, mult M,
  X = O, U = (P X X), Z = (P X Y), U = (M Y Z).


%  ?- zero Z.
%  
%  Z = Z
%  
%   The remaining flexible - flexible pairs:
%  <Z , F \ X \ (Z F (Z F X))>.
%   yes

%  ?- one Z.
%  
%  Z = Z
%  
%   The remaining flexible - flexible pairs:
%  <F \ X \ (Z F (Z F X)) , F \ X \ (Z F (F X))>.
%   yes

%  This is the result of problem 1 using LP2.6.  If only closed terms are
%  allowed for Var1 and Var6, then both of these must be the Church
%  numeral 1 to satisfy the flexible-flexible pairs below.  The bound
%  variables names were changed to make this example more readable.
%
%  ?- problem1 U V W Y.
%  
%  U = Var1,
%  V = F \ X \ (Var1 F (Var1 F X)),
%  W = Var6,
%  Y =  F \ X \ (Var1 F (Var6 F X))
%  
%   The remaining flexible - flexible pairs:
%  <F \ X \ (Var1 F (Var1 F X)),
%   F \ X \ (Var6 Y \ (Var1 F (Var6 F Y)) X)>
%  <F \ X \ (Var1 F (Var1 F X)),
%   F \ X \ (Var1 F (F X))>.

%  Notice that using the predicate church, we can attempt to solve these
%  equational systems using a naive generate and test stradegy.  In the
%  following example, the flexible-flexible pairs for constraints which
%  are tested against successive integers.
%  
%  ?- problem1 X U Y Z, church U.
%  
%  X = F \ X \ (F X)
%  Y = F \ X \ (F X)
%  Z = F \ X \ (F (F X))
%  U = F \ X \ (F (F X))
%  
%  This is a pretty silly way to solve such equations in practice.


% Added by Liang for testing teyjus:
% converts ints to church numerals
cn 0 Z :- zero Z.
cn 1 One :- one One.
cn N (P CNP One) :- N > 1, NP is (N - 1), plus P, one One, cn NP CNP.

% tests:
test 1 :- cn 30 A, cn 87 B, plus P, (P A B) = X.
test 2 :- cn 12 A, mult M, X = (M A A).
test 3 :- cn 14 A, cn 17 B, cn 8 C, (P B (M A C)) = D.
test 4 :- cn 70 A, cn 50 B, cn 60 C, (P A (P B (P A (P C B)))) = U.
test 5 :- cn 70 A, cn 50 B, cn 60 C, (M A (P B (P A (P C B)))) = U.
test 6 :-   cn 39 N, plus P, X = (P N (P N N)).  % more interesting
test 7 :- cn 50 A, cn 5 B, cn 19 C, X = (M A (P C B)).
test 8 :- mult M, cn 2 T, 
          X = (M T (M T (M T (M T (M T (M T ( M T (M T T)))))))).
test 9 :- cn 11 A, cn 13 B, mult M, X = (M A B), Y = (M X X).

go :- test X, fail.
go :- stop. 


 
% cn 500 N will cause "Term free variable name table is full."
% teyjus tests made with cn 200.




