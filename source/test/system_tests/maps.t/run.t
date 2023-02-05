  $ tjcc maps
  $ tjlink maps
  $ tjsim maps -b -s "mapfun [a,b] (X\ (g a X)) L.".
  
  The answer substitution:
  L = g a a :: g a b :: nil
  $ tjsim maps -b -s "mapfun [a] F [g a a]."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F a, g a a>
  $ tjsim maps -b -s "mapfun [a,b] F [g a a, g a b]."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F b, g a b>
  <F a, g a a>
  $ tjsim maps -b -s "mapfun [X] F [g a a]."
  
  The answer substitution:
  X = X
  F = F
  
  The remaining disagreement pairs list:
  <F X, g a a>
  $ tjsim maps -b -s "mappred [jane,john,james] father L."
  
  The answer substitution:
  L = moses :: peter :: peter :: nil
  
  The answer substitution:
  L = john :: peter :: peter :: nil
  $ tjsim maps -b -s "mappred [jane,james] (X1\ X2\ (sigma Y\ ((father X1 Y), (father X2 Y)))) L."
  
  The answer substitution:
  L = jane :: john :: nil
  
  The answer substitution:
  L = jane :: james :: nil
  
  The answer substitution:
  L = jane :: john :: nil
  
  The answer substitution:
  L = jane :: james :: nil
  $ tjsim maps -b -s "mappred [jane,james] P L."
  Error: solve: Ill-formed goal: uninstantiated variable as head.
  $ tjsim maps -b -s "mappred [john,peter] (X\ Y\ ((father Y X) ; (father X Y))) L."
  
  The answer substitution:
  L = jane :: john :: nil
  
  The answer substitution:
  L = jane :: james :: nil
  
  The answer substitution:
  L = jane :: charles :: nil
  
  The answer substitution:
  L = peter :: john :: nil
  
  The answer substitution:
  L = peter :: james :: nil
  
  The answer substitution:
  L = peter :: charles :: nil
  $ tjsim maps -b -s "mappred [john] (X\ Y\ ((father Y X) ; (father Y X))) L."
  
  The answer substitution:
  L = jane :: nil
  
  The answer substitution:
  L = jane :: nil
  $ tjsim maps -b -s "reduce [1,2,3,4,5] (A\ B\ (A + B)) 0 X."
  
  The answer substitution:
  X = 1 + (2 + (3 + (4 + (5 + 0))))
  $ tjsim maps -b -s "reduce_eval [1,2,3] (X\ Y\ (X + Y)) 0 Z."
  
  The answer substitution:
  Z = 6
