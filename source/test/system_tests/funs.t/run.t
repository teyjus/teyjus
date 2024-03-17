  $ tjcc funs
  $ tjcc lists
  $ tjlink funs
  $ tjsim funs -b -s "mapfun (X\ (g a X)) [a,b] L."
  
  The answer substitution:
  L = g a a :: g a b :: nil
  $ tjsim funs -b -s "mapfun F [a] [g a a]."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F a, g a a>
  $ tjsim funs -b -s "mapfun F [a,b] [g a a, g a b]."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F b, g a b>
  <F a, g a a>
  $ tjsim funs -b -s "mapfun F [X] [g a a]."
  
  The answer substitution:
  F = F
  X = X
  
  The remaining disagreement pairs list:
  <F X, g a a>
  $ tjsim funs -b -s "foldr F 5 [1,2,3,4] L."
  
  The answer substitution:
  F = F
  L = F 1 (F 2 (F 3 (F 4 5)))
  $ tjsim funs -b -s "foldl F 5 [1,2,3,4] L."
  
  The answer substitution:
  F = F
  L = F (F (F (F 5 4) 3) 2) 1
  $ tjsim funs -b -s "foldl F 5 [1,2,3,4] (5 + 4 + 3 + 2 + 1)."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F (F (F (F 5 4) 3) 2) 1, 5 + 4 + 3 + 2 + 1>
