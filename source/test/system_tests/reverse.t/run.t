  $ tjcc reverse
  $ tjlink reverse
  $ tjsim reverse -b -s "reverse (1::2::3::4::nil) L."
  
  The answer substitution:
  L = 4 :: 3 :: 2 :: 1 :: nil
  $ tjsim reverse -b -s "reverse (b::a::nil) (F X)."
  
  The answer substitution:
  X = X
  F = F
  
  The remaining disagreement pairs list:
  <F X, a :: b :: nil>
