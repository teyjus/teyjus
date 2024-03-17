  $ tjsim -b -s "pi p \ (p 5 => p X)."
  
  The answer substitution:
  X = 5
  $ tjsim -b -s "pi p \ (p 1 => p 2 => p 3 => p X)."
  
  The answer substitution:
  X = 3
  
  The answer substitution:
  X = 2
  
  The answer substitution:
  X = 1
  $ tjsim -b -s "pi p \ (p \"hello\" => p X)."
  
  The answer substitution:
  X = "hello"
  $ tjsim -b -s "pi p \ pi q \ pi r \ (pi P \ p P :- print \"1\", P) => (pi P \ q P :- print \"2\", P) => (r 0) => (p (q (r X)))."
  12
  The answer substitution:
  X = 0
  $ tjsim -b -s "pi findall \ ((pi P \ findall P [] []) => (pi P \ pi X \ pi L \ pi SL \ findall P [X|L] [X|SL] :- (P X), findall P L SL) => (pi P \ pi X \ pi L \ pi SL \ findall P [X|L] SL :- (not (P X)), findall P L SL) => (findall (x \ x > 3) [1,2,3,4,5,2,6,3] S))."
  
  The answer substitution:
  S = 4 :: 5 :: 6 :: nil
  $ tjsim -b -s "pi p \ pi q \ (pi x \ q x x) => (pi x \ p x :- (sigma y \ (pi r \ r 3 => (q x y, r y)))) => p X."
  
  The answer substitution:
  X = 3


