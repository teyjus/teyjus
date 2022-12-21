  $ tjcc control
  $ tjcc examples
  $ tjcc polyinfer
  $ tjcc poly_test
  $ tjcc unifytypes
  $ tjlink poly_test
  $ tjsim poly_test -b -s "poly_test \"successor\" Ty."
  
  The answer substitution:
  Ty = c (num --> num)
  $ tjsim poly_test -b -s "poly_test \"onep\" Ty."
  
  The answer substitution:
  Ty = all (W1\ c (num --> W1 --> W1 --> W1))
  $ tjsim poly_test -b -s "poly_test \"is_sym\" Ty."
  
  The answer substitution:
  Ty = all (W1\ all (W2\ c ((W2 --> W2 --> W1) --> W2 --> W2 --> bool)))
  $ tjsim poly_test -b -s "poly_test \"fib\" Ty."
  
  The answer substitution:
  Ty = c (num --> num)
  $ tjsim poly_test -b -s "poly_test \"map\" Ty."
  
  The answer substitution:
  Ty = all (W1\ all (W2\ c ((W2 --> W1) --> lst W2 --> lst W1)))
  $ tjsim poly_test -b -s "poly_test \"mem\" Ty."
  
  The answer substitution:
  Ty = all (W1\ c (W1 --> lst W1 --> bool))
  $ tjsim poly_test -b -s "poly_test \"fact\" Ty."
  
  The answer substitution:
  Ty = c (num --> num --> num)
  $ tjsim poly_test -b -s "poly_test \"app\" Ty."
  
  The answer substitution:
  Ty = all (W1\ c (lst W1 --> lst W1 --> lst W1))
  $ tjsim poly_test -b -s "poly_test \"gcd\" Ty."
  
  The answer substitution:
  Ty = c (num --> num --> num)
  $ tjsim poly_test -b -s "poly_test \"ex1\" Ty."
  $ tjsim poly_test -b -s "poly_test \"ex2\" Ty."
  $ tjsim poly_test -b -s "poly_test \"ex3\" Ty."
  $ tjsim poly_test -b -s "poly_test \"ex4\" Ty."
  
  The answer substitution:
  Ty = all (W1\ all (W2\ c (W1 --> W2 --> W2)))
  $ tjsim poly_test -b -s "poly_test \"ex5\" Ty."
  
  The answer substitution:
  Ty = c num
  $ tjsim poly_test -b -s "poly_test \"ex6\" Ty."
  
  The answer substitution:
  Ty = all (W1\ c (W1 --> W1))
  $ tjsim poly_test -b -s "poly_test \"i\" Ty."
  
  The answer substitution:
  Ty = all (W1\ c (W1 --> W1))
  $ tjsim poly_test -b -s "poly_test \"k\" Ty."
  
  The answer substitution:
  Ty = all (W1\ all (W2\ c (W2 --> W1 --> W2)))
  $ tjsim poly_test -b -s "poly_test \"s\" Ty."
  
  The answer substitution:
  Ty = all (W1\ all (W2\ all (W3\ c ((W3 --> W2 --> W1) --> (W3 --> W2) --> W3 --> W1))))
  $ tjsim poly_test -b -s "poly_test \"comp\" Ty."
  
  The answer substitution:
  Ty = all (W1\ all (W2\ all (W3\ c ((W2 --> W1) --> (W3 --> W2) --> W3 --> W1))))
