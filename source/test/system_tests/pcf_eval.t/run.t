  $ tjcc control
  $ tjcc eval
  $ tjcc eval_test
  $ tjcc examples
  $ tjlink eval_test
  $ tjsim eval_test -b -s "eval_test 1 V."
  
  The answer substitution:
  V = in 144
  $ tjsim eval_test -b -s "eval_test 2 V."
  
  The answer substitution:
  V = cons @ in 2 @ (cons @ in 8 @ empty)
  $ tjsim eval_test -b -s "eval_test 3 V."
  
  The answer substitution:
  V = cons @ in 3 @ (cons @ in 5 @ empty)
  $ tjsim eval_test -b -s "eval_test 4 V."
  
  The answer substitution:
  V = truth
  $ tjsim eval_test -b -s "eval_test 5 V."
  
  The answer substitution:
  V = false
