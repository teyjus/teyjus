  $ tjcc examples
  $ tjcc refl_syntax
  $ tjcc tailrec
  $ tjcc tr_test
  $ tjlink tr_test
  $ tjsim tr_test -b -s "tr_test \"successor\" T."
  
  The answer substitution:
  T = fn (W1\ plus @ W1 @ in 1)
  $ tjsim tr_test -b -s "tr_test \"onep\" T."
  
  The answer substitution:
  T = fn (W1\ fn (W2\ fn (W3\ cond (equal @ in 1 @ W1) W2 W3)))
  $ tjsim tr_test -b -s "tr_test \"is_sym\" T."
  
  The answer substitution:
  T = fn (W1\ fn (W2\ fn (W3\ equal @ (W1 @ W2 @ W3) @ (W1 @ W3 @ W2))))
  $ tjsim tr_test -b -s "tr_test \"fib\" T."
  $ tjsim tr_test -b -s "tr_test \"map\" T."
  $ tjsim tr_test -b -s "tr_test \"mem\" T."
  
  The answer substitution:
  T = fixpt (W1\ fn (W2\ fn (W3\ cond (nullp @ W3) false (cond (and @ (consp @ W3) @ (equal @ (car @ W3) @ W2)) truth (W1 @ W2 @ (cdr @ W3))))))
  $ tjsim tr_test -b -s "tr_test \"fact\" T."
  
  The answer substitution:
  T = fixpt (W1\ fn (W2\ fn (W3\ cond (equal @ W2 @ in 0) W3 (W1 @ (minus @ W2 @ in 1) @ (times @ W2 @ W3)))))
  $ tjsim tr_test -b -s "tr_test \"app\" T."
  $ tjsim tr_test -b -s "tr_test \"gcd\" T."
  
  The answer substitution:
  T = fixpt (W1\ fn (W2\ fn (W3\ cond (equal @ in 1 @ W2) (in 1) (cond (greater @ W3 @ W2) (W1 @ W3 @ W2) (cond (equal @ W2 @ W3) W2 (W1 @ (minus @ W2 @ W3) @ W3))))))

