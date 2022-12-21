  $ tjcc terms
  $ tjcc tr1_test
  $ tjcc tr_recognizer
  $ tjlink tr1_test
  $ tjsim tr1_test -b -s "test 1 F."
  
  The answer substitution:
  F = fix (W1\ abs (W2\ abs (W3\ cond (eq W2 (c 0)) W3 (app (app W1 (minus W2 (c 1))) (times W2 W3)))))
  $ tjsim tr1_test -b -s "test 2 F."
  
  The answer substitution:
  F = fix (W1\ abs (W2\ abs (W3\ cond (eq (c 1) W2) (c 1) (cond (lss W2 W3) (app (app W1 W3) W2) (cond (eq W2 W3) W2 (app (app W1 (minus W2 W3)) W3))))))
  $ tjsim tr1_test -b -s "test 3 F."
