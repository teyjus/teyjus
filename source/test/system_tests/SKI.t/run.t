  $ tjcc combos2
  $ tjcc randomlams
  $ tjlink combos2
  $ tjsim combos2 -b -s "single_test \"(I K)\" T."
  
  The answer substitution:
  T = abs (W1\ abs (W2\ W1))
  $ tjsim combos2 -b -s "single_test \"((((I K) I) S) I)\" T."
  
  The answer substitution:
  T = abs (W1\ W1)
  $ tjsim combos2 -b -s "single_test \"((K (S S)) (I ((K (K S)) S)))\" T."
  
  The answer substitution:
  T = abs (W1\ abs (W2\ abs (W3\ app (app W2 W3) (app (app W1 W2) W3))))
  $ tjsim combos2 -b -s "single_test \"((K S) S)\" T."
  
  The answer substitution:
  T = abs (W1\ abs (W2\ abs (W3\ app (app W1 W3) (app W2 W3))))
  $ tjsim combos2 -b -s "single_test \"((I (I K)) S)\" T."
  
  The answer substitution:
  T = abs (W1\ abs (W2\ abs (W3\ abs (W4\ app (app W2 W4) (app W3 W4)))))
