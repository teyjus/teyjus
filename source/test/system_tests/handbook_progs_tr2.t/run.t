  $ tjcc general_tr
  $ tjcc refl_syntax
  $ tjcc terms
  $ tjcc tr2_test
  $ tjlink tr2_test
  $ tjsim tr2_test -b -s "test 1 F."
  
  The answer substitution:
  F = fix (W1\ abs (W2\ abs (W3\ cond (eq W2 (c 0)) W3 (app (app W1 (minus W2 (c 1))) (times W2 W3)))))
  $ tjsim tr2_test -b -s "test 2 F."
  
  The answer substitution:
  F = fix (W1\ abs (W2\ abs (W3\ cond (eq (c 1) W2) (c 1) (cond (lss W2 W3) (app (app W1 W3) W2) (cond (eq W2 W3) W2 (app (app W1 (minus W2 W3)) W3))))))
  $ tjsim tr2_test -b -s "test 3 F."
  $ tjsim tr2_test -b -s "test 4 F."
  
  The answer substitution:
  F = fix (W1\ abs (W2\ cond (&& (prp W2) (eq (fst W2) (c 0))) (snd W2) (cond (prp W2) (app W1 (pr (minus (fst W2) (c 1)) (times (fst W2) (snd W2)))) err)))
