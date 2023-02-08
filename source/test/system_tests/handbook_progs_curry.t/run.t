  $ tjcc curry_test
  $ tjcc curry_transform
  $ tjcc terms
  $ tjlink curry_test
  $ tjsim curry_test -b -s "test 1 F."
  
  The answer substitution:
  F = fix (W1\ abs (W2\ abs (W3\ _T1 W2 W3 truth (W4\ W5\ app (app W1 W4) W5))))
  
  The remaining disagreement pairs list:
  <_T1 (fst #1) (snd #1) (prp #1) (W1\ W2\ app #2 (pr W1 W2)), cond (&& (prp #1) (eq (fst #1) (c 0))) (snd #1) (cond (prp #1) (app #2 (pr (minus (fst #1) (c 1)) (times (fst #1) (snd #1)))) err)>
