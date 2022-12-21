  $ tjcc eval_basic
  $ tjcc eval_examples
  $ tjcc eval
  $ tjcc terms
  $ tjlink eval_examples
  $ tjsim eval_examples -b -s "eval (app (abs f\ (abs x\ (abs y\ (app (app f x) y)))) (abs u\ (abs v\ u))) R."
  
  The answer substitution:
  R = abs (W1\ abs (W2\ app (app (abs (W3\ abs (W4\ W3))) W1) W2))
  $ tjsim eval_examples -b -s "eval (app (abs f\ (app (app f (abs x\ x)) (abs x\ (abs y\ y)))) (abs u\ (abs v\ u))) R."
  
  The answer substitution:
  R = abs (W1\ W1)
  $ tjsim eval_examples -b -s "eval (app (abs f\ (app (app f (abs x\ x)) (abs x\ (abs y\ y)))) (abs u\ (abs v\ v))) R."
  
  The answer substitution:
  R = abs (W1\ abs (W2\ W2))
  $ tjsim eval_examples -b -s "eval (eq (c 5) (c 0)) R."
  
  The answer substitution:
  R = false
  $ tjsim eval_examples -b -s "eval (eq (c 5) (c 5)) R."
  
  The answer substitution:
  R = truth
  $ tjsim eval_examples -b -s "eval (lss (c 5) (c 3)) R."
  
  The answer substitution:
  R = false
  $ tjsim eval_examples -b -s "eval (lss (c 3) (c 5)) R."
  
  The answer substitution:
  R = truth
  $ tjsim eval_examples -b -s "test 1 F."
  
  The answer substitution:
  F = c 120
  $ tjsim eval_examples -b -s "test 2 F."
  
  The answer substitution:
  F = c 1
  $ tjsim eval_examples -b -s "test 3 F."
  
  The answer substitution:
  F = c 5
  $ tjsim eval_examples -b -s "test 4 F."
  
  The answer substitution:
  F = c 3
  $ tjsim eval_examples -b -s "test 5 F."
  
  The answer substitution:
  F = cons (c 1) (cons (c 2) (cons (c 3) (cons (c 4) null)))
