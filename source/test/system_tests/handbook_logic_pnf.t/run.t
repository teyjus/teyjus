  $ tjcc pnf_examples
  $ tjcc pnf
  $ tjcc refl_syntax
  $ tjlink pnf_examples
  $ tjsim pnf_examples -b -s "test 1 F."
  
  The answer substitution:
  F = some (W1\ path a W1 imp tru)
  $ tjsim pnf_examples -b -s "test 2 F."
  
  The answer substitution:
  F = all (W1\ path a W1 imp tru)
  $ tjsim pnf_examples -b -s "test 3 F."
  
  The answer substitution:
  F = all (W1\ path a W1 and path W1 a)
  
  The answer substitution:
  F = all (W1\ all (W2\ path a W1 and path W2 a))
  
  The answer substitution:
  F = all (W1\ all (W2\ path a W2 and path W1 a))
  $ tjsim pnf_examples -b -s "test 4 F."
  
  The answer substitution:
  F = all (W1\ all (W2\ path a W1 imp path a W2))
  
  The answer substitution:
  F = all (W1\ all (W2\ path a W2 imp path a W1))
