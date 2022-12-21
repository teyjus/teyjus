  $ tjcc hcsyntax_examples
  $ tjcc hc_syntax
  $ tjcc refl_syntax
  $ tjlink hcsyntax_examples
  $ tjsim hcsyntax_examples -b -s "test_goal 1 F."
  
  The answer substitution:
  F = some (W1\ path a W1 and path W1 b)
  $ tjsim hcsyntax_examples -b -s "test_goal 2 F."
  $ tjsim hcsyntax_examples -b -s "test_goal 3 F."
  
  The answer substitution:
  F = path a b and path b a
  $ tjsim hcsyntax_examples -b -s "test_goal 4 F."
  $ tjsim hcsyntax_examples -b -s "test_defcl 4 F."
  
  The answer substitution:
  F = path a b imp path b a
  $ tjsim hcsyntax_examples -b -s "test_defcl 5 F."
  
  The answer substitution:
  F = path a b imp adj a b
  $ tjsim hcsyntax_examples -b -s "test_defcl 6 F."
  
  The answer substitution:
  F = all (W1\ all (W2\ path W1 W2 imp adj W1 W2))
  $ tjsim hcsyntax_examples -b -s "test_defcl 7 F."
