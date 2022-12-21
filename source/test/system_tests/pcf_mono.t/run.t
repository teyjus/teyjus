  $ tjcc examples
  $ tjcc monoinfer
  $ tjcc mono_test
  $ tjlink mono_test
  $ tjsim mono_test -b -s "mono_test \"successor\" Ty."
  
  The answer substitution:
  Ty = num --> num
  $ tjsim mono_test -b -s "mono_test \"onep\" Ty."
  
  The answer substitution:
  Ty = num --> _T1 --> _T1 --> _T1
  $ tjsim mono_test -b -s "mono_test \"is_sym\" Ty."
  
  The answer substitution:
  Ty = (_T1 --> _T1 --> _T2) --> _T1 --> _T1 --> bool
  $ tjsim mono_test -b -s "mono_test \"fib\" Ty."
  
  The answer substitution:
  Ty = num --> num
  $ tjsim mono_test -b -s "mono_test \"map\" Ty."
  
  The answer substitution:
  Ty = (_T1 --> _T2) --> lst _T1 --> lst _T2
  $ tjsim mono_test -b -s "mono_test \"mem\" Ty."
  
  The answer substitution:
  Ty = _T1 --> lst _T1 --> bool
  $ tjsim mono_test -b -s "mono_test \"fact\" Ty."
  
  The answer substitution:
  Ty = num --> num --> num
  $ tjsim mono_test -b -s "mono_test \"app\" Ty."
  
  The answer substitution:
  Ty = lst _T1 --> lst _T1 --> lst _T1
  $ tjsim mono_test -b -s "mono_test \"gcd\" Ty."
  
  The answer substitution:
  Ty = num --> num --> num
  $ tjsim mono_test -b -s "mono_test \"ex1\" Ty."
  $ tjsim mono_test -b -s "mono_test \"ex2\" Ty."
  $ tjsim mono_test -b -s "mono_test \"ex3\" Ty."
  $ tjsim mono_test -b -s "mono_test \"ex4\" Ty."
  
  The answer substitution:
  Ty = _T1 --> _T2 --> _T2
  $ tjsim mono_test -b -s "mono_test \"ex5\" Ty."
  
  The answer substitution:
  Ty = num
  $ tjsim mono_test -b -s "mono_test \"ex6\" Ty."
  
  The answer substitution:
  Ty = _T1 --> _T1
  $ tjsim mono_test -b -s "mono_test \"i\" Ty."
  
  The answer substitution:
  Ty = _T1 --> _T1
  $ tjsim mono_test -b -s "mono_test \"k\" Ty."
  
  The answer substitution:
  Ty = _T1 --> _T2 --> _T1
  $ tjsim mono_test -b -s "mono_test \"s\" Ty."
  
  The answer substitution:
  Ty = (_T1 --> _T2 --> _T3) --> (_T1 --> _T2) --> _T1 --> _T3
  $ tjsim mono_test -b -s "mono_test \"comp\" Ty."
  
  The answer substitution:
  Ty = (_T1 --> _T2) --> (_T3 --> _T1) --> _T3 --> _T2
