  $ tjcc segfault
  $ tjlink segfault
  $ tjsim segfault -b -s "main."

  yes

  $ tjcc cps
  $ tjlink cps
  $ tjsim cps -b -s "times' (s (s z)) (s (s z)) (x\ x = N)."

  The answer substitution:
  N = s (s (s (s z)))

  $ tjsim cps -b -s "times' (s (s (s z))) (s z) (x\ x = N)."

  The answer substitution:
  N = s (s (s z))
