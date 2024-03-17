  $ tjcc univimp
  $ tjlink univimp
  $ tjsim univimp -b -s "main X."

  The answer substitution:
  X = "Hi"
  $ tjcc regalloc
  $ tjlink regalloc
  $ tjsim regalloc -b -s "is_one X."

  The answer substitution:
  X = 1
