  $ tjcc unif
  $ tjlink unif
  $ tjsim unif -b -s "main."
  $ tjcc uncurry
  $ tjlink uncurry
  $ tjsim uncurry -b -s "uncurry_lift (abstr uncurry (x\ abstr uncurry (y\ abstr normal (z\ x)))) T."

  The answer substitution:
  T = abstr' (W1\ W1)

  $ tjsim uncurry -b -s "uncurry_lift (abstr uncurry (x\ abstr uncurry (y\ abstr uncurry (z\ abstr normal (w\ x))))) T."