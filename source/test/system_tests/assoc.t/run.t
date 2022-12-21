  $ tjcc assoclists
  $ tjcc assoc
  $ tjlink assoc
  $ tjsim assoc -b -s "assoc (foo:i) (X:int) [pair bar 2, pair foo 1]."
  
  The answer substitution:
  X = 1
  $ tjsim assoc -b -s "assoc (F:i) (X:int) [pair bar 2, pair foo 1]."
  
  The answer substitution:
  X = 2
  F = bar
  $ tjsim assoc -b -s "assoc ((F:i -> i) Y) (X:int) [pair bar 2, pair foo 1]."
  
  The answer substitution:
  X = 2
  Y = Y
  F = F
  
  The remaining disagreement pairs list:
  <F Y, bar>
