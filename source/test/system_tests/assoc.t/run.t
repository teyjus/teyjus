  $ tjcc assoclists
  $ tjcc assoc
  $ tjlink assoc
  $ tjsim assoc -b -s "assoc (foo:i) (X:int) [pair bar 2, pair foo 1]."
  
  The answer substitution:
  X = 1
  $ tjsim assoc -b -s "assoc (F:i) (X:int) [pair bar 2, pair foo 1]."
  
  The answer substitution:
  F = bar
  X = 2
  $ tjsim assoc -b -s "assoc ((F:i -> i) Y) (X:int) [pair bar 2, pair foo 1]."
  
  The answer substitution:
  Y = Y
  F = F
  X = 2
  
  The remaining disagreement pairs list:
  <F Y, bar>
