  $ tjcc lists
  $ tjlink lists
  $ tjsim lists -b -s "append (2::nil) (3::nil) L."
  
  The answer substitution:
  L = 2 :: 3 :: nil
  $ tjsim lists -b -s "append L1 L2 (1::2::3::4::5::nil)."
  
  The answer substitution:
  L1 = nil
  L2 = 1 :: 2 :: 3 :: 4 :: 5 :: nil
  
  The answer substitution:
  L1 = 1 :: nil
  L2 = 2 :: 3 :: 4 :: 5 :: nil
  
  The answer substitution:
  L1 = 1 :: 2 :: nil
  L2 = 3 :: 4 :: 5 :: nil
  
  The answer substitution:
  L1 = 1 :: 2 :: 3 :: nil
  L2 = 4 :: 5 :: nil
  
  The answer substitution:
  L1 = 1 :: 2 :: 3 :: 4 :: nil
  L2 = 5 :: nil
  
  The answer substitution:
  L1 = 1 :: 2 :: 3 :: 4 :: 5 :: nil
  L2 = nil
  $ tjsim lists -b -s "pi (X \ (append (X::nil) (2::X::nil) (F X)))."
  
  The answer substitution:
  F = W1\ W1 :: 2 :: W1 :: nil
  $ tjsim lists -b -s "append (F 2) (3::nil) (2::2::3::nil)."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F 2, 2 :: 2 :: nil>
  $ tjsim lists -b -s "append (F X) nil (2::3::2::5::nil)."
  
  The answer substitution:
  X = X
  F = F
  
  The remaining disagreement pairs list:
  <F X, 2 :: 3 :: 2 :: 5 :: nil>
  $ tjsim lists -b -s "sigma X \ (append [2] [3,3] (F X))."
  
  The answer substitution:
  F = F
  
  The remaining disagreement pairs list:
  <F _T1, 2 :: 3 :: 3 :: nil>
  $ tjsim lists -b -s "memb 1 (2::2::nil)."
  
  no (more) solutions
  
  $ tjsim lists -b -s "memb 1 (2::1::nil)."
  
  yes
  
  $ tjsim lists -b -s "memb X (2::1::nil)."
  
  The answer substitution:
  X = 2
  
  The answer substitution:
  X = 1
