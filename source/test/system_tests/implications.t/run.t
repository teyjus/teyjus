  $ tjcc main
  $ tjlink main
  $ tjsim main -b -s "test0."
  
  yes
  
  $ tjsim main -b -s "test1."
  
  yes
  
  $ tjsim main -b -s "test2."
  
  yes
  
  $ tjsim main -b -s "test3."
  12
  yes
  
  $ tjsim main -b -s "test4 X."
  1
  
  The answer substitution:
  X = X
  2
  
  The answer substitution:
  X = X
  $ tjsim main -b -s "test5."
  s
  yes
  
  $ tjcc queries
  $ tjlink queries
  $ tjsim queries -b -s "main X."
  
  The answer substitution:
  X = 3
  $ tjsim queries -b -s "main2."
  
  yes
  
  $ tjsim queries -b -s "main3".
  
  yes
  
