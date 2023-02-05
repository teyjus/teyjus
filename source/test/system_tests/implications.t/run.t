  $ tjcc main
  $ tjlink main
  $ tjsim -b -s "sigma X \ pi p \ pi q \ q X => (p X :- (p 3 => (q X))) => p X."

  yes

  $ tjsim main -b -s "main."

  yes

  $ tjsim main -b -s "main2."

  yes

  $ tjcc queries
  $ tjlink queries
  $ tjsim queries -b -s "main X."

  yes

  $ tjsim queries -b -s "main2."

  yes

  $ tjsim queries -b -s "main3".

  yes
  