  $ tjcc main
  $ tjlink main
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
  