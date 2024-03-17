  $ tjcc absparc
  $ tjcc bobabsyn
  $ tjcc bobcatgram
  $ tjcc bobparser
  $ tjcc kitty
  $ tjcc lambdayacc
  $ tjcc realsparc
  $ tjcc typecat
  $ tjlink realsparc
  $ tjsim realsparc -b -s "tigcompile \"test3.bob\" \"test3.s-acc\"."
  Scanning for tokens...
  Tokenizer stopped after line 49
  Parsing...
  
  Assembly code written to file.
  
  yes
  
  $ diff test3.s-acc test3.s-exp
  $ tjsim realsparc -b -s "tigcompile \"test4.bob\" \"test4.s-acc\"."
  Scanning for tokens...
  Tokenizer stopped after line 18
  Parsing...
  
  Assembly code written to file.
  
  yes
  
  $ diff test4.s-acc test4.s-exp
  $ tjsim realsparc -b -s "tigcompile \"test5.bob\" \"test5.s-acc\"."
  Scanning for tokens...
  Tokenizer stopped after line 25
  Parsing...
  
  Assembly code written to file.
  
  yes
  
  $ diff test5.s-acc test5.s-exp
  $ tjsim realsparc -b -s "tigcompile \"test6.bob\" \"test6.s-acc\"."
  Scanning for tokens...
  Tokenizer stopped after line 15
  Parsing...
  
  Assembly code written to file.
  
  yes
  
  $ diff test6.s-acc test6.s-exp
  $ tjsim realsparc -b -s "tigcompile \"test7.bob\" \"test7.s-acc\"."
  Scanning for tokens...
  Tokenizer stopped after line 40
  Parsing...
  
  Assembly code written to file.
  
  yes
  
  $ diff test7.s-acc test7.s-exp
