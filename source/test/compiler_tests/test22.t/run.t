  $ tjcc expdef
  $ tjdis expdef.lpo
  Disassembling from bytecode file: expdef.lpo
  Bytecode version: 2
  Module name: expdef
  
  LABEL               INSTRUCTION              OPERANDS
  
  L0                  fail                     
                      try_me_else              #0, L0
  foo1                proceed                  
                      try_me_else              #0, L0
  foo2                proceed                  
                      try_me_else              #0, L0
  foo5                proceed                  
                      try_me_else              #0, L0
  foo6                proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: foo5 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  3: foo6 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  4: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 5
     foo1
     foo2
     foo5
     foo6
     foo3
    Local predicates: 0
    Find function type: hash
    In-core table size: 4
     foo1
     foo2
     foo5
     foo6
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink expdef
  $ tjdis expdef.lp
  Disassembling from bytecode file: expdef.lp
  Bytecode version: 3
  Module name: expdef
  
  LABEL               INSTRUCTION              OPERANDS
  
  L0                  fail                     
                      try_me_else              #0, L0
  foo1                proceed                  
                      try_me_else              #0, L0
  foo2                proceed                  
                      try_me_else              #0, L0
  foo5                proceed                  
                      try_me_else              #0, L0
  foo6                proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: foo5 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  3: foo6 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  4: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 0
      Local constant table: 0
      Find function type: hash
      Search table: 4
     foo1
     foo2
     foo5
     foo6
  
  Accumulated tables:
  
  Imported tables:
