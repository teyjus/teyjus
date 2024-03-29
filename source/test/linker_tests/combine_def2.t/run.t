  $ tjcc acc
  $ tjdis acc.lpo
  Disassembling from bytecode file: acc.lpo
  Bytecode version: 2
  Module name: acc
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo1                switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
  foo2                switch_on_reg            #2, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #2
  L4                  try_me_else              #1, L5
                      proceed                  
  L5                  retry_me_else            #1, L6
                      proceed                  
  L6                  trust_me                 #1
                      proceed                  
  
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
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 2
     foo1
     foo2
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 2
     foo1
     foo2
  
  Accumulated tables:
  
  Imported tables:
  $ tjcc top
  $ tjdis top.lpo
  Disassembling from bytecode file: top.lpo
  Bytecode version: 2
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  
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
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 0
  
  Accumulated tables:
  0: acc
      Kind renamings:
      i -> i
      Constant renamings:
      foo2 -> foo2
      foo1 -> foo1
  
  Imported tables:
  $ tjlink top
  $ tjdis top.lp
  Disassembling from bytecode file: top.lp
  Bytecode version: 3
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo1                switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
  foo2                switch_on_reg            #2, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #2
  L4                  try_me_else              #1, L5
                      proceed                  
  L5                  retry_me_else            #1, L6
                      proceed                  
  L6                  trust_me                 #1
                      proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i -> o
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 2
     foo1
     foo2
      Local constant table: 0
      Find function type: hash
      Search table: 2
     foo1
     foo2
  
  Accumulated tables:
  
  Imported tables:
