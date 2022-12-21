  $ tjcc imp
  $ tjcc acc
  $ tjcc renaming
  $ tjdis imp.lpo
  Disassembling from bytecode file: imp.lpo
  Bytecode version: 2
  Module name: imp
  
  LABEL               INSTRUCTION              OPERANDS
  
  L0                  fail                     
                      try_me_else              #0, L0
  <local const #1>    proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> i
  1: i -> o
  2: i
  3: i -> i
  
  Global constant table: 
  0: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  1: c (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #3
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 0
    Local predicates: 1
     <local const #1>
    Find function type: hash
    In-core table size: 1
     <local const #1>
  
  Accumulated tables:
  
  Imported tables:
  $ tjdis acc.lpo
  Disassembling from bytecode file: acc.lpo
  Bytecode version: 2
  Module name: acc
  
  LABEL               INSTRUCTION              OPERANDS
  
  L0                  fail                     
                      try_me_else              #0, L0
  <local const #1>    proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> i
  1: i -> o
  2: i
  3: i -> i
  
  Global constant table: 
  0: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  1: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #3
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 0
    Local predicates: 1
     <local const #1>
    Find function type: hash
    In-core table size: 1
     <local const #1>
  
  Accumulated tables:
  
  Imported tables:
  $ tjdis renaming.lpo
  Disassembling from bytecode file: renaming.lpo
  Bytecode version: 2
  Module name: renaming
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i
  1: i -> o
  2: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  1: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     foo
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 1
     foo
  
  Accumulated tables:
  0: acc
      Kind renamings:
      i -> i
      Constant renamings:
      a -> a
      b -> <local const #0>
  
  Imported tables:
  0: imp
      Kind renamings:
      i -> i
      Constant renamings:
      c -> <local const #1>
      a -> a
  $ tjlink renaming
  $ tjdis renaming.lp
  Disassembling from bytecode file: renaming.lp
  Bytecode version: 3
  Module name: renaming
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
  L3                  fail                     
                      try_me_else              #0, L3
  <local const #3>    proceed                  
  L4                  fail                     
                      try_me_else              #0, L4
  <local const #5>    proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i
  1: i -> o
  2: i
  3: i -> i -> i
  4: i -> o
  5: i
  6: i -> i
  7: i -> i -> i
  8: i -> o
  9: i
  10: i -> i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  1: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #3
  3:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #4
  4:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #7
  5:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #8
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 1
      Next clause table: 1
     foo
      Local constant table: 4
     <local const #0>
     <local const #1>
     <local const #2>
     <local const #3>
      Find function type: hash
      Search table: 2
     foo
     <local const #3>
  
    Import table:
      number of code segments: 0
      Next clause table: 0
      Local constant table: 2
     <local const #4>
     <local const #5>
      Find function type: hash
      Search table: 1
     <local const #5>
  
  Accumulated tables:
  
  Imported tables:
