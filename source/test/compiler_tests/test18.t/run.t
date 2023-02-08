  $ tjcc acc
  $ tjcc combinedefs
  $ tjdis acc.lpo
  Disassembling from bytecode file: acc.lpo
  Bytecode version: 2
  Module name: acc
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  head_normalize_t         A1
                      execute_name             bar
                      try_me_else              #0, L2
  <local const #0>    proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     foo
    Exportdef predicates: 0
    Local predicates: 1
     <local const #0>
    Find function type: hash
    In-core table size: 2
     <local const #0>
     foo
  
  Accumulated tables:
  
  Imported tables:
  $ tjdis combinedefs.lpo
  Disassembling from bytecode file: combinedefs.lpo
  Bytecode version: 2
  Module name: combinedefs
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
                      try_me_else              #0, L2
  <local const #0>    proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     foo
    Exportdef predicates: 0
    Local predicates: 1
     <local const #0>
    Find function type: hash
    In-core table size: 2
     <local const #0>
     foo
  
  Accumulated tables:
  0: acc
      Kind renamings:
      i -> i
      Constant renamings:
      foo -> foo
      bar -> <local const #0>
  
  Imported tables:
  $ tjlink combinedefs
  $ tjdis combinedefs.lp
  Disassembling from bytecode file: combinedefs.lp
  Bytecode version: 3
  Module name: combinedefs
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L3                  trust_me                 #1
                      proceed                  
                      try_me_else              #0, L2
  <local const #0>    proceed                  
  L4                  fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
  L1                  try_me_else              #1, L3
                      head_normalize_t         A1
                      execute_name             <local const #0>
                      try_me_else              #0, L4
  <local const #1>    proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 1
     foo
      Local constant table: 2
     <local const #0>
     <local const #1>
      Find function type: hash
      Search table: 3
     <local const #0>
     foo
     <local const #1>
  
  Accumulated tables:
  
  Imported tables:
