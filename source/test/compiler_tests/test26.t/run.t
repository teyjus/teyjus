  $ tjcc redef_pervasive
  $ tjdis redef_pervasive.lpo
  Disassembling from bytecode file: redef_pervasive.lpo
  Bytecode version: 2
  Module name: redef_pervasive
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  sqrt                switch_on_reg            #2, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #2
                      try_me_else              #0, L2
  L1                  proceed                  
  mod                 switch_on_reg            #1, L3, L4
  L3                  try                      #2, L4
                      trust_ext                #2, #1
                      try_me_else              #0, L2
  L4                  proceed                  
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  
  Global constant table: 
  0: mod (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  1: sqrt (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 2
     mod
     sqrt
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 2
     mod
     sqrt
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink redef_pervasive
  $ tjdis redef_pervasive.lp
  Disassembling from bytecode file: redef_pervasive.lp
  Bytecode version: 3
  Module name: redef_pervasive
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  sqrt                switch_on_reg            #2, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #2
                      try_me_else              #0, L2
  L1                  proceed                  
  mod                 switch_on_reg            #1, L3, L4
  L3                  try                      #2, L4
                      trust_ext                #2, #1
                      try_me_else              #0, L2
  L4                  proceed                  
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  
  Global constant table: 
  0: mod (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  1: sqrt (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 2
     mod
     sqrt
      Local constant table: 0
      Find function type: hash
      Search table: 2
     mod
     sqrt
  
  Accumulated tables:
  
  Imported tables:
