  $ tjcc acc
  $ tjcc top
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
  L1                  get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
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
  
  Imported tables:
  $ tjdis top.lpo
  Disassembling from bytecode file: top.lpo
  Bytecode version: 2
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  bar                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  head_normalize_t         A1
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  
  Global constant table: 
  0: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     bar
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 1
     bar
  
  Accumulated tables:
  0: acc
      Kind renamings:
      i -> i
      Constant renamings:
      a -> a
      foo -> foo
  
  Imported tables:
  $ tjlink top
  $ tjdis top.lp
  Disassembling from bytecode file: top.lp
  Bytecode version: 3
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  bar                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  head_normalize_t         A1
                      execute_name             foo
  L5                  fail                     
  foo                 switch_on_reg            #1, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #1
                      try_me_else              #0, L5
  L4                  get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: i -> o
  3: i
  
  Global constant table: 
  0: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 2
     bar
     foo
      Local constant table: 0
      Find function type: hash
      Search table: 2
     bar
     foo
  
  Accumulated tables:
  
  Imported tables:
