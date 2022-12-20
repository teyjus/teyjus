  $ tjcc acc
  $ tjcc top
  $ tjdis acc.lpo
  Disassembling from bytecode file: acc.lpo
  Bytecode version: 2
  Module name: acc
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo                 switch_on_reg            #1, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #1
                      try_me_else              #0, L4
  L3                  switch_on_term           L5, L6, L4, L5
  L5                  try_me_else              #1, L7
  L1                  get_m_constant           A1, c
                      finish_unify             
                      proceed                  
  L7                  trust_me                 #1
  L0                  get_m_constant           A1, d
                      finish_unify             
                      proceed                  
  L6                  switch_on_constant       #2, <hash #0>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: c (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: d (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  4: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      d -> L0
      c -> L1
  
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
  
  L4                  fail                     
  foo                 switch_on_reg            #1, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #1
                      try_me_else              #0, L4
  L3                  switch_on_term           L5, L6, L4, L5
  L5                  try_me_else              #1, L7
  L1                  get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L7                  trust_me                 #1
  L0                  get_m_constant           A1, b
                      finish_unify             
                      proceed                  
  L6                  switch_on_constant       #2, <hash #0>
  
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
  2: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: c (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  4: d (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      b -> L0
      a -> L1
  
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
      d -> d
      c -> c
      b -> b
      a -> a
      foo -> foo
  
  Imported tables:
  $ tjlink top
  $ tjdis top.lp
  Disassembling from bytecode file: top.lp
  Bytecode version: 3
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  L6                  fail                     
  foo                 switch_on_reg            #1, L4, L5
  L4                  try                      #1, L5
                      trust_ext                #1, #1
                      try_me_else              #0, L6
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
  L12                 retry_me_else            #1, L7
  L3                  get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L7                  trust_me                 #1
  L2                  get_m_constant           A1, b
                      finish_unify             
                      proceed                  
                      fail                     
                      fail                     
  L8                  fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      try_me_else              #0, L8
  L5                  switch_on_term           L9, L10, L8, L9
  L9                  try_me_else              #1, L11
  L1                  get_m_constant           A1, c
                      finish_unify             
                      proceed                  
  L11                 retry_me_else            #1, L12
  L0                  get_m_constant           A1, d
                      finish_unify             
                      proceed                  
  L10                 switch_on_constant       #4, <hash #1>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: i -> o
  3: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: c (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  4: d (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 0
      Constants:
  1:
      Table size: 4
      Constants:
      d -> L0
      c -> L1
      b -> L2
      a -> L3
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 1
     foo
      Local constant table: 0
      Find function type: hash
      Search table: 1
     foo
  
  Accumulated tables:
  
  Imported tables:
