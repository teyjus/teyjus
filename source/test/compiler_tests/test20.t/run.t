  $ tjcc localconsts
  $ tjdis localconsts.lpo
  Disassembling from bytecode file: localconsts.lpo
  Bytecode version: 2
  Module name: localconsts
  
  LABEL               INSTRUCTION              OPERANDS
  
  L5                  fail                     
  <local const #0>    try_me_else              #1, L2
                      proceed                  
  L2                  trust_me                 #1
                      switch_on_term           L3, L4, L5, L3
  L3                  try_me_else              #1, L6
  L1                  get_m_constant           A1, <local const #2>
                      finish_unify             
                      proceed                  
  L6                  trust_me                 #1
  L0                  get_m_constant           A1, <local const #1>
                      finish_unify             
                      proceed                  
  L4                  switch_on_constant       #2, <hash #0>
  
  Global kind table:
  
  Local kind table:
  0: /0
  
  Type skeleton table:
  0:  -> o
  1: 
  
  Global constant table: 
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      <local const #1> -> L0
      <local const #2> -> L1
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 0
    Local predicates: 1
     <local const #0>
    Find function type: hash
    In-core table size: 1
     <local const #0>
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink localconsts
  $ tjdis localconsts.lp
  Disassembling from bytecode file: localconsts.lp
  Bytecode version: 3
  Module name: localconsts
  
  LABEL               INSTRUCTION              OPERANDS
  
  L5                  fail                     
  <local const #0>    try_me_else              #1, L2
                      proceed                  
  L2                  trust_me                 #1
                      switch_on_term           L3, L4, L5, L3
  L3                  try_me_else              #1, L6
  L1                  get_m_constant           A1, <local const #2>
                      finish_unify             
                      proceed                  
  L6                  trust_me                 #1
  L0                  get_m_constant           A1, <local const #1>
                      finish_unify             
                      proceed                  
  L4                  switch_on_constant       #2, <hash #0>
  
  Global kind table:
  
  Local kind table:
  0: /0
  
  Type skeleton table:
  0:  -> o
  1: 
  
  Global constant table: 
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      <local const #1> -> L0
      <local const #2> -> L1
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 0
      Local constant table: 3
     <local const #0>
     <local const #1>
     <local const #2>
      Find function type: hash
      Search table: 1
     <local const #0>
  
  Accumulated tables:
  
  Imported tables:
