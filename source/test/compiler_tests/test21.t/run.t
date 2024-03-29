  $ tjcc oneclause
  $ tjdis oneclause.lpo
  Disassembling from bytecode file: oneclause.lpo
  Bytecode version: 2
  Module name: oneclause
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo1                switch_on_reg            #1, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #1
                      try_me_else              #0, L4
  L3                  proceed                  
  foo2                switch_on_reg            #2, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #2
  L6                  try_me_else              #1, L7
                      switch_on_term           L8, L9, L4, L8
  L8                  try_me_else              #1, L10
  L11                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L10                 trust_me                 #1
  L12                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L0                  try                      #1, L11
                      trust                    #1, L12
  L9                  switch_on_constant       #1, <hash #0>
  L7                  trust_me                 #1
                      proceed                  
  foo3                switch_on_reg            #3, L13, L14
  L13                 try                      #1, L14
                      trust_ext                #1, #3
  L14                 try_me_else              #1, L15
                      get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L15                 trust_me                 #1
                      proceed                  
  foo4                switch_on_reg            #4, L16, L17
  L16                 try                      #1, L17
                      trust_ext                #1, #4
                      try_me_else              #0, L4
  L17                 switch_on_term           L18, L19, L4, L18
  L18                 try_me_else              #1, L20
  L21                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L20                 trust_me                 #1
  L22                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L1                  try                      #1, L21
                      trust                    #1, L22
  L19                 switch_on_constant       #1, <hash #1>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  4: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 1
      Constants:
      a -> L0
  1:
      Table size: 1
      Constants:
      a -> L1
  
  Module table:
    Predicate definitions possibly extending previous ones: 4
     foo1
     foo2
     foo3
     foo4
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 4
     foo1
     foo2
     foo3
     foo4
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink oneclause
  $ tjdis oneclause.lp
  Disassembling from bytecode file: oneclause.lp
  Bytecode version: 3
  Module name: oneclause
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo1                switch_on_reg            #1, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #1
                      try_me_else              #0, L4
  L3                  proceed                  
  foo2                switch_on_reg            #2, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #2
  L6                  try_me_else              #1, L7
                      switch_on_term           L8, L9, L4, L8
  L8                  try_me_else              #1, L10
  L11                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L10                 trust_me                 #1
  L12                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L0                  try                      #1, L11
                      trust                    #1, L12
  L9                  switch_on_constant       #1, <hash #0>
  L7                  trust_me                 #1
                      proceed                  
  foo3                switch_on_reg            #3, L13, L14
  L13                 try                      #1, L14
                      trust_ext                #1, #3
  L14                 try_me_else              #1, L15
                      get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L15                 trust_me                 #1
                      proceed                  
  foo4                switch_on_reg            #4, L16, L17
  L16                 try                      #1, L17
                      trust_ext                #1, #4
                      try_me_else              #0, L4
  L17                 switch_on_term           L18, L19, L4, L18
  L18                 try_me_else              #1, L20
  L21                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L20                 trust_me                 #1
  L22                 get_m_constant           A1, a
                      finish_unify             
                      proceed                  
  L1                  try                      #1, L21
                      trust                    #1, L22
  L19                 switch_on_constant       #1, <hash #1>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  4: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 1
      Constants:
      a -> L0
  1:
      Table size: 1
      Constants:
      a -> L1
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 4
     foo1
     foo2
     foo3
     foo4
      Local constant table: 0
      Find function type: hash
      Search table: 4
     foo1
     foo2
     foo3
     foo4
  
  Accumulated tables:
  
  Imported tables:
