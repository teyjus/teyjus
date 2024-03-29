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
                      try_me_else              #0, L2
  L4                  proceed                  
  foo3                switch_on_reg            #3, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #3
  L6                  try_me_else              #1, L7
                      proceed                  
  L7                  trust_me                 #1
                      proceed                  
  foo4                switch_on_reg            #4, L8, L9
  L8                  try                      #1, L9
                      trust_ext                #1, #4
  L9                  try_me_else              #1, L10
                      proceed                  
  L10                 retry_me_else            #1, L11
                      proceed                  
  L11                 trust_me                 #1
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
  2: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  3: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
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
  $ tjcc top
  $ tjdis top.lpo
  Disassembling from bytecode file: top.lpo
  Bytecode version: 2
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
  L5                  trust_me                 #1
                      proceed                  
  foo3                switch_on_reg            #3, L6, L7
  L6                  try                      #1, L7
                      trust_ext                #1, #3
                      try_me_else              #0, L2
  L7                  proceed                  
  foo4                switch_on_reg            #4, L8, L9
  L8                  try                      #1, L9
                      trust_ext                #1, #4
  L9                  try_me_else              #1, L10
                      proceed                  
  L10                 retry_me_else            #1, L11
                      proceed                  
  L11                 trust_me                 #1
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
  2: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  3: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
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
  0: acc
      Kind renamings:
      i -> i
      Constant renamings:
      foo4 -> foo4
      foo3 -> foo3
      foo2 -> foo2
      foo1 -> foo1
  
  Imported tables:
  $ tjlink top
  $ tjdis top.lp
  Disassembling from bytecode file: top.lp
  Bytecode version: 3
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo1                switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L11                 trust_me                 #1
                      proceed                  
  foo2                switch_on_reg            #2, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #2
  L12                 retry_me_else            #1, L4
                      proceed                  
  L4                  trust_me                 #1
                      proceed                  
  foo3                switch_on_reg            #3, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #3
  L14                 trust_me                 #1
                      proceed                  
  foo4                switch_on_reg            #4, L7, L8
  L7                  try                      #1, L8
                      trust_ext                #1, #4
  L17                 retry_me_else            #1, L9
                      proceed                  
  L9                  retry_me_else            #1, L10
                      proceed                  
  L10                 trust_me                 #1
                      proceed                  
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
  L1                  try_me_else              #1, L11
                      proceed                  
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
  L3                  try_me_else              #1, L12
                      proceed                  
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
  L6                  try_me_else              #1, L13
                      proceed                  
  L13                 retry_me_else            #1, L14
                      proceed                  
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
                      fail                     
  L8                  try_me_else              #1, L15
                      proceed                  
  L15                 retry_me_else            #1, L16
                      proceed                  
  L16                 retry_me_else            #1, L17
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
  2: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  3: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
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
