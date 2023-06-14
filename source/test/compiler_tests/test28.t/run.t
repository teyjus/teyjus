  $ tjcc bvScope
  $ tjdis bvScope.lpo
  Disassembling from bytecode file: bvScope.lpo
  Bytecode version: 2
  Module name: bvScope
  
  LABEL               INSTRUCTION              OPERANDS
  
  L3                  fail                     
  foo                 switch_on_reg            #1, L1, L2
  L1                  try                      #1, L2
                      trust_ext                #1, #1
                      try_me_else              #0, L3
  L2                  switch_on_term           L4, L5, L3, L4
  L4                  try_me_else              #1, L6
  L7                  get_m_structure          A1, abs, #1
                      unify_variable_t         A255
                      put_m_const              A254, abs
                      put_index                A253, #1
                      put_lambda               A252, A253, #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      pattern_unify_t          A254, A255
                      finish_unify             
                      proceed                  
  L6                  trust_me                 #1
  L8                  get_m_structure          A1, abs, #1
                      unify_variable_t         A255
                      put_m_const              A254, app
                      put_m_const              A253, abs
                      put_index                A252, #1
                      put_lambda               A251, A252, #1
                      put_app                  A252, A253, #1
                      set_value_t              A251
                      put_app                  A253, A254, #2
                      set_index                #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      pattern_unify_t          A254, A255
                      finish_unify             
                      proceed                  
  L0                  try                      #1, L7
                      trust                    #1, L8
  L5                  switch_on_constant       #1, <hash #0>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: (i -> i) -> i
  2: i -> i -> i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: abs (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: app (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 1
      Constants:
      abs -> L0
  
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
  $ tjlink bvScope
  $ tjdis bvScope.lp
  Disassembling from bytecode file: bvScope.lp
  Bytecode version: 3
  Module name: bvScope
  
  LABEL               INSTRUCTION              OPERANDS
  
  L3                  fail                     
  foo                 switch_on_reg            #1, L1, L2
  L1                  try                      #1, L2
                      trust_ext                #1, #1
                      try_me_else              #0, L3
  L2                  switch_on_term           L4, L5, L3, L4
  L4                  try_me_else              #1, L6
  L7                  get_m_structure          A1, abs, #1
                      unify_variable_t         A255
                      put_m_const              A254, abs
                      put_index                A253, #1
                      put_lambda               A252, A253, #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      pattern_unify_t          A254, A255
                      finish_unify             
                      proceed                  
  L6                  trust_me                 #1
  L8                  get_m_structure          A1, abs, #1
                      unify_variable_t         A255
                      put_m_const              A254, app
                      put_m_const              A253, abs
                      put_index                A252, #1
                      put_lambda               A251, A252, #1
                      put_app                  A252, A253, #1
                      set_value_t              A251
                      put_app                  A253, A254, #2
                      set_index                #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      pattern_unify_t          A254, A255
                      finish_unify             
                      proceed                  
  L0                  try                      #1, L7
                      trust                    #1, L8
  L5                  switch_on_constant       #1, <hash #0>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: (i -> i) -> i
  2: i -> i -> i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: abs (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: app (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 1
      Constants:
      abs -> L0
  
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
