  $ tjcc abstraction
  $ tjdis abstraction.lpo
  Disassembling from bytecode file: abstraction.lpo
  Bytecode version: 2
  Module name: abstraction
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo1                switch_on_reg            #1, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #1
                      try_me_else              #0, L4
  L3                  put_index                A255, #1
                      put_lambda               A254, A255, #1
                      pattern_unify_t          A254, A1
                      finish_unify             
                      put_index                A255, #1
                      put_lambda               A1, A255, #1
                      execute_name             foo1
  foo3                switch_on_reg            #3, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #3
                      try_me_else              #0, L4
  L6                  put_m_const              A255, appl
                      put_app                  A254, A255, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A255, A254, #2
                      pattern_unify_t          A255, A1
                      finish_unify             
                      put_m_const              A254, appl
                      put_app                  A255, A254, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A1, A255, #2
                      execute_name             foo3
  foo2                switch_on_reg            #2, L7, L8
  L7                  try                      #1, L8
                      trust_ext                #1, #2
                      try_me_else              #0, L4
  L8                  switch_on_term           L9, L10, L4, L9
  L9                  try_me_else              #1, L11
  L1                  get_m_structure          A1, abst, #1
                      unify_variable_t         A255
                      put_index                A254, #1
                      put_lambda               A253, A254, #1
                      pattern_unify_t          A253, A255
                      finish_unify             
                      put_m_const              A255, abst
                      put_index                A254, #1
                      put_lambda               A253, A254, #1
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      execute_name             foo2
  L11                 trust_me                 #1
  L0                  get_m_structure          A1, nabst, #1
                      unify_variable_t         A255
                      put_m_const              A254, appl
                      put_app                  A253, A254, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A254, A253, #2
                      pattern_unify_t          A254, A255
                      finish_unify             
                      put_m_const              A255, nabst
                      put_m_const              A254, appl
                      put_app                  A253, A254, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A254, A253, #2
                      put_app                  A1, A255, #1
                      set_value_t              A254
                      execute_name             foo2
  L10                 switch_on_constant       #2, <hash #0>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: (i -> i) -> o
  1: i -> o
  2: (i -> i) -> i
  3: (i -> i -> i) -> o
  4: i -> i -> i
  5: (i -> i -> i) -> i
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: abst (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  3: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #3
  4: appl (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #4
  5: nabst (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #5
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      nabst -> L0
      abst -> L1
  
  Module table:
    Predicate definitions possibly extending previous ones: 3
     foo1
     foo2
     foo3
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 3
     foo1
     foo2
     foo3
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink abstraction
  $ tjdis abstraction.lp
  Disassembling from bytecode file: abstraction.lp
  Bytecode version: 3
  Module name: abstraction
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo1                switch_on_reg            #1, L2, L3
  L2                  try                      #1, L3
                      trust_ext                #1, #1
                      try_me_else              #0, L4
  L3                  put_index                A255, #1
                      put_lambda               A254, A255, #1
                      pattern_unify_t          A254, A1
                      finish_unify             
                      put_index                A255, #1
                      put_lambda               A1, A255, #1
                      execute_name             foo1
  foo3                switch_on_reg            #3, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #3
                      try_me_else              #0, L4
  L6                  put_m_const              A255, appl
                      put_app                  A254, A255, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A255, A254, #2
                      pattern_unify_t          A255, A1
                      finish_unify             
                      put_m_const              A254, appl
                      put_app                  A255, A254, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A1, A255, #2
                      execute_name             foo3
  foo2                switch_on_reg            #2, L7, L8
  L7                  try                      #1, L8
                      trust_ext                #1, #2
                      try_me_else              #0, L4
  L8                  switch_on_term           L9, L10, L4, L9
  L9                  try_me_else              #1, L11
  L1                  get_m_structure          A1, abst, #1
                      unify_variable_t         A255
                      put_index                A254, #1
                      put_lambda               A253, A254, #1
                      pattern_unify_t          A253, A255
                      finish_unify             
                      put_m_const              A255, abst
                      put_index                A254, #1
                      put_lambda               A253, A254, #1
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      execute_name             foo2
  L11                 trust_me                 #1
  L0                  get_m_structure          A1, nabst, #1
                      unify_variable_t         A255
                      put_m_const              A254, appl
                      put_app                  A253, A254, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A254, A253, #2
                      pattern_unify_t          A254, A255
                      finish_unify             
                      put_m_const              A255, nabst
                      put_m_const              A254, appl
                      put_app                  A253, A254, #2
                      set_index                #2
                      set_index                #1
                      put_lambda               A254, A253, #2
                      put_app                  A1, A255, #1
                      set_value_t              A254
                      execute_name             foo2
  L10                 switch_on_constant       #2, <hash #0>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: (i -> i) -> o
  1: i -> o
  2: (i -> i) -> i
  3: (i -> i -> i) -> o
  4: i -> i -> i
  5: (i -> i -> i) -> i
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: abst (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  3: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #3
  4: appl (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #4
  5: nabst (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #5
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      nabst -> L0
      abst -> L1
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 3
     foo1
     foo2
     foo3
      Local constant table: 0
      Find function type: hash
      Search table: 3
     foo1
     foo2
     foo3
  
  Accumulated tables:
  
  Imported tables:
