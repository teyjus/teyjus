  $ tjcc index
  $ tjdis index.lpo
  Disassembling from bytecode file: index.lpo
  Bytecode version: 2
  Module name: index
  
  LABEL               INSTRUCTION              OPERANDS
  
  L5                  fail                     
  foo1                switch_on_reg            #1, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #1
                      try_me_else              #0, L5
  L4                  head_normalize_t         A1
                      execute_name             foo1
  foo2                switch_on_reg            #2, L6, L7
  L6                  try                      #1, L7
                      trust_ext                #1, #2
  L7                  try_me_else              #1, L8
                      head_normalize_t         A1
                      execute_name             foo2
  L8                  trust_me                 #1
                      get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo2
  foo3                switch_on_reg            #3, L9, L10
  L9                  try                      #1, L10
                      trust_ext                #1, #3
  L10                 try_me_else              #1, L11
                      head_normalize_t         A1
                      execute_name             foo3
  L11                 retry_me_else            #1, L12
                      get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo3
  L12                 retry_me_else            #1, L13
                      put_m_const              A1, a
                      execute_name             foo3
  L13                 trust_me                 #1
                      get_m_constant           A1, b
                      finish_unify             
                      put_variable_t           A255, A1
                      execute_name             foo3
  foo5                switch_on_reg            #4, L14, L15
  L14                 try                      #1, L15
                      trust_ext                #1, #4
                      try_me_else              #0, L5
  L15                 switch_on_term           L16, L17, L18, L16
  L16                 try_me_else              #1, L19
  L21                 get_list                 A1
                      unify_m_constant         a
                      unify_variable_t         A1
                      finish_unify             
                      head_normalize_t         A1
                      execute_name             foo5
  L19                 retry_me_else            #1, L20
  L22                 get_list                 A1
                      unify_variable_t         A255
                      unify_variable_t         A1
                      finish_unify             
                      head_normalize_t         A1
                      execute_name             foo5
  L20                 trust_me                 #1
  L0                  get_nil                  A1
                      finish_unify             
                      proceed                  
  L17                 switch_on_constant       #1, <hash #0>
  L18                 try                      #1, L21
                      trust                    #1, L22
  foo6                switch_on_reg            #5, L23, L24
  L23                 try                      #1, L24
                      trust_ext                #1, #5
  L24                 try_me_else              #1, L25
                      get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo6
  L25                 retry_me_else            #1, L26
                      put_m_const              A1, a
                      execute_name             foo6
  L26                 trust_me                 #1
                      switch_on_term           L27, L28, L5, L27
  L27                 try_me_else              #1, L29
  L34                 get_m_constant           A1, b
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo6
  L29                 retry_me_else            #1, L30
  L32                 get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo6
  L30                 retry_me_else            #1, L31
  L35                 get_m_constant           A1, b
                      finish_unify             
                      put_m_const              A1, b
                      execute_name             foo6
  L31                 trust_me                 #1
  L33                 get_m_constant           A1, a
                      finish_unify             
                      put_variable_t           A255, A1
                      execute_name             foo6
  L1                  try                      #1, L32
                      trust                    #1, L33
  L2                  try                      #1, L34
                      trust                    #1, L35
  L28                 switch_on_constant       #2, <hash #1>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: (list i) -> o
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  4: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  5: foo5 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  6: foo6 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  7: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 1
      Constants:
      nil -> L0
  1:
      Table size: 2
      Constants:
      a -> L1
      b -> L2
  
  Module table:
    Predicate definitions possibly extending previous ones: 5
     foo1
     foo2
     foo3
     foo5
     foo6
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 5
     foo1
     foo2
     foo3
     foo5
     foo6
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink index
  $ tjdis index.lp
  Disassembling from bytecode file: index.lp
  Bytecode version: 3
  Module name: index
  
  LABEL               INSTRUCTION              OPERANDS
  
  L5                  fail                     
  foo1                switch_on_reg            #1, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #1
                      try_me_else              #0, L5
  L4                  head_normalize_t         A1
                      execute_name             foo1
  foo2                switch_on_reg            #2, L6, L7
  L6                  try                      #1, L7
                      trust_ext                #1, #2
  L7                  try_me_else              #1, L8
                      head_normalize_t         A1
                      execute_name             foo2
  L8                  trust_me                 #1
                      get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo2
  foo3                switch_on_reg            #3, L9, L10
  L9                  try                      #1, L10
                      trust_ext                #1, #3
  L10                 try_me_else              #1, L11
                      head_normalize_t         A1
                      execute_name             foo3
  L11                 retry_me_else            #1, L12
                      get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo3
  L12                 retry_me_else            #1, L13
                      put_m_const              A1, a
                      execute_name             foo3
  L13                 trust_me                 #1
                      get_m_constant           A1, b
                      finish_unify             
                      put_variable_t           A255, A1
                      execute_name             foo3
  foo5                switch_on_reg            #4, L14, L15
  L14                 try                      #1, L15
                      trust_ext                #1, #4
                      try_me_else              #0, L5
  L15                 switch_on_term           L16, L17, L18, L16
  L16                 try_me_else              #1, L19
  L21                 get_list                 A1
                      unify_m_constant         a
                      unify_variable_t         A1
                      finish_unify             
                      head_normalize_t         A1
                      execute_name             foo5
  L19                 retry_me_else            #1, L20
  L22                 get_list                 A1
                      unify_variable_t         A255
                      unify_variable_t         A1
                      finish_unify             
                      head_normalize_t         A1
                      execute_name             foo5
  L20                 trust_me                 #1
  L0                  get_nil                  A1
                      finish_unify             
                      proceed                  
  L17                 switch_on_constant       #1, <hash #0>
  L18                 try                      #1, L21
                      trust                    #1, L22
  foo6                switch_on_reg            #5, L23, L24
  L23                 try                      #1, L24
                      trust_ext                #1, #5
  L24                 try_me_else              #1, L25
                      get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo6
  L25                 retry_me_else            #1, L26
                      put_m_const              A1, a
                      execute_name             foo6
  L26                 trust_me                 #1
                      switch_on_term           L27, L28, L5, L27
  L27                 try_me_else              #1, L29
  L34                 get_m_constant           A1, b
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo6
  L29                 retry_me_else            #1, L30
  L32                 get_m_constant           A1, a
                      finish_unify             
                      put_m_const              A1, a
                      execute_name             foo6
  L30                 retry_me_else            #1, L31
  L35                 get_m_constant           A1, b
                      finish_unify             
                      put_m_const              A1, b
                      execute_name             foo6
  L31                 trust_me                 #1
  L33                 get_m_constant           A1, a
                      finish_unify             
                      put_variable_t           A255, A1
                      execute_name             foo6
  L1                  try                      #1, L32
                      trust                    #1, L33
  L2                  try                      #1, L34
                      trust                    #1, L35
  L28                 switch_on_constant       #2, <hash #1>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: (list i) -> o
  
  Global constant table: 
  0: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  2: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: foo3 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  4: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  5: foo5 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  6: foo6 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  7: foo4 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 1
      Constants:
      nil -> L0
  1:
      Table size: 2
      Constants:
      a -> L1
      b -> L2
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 5
     foo1
     foo2
     foo3
     foo5
     foo6
      Local constant table: 0
      Find function type: hash
      Search table: 5
     foo1
     foo2
     foo3
     foo5
     foo6
  
  Accumulated tables:
  
  Imported tables:
