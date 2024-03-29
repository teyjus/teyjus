  $ tjcc tconst
  $ tjdis tconst.lpo
  Disassembling from bytecode file: tconst.lpo
  Bytecode version: 2
  Module name: tconst
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo                 switch_on_reg            #1, L2, L3
  L2                  try                      #2, L3
                      trust_ext                #2, #1
                      try_me_else              #0, L4
  L3                  switch_on_term           L5, L6, L4, L5
  L5                  try_me_else              #2, L7
  L11                 get_type_arrow           A2
                      unify_type_variable_t    A3
                      unify_type_variable_t    A4
                      get_p_constant           A1, tc1, L8
                      unify_envty_local_value_t A4
                      unify_envty_local_value_t A3
  L8                  finish_unify             
                      put_type_arrow           A2
                      set_type_variable_t      A3
                      set_type_variable_t      A4
                      put_p_const              A1, tc1
                      set_type_local_value_t   A4
                      set_type_local_value_t   A3
                      execute_name             foo
  L7                  retry_me_else            #2, L9
  L0                  get_type_structure       A2, pair
                      unify_type_variable_t    A255
                      unify_type_variable_t    A255
                      get_m_constant           A1, tc2
                      finish_unify             
                      put_type_structure       A2, pair
                      set_type_variable_t      A255
                      set_type_variable_t      A255
                      put_m_const              A1, tc2
                      execute_name             foo
  L9                  retry_me_else            #2, L10
  L12                 get_p_structure          A1, tc1, #1
                      unify_envty_local_value_t A2
                      unify_type_variable_t    A255
                      get_type_structure       A255, pair
                      unify_type_variable_t    A255
                      unify_type_variable_t    A255
                      unify_m_constant         tc2
                      finish_unify             
                      put_type_variable_t      A2, A3
                      put_type_structure       A254, pair
                      set_type_variable_t      A253
                      set_type_variable_t      A253
                      put_p_const              A255, tc1
                      set_type_local_value_t   A3
                      set_type_value_t         A254
                      put_app                  A1, A255, #1
                      set_m_const              tc2
                      execute_name             foo
  L10                 trust_me                 #2
  L13                 get_p_structure          A1, tc1, #1
                      unify_envty_local_value_t A2
                      unify_type_variable_t    A3
                      unify_variable_t         A255
                      get_p_structure          A255, tc1, #1
                      unify_envty_local_value_t A3
                      unify_type_variable_t    A3
                      unify_variable_t         A4
                      finish_unify             
                      put_type_variable_t      A2, A5
                      put_p_const              A255, tc1
                      set_type_local_value_t   A5
                      set_type_variable_t      A5
                      put_p_const              A254, tc1
                      set_type_local_value_t   A5
                      set_type_local_value_t   A3
                      put_app                  A253, A254, #1
                      set_value_t              A4
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      execute_name             foo
  L1                  try                      #2, L11
                      retry                    #2, L12
                      trust                    #2, L13
  L6                  switch_on_constant       #2, <hash #0>
  
  Global kind table:
  0: pair/2
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  1: B -> A
  2: pair A B
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  1: tc1 (No Fixity, precedence 0)
      Env Size: 2, Type Skeleton: #1
  2: tc2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      tc2 -> L0
      tc1 -> L1
  
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
  $ tjlink tconst
  $ tjdis tconst.lp
  Disassembling from bytecode file: tconst.lp
  Bytecode version: 3
  Module name: tconst
  
  LABEL               INSTRUCTION              OPERANDS
  
  L4                  fail                     
  foo                 switch_on_reg            #1, L2, L3
  L2                  try                      #2, L3
                      trust_ext                #2, #1
                      try_me_else              #0, L4
  L3                  switch_on_term           L5, L6, L4, L5
  L5                  try_me_else              #2, L7
  L11                 get_type_arrow           A2
                      unify_type_variable_t    A3
                      unify_type_variable_t    A4
                      get_p_constant           A1, tc1, L8
                      unify_envty_local_value_t A4
                      unify_envty_local_value_t A3
  L8                  finish_unify             
                      put_type_arrow           A2
                      set_type_variable_t      A3
                      set_type_variable_t      A4
                      put_p_const              A1, tc1
                      set_type_local_value_t   A4
                      set_type_local_value_t   A3
                      execute_name             foo
  L7                  retry_me_else            #2, L9
  L0                  get_type_structure       A2, pair
                      unify_type_variable_t    A255
                      unify_type_variable_t    A255
                      get_m_constant           A1, tc2
                      finish_unify             
                      put_type_structure       A2, pair
                      set_type_variable_t      A255
                      set_type_variable_t      A255
                      put_m_const              A1, tc2
                      execute_name             foo
  L9                  retry_me_else            #2, L10
  L12                 get_p_structure          A1, tc1, #1
                      unify_envty_local_value_t A2
                      unify_type_variable_t    A255
                      get_type_structure       A255, pair
                      unify_type_variable_t    A255
                      unify_type_variable_t    A255
                      unify_m_constant         tc2
                      finish_unify             
                      put_type_variable_t      A2, A3
                      put_type_structure       A254, pair
                      set_type_variable_t      A253
                      set_type_variable_t      A253
                      put_p_const              A255, tc1
                      set_type_local_value_t   A3
                      set_type_value_t         A254
                      put_app                  A1, A255, #1
                      set_m_const              tc2
                      execute_name             foo
  L10                 trust_me                 #2
  L13                 get_p_structure          A1, tc1, #1
                      unify_envty_local_value_t A2
                      unify_type_variable_t    A3
                      unify_variable_t         A255
                      get_p_structure          A255, tc1, #1
                      unify_envty_local_value_t A3
                      unify_type_variable_t    A3
                      unify_variable_t         A4
                      finish_unify             
                      put_type_variable_t      A2, A5
                      put_p_const              A255, tc1
                      set_type_local_value_t   A5
                      set_type_variable_t      A5
                      put_p_const              A254, tc1
                      set_type_local_value_t   A5
                      set_type_local_value_t   A3
                      put_app                  A253, A254, #1
                      set_value_t              A4
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      execute_name             foo
  L1                  try                      #2, L11
                      retry                    #2, L12
                      trust                    #2, L13
  L6                  switch_on_constant       #2, <hash #0>
  
  Global kind table:
  0: pair/2
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  1: B -> A
  2: pair A B
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  1: tc1 (No Fixity, precedence 0)
      Env Size: 2, Type Skeleton: #1
  2: tc2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 2
      Constants:
      tc2 -> L0
      tc1 -> L1
  
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
