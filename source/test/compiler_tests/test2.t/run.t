  $ tjcc fapp
  $ tjdis fapp.lpo
  Disassembling from bytecode file: fapp.lpo
  Bytecode version: 2
  Module name: fapp
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #2
                      put_variable_t           A254, A255
                      put_app                  A254, A255, #2
                      set_m_const              a
                      set_m_const              b
                      pattern_unify_t          A254, A1
                      finish_unify             
                      put_variable_p           Y1, A255
                      globalize_t              A255
                      put_app                  A1, A255, #1
                      set_m_const              a
                      call_name                #1, foo
                      globalize_pt             Y1, A255
                      put_app                  A1, A255, #1
                      set_m_const              b
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  L2                  trust_me                 #1
                      allocate                 #2
                      get_m_structure          A1, appl, #1
                      unify_variable_t         A255
                      put_variable_t           A253, A254
                      put_app                  A253, A254, #2
                      set_m_const              a
                      set_m_const              b
                      pattern_unify_t          A253, A255
                      finish_unify             
                      put_m_const              A255, appl
                      put_variable_p           Y1, A254
                      globalize_t              A254
                      put_app                  A253, A254, #1
                      set_m_const              a
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      call_name                #1, foo
                      put_m_const              A255, appl
                      globalize_pt             Y1, A254
                      put_app                  A253, A254, #1
                      set_m_const              b
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: i -> i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: appl (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
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
  $ tjlink fapp
  $ tjdis fapp.lp
  Disassembling from bytecode file: fapp.lp
  Bytecode version: 3
  Module name: fapp
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #2
                      put_variable_t           A254, A255
                      put_app                  A254, A255, #2
                      set_m_const              a
                      set_m_const              b
                      pattern_unify_t          A254, A1
                      finish_unify             
                      put_variable_p           Y1, A255
                      globalize_t              A255
                      put_app                  A1, A255, #1
                      set_m_const              a
                      call_name                #1, foo
                      globalize_pt             Y1, A255
                      put_app                  A1, A255, #1
                      set_m_const              b
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  L2                  trust_me                 #1
                      allocate                 #2
                      get_m_structure          A1, appl, #1
                      unify_variable_t         A255
                      put_variable_t           A253, A254
                      put_app                  A253, A254, #2
                      set_m_const              a
                      set_m_const              b
                      pattern_unify_t          A253, A255
                      finish_unify             
                      put_m_const              A255, appl
                      put_variable_p           Y1, A254
                      globalize_t              A254
                      put_app                  A253, A254, #1
                      set_m_const              a
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      call_name                #1, foo
                      put_m_const              A255, appl
                      globalize_pt             Y1, A254
                      put_app                  A253, A254, #1
                      set_m_const              b
                      put_app                  A1, A255, #1
                      set_value_t              A253
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: i -> i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: a (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: b (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  3: appl (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
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
