  $ tjcc foo
  $ tjdis foo.lpo
  Disassembling from bytecode file: foo.lpo
  Bytecode version: 2
  Module name: foo
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  nth                 switch_on_reg            #1, L0, L1
  L0                  try                      #4, L1
                      trust_ext                #4, #1
                      try_me_else              #0, L2
  L1                  allocate                 #6
                      get_type_variable_p      Y1, A4
                      get_variable_p           Y2, A1
                      get_variable_p           Y3, A2
                      get_list                 A3
                      unify_variable_t         A2
                      unify_variable_p         Y4
                      finish_unify             
                      put_p_const              A255, =
                      set_type_local_value_p   Y1
                      put_app                  A1, A255, #2
                      globalize_pt             Y3, A255
                      set_value_t              A255
                      set_value_t              A2
                      call_builtin             #5, #2
                      put_type_const           A3, int
                      put_variable_p           Y5, A1
                      put_m_const              A255, %i-
                      put_app                  A2, A255, #2
                      globalize_pt             Y2, A255
                      set_value_t              A255
                      set_integer              1
                      call_builtin             #5, #1
                      put_type_value_p         Y1, A4
                      put_unsafe_value         Y5, A1
                      head_normalize_t         A1
                      put_value_p              Y3, A2
                      head_normalize_t         A2
                      put_value_p              Y4, A3
                      head_normalize_t         A3
                      deallocate               
                      execute_name             nth
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> A -> (list A) -> o
  
  Global constant table: 
  0: nth (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     nth
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 1
     nth
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink foo
  $ tjdis foo.lp
  Disassembling from bytecode file: foo.lp
  Bytecode version: 3
  Module name: foo
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  nth                 switch_on_reg            #1, L0, L1
  L0                  try                      #4, L1
                      trust_ext                #4, #1
                      try_me_else              #0, L2
  L1                  allocate                 #6
                      get_type_variable_p      Y1, A4
                      get_variable_p           Y2, A1
                      get_variable_p           Y3, A2
                      get_list                 A3
                      unify_variable_t         A2
                      unify_variable_p         Y4
                      finish_unify             
                      put_p_const              A255, =
                      set_type_local_value_p   Y1
                      put_app                  A1, A255, #2
                      globalize_pt             Y3, A255
                      set_value_t              A255
                      set_value_t              A2
                      call_builtin             #5, #2
                      put_type_const           A3, int
                      put_variable_p           Y5, A1
                      put_m_const              A255, %i-
                      put_app                  A2, A255, #2
                      globalize_pt             Y2, A255
                      set_value_t              A255
                      set_integer              1
                      call_builtin             #5, #1
                      put_type_value_p         Y1, A4
                      put_unsafe_value         Y5, A1
                      head_normalize_t         A1
                      put_value_p              Y3, A2
                      head_normalize_t         A2
                      put_value_p              Y4, A3
                      head_normalize_t         A3
                      deallocate               
                      execute_name             nth
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> A -> (list A) -> o
  
  Global constant table: 
  0: nth (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 1
     nth
      Local constant table: 0
      Find function type: hash
      Search table: 1
     nth
  
  Accumulated tables:
  
  Imported tables:
