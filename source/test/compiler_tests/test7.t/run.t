  $ tjcc flexgoal
  $ tjdis flexgoal.lpo
  Disassembling from bytecode file: flexgoal.lpo
  Bytecode version: 2
  Module name: flexgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      put_value_t              A1, A3
                      put_variable_t           A254, A255
                      put_app                  A1, A255, #2
                      globalize_t              A3
                      set_value_t              A3
                      globalize_t              A2
                      set_value_t              A2
                      builtin                  #0
  L2                  retry_me_else            #2, L3
                      put_value_t              A1, A3
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_t              A3
                      set_value_t              A3
                      head_normalize_t         A1
                      builtin                  #0
  L3                  trust_me                 #2
                      allocate                 #3
                      get_variable_p           Y1, A1
                      put_variable_p           Y2, A255
                      globalize_t              A255
                      put_app                  A1, A255, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      call_builtin             #2, #0
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_unsafe_value         Y2, A2
                      head_normalize_t         A2
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> (i -> o) -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
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
  $ tjlink flexgoal
  $ tjdis flexgoal.lp
  Disassembling from bytecode file: flexgoal.lp
  Bytecode version: 3
  Module name: flexgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      put_value_t              A1, A3
                      put_variable_t           A254, A255
                      put_app                  A1, A255, #2
                      globalize_t              A3
                      set_value_t              A3
                      globalize_t              A2
                      set_value_t              A2
                      builtin                  #0
  L2                  retry_me_else            #2, L3
                      put_value_t              A1, A3
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_t              A3
                      set_value_t              A3
                      head_normalize_t         A1
                      builtin                  #0
  L3                  trust_me                 #2
                      allocate                 #3
                      get_variable_p           Y1, A1
                      put_variable_p           Y2, A255
                      globalize_t              A255
                      put_app                  A1, A255, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      call_builtin             #2, #0
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_unsafe_value         Y2, A2
                      head_normalize_t         A2
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> (i -> o) -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
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
