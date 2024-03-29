  $ tjcc disjgoal
  $ tjdis disjgoal.lpo
  Disassembling from bytecode file: disjgoal.lpo
  Bytecode version: 2
  Module name: disjgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      put_value_t              A2, A3
                      head_normalize_t         A1
                      put_variable_t           A255, A2
                      head_normalize_t         A3
                      execute                  L3
  L2                  retry_me_else            #2, L4
                      allocate                 #5
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      put_variable_p           Y3, A1
                      put_variable_p           Y4, A2
                      call_name                #4, foo
                      put_unsafe_value         Y3, A1
                      head_normalize_t         A1
                      put_value_p              Y1, A2
                      head_normalize_t         A2
                      put_unsafe_value         Y4, A3
                      head_normalize_t         A3
                      put_value_p              Y2, A4
                      head_normalize_t         A4
                      deallocate               
                      execute                  L5
  L4                  trust_me                 #2
                      allocate                 #7
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      put_variable_p           Y5, A1
                      put_variable_p           Y6, A2
                      call_name                #6, foo
                      put_variable_p           Y3, A1
                      put_value_p              Y1, A2
                      head_normalize_t         A2
                      put_variable_p           Y4, A3
                      put_value_p              Y2, A4
                      head_normalize_t         A4
                      call                     #4, L6
                      put_unsafe_value         Y3, A1
                      head_normalize_t         A1
                      put_unsafe_value         Y4, A2
                      head_normalize_t         A2
                      deallocate               
                      execute_name             foo
  L6                  try_me_else              #4, L7
                      put_value_t              A1, A3
                      put_value_t              A2, A4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L7                  trust_me                 #4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L5                  try_me_else              #4, L8
                      put_value_t              A1, A3
                      put_value_t              A2, A4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L8                  trust_me                 #4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L3                  try_me_else              #3, L9
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             foo
  L9                  trust_me                 #3
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> o
  
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
  $ tjlink disjgoal
  $ tjdis disjgoal.lp
  Disassembling from bytecode file: disjgoal.lp
  Bytecode version: 3
  Module name: disjgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      put_value_t              A2, A3
                      head_normalize_t         A1
                      put_variable_t           A255, A2
                      head_normalize_t         A3
                      execute                  L3
  L2                  retry_me_else            #2, L4
                      allocate                 #5
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      put_variable_p           Y3, A1
                      put_variable_p           Y4, A2
                      call_name                #4, foo
                      put_unsafe_value         Y3, A1
                      head_normalize_t         A1
                      put_value_p              Y1, A2
                      head_normalize_t         A2
                      put_unsafe_value         Y4, A3
                      head_normalize_t         A3
                      put_value_p              Y2, A4
                      head_normalize_t         A4
                      deallocate               
                      execute                  L5
  L4                  trust_me                 #2
                      allocate                 #7
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      put_variable_p           Y5, A1
                      put_variable_p           Y6, A2
                      call_name                #6, foo
                      put_variable_p           Y3, A1
                      put_value_p              Y1, A2
                      head_normalize_t         A2
                      put_variable_p           Y4, A3
                      put_value_p              Y2, A4
                      head_normalize_t         A4
                      call                     #4, L6
                      put_unsafe_value         Y3, A1
                      head_normalize_t         A1
                      put_unsafe_value         Y4, A2
                      head_normalize_t         A2
                      deallocate               
                      execute_name             foo
  L6                  try_me_else              #4, L7
                      put_value_t              A1, A3
                      put_value_t              A2, A4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L7                  trust_me                 #4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L5                  try_me_else              #4, L8
                      put_value_t              A1, A3
                      put_value_t              A2, A4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L8                  trust_me                 #4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L3                  try_me_else              #3, L9
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             foo
  L9                  trust_me                 #3
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> o
  
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
