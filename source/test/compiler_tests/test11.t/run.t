  $ tjcc vars
  $ tjdis vars.lpo
  Disassembling from bytecode file: vars.lpo
  Bytecode version: 2
  Module name: vars
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #3, L1
                      trust_ext                #3, #1
                      try_me_else              #0, L2
  L1                  allocate                 #4
                      get_variable_p           Y1, A2
                      get_variable_p           Y2, A3
                      head_normalize_t         A1
                      put_value_p              Y1, A2
                      head_normalize_t         A2
                      put_variable_p           Y3, A3
                      call_name                #3, foo
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      put_unsafe_value         Y3, A3
                      head_normalize_t         A3
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> i -> o
  
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
  $ tjlink vars
  $ tjdis vars.lp
  Disassembling from bytecode file: vars.lp
  Bytecode version: 3
  Module name: vars
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #3, L1
                      trust_ext                #3, #1
                      try_me_else              #0, L2
  L1                  allocate                 #4
                      get_variable_p           Y1, A2
                      get_variable_p           Y2, A3
                      head_normalize_t         A1
                      put_value_p              Y1, A2
                      head_normalize_t         A2
                      put_variable_p           Y3, A3
                      call_name                #3, foo
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      put_unsafe_value         Y3, A3
                      head_normalize_t         A3
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> i -> o
  
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
