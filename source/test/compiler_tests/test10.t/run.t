  $ tjcc cutgoal
  $ tjdis cutgoal.lpo
  Disassembling from bytecode file: cutgoal.lpo
  Bytecode version: 2
  Module name: cutgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      neck_cut                 
                      head_normalize_t         A1
                      execute_name             bar
  L2                  retry_me_else            #1, L3
                      allocate                 #3
                      get_variable_p           Y1, A1
                      get_level                Y2
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      cut                      Y2
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             bar
  L3                  trust_me                 #1
                      neck_cut                 
                      fail                     
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
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
  $ tjlink cutgoal
  $ tjdis cutgoal.lp
  Disassembling from bytecode file: cutgoal.lp
  Bytecode version: 3
  Module name: cutgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      neck_cut                 
                      head_normalize_t         A1
                      execute_name             bar
  L2                  retry_me_else            #1, L3
                      allocate                 #3
                      get_variable_p           Y1, A1
                      get_level                Y2
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      cut                      Y2
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             bar
  L3                  trust_me                 #1
                      neck_cut                 
                      fail                     
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
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
