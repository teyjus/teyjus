  $ tjcc clauses
  $ tjdis clauses.lpo
  Disassembling from bytecode file: clauses.lpo
  Bytecode version: 2
  Module name: clauses
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      proceed                  
  L2                  retry_me_else            #2, L3
                      put_value_t              A1, A3
                      put_value_t              A2, A4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L3                  trust_me                 #2
                      allocate                 #3
                      head_normalize_t         A1
                      put_variable_p           Y1, A2
                      call_name                #2, foo
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_variable_p           Y2, A2
                      call_name                #2, foo
                      put_unsafe_value         Y2, A1
                      head_normalize_t         A1
                      put_variable_t           A255, A2
                      deallocate               
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
  $ tjlink clauses
  $ tjdis clauses.lp
  Disassembling from bytecode file: clauses.lp
  Bytecode version: 3
  Module name: clauses
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      proceed                  
  L2                  retry_me_else            #2, L3
                      put_value_t              A1, A3
                      put_value_t              A2, A4
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A3, A2
                      head_normalize_t         A2
                      execute_name             foo
  L3                  trust_me                 #2
                      allocate                 #3
                      head_normalize_t         A1
                      put_variable_p           Y1, A2
                      call_name                #2, foo
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_variable_p           Y2, A2
                      call_name                #2, foo
                      put_unsafe_value         Y2, A1
                      head_normalize_t         A1
                      put_variable_t           A255, A2
                      deallocate               
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
