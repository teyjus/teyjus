  $ tjcc existentialvars
  $ tjdis existentialvars.lpo
  Disassembling from bytecode file: existentialvars.lpo
  Bytecode version: 2
  Module name: existentialvars
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      tag_exists_t             A1
                      head_normalize_t         A1
                      execute_name             foo
  L2                  retry_me_else            #1, L3
                      tag_exists_t             A1
                      head_normalize_t         A1
                      execute_name             foo
  L3                  retry_me_else            #1, L4
                      head_normalize_t         A1
                      execute_name             foo
  L4                  trust_me                 #1
                      allocate                 #3
                      get_variable_p           Y1, A1
                      tag_exists_p             Y2
                      put_value_p              Y2, A1
                      head_normalize_t         A1
                      call_name                #2, foo
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
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
  $ tjlink existentialvars
  $ tjdis existentialvars.lp
  Disassembling from bytecode file: existentialvars.lp
  Bytecode version: 3
  Module name: existentialvars
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      tag_exists_t             A1
                      head_normalize_t         A1
                      execute_name             foo
  L2                  retry_me_else            #1, L3
                      tag_exists_t             A1
                      head_normalize_t         A1
                      execute_name             foo
  L3                  retry_me_else            #1, L4
                      head_normalize_t         A1
                      execute_name             foo
  L4                  trust_me                 #1
                      allocate                 #3
                      get_variable_p           Y1, A1
                      tag_exists_p             Y2
                      put_value_p              Y2, A1
                      head_normalize_t         A1
                      call_name                #2, foo
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
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
