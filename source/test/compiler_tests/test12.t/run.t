  $ tjcc universalvars
  $ tjdis universalvars.lpo
  Disassembling from bytecode file: universalvars.lpo
  Bytecode version: 2
  Module name: universalvars
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, foo
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, foo
                      decr_universe            
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #1, L4
                      head_normalize_t         A1
                      execute_name             foo
  L4                  trust_me                 #1
                      allocate                 #3
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #3>
                      put_value_p              Y2, A1
                      head_normalize_t         A1
                      call_name                #2, foo
                      decr_universe            
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: A
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #1
  1:  Type Skeleton: #1
  2:  Type Skeleton: #2
  3:  Type Skeleton: #1
  
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
  $ tjlink universalvars
  $ tjdis universalvars.lp
  Disassembling from bytecode file: universalvars.lp
  Bytecode version: 3
  Module name: universalvars
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, foo
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, foo
                      decr_universe            
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #1, L4
                      head_normalize_t         A1
                      execute_name             foo
  L4                  trust_me                 #1
                      allocate                 #3
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #3>
                      put_value_p              Y2, A1
                      head_normalize_t         A1
                      call_name                #2, foo
                      decr_universe            
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i
  2: A
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #1
  1:  Type Skeleton: #1
  2:  Type Skeleton: #2
  3:  Type Skeleton: #1
  
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
