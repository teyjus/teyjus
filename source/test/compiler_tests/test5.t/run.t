  $ tjcc genericgoal
  $ tjdis genericgoal.lpo
  Disassembling from bytecode file: genericgoal.lpo
  Bytecode version: 2
  Module name: genericgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      allocate                 #3
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      set_univ_tag             Y2, <hidden const #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #2, L3
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             <hidden const #2>
  L3                  trust_me                 #2
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             <hidden const #3>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> o
  1: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #1
  1:  Type Skeleton: #1
  2:  Type Skeleton: #0
  3:  Type Skeleton: #0
  
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
  $ tjlink genericgoal
  $ tjdis genericgoal.lp
  Disassembling from bytecode file: genericgoal.lp
  Bytecode version: 3
  Module name: genericgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      allocate                 #3
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      set_univ_tag             Y2, <hidden const #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #2, L3
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             <hidden const #2>
  L3                  trust_me                 #2
                      head_normalize_t         A1
                      head_normalize_t         A2
                      execute_name             <hidden const #3>
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> i -> o
  1: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #1
  1:  Type Skeleton: #1
  2:  Type Skeleton: #0
  3:  Type Skeleton: #0
  
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
