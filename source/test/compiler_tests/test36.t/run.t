  $ tjcc bug
  $ tjdis bug.lpo
  Disassembling from bytecode file: bug.lpo
  Bytecode version: 2
  Module name: bug
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  nabla               switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #0>
                      put_value_t              A1, A2
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      put_variable_t           A2, A255
                      put_app                  A254, A255, #1
                      set_index                #1
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #1, L4
                      allocate                 #2
                      put_variable_t           A2, A255
                      put_app                  A254, A255, #1
                      set_index                #1
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #2>
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  L4                  trust_me                 #1
                      allocate                 #2
                      put_variable_t           A2, A255
                      put_app                  A254, A255, #1
                      set_index                #1
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #3>
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  
  Global kind table:
  0: n/0
  
  Local kind table:
  
  Type skeleton table:
  0: (n -> o) -> o
  1: n
  
  Global constant table: 
  0: nabla (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #1
  1:  Type Skeleton: #1
  2:  Type Skeleton: #1
  3:  Type Skeleton: #1
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     nabla
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 1
     nabla
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink bug
  $ tjdis bug.lp
  Disassembling from bytecode file: bug.lp
  Bytecode version: 3
  Module name: bug
  
  LABEL               INSTRUCTION              OPERANDS
  
                      fail                     
  nabla               switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #0>
                      put_value_t              A1, A2
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      put_variable_t           A2, A255
                      put_app                  A254, A255, #1
                      set_index                #1
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #1, L4
                      allocate                 #2
                      put_variable_t           A2, A255
                      put_app                  A254, A255, #1
                      set_index                #1
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #2>
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  L4                  trust_me                 #1
                      allocate                 #2
                      put_variable_t           A2, A255
                      put_app                  A254, A255, #1
                      set_index                #1
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #3>
                      globalize_t              A2
                      put_app                  A1, A2, #1
                      globalize_pt             Y1, A255
                      set_value_t              A255
                      head_normalize_t         A1
                      call_builtin             #1, #0
                      decr_universe            
                      deallocate               
                      proceed                  
  
  Global kind table:
  0: n/0
  
  Local kind table:
  
  Type skeleton table:
  0: (n -> o) -> o
  1: n
  
  Global constant table: 
  0: nabla (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #1
  1:  Type Skeleton: #1
  2:  Type Skeleton: #1
  3:  Type Skeleton: #1
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 1
     nabla
      Local constant table: 0
      Find function type: hash
      Search table: 1
     nabla
  
  Accumulated tables:
  
  Imported tables:
