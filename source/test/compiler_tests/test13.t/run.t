  $ tjcc universalvars
  $ tjdis universalvars.lpo
  Disassembling from bytecode file: universalvars.lpo
  Bytecode version: 2
  Module name: universalvars
  
  LABEL               INSTRUCTION              OPERANDS
  
  L7                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #3
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #0>
                      push_impl_point          #2, <impl #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      push_impl_point          #1, <impl #1>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, bar
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #1, L4
                      allocate                 #3
                      get_variable_p           Y1, A1
                      tag_variable             Y2
                      push_impl_point          #2, <impl #2>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L4                  trust_me                 #1
                      allocate                 #1
                      push_impl_point          #0, <impl #3>
                      head_normalize_t         A1
                      call_name                #0, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  bar                 switch_on_reg            #1, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L6                  head_normalize_t         A1
                      execute_name             foo
  bar                 switch_on_reg            #1, L8, L9
  L8                  try                      #1, L9
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L9                  init_variable_t          A2, Y1
                      init_variable_t          A3, Y2
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      execute_name             bar
  bar                 switch_on_reg            #1, L10, L11
  L10                 try                      #1, L11
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L11                 init_variable_t          A2, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A2, A1
                      head_normalize_t         A1
                      execute_name             foo
  bar                 switch_on_reg            #1, L12, L13
  L12                 try                      #1, L13
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L13                 init_variable_t          A2, Y2
                      init_variable_t          A3, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: A
  2: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #2
  1:  Type Skeleton: #2
  2:  Type Skeleton: #1
  
  String table:
  
  Implication Tables:
  0:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  1:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  2:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  3:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  
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
  
  L7                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #3
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #0>
                      push_impl_point          #2, <impl #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      incr_universe            
                      set_univ_tag             Y1, <hidden const #1>
                      push_impl_point          #1, <impl #1>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, bar
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #1, L4
                      allocate                 #3
                      get_variable_p           Y1, A1
                      tag_variable             Y2
                      push_impl_point          #2, <impl #2>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L4                  trust_me                 #1
                      allocate                 #1
                      push_impl_point          #0, <impl #3>
                      head_normalize_t         A1
                      call_name                #0, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  bar                 switch_on_reg            #1, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L6                  head_normalize_t         A1
                      execute_name             foo
  bar                 switch_on_reg            #1, L8, L9
  L8                  try                      #1, L9
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L9                  init_variable_t          A2, Y1
                      init_variable_t          A3, Y2
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      execute_name             bar
  bar                 switch_on_reg            #1, L10, L11
  L10                 try                      #1, L11
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L11                 init_variable_t          A2, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A2, A1
                      head_normalize_t         A1
                      execute_name             foo
  bar                 switch_on_reg            #1, L12, L13
  L12                 try                      #1, L13
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L13                 init_variable_t          A2, Y2
                      init_variable_t          A3, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: A
  2: i
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #2
  1:  Type Skeleton: #2
  2:  Type Skeleton: #1
  
  String table:
  
  Implication Tables:
  0:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  1:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  2:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  3:
    Predicate definitions possibly extending previous ones: 1
     bar
    Find function type: hash
    In-core table size: 1
     bar
  
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
