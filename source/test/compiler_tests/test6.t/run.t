  $ tjcc augmentedgoal
  $ tjdis augmentedgoal.lpo
  Disassembling from bytecode file: augmentedgoal.lpo
  Bytecode version: 2
  Module name: augmentedgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
  L10                 fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      allocate                 #3
                      get_variable_p           Y1, A2
                      tag_variable             Y2
                      push_impl_point          #2, <impl #0>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #2, L3
                      allocate                 #3
                      get_variable_p           Y1, A2
                      tag_variable             Y2
                      push_impl_point          #2, <impl #1>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #2, L4
                      allocate                 #4
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      tag_variable             Y3
                      push_impl_point          #3, <impl #2>
                      put_value_p              Y3, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #3, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L4                  retry_me_else            #2, L5
                      allocate                 #3
                      get_variable_p           Y1, A2
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #0>
                      push_impl_point          #2, <impl #3>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L5                  retry_me_else            #2, L6
                      allocate                 #3
                      get_variable_p           Y1, A2
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #1>
                      push_impl_point          #2, <impl #4>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L6                  retry_me_else            #2, L7
                      allocate                 #4
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      tag_variable             Y3
                      push_impl_point          #3, <impl #5>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #3, <hidden const #2>
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L7                  trust_me                 #2
                      allocate                 #6
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      put_variable_p           Y3, A1
                      put_variable_p           Y5, A2
                      call_name                #5, foo
                      tag_variable             Y4
                      push_impl_point          #4, <impl #6>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #4, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  foo                 switch_on_reg            #1, L8, L9
  L8                  try                      #2, L9
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L9                  init_variable_t          A3, Y3
                      init_variable_t          A4, Y4
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      proceed                  
  <hidden const #2>   try_me_else              #2, L11
                      init_variable_t          A3, Y1
                      init_variable_t          A4, Y2
                      init_variable_t          A5, Y3
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A5, A2
                      head_normalize_t         A2
                      execute_name             foo
  L11                 trust_me                 #2
                      init_variable_t          A3, Y1
                      init_variable_t          A4, Y3
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L12, L13
  L12                 try                      #2, L13
                      trust_ext                #2, #1
  L13                 try_me_else              #2, L14
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  L14                 trust_me                 #2
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L15, L16
  L15                 try                      #2, L16
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L16                 init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      proceed                  
  foo                 switch_on_reg            #1, L17, L18
  L17                 try                      #2, L18
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L18                 init_variable_t          A3, Y1
                      init_variable_t          A4, Y3
                      init_variable_t          A5, Y2
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A5, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L19, L20
  L19                 try                      #2, L20
                      trust_ext                #2, #1
  L20                 try_me_else              #2, L21
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  L21                 trust_me                 #2
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L22, L23
  L22                 try                      #2, L23
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L23                 init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      proceed                  
  
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
  
  String table:
  
  Implication Tables:
  0:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  1:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  2:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  3:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  4:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  5:
    Predicate definitions possibly extending previous ones: 0
    Find function type: hash
    In-core table size: 1
     <hidden const #2>
  6:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  
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
  $ tjlink augmentedgoal
  $ tjdis augmentedgoal.lp
  Disassembling from bytecode file: augmentedgoal.lp
  Bytecode version: 3
  Module name: augmentedgoal
  
  LABEL               INSTRUCTION              OPERANDS
  
  L10                 fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
  L1                  try_me_else              #2, L2
                      allocate                 #3
                      get_variable_p           Y1, A2
                      tag_variable             Y2
                      push_impl_point          #2, <impl #0>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #2, L3
                      allocate                 #3
                      get_variable_p           Y1, A2
                      tag_variable             Y2
                      push_impl_point          #2, <impl #1>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L3                  retry_me_else            #2, L4
                      allocate                 #4
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      tag_variable             Y3
                      push_impl_point          #3, <impl #2>
                      put_value_p              Y3, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #3, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L4                  retry_me_else            #2, L5
                      allocate                 #3
                      get_variable_p           Y1, A2
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #0>
                      push_impl_point          #2, <impl #3>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L5                  retry_me_else            #2, L6
                      allocate                 #3
                      get_variable_p           Y1, A2
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #1>
                      push_impl_point          #2, <impl #4>
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #2, foo
                      pop_impl_point           
                      decr_universe            
                      deallocate               
                      proceed                  
  L6                  retry_me_else            #2, L7
                      allocate                 #4
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      tag_variable             Y3
                      push_impl_point          #3, <impl #5>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #3, <hidden const #2>
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L7                  trust_me                 #2
                      allocate                 #6
                      get_variable_p           Y1, A1
                      get_variable_p           Y2, A2
                      put_variable_p           Y3, A1
                      put_variable_p           Y5, A2
                      call_name                #5, foo
                      tag_variable             Y4
                      push_impl_point          #4, <impl #6>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      put_value_p              Y2, A2
                      head_normalize_t         A2
                      call_name                #4, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  foo                 switch_on_reg            #1, L8, L9
  L8                  try                      #2, L9
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L9                  init_variable_t          A3, Y3
                      init_variable_t          A4, Y4
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      proceed                  
  <hidden const #2>   try_me_else              #2, L11
                      init_variable_t          A3, Y1
                      init_variable_t          A4, Y2
                      init_variable_t          A5, Y3
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A5, A2
                      head_normalize_t         A2
                      execute_name             foo
  L11                 trust_me                 #2
                      init_variable_t          A3, Y1
                      init_variable_t          A4, Y3
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L12, L13
  L12                 try                      #2, L13
                      trust_ext                #2, #1
  L13                 try_me_else              #2, L14
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  L14                 trust_me                 #2
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L15, L16
  L15                 try                      #2, L16
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L16                 init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      proceed                  
  foo                 switch_on_reg            #1, L17, L18
  L17                 try                      #2, L18
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L18                 init_variable_t          A3, Y1
                      init_variable_t          A4, Y3
                      init_variable_t          A5, Y2
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_value_t              A5, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L19, L20
  L19                 try                      #2, L20
                      trust_ext                #2, #1
  L20                 try_me_else              #2, L21
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  L21                 trust_me                 #2
                      init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      put_value_t              A4, A2
                      head_normalize_t         A2
                      execute_name             foo
  foo                 switch_on_reg            #1, L22, L23
  L22                 try                      #2, L23
                      trust_ext                #2, #1
                      try_me_else              #0, L10
  L23                 init_variable_t          A3, Y2
                      init_variable_t          A4, Y1
                      pattern_unify_t          A3, A1
                      pattern_unify_t          A4, A2
                      finish_unify             
                      proceed                  
  
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
  
  String table:
  
  Implication Tables:
  0:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  1:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  2:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  3:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  4:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  5:
    Predicate definitions possibly extending previous ones: 0
    Find function type: hash
    In-core table size: 1
     <hidden const #2>
  6:
    Predicate definitions possibly extending previous ones: 1
     foo
    Find function type: hash
    In-core table size: 1
     foo
  
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
