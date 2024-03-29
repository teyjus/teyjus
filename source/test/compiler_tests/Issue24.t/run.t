  $ tjcc post
  $ tjdis post.lpo
  Disassembling from bytecode file: post.lpo
  Bytecode version: 2
  Module name: post
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  two_corresp         switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #4
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #1>
                      set_univ_tag             Y3, <hidden const #0>
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      globalize_pt             Y1, A255
                      globalize_pt             Y2, A254
                      globalize_pt             Y3, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      put_app                  A1, A255, #2
                      set_value_t              A254
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y1, A255
                      copy_value               Y3, A254
                      copy_value               Y2, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      put_app                  A2, A255, #2
                      set_value_p              Y2
                      set_value_t              A254
                      head_normalize_t         A2
                      call_builtin             #3, #3
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      copy_value               Y1, A255
                      put_app                  A1, A255, #2
                      set_value_p              Y2
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y2, A254
                      put_variable_te          A252, A253
                      put_app                  A252, A253, #2
                      set_value_p              Y2
                      set_index                #1
                      put_app                  A255, A254, #1
                      set_value_t              A252
                      put_lambda               A2, A255, #1
                      call_builtin             #2, #3
                      decr_universe            
                      deallocate               
                      proceed                  
  three_corresp       switch_on_reg            #2, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #2
                      try_me_else              #0, L2
  L4                  allocate                 #4
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #3>
                      set_univ_tag             Y3, <hidden const #2>
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      globalize_pt             Y1, A255
                      globalize_pt             Y2, A254
                      globalize_pt             Y3, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      put_app                  A1, A255, #3
                      set_value_t              A254
                      set_value_p              Y3
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y1, A255
                      copy_value               Y3, A254
                      copy_value               Y2, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      copy_value               Y3, A253
                      copy_value               Y2, A252
                      put_app                  A251, A252, #1
                      set_index                #1
                      put_app                  A252, A253, #1
                      set_value_t              A251
                      put_lambda               A253, A252, #1
                      put_index                A252, #1
                      put_lambda               A251, A252, #1
                      put_app                  A2, A255, #3
                      set_value_t              A254
                      set_value_t              A253
                      set_value_t              A251
                      head_normalize_t         A2
                      call_builtin             #3, #3
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      copy_value               Y1, A255
                      put_app                  A1, A255, #3
                      set_value_p              Y2
                      set_value_p              Y2
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y2, A254
                      put_variable_te          A252, A253
                      put_app                  A252, A253, #2
                      set_value_p              Y2
                      set_index                #1
                      put_app                  A255, A254, #1
                      set_value_t              A252
                      put_lambda               A2, A255, #1
                      call_builtin             #2, #3
                      decr_universe            
                      deallocate               
                      proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: ((i -> i) -> (i -> i) -> i -> i) -> o
  1: ((i -> i) -> (i -> i) -> (i -> i) -> i -> i) -> o
  2: i -> i
  
  Global constant table: 
  0: two_corresp (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: three_corresp (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #2
  1:  Type Skeleton: #2
  2:  Type Skeleton: #2
  3:  Type Skeleton: #2
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 2
     two_corresp
     three_corresp
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 2
     two_corresp
     three_corresp
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink post
  $ tjdis post.lp
  Disassembling from bytecode file: post.lp
  Bytecode version: 3
  Module name: post
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  two_corresp         switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #4
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #1>
                      set_univ_tag             Y3, <hidden const #0>
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      globalize_pt             Y1, A255
                      globalize_pt             Y2, A254
                      globalize_pt             Y3, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      put_app                  A1, A255, #2
                      set_value_t              A254
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y1, A255
                      copy_value               Y3, A254
                      copy_value               Y2, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      put_app                  A2, A255, #2
                      set_value_p              Y2
                      set_value_t              A254
                      head_normalize_t         A2
                      call_builtin             #3, #3
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      copy_value               Y1, A255
                      put_app                  A1, A255, #2
                      set_value_p              Y2
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y2, A254
                      put_variable_te          A252, A253
                      put_app                  A252, A253, #2
                      set_value_p              Y2
                      set_index                #1
                      put_app                  A255, A254, #1
                      set_value_t              A252
                      put_lambda               A2, A255, #1
                      call_builtin             #2, #3
                      decr_universe            
                      deallocate               
                      proceed                  
  three_corresp       switch_on_reg            #2, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #2
                      try_me_else              #0, L2
  L4                  allocate                 #4
                      get_variable_p           Y1, A1
                      incr_universe            
                      set_univ_tag             Y2, <hidden const #3>
                      set_univ_tag             Y3, <hidden const #2>
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      globalize_pt             Y1, A255
                      globalize_pt             Y2, A254
                      globalize_pt             Y3, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      put_app                  A1, A255, #3
                      set_value_t              A254
                      set_value_p              Y3
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y1, A255
                      copy_value               Y3, A254
                      copy_value               Y2, A253
                      put_app                  A252, A253, #1
                      set_index                #1
                      put_app                  A253, A254, #1
                      set_value_t              A252
                      put_lambda               A254, A253, #1
                      copy_value               Y3, A253
                      copy_value               Y2, A252
                      put_app                  A251, A252, #1
                      set_index                #1
                      put_app                  A252, A253, #1
                      set_value_t              A251
                      put_lambda               A253, A252, #1
                      put_index                A252, #1
                      put_lambda               A251, A252, #1
                      put_app                  A2, A255, #3
                      set_value_t              A254
                      set_value_t              A253
                      set_value_t              A251
                      head_normalize_t         A2
                      call_builtin             #3, #3
                      put_type_arrow           A3
                      set_type_constant        i
                      set_type_constant        i
                      copy_value               Y1, A255
                      put_app                  A1, A255, #3
                      set_value_p              Y2
                      set_value_p              Y2
                      set_value_p              Y2
                      head_normalize_t         A1
                      copy_value               Y2, A254
                      put_variable_te          A252, A253
                      put_app                  A252, A253, #2
                      set_value_p              Y2
                      set_index                #1
                      put_app                  A255, A254, #1
                      set_value_t              A252
                      put_lambda               A2, A255, #1
                      call_builtin             #2, #3
                      decr_universe            
                      deallocate               
                      proceed                  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: ((i -> i) -> (i -> i) -> i -> i) -> o
  1: ((i -> i) -> (i -> i) -> (i -> i) -> i -> i) -> o
  2: i -> i
  
  Global constant table: 
  0: two_corresp (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: three_corresp (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  
  Hidden constant table: 
  0:  Type Skeleton: #2
  1:  Type Skeleton: #2
  2:  Type Skeleton: #2
  3:  Type Skeleton: #2
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 2
     two_corresp
     three_corresp
      Local constant table: 0
      Find function type: hash
      Search table: 2
     two_corresp
     three_corresp
  
  Accumulated tables:
  
  Imported tables:
