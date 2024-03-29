  $ tjcc existentialvars
  $ tjdis existentialvars.lpo
  Disassembling from bytecode file: existentialvars.lpo
  Bytecode version: 2
  Module name: existentialvars
  
  LABEL               INSTRUCTION              OPERANDS
  
  L6                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #3
                      get_variable_p           Y1, A1
                      tag_exists_p             Y2
                      push_impl_point          #2, <impl #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      tag_exists_p             Y1
                      push_impl_point          #1, <impl #1>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L3                  trust_me                 #1
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
  bar                 switch_on_reg            #1, L4, L5
  L4                  try                      #1, L5
                      trust_ext                #1, #1
                      try_me_else              #0, L6
  L5                  init_variable_t          A2, Y1
                      init_variable_t          A3, Y2
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      execute_name             bar
  bar                 switch_on_reg            #1, L7, L8
  L7                  try                      #1, L8
                      trust_ext                #1, #1
                      try_me_else              #0, L6
  L8                  init_variable_t          A2, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A2, A1
                      head_normalize_t         A1
                      execute_name             foo
  bar                 switch_on_reg            #1, L9, L10
  L9                  try                      #1, L10
                      trust_ext                #1, #1
                      try_me_else              #0, L6
  L10                 init_variable_t          A2, Y2
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
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
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
  
  L6                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
  L1                  try_me_else              #1, L2
                      allocate                 #3
                      get_variable_p           Y1, A1
                      tag_exists_p             Y2
                      push_impl_point          #2, <impl #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #2, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L2                  retry_me_else            #1, L3
                      allocate                 #2
                      tag_exists_p             Y1
                      push_impl_point          #1, <impl #1>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_name                #1, bar
                      pop_impl_point           
                      deallocate               
                      proceed                  
  L3                  trust_me                 #1
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
  bar                 switch_on_reg            #1, L4, L5
  L4                  try                      #1, L5
                      trust_ext                #1, #1
                      try_me_else              #0, L6
  L5                  init_variable_t          A2, Y1
                      init_variable_t          A3, Y2
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A3, A1
                      head_normalize_t         A1
                      execute_name             bar
  bar                 switch_on_reg            #1, L7, L8
  L7                  try                      #1, L8
                      trust_ext                #1, #1
                      try_me_else              #0, L6
  L8                  init_variable_t          A2, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      put_value_t              A2, A1
                      head_normalize_t         A1
                      execute_name             foo
  bar                 switch_on_reg            #1, L9, L10
  L9                  try                      #1, L10
                      trust_ext                #1, #1
                      try_me_else              #0, L6
  L10                 init_variable_t          A2, Y2
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
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: bar (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
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
