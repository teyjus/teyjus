  $ tjcc foo
  $ tjdis foo.lpo
  Disassembling from bytecode file: foo.lpo
  Bytecode version: 2
  Module name: foo
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #3
                      put_value_t              A1, A4
                      put_type_const           A3, int
                      put_variable_p           Y1, A1
                      put_m_const              A255, %i-
                      put_app                  A2, A255, #2
                      globalize_t              A4
                      set_value_t              A4
                      set_integer              1
                      call_builtin             #2, #1
                      push_impl_point          #2, <impl #0>
                      put_variable_p           Y2, A1
                      call_name                #2, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  bar                 switch_on_reg            #1, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L4                  init_variable_t          A2, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      proceed                  
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> o
  
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
  $ tjlink foo
  $ tjdis foo.lp
  Disassembling from bytecode file: foo.lp
  Bytecode version: 3
  Module name: foo
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #3
                      put_value_t              A1, A4
                      put_type_const           A3, int
                      put_variable_p           Y1, A1
                      put_m_const              A255, %i-
                      put_app                  A2, A255, #2
                      globalize_t              A4
                      set_value_t              A4
                      set_integer              1
                      call_builtin             #2, #1
                      push_impl_point          #2, <impl #0>
                      put_variable_p           Y2, A1
                      call_name                #2, foo
                      pop_impl_point           
                      deallocate               
                      proceed                  
  bar                 switch_on_reg            #1, L3, L4
  L3                  try                      #1, L4
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L4                  init_variable_t          A2, Y1
                      pattern_unify_t          A2, A1
                      finish_unify             
                      proceed                  
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> o
  
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
