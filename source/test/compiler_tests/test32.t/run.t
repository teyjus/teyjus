  $ tjcc typeset
  $ tjdis typeset.lpo
  Disassembling from bytecode file: typeset.lpo
  Bytecode version: 2
  Module name: typeset
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #2
                      put_value_t              A1, A4
                      put_type_const           A3, int
                      put_variable_p           Y1, A1
                      put_m_const              A255, %i+
                      put_app                  A2, A255, #2
                      globalize_t              A4
                      set_value_t              A4
                      set_integer              1
                      call_builtin             #1, #1
                      put_unsafe_value         Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute                  L3
  L3                  try_me_else              #1, L4
                      head_normalize_t         A1
                      execute_name             bar
  L4                  trust_me                 #1
                      head_normalize_t         A1
                      execute_name             foo
  
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
  $ tjlink typeset
  $ tjdis typeset.lp
  Disassembling from bytecode file: typeset.lp
  Bytecode version: 3
  Module name: typeset
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #2
                      put_value_t              A1, A4
                      put_type_const           A3, int
                      put_variable_p           Y1, A1
                      put_m_const              A255, %i+
                      put_app                  A2, A255, #2
                      globalize_t              A4
                      set_value_t              A4
                      set_integer              1
                      call_builtin             #1, #1
                      put_unsafe_value         Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute                  L3
  L3                  try_me_else              #1, L4
                      head_normalize_t         A1
                      execute_name             bar
  L4                  trust_me                 #1
                      head_normalize_t         A1
                      execute_name             foo
  
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
