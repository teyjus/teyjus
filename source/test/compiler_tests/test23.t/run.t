  $ tjcc foo
  $ tjdis foo.lpo
  Disassembling from bytecode file: foo.lpo
  Bytecode version: 2
  Module name: foo
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  bar                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
                      try_me_else              #0, L2
  L1                  get_type_constant        A2, int
                      put_m_const              A255, %i+
                      put_app                  A254, A255, #2
                      set_index                #1
                      set_integer              3
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      proceed                  
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: (A -> A) -> o
  
  Global constant table: 
  0: bar (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     bar
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 1
     bar
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink foo
  $ tjdis foo.lp
  Disassembling from bytecode file: foo.lp
  Bytecode version: 3
  Module name: foo
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  bar                 switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
                      try_me_else              #0, L2
  L1                  get_type_constant        A2, int
                      put_m_const              A255, %i+
                      put_app                  A254, A255, #2
                      set_index                #1
                      set_integer              3
                      put_lambda               A255, A254, #1
                      pattern_unify_t          A255, A1
                      finish_unify             
                      proceed                  
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: (A -> A) -> o
  
  Global constant table: 
  0: bar (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 1
     bar
      Local constant table: 0
      Find function type: hash
      Search table: 1
     bar
  
  Accumulated tables:
  
  Imported tables:
