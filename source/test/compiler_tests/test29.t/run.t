  $ tjcc overloadOp
  $ tjdis overloadOp.lpo
  Disassembling from bytecode file: overloadOp.lpo
  Bytecode version: 2
  Module name: overloadOp
  
  LABEL               INSTRUCTION              OPERANDS
  
  L5                  fail                     
  foo                 switch_on_reg            #1, L3, L4
  L3                  try                      #2, L4
                      trust_ext                #2, #1
                      try_me_else              #0, L5
  L4                  switch_on_term           L6, L7, L5, L6
  L6                  try_me_else              #2, L8
  L11                 get_type_constant        A2, int
                      get_m_structure          A1, %i+, #2
                      unify_variable_t         A255
                      unify_variable_t         A255
                      finish_unify             
                      proceed                  
  L8                  retry_me_else            #2, L9
  L12                 get_type_constant        A2, int
                      get_m_structure          A1, %i+, #2
                      unify_variable_t         A255
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L9                  retry_me_else            #2, L10
  L1                  get_type_constant        A2, real
                      get_m_structure          A1, %r-, #2
                      unify_float              3.5
                      unify_variable_t         A255
                      finish_unify             
                      proceed                  
  L10                 trust_me                 #2
  L0                  get_type_constant        A2, o
                      get_m_structure          A1, %s<, #2
                      unify_string             <string #0>
                      unify_variable_t         A255
                      finish_unify             
                      proceed                  
  L2                  try                      #2, L11
                      trust                    #2, L12
  L7                  switch_on_constant       #3, <hash #0>
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  0: hello
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 3
      Constants:
      %s< -> L0
      %r- -> L1
      %i+ -> L2
  
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
  $ tjlink overloadOp
  $ tjdis overloadOp.lp
  Disassembling from bytecode file: overloadOp.lp
  Bytecode version: 3
  Module name: overloadOp
  
  LABEL               INSTRUCTION              OPERANDS
  
  L5                  fail                     
  foo                 switch_on_reg            #1, L3, L4
  L3                  try                      #2, L4
                      trust_ext                #2, #1
                      try_me_else              #0, L5
  L4                  switch_on_term           L6, L7, L5, L6
  L6                  try_me_else              #2, L8
  L11                 get_type_constant        A2, int
                      get_m_structure          A1, %i+, #2
                      unify_variable_t         A255
                      unify_variable_t         A255
                      finish_unify             
                      proceed                  
  L8                  retry_me_else            #2, L9
  L12                 get_type_constant        A2, int
                      get_m_structure          A1, %i+, #2
                      unify_variable_t         A255
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L9                  retry_me_else            #2, L10
  L1                  get_type_constant        A2, real
                      get_m_structure          A1, %r-, #2
                      unify_float              3.5
                      unify_variable_t         A255
                      finish_unify             
                      proceed                  
  L10                 trust_me                 #2
  L0                  get_type_constant        A2, o
                      get_m_structure          A1, %s<, #2
                      unify_string             <string #0>
                      unify_variable_t         A255
                      finish_unify             
                      proceed                  
  L2                  try                      #2, L11
                      trust                    #2, L12
  L7                  switch_on_constant       #3, <hash #0>
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  0: hello
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 3
      Constants:
      %s< -> L0
      %r- -> L1
      %i+ -> L2
  
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
