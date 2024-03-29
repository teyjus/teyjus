  $ tjcc fixity_prec
  $ tjdis fixity_prec.lpo
  Disassembling from bytecode file: fixity_prec.lpo
  Bytecode version: 2
  Module name: fixity_prec
  
  LABEL               INSTRUCTION              OPERANDS
  
  L7                  fail                     
  foo                 switch_on_reg            #1, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L6                  switch_on_term           L8, L9, L7, L8
  L8                  try_me_else              #1, L10
  L4                  get_m_structure          A1, minus, #2
                      unify_variable_t         A255
                      unify_integer            7
                      get_m_structure          A255, plus, #2
                      unify_integer            3
                      unify_variable_t         A255
                      get_m_structure          A255, divide, #2
                      unify_variable_t         A255
                      unify_integer            6
                      get_m_structure          A255, mul, #2
                      unify_integer            2
                      unify_integer            5
                      finish_unify             
                      proceed                  
  L10                 retry_me_else            #1, L11
  L3                  get_m_structure          A1, **, #2
                      unify_variable_t         A255
                      unify_integer            2
                      get_m_structure          A255, **, #2
                      unify_integer            4
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L11                 retry_me_else            #1, L12
  L18                 get_m_structure          A1, inc, #1
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L12                 retry_me_else            #1, L13
  L19                 get_m_structure          A1, inc, #1
                      unify_variable_t         A255
                      get_m_structure          A255, inc, #1
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L13                 retry_me_else            #1, L14
  L1                  get_m_structure          A1, inc_non, #1
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L14                 retry_me_else            #1, L15
  L16                 get_m_structure          A1, preinc, #1
                      unify_integer            4
                      finish_unify             
                      proceed                  
  L15                 trust_me                 #1
  L17                 get_m_structure          A1, preinc, #1
                      unify_variable_t         A255
                      get_m_structure          A255, preinc, #1
                      unify_integer            4
                      finish_unify             
                      proceed                  
  L0                  try                      #1, L16
                      trust                    #1, L17
  L2                  try                      #1, L18
                      trust                    #1, L19
  L9                  switch_on_constant       #5, <hash #0>
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> o
  1: int -> int -> int
  2: int -> int
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: plus (infixl, precedence 6)
      Env Size: 0, Type Skeleton: #1
  2: mul (infixl, precedence 7)
      Env Size: 0, Type Skeleton: #1
  3: divide (infixl, precedence 7)
      Env Size: 0, Type Skeleton: #1
  4: minus (infixl, precedence 6)
      Env Size: 0, Type Skeleton: #1
  5: ** (infixl, precedence 9)
      Env Size: 0, Type Skeleton: #1
  6: inc (postfixl, precedence 10)
      Env Size: 0, Type Skeleton: #2
  7: inc_non (postfix, precedence 10)
      Env Size: 0, Type Skeleton: #2
  8: preinc (prefixr, precedence 10)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 5
      Constants:
      preinc -> L0
      inc_non -> L1
      inc -> L2
      ** -> L3
      minus -> L4
  
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
  $ tjlink fixity_prec
  $ tjdis fixity_prec.lp
  Disassembling from bytecode file: fixity_prec.lp
  Bytecode version: 3
  Module name: fixity_prec
  
  LABEL               INSTRUCTION              OPERANDS
  
  L7                  fail                     
  foo                 switch_on_reg            #1, L5, L6
  L5                  try                      #1, L6
                      trust_ext                #1, #1
                      try_me_else              #0, L7
  L6                  switch_on_term           L8, L9, L7, L8
  L8                  try_me_else              #1, L10
  L4                  get_m_structure          A1, minus, #2
                      unify_variable_t         A255
                      unify_integer            7
                      get_m_structure          A255, plus, #2
                      unify_integer            3
                      unify_variable_t         A255
                      get_m_structure          A255, divide, #2
                      unify_variable_t         A255
                      unify_integer            6
                      get_m_structure          A255, mul, #2
                      unify_integer            2
                      unify_integer            5
                      finish_unify             
                      proceed                  
  L10                 retry_me_else            #1, L11
  L3                  get_m_structure          A1, **, #2
                      unify_variable_t         A255
                      unify_integer            2
                      get_m_structure          A255, **, #2
                      unify_integer            4
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L11                 retry_me_else            #1, L12
  L18                 get_m_structure          A1, inc, #1
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L12                 retry_me_else            #1, L13
  L19                 get_m_structure          A1, inc, #1
                      unify_variable_t         A255
                      get_m_structure          A255, inc, #1
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L13                 retry_me_else            #1, L14
  L1                  get_m_structure          A1, inc_non, #1
                      unify_integer            3
                      finish_unify             
                      proceed                  
  L14                 retry_me_else            #1, L15
  L16                 get_m_structure          A1, preinc, #1
                      unify_integer            4
                      finish_unify             
                      proceed                  
  L15                 trust_me                 #1
  L17                 get_m_structure          A1, preinc, #1
                      unify_variable_t         A255
                      get_m_structure          A255, preinc, #1
                      unify_integer            4
                      finish_unify             
                      proceed                  
  L0                  try                      #1, L16
                      trust                    #1, L17
  L2                  try                      #1, L18
                      trust                    #1, L19
  L9                  switch_on_constant       #5, <hash #0>
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> o
  1: int -> int -> int
  2: int -> int
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: plus (infixl, precedence 6)
      Env Size: 0, Type Skeleton: #1
  2: mul (infixl, precedence 7)
      Env Size: 0, Type Skeleton: #1
  3: divide (infixl, precedence 7)
      Env Size: 0, Type Skeleton: #1
  4: minus (infixl, precedence 6)
      Env Size: 0, Type Skeleton: #1
  5: ** (infixl, precedence 9)
      Env Size: 0, Type Skeleton: #1
  6: inc (postfixl, precedence 10)
      Env Size: 0, Type Skeleton: #2
  7: inc_non (postfix, precedence 10)
      Env Size: 0, Type Skeleton: #2
  8: preinc (prefixr, precedence 10)
      Env Size: 0, Type Skeleton: #2
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  0:
      Table size: 5
      Constants:
      preinc -> L0
      inc_non -> L1
      inc -> L2
      ** -> L3
      minus -> L4
  
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
