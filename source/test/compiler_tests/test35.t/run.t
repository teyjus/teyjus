  $ tjcc bad_module
  $ tjdis bad_module.lpo
  Disassembling from bytecode file: bad_module.lpo
  Bytecode version: 2
  Module name: bad_module
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  value               switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
                      try_me_else              #0, L2
  L1                  head_normalize_t         A1
                      put_variable_t           A255, A2
                      builtin                  #31
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  
  Global constant table: 
  0: value (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     value
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 1
     value
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink bad_module
  $ tjdis bad_module.lp
  Disassembling from bytecode file: bad_module.lp
  Bytecode version: 3
  Module name: bad_module
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  value               switch_on_reg            #1, L0, L1
  L0                  try                      #2, L1
                      trust_ext                #2, #1
                      try_me_else              #0, L2
  L1                  head_normalize_t         A1
                      put_variable_t           A255, A2
                      builtin                  #31
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: A -> o
  
  Global constant table: 
  0: value (No Fixity, precedence 0)
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
     value
      Local constant table: 0
      Find function type: hash
      Search table: 1
     value
  
  Accumulated tables:
  
  Imported tables:
