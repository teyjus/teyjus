  $ tjcc comma
  $ tjdis comma.lpo
  Disassembling from bytecode file: comma.lpo
  Bytecode version: 2
  Module name: comma
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #1
                      get_list                 A1
                      unify_m_constant         foo1
                      unify_variable_t         A255
                      get_list                 A255
                      unify_m_constant         foo2
                      unify_nil                
                      finish_unify             
                      call_name                #0, foo1
                      deallocate               
                      execute_name             foo2
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: (list o) -> o
  1: o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
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
  $ tjlink comma
  $ tjdis comma.lp
  Disassembling from bytecode file: comma.lp
  Bytecode version: 3
  Module name: comma
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  allocate                 #1
                      get_list                 A1
                      unify_m_constant         foo1
                      unify_variable_t         A255
                      get_list                 A255
                      unify_m_constant         foo2
                      unify_nil                
                      finish_unify             
                      call_name                #0, foo1
                      deallocate               
                      execute_name             foo2
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: (list o) -> o
  1: o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  1: foo1 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  2: foo2 (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
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
