  $ tjcc linkonly
  $ tjdis linkonly.lpo
  Disassembling from bytecode file: linkonly.lpo
  Bytecode version: 2
  Module name: linkonly
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
  <local const #0>    try_me_else              #1, L3
                      head_normalize_t         A1
                      execute_link_only        <local const #0>
  L3                  trust_me                 #1
                      allocate                 #2
                      get_variable_p           Y1, A1
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call_link_only           #1, <local const #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     foo
    Exportdef predicates: 0
    Local predicates: 1
     <local const #0>
    Find function type: hash
    In-core table size: 2
     <local const #0>
     foo
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink linkonly
  $ tjdis linkonly.lp
  Disassembling from bytecode file: linkonly.lp
  Bytecode version: 3
  Module name: linkonly
  
  LABEL               INSTRUCTION              OPERANDS
  
  L2                  fail                     
  foo                 switch_on_reg            #1, L0, L1
  L0                  try                      #1, L1
                      trust_ext                #1, #1
                      try_me_else              #0, L2
  L1                  proceed                  
  <local const #0>    try_me_else              #1, L3
                      head_normalize_t         A1
                      execute                  <local const #0>
  L3                  trust_me                 #1
                      allocate                 #2
                      get_variable_p           Y1, A1
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      call                     #1, <local const #0>
                      put_value_p              Y1, A1
                      head_normalize_t         A1
                      deallocate               
                      execute_name             foo
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 1
     foo
      Local constant table: 1
     <local const #0>
      Find function type: hash
      Search table: 2
     <local const #0>
     foo
  
  Accumulated tables:
  
  Imported tables:
