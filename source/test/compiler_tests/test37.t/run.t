  $ tjcc sig_in_mod
  $ tjdis sig_in_mod.lpo
  Disassembling from bytecode file: sig_in_mod.lpo
  Bytecode version: 2
  Module name: sig_in_mod
  
  LABEL               INSTRUCTION              OPERANDS
  
  L0                  fail                     
                      try_me_else              #0, L0
  <local const #0>    put_value_t              A3, A4
                      put_value_t              A1, A5
                      put_value_t              A2, A6
                      put_type_const           A3, int
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_m_const              A255, %i+
                      put_app                  A2, A255, #2
                      globalize_t              A5
                      set_value_t              A5
                      globalize_t              A6
                      set_value_t              A6
                      builtin                  #1
  double              switch_on_reg            #1, L1, L2
  L1                  try                      #2, L2
                      trust_ext                #2, #1
                      try_me_else              #0, L0
  L2                  put_value_t              A2, A3
                      head_normalize_t         A1
                      put_value_t              A1, A2
                      head_normalize_t         A2
                      head_normalize_t         A3
                      execute_link_only        <local const #0>
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> int -> int -> o
  1: int -> int -> o
  
  Global constant table: 
  0: double (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 1
     double
    Exportdef predicates: 0
    Local predicates: 1
     <local const #0>
    Find function type: hash
    In-core table size: 2
     <local const #0>
     double
  
  Accumulated tables:
  
  Imported tables:
  $ tjlink sig_in_mod
  $ tjdis sig_in_mod.lp
  Disassembling from bytecode file: sig_in_mod.lp
  Bytecode version: 3
  Module name: sig_in_mod
  
  LABEL               INSTRUCTION              OPERANDS
  
  L0                  fail                     
                      try_me_else              #0, L0
  <local const #0>    put_value_t              A3, A4
                      put_value_t              A1, A5
                      put_value_t              A2, A6
                      put_type_const           A3, int
                      put_value_t              A4, A1
                      head_normalize_t         A1
                      put_m_const              A255, %i+
                      put_app                  A2, A255, #2
                      globalize_t              A5
                      set_value_t              A5
                      globalize_t              A6
                      set_value_t              A6
                      builtin                  #1
  double              switch_on_reg            #1, L1, L2
  L1                  try                      #2, L2
                      trust_ext                #2, #1
                      try_me_else              #0, L0
  L2                  put_value_t              A2, A3
                      head_normalize_t         A1
                      put_value_t              A1, A2
                      head_normalize_t         A2
                      head_normalize_t         A3
                      execute                  <local const #0>
  
  Global kind table:
  
  Local kind table:
  
  Type skeleton table:
  0: int -> int -> int -> o
  1: int -> int -> o
  
  Global constant table: 
  0: double (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #1
  
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
     double
      Local constant table: 1
     <local const #0>
      Find function type: hash
      Search table: 2
     <local const #0>
     double
  
  Accumulated tables:
  
  Imported tables:
