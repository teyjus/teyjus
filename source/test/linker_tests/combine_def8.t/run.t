  $ tjcc acc
  $ tjdis acc.lpo
  Disassembling from bytecode file: acc.lpo
  Bytecode version: 2
  Module name: acc
  
  LABEL               INSTRUCTION              OPERANDS
  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  0: foo (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Local constant table: 
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 0
  
  Accumulated tables:
  
  Imported tables:
  $ tjcc top
  $ tjdis top.lpo
  Disassembling from bytecode file: top.lpo
  Bytecode version: 2
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  
  Global constant table: 
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 0, Type Skeleton: #0
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Module table:
    Predicate definitions possibly extending previous ones: 0
    Exportdef predicates: 0
    Local predicates: 0
    Find function type: hash
    In-core table size: 0
  
  Accumulated tables:
  0: acc
      Kind renamings:
      i -> i
      Constant renamings:
      foo -> <local const #0>
  
  Imported tables:
  $ tjlink top
  $ tjdis top.lp
  Disassembling from bytecode file: top.lp
  Bytecode version: 3
  Module name: top
  
  LABEL               INSTRUCTION              OPERANDS
  
  
  Global kind table:
  0: i/0
  
  Local kind table:
  
  Type skeleton table:
  0: i -> o
  1: i -> o
  
  Global constant table: 
  
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
      Next clause table: 0
      Local constant table: 1
     <local const #0>
      Find function type: hash
      Search table: 0
  
  Accumulated tables:
  
  Imported tables:
