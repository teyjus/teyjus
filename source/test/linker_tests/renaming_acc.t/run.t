  $ tjcc acc
  $ tjdis acc.lpo
  Disassembling from bytecode file: acc.lpo
  Bytecode version: 2
  Module name: acc
  
  LABEL               INSTRUCTION              OPERANDS
  
  
  Global kind table:
  0: k/2
  1: j/1
  2: i/0
  
  Local kind table:
  0: /3
  
  Type skeleton table:
  0: A -> A -> A -> A
  1: A
  2: A -> A
  3: A -> A -> A
  
  Global constant table: 
  0: a (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #1
  1: b (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #2
  2: c (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #3
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  
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
  0: /2
  1: /1
  
  Type skeleton table:
  0: A -> A
  1: A -> A -> A
  2: A
  
  Global constant table: 
  0: a (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #2
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #1
  
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
      k -> <local kind #0>
      i -> i
      j -> <local kind #1>
      Constant renamings:
      c -> <local const #1>
      a -> a
      b -> <local const #0>
  
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
  0: /2
  1: /1
  2: /3
  
  Type skeleton table:
  0: A -> A
  1: A -> A -> A
  2: A
  3: A -> A -> A -> A
  4: A
  5: A -> A
  6: A -> A -> A
  
  Global constant table: 
  0: a (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #2
  
  Local constant table: 
  0:  (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #0
  1:  (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #1
  2:  (No Fixity, precedence 0)
      Env Size: 1, Type Skeleton: #3
  
  Hidden constant table: 
  
  String table:
  
  Implication Tables:
  
  Hash tables:
  
  Import tables:
  
    Import table:
      number of code segments: 0
      Next clause table: 0
      Local constant table: 3
     <local const #0>
     <local const #1>
     <local const #2>
      Find function type: hash
      Search table: 0
  
  Accumulated tables:
  
  Imported tables:
