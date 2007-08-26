(**********************************************************************
*Table:
*
**********************************************************************)
module SymbolTable : Map.S with type key = Symbol.symbol

type 'a symboltable = 'a SymbolTable.t

val find : SymbolTable.key -> 'a SymbolTable.t -> 'a option
val add : SymbolTable.key -> 'a -> 'a SymbolTable.t -> 'a SymbolTable.t
val remove : SymbolTable.key -> 'a SymbolTable.t -> 'a SymbolTable.t
val iter : (SymbolTable.key -> 'a -> unit) -> 'a SymbolTable.t -> unit
val fold : (SymbolTable.key -> 'a -> 'b -> 'b) -> 'a SymbolTable.t -> 'b -> 'b
val empty : 'a symboltable

val printTable : ('a -> string) -> 'a SymbolTable.t -> unit 
