(**********************************************************************
*Table:
*
**********************************************************************)
module SymbolTable : Map.S with type key = Symbol.symbol

val find : SymbolTable.key -> 'a SymbolTable.t -> 'a option
val add : SymbolTable.key -> 'a -> 'a SymbolTable.t -> 'a SymbolTable.t
val remove : SymbolTable.key -> 'a SymbolTable.t -> 'a SymbolTable.t
val iter : (SymbolTable.key -> 'a -> unit) -> 'a SymbolTable.t -> unit

