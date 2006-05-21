(**********************************************************************
*Table:
*	Tables implemented using balanced binary trees.
**********************************************************************)
module OrderedSymbol =
struct
	type t = Symbol.symbol
	let compare = 
		fun s1 s2 ->
			if((Symbol.id s1) < (Symbol.id s2)) then
				-1
			else if((Symbol.id s1) > (Symbol.id s2)) then
				1
			else
				0
end
module SymbolTable = Map.Make(OrderedSymbol)
