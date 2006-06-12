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

let find =
	fun k table ->
		try
			let v = SymbolTable.find k table in
			Some v
		with Not_found -> None

let add =
	fun k v table ->
		(SymbolTable.add k v table)

let remove =
	fun k table ->
		(SymbolTable.remove k table)

let iter = 
	fun f table ->
		SymbolTable.iter f table
