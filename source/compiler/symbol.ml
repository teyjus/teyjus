(**********************************************************************
*Symbol Module:
*	Implements a simple module mapping strings to unique identifiers.
*	Uses the standard library Hashtbl module.
**********************************************************************)
type symbol = string * int

let hashtable = Hashtbl.create 1

let nextsym = ref 0

let name = fun(s,n) -> s
let symbol = fun(s: string) ->
  try
  	(s, (Hashtbl.find hashtable s))
  with
  	Not_found ->	let id : int = !nextsym
									in
										begin
											nextsym := id + 1;
											(Hashtbl.add hashtable s id);
											(s,id)
										end
let id = fun(s,n) -> n
