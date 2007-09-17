(**********************************************************************
*Symbol Module:
* Implements a simple module mapping strings to unique identifiers.
* Uses the standard library Hashtbl module.
**********************************************************************)
type symbol

(**********************************************************************
*symbol:
* Produces a new symbol from the given string.  The ID of symbols with
* a given name will be the same across calls to this function.
**********************************************************************)
val symbol : string -> symbol

(**********************************************************************
*name:
* Returns the name of the given symbol.
**********************************************************************)
val name : symbol -> string

(**********************************************************************
*id:
* Returns an integer representing the given symbol.
**********************************************************************)
val id : symbol -> int

(**********************************************************************
*equal:
* Compares the ids of two symbols to see if the symbols are equal.
**********************************************************************)
val equal : symbol -> symbol -> bool

(**********************************************************************
*generate:
* Produces a fresh symbol.
**********************************************************************)
val generate : unit -> symbol

(**********************************************************************
*generateName:
* Produces a fresh symbol whose name is similar to the given name.
**********************************************************************)
val generateName : string -> symbol
