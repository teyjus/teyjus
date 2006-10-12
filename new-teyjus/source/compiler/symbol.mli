(**********************************************************************
*Symbol Module:
* Implements a simple module mapping strings to unique identifiers.
* Uses the standard library Hashtbl module.
**********************************************************************)
type symbol
val symbol : string -> symbol
val name : symbol -> string
val id : symbol -> int
