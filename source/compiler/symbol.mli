(**********************************************************************
*Symbol Module:
*	Implements a simple module mapping strings to unique identifiers.
*	Uses the standard library Hashtbl module.
**********************************************************************)
module type SYMBOL =
sig
  type symbol
  val symbol : string -> symbol
  val name : symbol -> string
end
