type symbol = Symbol.symbol

val pervasiveKinds : Absyn.akind Table.SymbolTable.t
val pervasiveConstants : Absyn.aconstant Table.SymbolTable.t
val pervasiveTypeAbbrevs : Absyn.atypeabbrev Table.SymbolTable.t

val genericApplyConstant : Absyn.aconstant
val commaConstant : Absyn.aconstant
val consConstant : Absyn.aconstant
val nilConstant : Absyn.aconstant
val andConstant : Absyn.aconstant
val orConstant : Absyn.aconstant
val colonDashConstant : Absyn.aconstant
val someConstant : Absyn.aconstant
val allConstant : Absyn.aconstant
val implicationConstant : Absyn.aconstant
val cutConstant : Absyn.aconstant

val implicationTerm : Absyn.aterm
val andTerm : Absyn.aterm

val boolKind : Absyn.akind

(*
val nextId : unit -> int
val appFixity : Absyn.afixity
val appPrec : int
*)
