type symbol = Symbol.symbol

val pervasiveKinds : Absyn.akind Table.SymbolTable.t
val pervasiveConstants : Absyn.aconstant Table.SymbolTable.t
val pervasiveTypeAbbrevs : Absyn.atypeabbrev Table.SymbolTable.t

val genericApplyConstant : Absyn.aconstant
val commaConstant : Absyn.aconstant
val consConstant : Absyn.aconstant
val nilConstant : Absyn.aconstant
val allConstant : Absyn.aconstant
val implicationConstant : Absyn.aconstant

(*
val nextId : unit -> int
val appFixity : Absyn.afixity
val appPrec : int
*)
